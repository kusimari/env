#!/bin/bash
#
# AWS OIDC Setup Script for GitHub Actions Integration
# Run this once in AWS CloudShell or with AWS CLI configured
# This enables GitHub Actions to authenticate to AWS without storing credentials
#

set -euo pipefail

# Configuration (matches cloud/config.yaml)
OIDC_PROVIDER_NAME="env-github-oidc"
ROLE_NAME="env-cloud-deployer-role"
POLICY_NAME="env-cloud-deployer-policy"
SG_NAME="env-cloud-ssh"
GITHUB_REPO="kusimari/env"
GITHUB_ORG="kusimari"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[$(date '+%H:%M:%S')]${NC} $1"
}

success() {
    echo -e "${GREEN}✅ $1${NC}"
}

warn() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

error() {
    echo -e "${RED}❌ $1${NC}"
    exit 1
}

# Check AWS CLI is configured
if ! aws sts get-caller-identity >/dev/null 2>&1; then
    error "AWS CLI is not configured or credentials are invalid. Please run 'aws configure'"
fi

AWS_ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
AWS_REGION=${AWS_DEFAULT_REGION:-$(aws configure get region)}
AWS_REGION=${AWS_REGION:-us-east-1}

log "Setting up OIDC integration for GitHub Actions in AWS Account: $AWS_ACCOUNT_ID"
log "Region: $AWS_REGION"
log "GitHub Repository: $GITHUB_REPO"

# Create or verify OIDC Identity Provider
log "Creating OIDC Identity Provider: $OIDC_PROVIDER_NAME"
OIDC_PROVIDER_ARN="arn:aws:iam::${AWS_ACCOUNT_ID}:oidc-provider/token.actions.githubusercontent.com"

if aws iam get-open-id-connect-provider --open-id-connect-provider-arn "$OIDC_PROVIDER_ARN" >/dev/null 2>&1; then
    warn "OIDC Provider already exists"
else
    # Dynamically fetch the GitHub OIDC thumbprint
    log "Fetching GitHub OIDC thumbprint..."
    THUMBPRINT=$(openssl s_client -connect token.actions.githubusercontent.com:443 < /dev/null 2>/dev/null \
        | openssl x509 -fingerprint -noout -sha1 2>/dev/null \
        | sed 's/.*=//' | tr -d ':' | tr '[:upper:]' '[:lower:]')

    if [[ -z "$THUMBPRINT" ]]; then
        # Fallback: AWS ignores thumbprint for GitHub OIDC, use placeholder
        THUMBPRINT="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        warn "Could not fetch thumbprint dynamically, using placeholder (AWS ignores this for GitHub OIDC)"
    fi

    aws iam create-open-id-connect-provider \
        --url "https://token.actions.githubusercontent.com" \
        --client-id-list "sts.amazonaws.com" \
        --thumbprint-list "$THUMBPRINT" \
        --tags Key=Name,Value="$OIDC_PROVIDER_NAME" \
              Key=Purpose,Value="GitHub Actions OIDC" \
              Key=Repository,Value="$GITHUB_REPO"
    success "OIDC Identity Provider created"
fi

# Create IAM policy for EC2 management
log "Creating IAM policy: $POLICY_NAME"
POLICY_ARN="arn:aws:iam::${AWS_ACCOUNT_ID}:policy/${POLICY_NAME}"

# Policy document with EC2 permissions scoped by tag
POLICY_DOCUMENT='{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "EC2RunInstances",
            "Effect": "Allow",
            "Action": [
                "ec2:RunInstances"
            ],
            "Resource": "*",
            "Condition": {
                "StringEquals": {
                    "aws:RequestTag/Purpose": "env-cloud"
                }
            }
        },
        {
            "Sid": "EC2RunInstancesResources",
            "Effect": "Allow",
            "Action": [
                "ec2:RunInstances"
            ],
            "Resource": [
                "arn:aws:ec2:*::image/*",
                "arn:aws:ec2:*:*:subnet/*",
                "arn:aws:ec2:*:*:network-interface/*",
                "arn:aws:ec2:*:*:security-group/*",
                "arn:aws:ec2:*:*:volume/*"
            ]
        },
        {
            "Sid": "EC2ManageTaggedInstances",
            "Effect": "Allow",
            "Action": [
                "ec2:TerminateInstances",
                "ec2:StopInstances",
                "ec2:StartInstances"
            ],
            "Resource": "arn:aws:ec2:*:*:instance/*",
            "Condition": {
                "StringEquals": {
                    "aws:ResourceTag/Purpose": "env-cloud"
                }
            }
        },
        {
            "Sid": "EC2ReadOnly",
            "Effect": "Allow",
            "Action": [
                "ec2:DescribeInstances",
                "ec2:DescribeImages",
                "ec2:DescribeSnapshots",
                "ec2:DescribeSecurityGroups",
                "ec2:DescribeSubnets",
                "ec2:DescribeVpcs",
                "ec2:DescribeKeyPairs",
                "ec2:DescribeInstanceAttribute",
                "ec2:DescribeInstanceStatus",
                "ec2:DescribeInstanceTypes",
                "ec2:DescribeTags"
            ],
            "Resource": "*"
        },
        {
            "Sid": "EC2CreateTags",
            "Effect": "Allow",
            "Action": [
                "ec2:CreateTags"
            ],
            "Resource": "*",
            "Condition": {
                "StringEquals": {
                    "ec2:CreateAction": "RunInstances"
                }
            }
        },
        {
            "Sid": "EC2InstanceConnect",
            "Effect": "Allow",
            "Action": [
                "ec2-instance-connect:SendSSHPublicKey"
            ],
            "Resource": "*",
            "Condition": {
                "StringEquals": {
                    "ec2:osuser": ["ec2-user", "nixos", "kusimari"]
                }
            }
        },
        {
            "Sid": "SecurityGroupManagement",
            "Effect": "Allow",
            "Action": [
                "ec2:CreateSecurityGroup",
                "ec2:DeleteSecurityGroup",
                "ec2:AuthorizeSecurityGroupIngress",
                "ec2:AuthorizeSecurityGroupEgress",
                "ec2:RevokeSecurityGroupIngress",
                "ec2:RevokeSecurityGroupEgress"
            ],
            "Resource": "*"
        }
    ]
}'

if aws iam get-policy --policy-arn "$POLICY_ARN" >/dev/null 2>&1; then
    warn "IAM policy already exists"
    # Clean up old policy versions before creating new one (AWS limit: 5 versions)
    log "Cleaning up old policy versions..."
    OLD_VERSIONS=$(aws iam list-policy-versions --policy-arn "$POLICY_ARN" \
        --query 'Versions[?!IsDefaultVersion].VersionId' --output text)
    for ver in $OLD_VERSIONS; do
        aws iam delete-policy-version --policy-arn "$POLICY_ARN" --version-id "$ver" 2>/dev/null || true
    done
    log "Updating existing policy..."
    aws iam create-policy-version \
        --policy-arn "$POLICY_ARN" \
        --policy-document "$POLICY_DOCUMENT" \
        --set-as-default >/dev/null
    success "IAM policy updated"
else
    aws iam create-policy \
        --policy-name "$POLICY_NAME" \
        --policy-document "$POLICY_DOCUMENT" \
        --description "Permissions for GitHub Actions to manage EC2 instances for env cloud deployment" \
        --tags Key=Name,Value="$POLICY_NAME" \
              Key=Purpose,Value="GitHub Actions EC2 Management" \
              Key=Repository,Value="$GITHUB_REPO"
    success "IAM policy created"
fi

# Create trust policy for the IAM role
TRUST_POLICY='{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Principal": {
                "Federated": "'$OIDC_PROVIDER_ARN'"
            },
            "Action": "sts:AssumeRoleWithWebIdentity",
            "Condition": {
                "StringEquals": {
                    "token.actions.githubusercontent.com:aud": "sts.amazonaws.com",
                    "token.actions.githubusercontent.com:sub": [
                        "repo:'$GITHUB_REPO':ref:refs/heads/main",
                        "repo:'$GITHUB_REPO':ref:refs/heads/gcp-aws"
                    ]
                },
                "StringLike": {
                    "token.actions.githubusercontent.com:actor": "'$GITHUB_ORG'"
                }
            }
        }
    ]
}'

# Create IAM role
log "Creating IAM role: $ROLE_NAME"
ROLE_ARN="arn:aws:iam::${AWS_ACCOUNT_ID}:role/${ROLE_NAME}"

if aws iam get-role --role-name "$ROLE_NAME" >/dev/null 2>&1; then
    warn "IAM role already exists"
    # Update trust policy
    log "Updating trust policy..."
    aws iam update-assume-role-policy \
        --role-name "$ROLE_NAME" \
        --policy-document "$TRUST_POLICY"
    success "Trust policy updated"
else
    aws iam create-role \
        --role-name "$ROLE_NAME" \
        --assume-role-policy-document "$TRUST_POLICY" \
        --description "Role for GitHub Actions to deploy cloud VMs" \
        --max-session-duration 3600 \
        --tags Key=Name,Value="$ROLE_NAME" \
              Key=Purpose,Value="GitHub Actions EC2 Deployment" \
              Key=Repository,Value="$GITHUB_REPO"
    success "IAM role created"
fi

# Attach policy to role
log "Attaching policy to role..."
aws iam attach-role-policy \
    --role-name "$ROLE_NAME" \
    --policy-arn "$POLICY_ARN"
success "Policy attached to role"

# Create security group for SSH access
log "Creating security group: $SG_NAME"
DEFAULT_VPC_ID=$(aws ec2 describe-vpcs --filters "Name=isDefault,Values=true" \
    --query 'Vpcs[0].VpcId' --output text --region "$AWS_REGION")

if aws ec2 describe-security-groups --filters "Name=group-name,Values=$SG_NAME" \
   --region "$AWS_REGION" --query 'SecurityGroups[0].GroupId' --output text 2>/dev/null | grep -q "sg-"; then
    warn "Security group already exists"
else
    SG_ID=$(aws ec2 create-security-group \
        --group-name "$SG_NAME" \
        --description "Allow SSH for env cloud VMs" \
        --vpc-id "$DEFAULT_VPC_ID" \
        --region "$AWS_REGION" \
        --query 'GroupId' --output text)
    aws ec2 authorize-security-group-ingress \
        --group-id "$SG_ID" \
        --protocol tcp --port 22 --cidr "0.0.0.0/0" \
        --region "$AWS_REGION"
    aws ec2 create-tags --resources "$SG_ID" \
        --tags Key=Name,Value="$SG_NAME" Key=Purpose,Value=env-cloud \
        --region "$AWS_REGION"
    success "Security group created: $SG_ID"
fi

# Wait a moment for eventual consistency
log "Waiting for IAM changes to propagate..."
sleep 10

# Output configuration for GitHub Actions
echo ""
log "🎉 Setup complete! Use these values in your GitHub Actions workflow:"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Workflow inputs (enter these when running the GitHub Action):"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "  aws_region: $AWS_REGION"
echo "  aws_role_arn: $ROLE_ARN"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
warn "⚠️  IMPORTANT: These values should be entered as workflow inputs, NOT stored as repository secrets"
warn "   The OIDC integration provides authentication without storing credentials"
echo ""
success "AWS OIDC setup completed successfully!"
