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
    # Get GitHub's OIDC thumbprint (this is static and well-known)
    THUMBPRINT="6938fd4d98bab03faadb97b34396831e3780aea1"

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

# Policy document with EC2 permissions
POLICY_DOCUMENT='{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "EC2Management",
            "Effect": "Allow",
            "Action": [
                "ec2:RunInstances",
                "ec2:TerminateInstances",
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
                "ec2:CreateTags",
                "ec2:DescribeTags"
            ],
            "Resource": "*"
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
                "ec2:RevokeSecurityGroupEgress",
                "ec2:DescribeSecurityGroups"
            ],
            "Resource": "*",
            "Condition": {
                "StringEquals": {
                    "aws:RequestedRegion": ["us-east-1", "us-west-2", "eu-west-1"]
                }
            }
        }
    ]
}'

if aws iam get-policy --policy-arn "$POLICY_ARN" >/dev/null 2>&1; then
    warn "IAM policy already exists"
    # Update the policy with the latest version
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

# Wait a moment for eventual consistency
log "Waiting for IAM changes to propagate..."
sleep 10

# Output configuration for GitHub Actions
echo ""
log "🎉 Setup complete! Use these values in your GitHub Actions workflow:"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "GitHub Repository Settings → Secrets and variables → Actions"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Workflow inputs (enter these when running the GitHub Action):"
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
