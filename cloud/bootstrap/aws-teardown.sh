#!/bin/bash
#
# AWS OIDC Teardown Script
# Run this to clean up all resources created by aws-setup.sh
# This will remove the OIDC provider, IAM role, and policies
#

set -euo pipefail

# Configuration (matches aws-setup.sh)
OIDC_PROVIDER_NAME="env-github-oidc"
ROLE_NAME="env-cloud-deployer-role"
POLICY_NAME="env-cloud-deployer-policy"
GITHUB_REPO="kusimari/env"

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
}

# Check AWS CLI is configured
if ! aws sts get-caller-identity >/dev/null 2>&1; then
    error "AWS CLI is not configured or credentials are invalid. Please run 'aws configure'"
fi

AWS_ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
AWS_REGION=${AWS_DEFAULT_REGION:-$(aws configure get region)}
AWS_REGION=${AWS_REGION:-us-east-1}

# Confirmation prompt
echo ""
warn "⚠️  This will DELETE all OIDC resources for GitHub Actions integration in AWS Account: $AWS_ACCOUNT_ID"
echo ""
echo "Resources to be deleted:"
echo "  • IAM Role: $ROLE_NAME"
echo "  • IAM Policy: $POLICY_NAME"
echo "  • OIDC Identity Provider: $OIDC_PROVIDER_NAME"
echo "  • All associated policy attachments"
echo ""
read -p "Are you sure you want to continue? [y/N] " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    log "Teardown cancelled"
    exit 0
fi

log "Starting teardown of OIDC integration in AWS Account: $AWS_ACCOUNT_ID"

# Detach and delete IAM role
ROLE_ARN="arn:aws:iam::${AWS_ACCOUNT_ID}:role/${ROLE_NAME}"
POLICY_ARN="arn:aws:iam::${AWS_ACCOUNT_ID}:policy/${POLICY_NAME}"

log "Processing IAM role: $ROLE_NAME"
if aws iam get-role --role-name "$ROLE_NAME" >/dev/null 2>&1; then

    # Detach managed policies
    log "  Detaching policies from role..."
    aws iam list-attached-role-policies --role-name "$ROLE_NAME" --query 'AttachedPolicies[].PolicyArn' --output text | \
    while read -r policy_arn; do
        if [[ -n "$policy_arn" ]]; then
            log "    Detaching policy: $policy_arn"
            aws iam detach-role-policy --role-name "$ROLE_NAME" --policy-arn "$policy_arn" || warn "Failed to detach $policy_arn"
        fi
    done

    # Delete inline policies (if any)
    log "  Checking for inline policies..."
    aws iam list-role-policies --role-name "$ROLE_NAME" --query 'PolicyNames' --output text | \
    while read -r policy_name; do
        if [[ -n "$policy_name" && "$policy_name" != "None" ]]; then
            log "    Deleting inline policy: $policy_name"
            aws iam delete-role-policy --role-name "$ROLE_NAME" --policy-name "$policy_name" || warn "Failed to delete inline policy $policy_name"
        fi
    done

    # Delete the role
    log "  Deleting role..."
    aws iam delete-role --role-name "$ROLE_NAME"
    success "IAM role deleted"
else
    warn "IAM role not found (already deleted)"
fi

# Delete IAM policy (only if it exists and has no more attachments)
log "Processing IAM policy: $POLICY_NAME"
if aws iam get-policy --policy-arn "$POLICY_ARN" >/dev/null 2>&1; then

    # Check for remaining attachments
    ATTACHMENT_COUNT=$(aws iam list-entities-for-policy --policy-arn "$POLICY_ARN" \
        --query 'length(PolicyGroups) + length(PolicyRoles) + length(PolicyUsers)' --output text)

    if [[ "$ATTACHMENT_COUNT" -eq 0 ]]; then
        # Delete all policy versions except the default
        log "  Deleting non-default policy versions..."
        aws iam list-policy-versions --policy-arn "$POLICY_ARN" --query 'Versions[?!IsDefaultVersion].VersionId' --output text | \
        while read -r version_id; do
            if [[ -n "$version_id" && "$version_id" != "None" ]]; then
                log "    Deleting version: $version_id"
                aws iam delete-policy-version --policy-arn "$POLICY_ARN" --version-id "$version_id" || warn "Failed to delete version $version_id"
            fi
        done

        # Delete the policy
        log "  Deleting policy..."
        aws iam delete-policy --policy-arn "$POLICY_ARN"
        success "IAM policy deleted"
    else
        warn "IAM policy has $ATTACHMENT_COUNT remaining attachments - skipping deletion"
        warn "You may need to manually detach and delete this policy"
    fi
else
    warn "IAM policy not found (already deleted)"
fi

# Delete OIDC Identity Provider
OIDC_PROVIDER_ARN="arn:aws:iam::${AWS_ACCOUNT_ID}:oidc-provider/token.actions.githubusercontent.com"
log "Deleting OIDC Identity Provider..."
if aws iam get-open-id-connect-provider --open-id-connect-provider-arn "$OIDC_PROVIDER_ARN" >/dev/null 2>&1; then
    aws iam delete-open-id-connect-provider --open-id-connect-provider-arn "$OIDC_PROVIDER_ARN"
    success "OIDC Identity Provider deleted"
else
    warn "OIDC Identity Provider not found (already deleted)"
fi

echo ""
success "🎉 Teardown completed successfully!"
echo ""
log "All OIDC integration resources have been removed from AWS Account: $AWS_ACCOUNT_ID"
log "GitHub Actions will no longer be able to authenticate to this AWS account"
echo ""
warn "Note: If you have any running EC2 instances created by the cloud deployment, they will continue running"
warn "You may want to check and terminate them manually if no longer needed:"
warn ""
warn "  aws ec2 describe-instances --filters 'Name=tag:Purpose,Values=env-cloud' --query 'Reservations[].Instances[].[InstanceId,State.Name,Tags[?Key==\`Name\`].Value|[0]]' --output table"
