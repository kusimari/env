#!/bin/bash
#
# GCP OIDC Teardown Script
# Run this to clean up all resources created by gcp-setup.sh
# This will remove the Workload Identity Federation and associated resources
#

set -euo pipefail

# Configuration (matches gcp-setup.sh)
PROJECT_ID="${GOOGLE_CLOUD_PROJECT:-$(gcloud config get-value project)}"
SERVICE_ACCOUNT="env-cloud-deployer"
WI_POOL="env-github-pool"
WI_PROVIDER="env-github-provider"
FIREWALL_RULE="allow-ssh-env-cloud"
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

# Check prerequisites
if [[ -z "$PROJECT_ID" ]]; then
    error "Could not determine GCP project ID. Please run 'gcloud config set project YOUR_PROJECT_ID'"
fi

# Confirmation prompt
echo ""
warn "⚠️  This will DELETE all OIDC resources for GitHub Actions integration in project: $PROJECT_ID"
echo ""
echo "Resources to be deleted:"
echo "  • Firewall rule: $FIREWALL_RULE"
echo "  • Workload Identity Provider: $WI_PROVIDER"
echo "  • Workload Identity Pool: $WI_POOL"
echo "  • Service Account: $SERVICE_ACCOUNT@$PROJECT_ID.iam.gserviceaccount.com"
echo "  • All associated IAM bindings"
echo ""
read -p "Are you sure you want to continue? [y/N] " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    log "Teardown cancelled"
    exit 0
fi

log "Starting teardown of OIDC integration in project: $PROJECT_ID"

# Delete firewall rule
log "Deleting firewall rule: $FIREWALL_RULE"
if gcloud compute firewall-rules describe "$FIREWALL_RULE" \
   --project="$PROJECT_ID" >/dev/null 2>&1; then
    gcloud compute firewall-rules delete "$FIREWALL_RULE" \
        --project="$PROJECT_ID" --quiet
    success "Firewall rule deleted"
else
    warn "Firewall rule not found (already deleted)"
fi

# Remove IAM policy bindings from service account
SA_EMAIL="$SERVICE_ACCOUNT@$PROJECT_ID.iam.gserviceaccount.com"
WI_POOL_FULL="projects/$PROJECT_ID/locations/global/workloadIdentityPools/$WI_POOL"
PRINCIPAL_SET="principalSet://iam.googleapis.com/$WI_POOL_FULL/attribute.repository/$GITHUB_REPO"

log "Removing Workload Identity binding from service account..."
if gcloud iam service-accounts describe "$SA_EMAIL" \
   --project="$PROJECT_ID" >/dev/null 2>&1; then
    gcloud iam service-accounts remove-iam-policy-binding "$SA_EMAIL" \
        --role="roles/iam.workloadIdentityUser" \
        --member="$PRINCIPAL_SET" \
        --project="$PROJECT_ID" --quiet >/dev/null 2>&1 || warn "Binding not found or already removed"
    success "Workload Identity binding removed"
fi

# Delete Workload Identity Provider
log "Deleting Workload Identity Provider: $WI_PROVIDER"
if gcloud iam workload-identity-pools providers describe "$WI_PROVIDER" \
   --workload-identity-pool="$WI_POOL" \
   --location="global" --project="$PROJECT_ID" >/dev/null 2>&1; then
    gcloud iam workload-identity-pools providers delete "$WI_PROVIDER" \
        --workload-identity-pool="$WI_POOL" \
        --location="global" \
        --project="$PROJECT_ID" --quiet
    success "Workload Identity Provider deleted"
else
    warn "Workload Identity Provider not found (already deleted)"
fi

# Delete Workload Identity Pool
log "Deleting Workload Identity Pool: $WI_POOL"
if gcloud iam workload-identity-pools describe "$WI_POOL" \
   --location="global" --project="$PROJECT_ID" >/dev/null 2>&1; then
    gcloud iam workload-identity-pools delete "$WI_POOL" \
        --location="global" \
        --project="$PROJECT_ID" --quiet
    success "Workload Identity Pool deleted"
else
    warn "Workload Identity Pool not found (already deleted)"
fi

# Remove project-level IAM bindings for service account
log "Removing project IAM bindings for service account..."
if gcloud iam service-accounts describe "$SA_EMAIL" \
   --project="$PROJECT_ID" >/dev/null 2>&1; then

    ROLES=(
        "roles/compute.instanceAdmin.v1"
        "roles/compute.securityAdmin"
        "roles/compute.networkAdmin"
        "roles/iam.serviceAccountUser"
    )

    for role in "${ROLES[@]}"; do
        log "  Removing role: $role"
        gcloud projects remove-iam-policy-binding "$PROJECT_ID" \
            --member="serviceAccount:$SA_EMAIL" \
            --role="$role" --quiet >/dev/null 2>&1 || warn "Binding for $role not found"
    done
    success "Project IAM bindings removed"
fi

# Delete service account
log "Deleting service account: $SERVICE_ACCOUNT"
if gcloud iam service-accounts describe "$SA_EMAIL" \
   --project="$PROJECT_ID" >/dev/null 2>&1; then
    gcloud iam service-accounts delete "$SA_EMAIL" \
        --project="$PROJECT_ID" --quiet
    success "Service account deleted"
else
    warn "Service account not found (already deleted)"
fi

echo ""
success "🎉 Teardown completed successfully!"
echo ""
log "All OIDC integration resources have been removed from project: $PROJECT_ID"
log "GitHub Actions will no longer be able to authenticate to this GCP project"
echo ""
warn "Note: If you have any running VMs created by the cloud deployment, they will continue running"
warn "You may want to check and delete them manually if no longer needed"
