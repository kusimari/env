#!/bin/bash
#
# GCP OIDC Setup Script for GitHub Actions Integration
# Run this once in GCP Cloud Shell to set up Workload Identity Federation
# This enables GitHub Actions to authenticate to GCP without storing credentials
#

set -euo pipefail

# Configuration (matches cloud/config.yaml)
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
    exit 1
}

# Check prerequisites
if [[ -z "$PROJECT_ID" ]]; then
    error "Could not determine GCP project ID. Please run 'gcloud config set project YOUR_PROJECT_ID'"
fi

log "Setting up OIDC integration for GitHub Actions in project: $PROJECT_ID"
log "GitHub Repository: $GITHUB_REPO"

# Enable required APIs
log "Enabling required GCP APIs..."
gcloud services enable compute.googleapis.com \
                      iam.googleapis.com \
                      iamcredentials.googleapis.com \
                      --project="$PROJECT_ID"
success "APIs enabled"

# Create service account
log "Creating service account: $SERVICE_ACCOUNT"
if gcloud iam service-accounts describe "$SERVICE_ACCOUNT@$PROJECT_ID.iam.gserviceaccount.com" \
   --project="$PROJECT_ID" >/dev/null 2>&1; then
    warn "Service account already exists"
else
    gcloud iam service-accounts create "$SERVICE_ACCOUNT" \
        --display-name="Environment Cloud Deployer" \
        --description="Service account for GitHub Actions cloud VM deployment" \
        --project="$PROJECT_ID"
    success "Service account created"
fi

# Grant necessary permissions to service account
log "Granting permissions to service account..."
SA_EMAIL="$SERVICE_ACCOUNT@$PROJECT_ID.iam.gserviceaccount.com"

# Required roles for VM management
ROLES=(
    "roles/compute.instanceAdmin.v1"    # Create/delete instances
    "roles/compute.securityAdmin"       # Manage firewall rules
    "roles/compute.networkAdmin"        # Manage networks
    "roles/iam.serviceAccountUser"      # Use service accounts
)

for role in "${ROLES[@]}"; do
    log "  Adding role: $role"
    gcloud projects add-iam-policy-binding "$PROJECT_ID" \
        --member="serviceAccount:$SA_EMAIL" \
        --role="$role" \
        --condition=None >/dev/null
done
success "Service account permissions configured"

# Create Workload Identity Pool
log "Creating Workload Identity Pool: $WI_POOL"
if gcloud iam workload-identity-pools describe "$WI_POOL" \
   --location="global" --project="$PROJECT_ID" >/dev/null 2>&1; then
    warn "Workload Identity Pool already exists"
else
    gcloud iam workload-identity-pools create "$WI_POOL" \
        --location="global" \
        --display-name="GitHub Actions Pool" \
        --description="Pool for GitHub Actions authentication" \
        --project="$PROJECT_ID"
    success "Workload Identity Pool created"
fi

# Create Workload Identity Provider
log "Creating Workload Identity Provider: $WI_PROVIDER"
if gcloud iam workload-identity-pools providers describe "$WI_PROVIDER" \
   --workload-identity-pool="$WI_POOL" \
   --location="global" --project="$PROJECT_ID" >/dev/null 2>&1; then
    warn "Workload Identity Provider already exists"
else
    gcloud iam workload-identity-pools providers create-oidc "$WI_PROVIDER" \
        --workload-identity-pool="$WI_POOL" \
        --location="global" \
        --issuer-uri="https://token.actions.githubusercontent.com" \
        --attribute-mapping="google.subject=assertion.sub,attribute.actor=assertion.actor,attribute.repository=assertion.repository,attribute.repository_owner=assertion.repository_owner,attribute.ref=assertion.ref" \
        --attribute-condition="assertion.repository_owner == 'kusimari' && assertion.repository == 'kusimari/env' && (assertion.ref == 'refs/heads/main' || assertion.ref == 'refs/heads/gcp-aws')" \
        --project="$PROJECT_ID"
    success "Workload Identity Provider created"
fi

# Bind service account to Workload Identity Pool
log "Binding service account to Workload Identity Pool..."
WI_POOL_FULL="projects/$PROJECT_ID/locations/global/workloadIdentityPools/$WI_POOL"
PRINCIPAL_SET="principalSet://iam.googleapis.com/$WI_POOL_FULL/attribute.repository/$GITHUB_REPO"

gcloud iam service-accounts add-iam-policy-binding "$SA_EMAIL" \
    --role="roles/iam.workloadIdentityUser" \
    --member="$PRINCIPAL_SET" \
    --project="$PROJECT_ID" >/dev/null
success "Service account bound to Workload Identity Pool"

# Create firewall rule for SSH access
log "Creating firewall rule: $FIREWALL_RULE"
if gcloud compute firewall-rules describe "$FIREWALL_RULE" \
   --project="$PROJECT_ID" >/dev/null 2>&1; then
    warn "Firewall rule already exists"
else
    gcloud compute firewall-rules create "$FIREWALL_RULE" \
        --allow="tcp:22" \
        --source-ranges="0.0.0.0/0" \
        --description="Allow SSH for environment cloud VMs" \
        --target-tags="env-cloud-vm" \
        --project="$PROJECT_ID"
    success "Firewall rule created"
fi

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
echo "  gcp_project_id: $PROJECT_ID"
echo "  gcp_workload_identity_provider: projects/$PROJECT_ID/locations/global/workloadIdentityPools/$WI_POOL/providers/$WI_PROVIDER"
echo "  gcp_service_account: $SA_EMAIL"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
warn "⚠️  IMPORTANT: These values should be entered as workflow inputs, NOT stored as repository secrets"
warn "   The Workload Identity Federation provides authentication without storing credentials"
echo ""
success "GCP OIDC setup completed successfully!"
