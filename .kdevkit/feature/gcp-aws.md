# Cloud VM Provisioning Feature

## Feature Brief

Add the ability to provision cloud developer VMs (GCP and AWS) from this Nix environment repo with zero local state architecture. Uses NixOS on both platforms with the same configuration pattern as nix-darwin (NixOS system config + integrated home-manager). Authentication uses OIDC federation, setup via Cloud Shell/CLI, and supports both browser and CLI SSH access.

**Key Innovation**: Mirrors the Darwin pattern (`nixosConfigurations` + integrated home-manager) rather than the Ubuntu pattern (standalone home-manager), enabling full system control on cloud instances.

## Requirements

- **Zero Local State**: No secrets stored anywhere in repo or locally
- **OIDC Authentication**: GitHub Actions ↔ cloud provider trust via Workload Identity Federation (GCP) / IAM OIDC (AWS)
- **User-Scoped Security**: Only GitHub user `kusimari` can trigger deployments
- **NixOS on Both Platforms**: AWS uses official NixOS AMIs, GCP uses nixos-infect conversion
- **Configuration Reuse**: Same `./home/home.nix` as Darwin/Ubuntu for all CLI tools
- **Re-runnable Config**: Can update environment from within the launched machine
- **Multiple VM Support**: User-specified VM names prevent conflicts
- **Multi-Access SSH**: Browser SSH + CLI SSH without key files

## Technical Design

### Cloud Strategy (Research Validated ✅)

**AWS**: Official NixOS AMIs (Account ID: `427812963091`)
- Weekly updates to all regions, x86_64 + aarch64
- Direct `nixos-rebuild switch --flake github:kusimari/env#cloud`

**GCP**: Ubuntu 24.04 LTS + nixos-infect
- Tested on GCP Compute Engine
- `nixos-infect` → reboot → `nixos-rebuild switch --flake github:kusimari/env#cloud`

### Configuration Architecture

**Current Pattern**:
```
Darwin:    nixosConfigurations.darwin + integrated home-manager → ./home/home.nix
Ubuntu:    homeConfigurations.ubuntu (standalone)              → ./home/home.nix
AL2:       homeConfigurations.kelasa-al2 (standalone)          → ./home/home.nix
```

**New Pattern**:
```
Cloud:     nixosConfigurations.cloud + integrated home-manager → ./home/home.nix
```

**Layer Separation**:
- `commonConfiguration` → overlays, nix settings (shared everywhere)
- `cloudSystemConfiguration` → NixOS system stuff (SSH, users, firewall, boot)
- `./home/home.nix` → user environment (CLI tools, zsh, tmux, etc.)

### File Structure

```
cloud/
  config.yaml                   # Default specs, AMI/image selection
  bootstrap/
    gcp-setup.sh                # Creates user-scoped OIDC trust
    gcp-teardown.sh             # Removes OIDC trust
    aws-setup.sh                # Creates user-scoped OIDC trust
    aws-teardown.sh             # Removes OIDC trust
  init-aws.sh                   # AWS: Direct nixos-rebuild (NixOS AMI ready)
  init-gcp.sh                   # GCP: nixos-infect + nixos-rebuild
build-nix/
  cloud.sh                      # Build script (local testing)
.github/
  workflows/
    cloud-vm.yml                # Manual trigger with typed inputs
```

### Authentication Flow

1. **Bootstrap** (one-time per cloud account): Run `cloud/bootstrap/{gcp,aws}-setup.sh` in Cloud Shell/CLI
2. **GitHub Action**: User triggers with cloud account identifiers as inputs (no stored state)
3. **OIDC Trust**: GitHub authenticates via Workload Identity Federation/IAM OIDC
4. **User Verification**: Trust policies verify GitHub actor is `kusimari`
5. **VM Provisioning**: Create instance with init script as startup/user-data
6. **NixOS Apply**: Init script runs `nixos-rebuild switch --flake github:kusimari/env#cloud`

## Implementation Tasks

### Phase 1: Core Configuration

1. **Add `nixosConfigurations.cloud` to `flake.nix`**:
   ```nix
   nixosConfigurations.cloud = nixpkgs.lib.nixosSystem {
     system = "x86_64-linux";
     modules = [
       commonConfiguration
       cloudSystemConfiguration
       home-manager.nixosModules.home-manager {
         home-manager.useGlobalPkgs = true;
         home-manager.useUserPackages = true;
         home-manager.users.${user} = import ./home/home.nix;
       }
     ];
   };
   ```

2. **Define `cloudSystemConfiguration`**:
   - User account with sudo privileges
   - SSH service enabled
   - Firewall (port 22 only)
   - Cloud-optimized boot config
   - Essential system packages

3. **Create `build-nix/cloud.sh`** (local testing):
   ```bash
   export NIX_COMMAND='nix build "$FLAKE_DIR#nixosConfigurations.cloud.config.system.build.toplevel"'
   ```

4. **Update `build-nix/test.sh`**:
   ```bash
   echo "Evaluating nixosConfigurations.cloud..."
   nix eval "$FLAKE_DIR#nixosConfigurations.cloud.config.system.build.toplevel.name"
   ```

### Phase 2: Cloud Configuration

1. **Create `cloud/config.yaml`**:
   - Default machine specs (region, instance type, disk size)
   - Image selection (AWS NixOS AMI owner/filter, GCP Ubuntu image)
   - Bootstrap resource naming conventions
   - Repository metadata

2. **Create init scripts**:
   - `cloud/init-aws.sh`: Direct `nixos-rebuild switch` (NixOS ready)
   - `cloud/init-gcp.sh`: nixos-infect + systemd service for post-reboot config

### Phase 3: OIDC Bootstrap Scripts

1. **Create `cloud/bootstrap/gcp-setup.sh`**:
   - Enable APIs (compute, iam, iamcredentials)
   - Create service account with compute admin
   - Create Workload Identity Pool + Provider
   - IAM bindings scoped to user + branches
   - Firewall rule for SSH
   - Print WIF provider + SA email for GitHub Action inputs

2. **Create `cloud/bootstrap/gcp-teardown.sh`**:
   - Reverse all setup steps in correct order

3. **Create `cloud/bootstrap/aws-setup.sh`**:
   - Create/verify OIDC provider for GitHub
   - Create IAM role with user-scoped trust policy
   - Attach EC2 permissions (RunInstances, DescribeInstances, etc.)
   - Print role ARN for GitHub Action input

4. **Create `cloud/bootstrap/aws-teardown.sh`**:
   - Delete role, policies, OIDC provider

### Phase 4: GitHub Action

1. **Create `.github/workflows/cloud-vm.yml`**:
   - Manual trigger (`workflow_dispatch`)
   - Inputs: provider, action (create/destroy), VM name, machine specs, cloud identifiers
   - User verification: `${{ github.actor }}` must be `kusimari`
   - Provider-specific authentication (GCP WIF, AWS IAM role)
   - VM provisioning with init script
   - Multiple SSH connection methods in job summary
   - Destroy confirmation with typed input

### Phase 5: Verification & Documentation

1. **Test flake evaluation**: `./build-nix/test.sh`
2. **Test bootstrap scripts** in GCP Cloud Shell + AWS CloudShell
3. **Test GitHub Action** end-to-end (create, SSH access, destroy)
4. **Test re-runnable config**: SSH to instance, run `nixos-rebuild switch`
5. **Update `.kdevkit/project.md`** with new cloud targets
6. **Document Configuration Architecture**: Update project documentation and/or add comments in `flake.nix` to document the complete pattern:
   - **Existing OS Setup**: Use `build-nix/<os>.sh` scripts (darwin.sh, ubuntu.sh, al2.sh)
   - **Cloud Provisioning**: Use GitHub Action → `build-nix/cloud.sh`
   - **Script Pattern**: All build scripts follow pre-nix → nix → post-nix workflow
   - **Configuration Mapping**:
     * Darwin: `nixosConfigurations.darwin` + integrated home-manager
     * Ubuntu: `homeConfigurations.ubuntu` (standalone)
     * AL2: `homeConfigurations.kelasa-al2` (standalone)
     * Cloud: `nixosConfigurations.cloud` + integrated home-manager

## Expected Outcome

- **Zero-friction cloud development**: One-click VM provisioning from GitHub
- **Consistent environment**: Same dev tools across macOS, Ubuntu, AL2, and cloud
- **Security best practices**: No secrets, user-scoped OIDC, minimal permissions
- **Multiple access methods**: Browser SSH, CLI SSH, direct SSH
- **Cost-effective**: Pay-per-use VMs, easy destroy, no persistent infrastructure
- **Re-runnable configuration**: Update environment from within VMs
- **Multiple VM support**: Named instances prevent conflicts

## Success Criteria

### Configuration
- [ ] `nix eval .#nixosConfigurations.cloud.config.system.build.toplevel.name` succeeds
- [ ] `./build-nix/test.sh` passes with cloud config
- [ ] Same `./home/home.nix` works across all platforms (Darwin, Ubuntu, AL2, Cloud)

### Bootstrap & OIDC
- [ ] GCP bootstrap creates WIF + service account with correct permissions
- [ ] AWS bootstrap creates OIDC provider + role with correct trust policy
- [ ] User scoping verified: only `kusimari` GitHub actor can authenticate
- [ ] Branch scoping verified: only `main` and `gcp-aws` branches can authenticate
- [ ] Teardown scripts cleanly remove all cloud resources

### VM Provisioning
- [ ] AWS: NixOS AMI launches and applies configuration successfully
- [ ] GCP: Ubuntu converts via nixos-infect and applies configuration successfully
- [ ] User `kusimari` created with sudo privileges and zsh shell
- [ ] SSH access works via browser (GCP Console, EC2 Instance Connect)
- [ ] CLI SSH works (`gcloud compute ssh`, `aws ec2-instance-connect ssh`)
- [ ] VM destroy works correctly and removes all resources

### Environment Consistency
- [ ] All CLI tools available (git, tmux, zsh, emacs, claude-code, etc.)
- [ ] Same zsh configuration and prompt as local/other platforms
- [ ] Same tmux configuration and key bindings
- [ ] Same emacs configuration and packages
- [ ] Re-runnable: `nixos-rebuild switch --flake github:kusimari/env#cloud` updates environment

### Security & Operations
- [ ] No secrets stored in repository or GitHub variables
- [ ] OIDC authentication works without stored credentials
- [ ] Multiple VMs can be launched with different names
- [ ] VM lifecycle (create/destroy) managed via GitHub Actions
- [ ] Action logs contain connection information (IP, SSH commands)

## Notes

- **NixOS AMI Research**: AWS Account ID `427812963091`, updated weekly, both architectures
- **nixos-infect Research**: Works on Ubuntu 24.04, tested on GCP, supports custom configurations
- **Configuration Pattern**: Mirrors Darwin (system + home-manager) not Ubuntu (home-manager only)
- **Simple Init Scripts**: Just `nixos-rebuild switch` after NixOS is ready
- **Zero State Goal**: Everything typed as workflow inputs, nothing stored permanently