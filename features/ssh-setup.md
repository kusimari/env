# SSH Setup - Universal Pattern

## Overview

Automatic SSH configuration that works on all systems (Linux, macOS, workspaces, personal machines). The setup runs during `home-manager switch` and handles:

- **Include directive**: Adds `Include ~/.ssh/config_nix` to your SSH config (preserves existing entries)
- **SSH key creation**: Optionally creates GitHub SSH key with interactive prompts
- **Automated upload**: Uploads key to GitHub if `GH_TOKEN` environment variable is set

## How It Works

### File Structure

```
~/.ssh/
├── config             # System-managed (you can edit this)
│   └── (contains "Include ~/.ssh/config_nix" at bottom)
├── config_nix         # Nix-managed (symlink to /nix/store)
├── known_hosts        # System-managed (for your own hosts)
└── known_hosts_nix    # Nix-managed (GitHub keys)
```

### Separation of Concerns

- **System files** (`config`, `known_hosts`): You and other tools can write to these freely
- **Nix files** (`config_nix`, `known_hosts_nix`): Managed declaratively by home-manager
- **Include directive**: SSH reads system config first, then nix config
- **UserKnownHostsFile**: SSH checks both known_hosts files for host keys

This pattern allows:
- System processes to write to standard SSH files
- Nix to manage its own configuration declaratively
- No merge conflicts or backup logic needed
- Works identically on all systems

### What Happens During Activation

When you run `home-manager switch`, the activation script:

1. Creates `~/.ssh/config` if it doesn't exist
2. Adds `Include ~/.ssh/config_nix` at the bottom (if not already present)
3. Creates `~/.ssh/known_hosts` if it doesn't exist
4. If `~/.ssh/github_id` doesn't exist:
   - Prompts to create SSH key (interactive)
   - Generates ed25519 key with your email
   - Uploads to GitHub (if `GH_TOKEN` set) or shows manual instructions
   - Tests GitHub SSH connection

### GitHub SSH Configuration

The nix-managed `config_nix` contains:

```ssh-config
Host github.com
  IdentityFile ~/.ssh/github_id
  UserKnownHostsFile ~/.ssh/known_hosts ~/.ssh/known_hosts_nix
```

GitHub's public keys are fetched dynamically at activation time using `ssh-keyscan` and added to the nix-managed `~/.ssh/known_hosts_nix` file (not hardcoded). The `UserKnownHostsFile` directive ensures SSH checks both system and nix-managed known_hosts files.

## Usage

### Automatic Setup

Just run:
```bash
home-manager switch
```

The activation script runs automatically and guides you through any missing setup.

### Automated Key Upload

To enable automated GitHub SSH key upload:

```bash
export GH_TOKEN=ghp_your_token_here
home-manager switch
```

Get a token at: https://github.com/settings/tokens (needs `write:public_key` scope)

### Manual Key Creation

If you prefer to create the key manually:

```bash
ssh-keygen -t ed25519 -f ~/.ssh/github_id -C "your-email@example.com"
```

Upload the public key at: https://github.com/settings/ssh/new

### Testing

Verify GitHub SSH works:

```bash
ssh -T git@github.com
```

Expected output: `Hi username! You've successfully authenticated...`

## Design Benefits

1. **Universal**: Same code works on all systems (no conditional logic)
2. **Non-invasive**: System processes can continue writing to standard SSH files
3. **Idempotent**: Safe to run multiple times
4. **Automated**: Optional interactive key creation and upload
5. **Safe**: Backup/rollback logic protects against failures
6. **Extensible**: Pattern can be applied to other configs (git, shell, etc.)

## Implementation

The SSH setup is implemented as a modular Nix configuration split across two files:

### File Structure

- **`home/ssh-setup.nix`**: Nix module with SSH configuration
  - Defines `config_nix` content
  - Sets up activation script to run setup logic and fetch GitHub keys
  - Imported by `home/home.nix`

- **`home/ssh-setup.sh`**: Shell script with setup logic
  - Adds Include directive to system SSH config
  - Creates SSH keys interactively if missing
  - Uploads keys to GitHub (if GH_TOKEN set)
  - Runs directly from nix store (not copied to home directory)

- **`home/user-host.nix`**: User and hostname configuration
  - Contains placeholder values: `replace-user` and `replace-hostname`
  - Must be tracked in git (Nix flakes require all imported files tracked)
  - Build script temporarily replaces placeholders during activation
  - **Never commit actual username/hostname values**

### Why This Design?

- **Modular**: All SSH config in one place (`ssh-setup.nix`)
- **Clean**: Script runs from nix store, no files copied to home directory
- **Maintainable**: Shell logic separate from Nix declarations
- **Testable**: Can test script independently
- **Reusable**: Module can be imported into any home-manager config

### Implementation Notes

#### Absolute Paths for System Utilities

The activation script uses absolute `/usr/bin` paths for system utilities:
```bash
/usr/bin/timeout
/usr/bin/ssh-keyscan
/usr/bin/host
/usr/bin/awk
```

**Why?** During home-manager activation, PATH is limited and doesn't include standard directories. While nix is installed, `/usr/bin` utilities are more reliable during the bootstrap process.

#### GitHub IP Scanning

The script resolves `github.com` to IP addresses and scans both hostname and IPs:
```bash
GITHUB_IPS=$(/usr/bin/host github.com 2>/dev/null | grep "has address" | awk '{print $NF}')
ssh-keyscan github.com $GITHUB_IPS >> known_hosts_nix
```

**Why?** When SSH connects to github.com, it resolves to an IP (140.82.116.x). If the IP isn't in known_hosts, SSH automatically adds it to the **system** `~/.ssh/known_hosts`, polluting that file. By scanning both the hostname and IPs upfront, all entries go into `known_hosts_nix`, keeping the system file clean.

#### Nix Flake File Tracking Requirement

`home/user-host.nix` must be tracked in git with placeholder values. It cannot be in `.gitignore`.

**Why?** Nix flakes require all imported files to be tracked by Git. If the file is ignored, you get:
```
error: Path 'home/user-host.nix' in the repository is not tracked by Git
```

The build script (`build-nix/_common.sh`) handles this by:
1. Temporarily replacing placeholders with actual values before running nix
2. Restoring placeholders after build completes
3. Warning comment in file prevents accidental commits of personal info

## Troubleshooting

### Include directive not added

Check if the activation script ran:
```bash
home-manager switch --show-trace
```

### SSH key not working

Verify key is loaded:
```bash
ssh -G github.com | grep identityfile
```

Should show: `identityfile ~/.ssh/github_id`

### Permission issues

Ensure correct permissions:
```bash
chmod 700 ~/.ssh
chmod 600 ~/.ssh/config
chmod 600 ~/.ssh/github_id
```

### GitHub authentication fails

Check if GitHub keys exist in known_hosts_nix:
```bash
cat ~/.ssh/known_hosts_nix
```

If empty, this usually means the activation script couldn't reach GitHub (network issue during initial build). Re-run activation to fetch keys:
```bash
home-manager switch --flake .#<your-config>
```

The activation script will show a warning if key fetching fails and preserve any existing keys.

## Headless Environments

For headless setups (cloud dev environments, NixOS servers, CI/CD), the interactive SSH key creation won't work since there's no terminal input.

### Approach 1: Pre-create Key Before First Build

Create the SSH key manually before running `home-manager switch`:

```bash
# Create key
ssh-keygen -t ed25519 -f ~/.ssh/github_id -C "your-email@example.com" -N ""

# Copy public key and add to GitHub manually
cat ~/.ssh/github_id.pub
# Visit: https://github.com/settings/ssh/new

# Then run home-manager switch
home-manager switch --flake .#<your-config>
```

The activation script will detect the existing key and skip creation.

### Approach 2: Use GH_TOKEN for Automated Upload

Set `GH_TOKEN` environment variable to automate key upload:

```bash
# Create key
ssh-keygen -t ed25519 -f ~/.ssh/github_id -C "your-email@example.com" -N ""

# Set token and run activation (will auto-upload)
export GH_TOKEN=ghp_your_token_here
home-manager switch --flake .#<your-config>
```

Get a token at: https://github.com/settings/tokens (needs `write:public_key` scope)

### Approach 3: Use GitHub Deploy Keys (Repository-Specific)

For repository-specific access (common in CI/CD):

1. Generate a key: `ssh-keygen -t ed25519 -f ~/.ssh/deploy_key -N ""`
2. Add public key to repository: Settings → Deploy keys
3. Modify `config_nix` to use the deploy key:
   ```
   Host github.com
     IdentityFile ~/.ssh/deploy_key
   ```

### Approach 4: Cloud Init / User Data Scripts

For automated cloud environment setup:

```bash
#!/bin/bash
# In cloud-init or user-data script

# Create SSH key non-interactively
ssh-keygen -t ed25519 -f /home/user/.ssh/github_id -C "cloud@example.com" -N ""
chown user:user /home/user/.ssh/github_id*

# Option A: Upload via gh CLI with pre-configured token
export GH_TOKEN="${SECRET_GH_TOKEN}"
sudo -u user gh auth login --with-token <<< "$GH_TOKEN"
sudo -u user gh ssh-key add /home/user/.ssh/github_id.pub --title "cloud-$(hostname)"

# Option B: Use pre-created key from secrets manager
# (Store private key in AWS Secrets Manager, GCP Secret Manager, etc.)
# Fetch and write to ~/.ssh/github_id

# Then run home-manager
sudo -u user home-manager switch --flake .#<config>
```

### Design Consideration

The current design assumes:
- First-time setup may be interactive (workstations, laptops)
- SSH key is already present (headless/automated environments)

For fully automated environments, you must provision the SSH key before the first `home-manager switch`. The activation script gracefully handles this: if `~/.ssh/github_id` exists, it skips creation and only fetches GitHub host keys.
