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

### Why This Design?

- **Modular**: All SSH config in one place (`ssh-setup.nix`)
- **Clean**: Script runs from nix store, no files copied to home directory
- **Maintainable**: Shell logic separate from Nix declarations
- **Testable**: Can test script independently
- **Reusable**: Module can be imported into any home-manager config

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

If empty, re-run activation to fetch keys:
```bash
home-manager switch --flake .#<your-config>
```
