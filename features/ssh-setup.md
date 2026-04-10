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
├── known_hosts        # System-managed (you can edit this)
└── known_hosts_nix    # Nix-managed (symlink to /nix/store)
```

### Separation of Concerns

- **System files** (`config`, `known_hosts`): You and other tools can write to these freely
- **Nix files** (`config_nix`, `known_hosts_nix`): Managed declaratively by home-manager
- **Include directive**: SSH reads both sets of files (system first, then nix)

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

The nix-managed `known_hosts_nix` contains GitHub's public keys (verified).

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

The SSH setup logic lives in `home/ssh-setup.sh` and is called by the home-manager activation script defined in `home/home.nix`.

### Script Location

- `home/ssh-setup.sh`: Standalone shell script with all setup logic
- Installed to: `~/.config/env/ssh-setup.sh`
- Called during: `home-manager switch` activation phase

### Why External Script?

- Easier to read and maintain than inline activation code
- Can be tested independently
- Clear separation of concerns
- Can be reused or called manually if needed

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

Check known_hosts includes GitHub:
```bash
ssh -G github.com | grep userknownhostsfile
```

Should show both `known_hosts` and `known_hosts_nix`.
