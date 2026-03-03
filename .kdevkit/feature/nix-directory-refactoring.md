# Nix Directory Refactoring Feature

## Feature Brief

Refactor the unified Nix flake structure to improve project organization and accessibility by:
- Moving `flake.nix` to the root directory for easier access
- Renaming `nix/` directory to `nix-install-scripts/` to better reflect its purpose as install scripts
- Flattening platform run scripts (e.g., `darwin/run.sh` → `darwin-run.sh`)
- Moving `home/` directory to root level alongside the flake
- Updating documentation with simplified usage instructions

## Requirements

- Maintain all existing platform functionality (Darwin, Ubuntu, kelasa-al2)
- Preserve all configuration imports and dependencies
- Keep all SSL fix scripts for kelasa-al2 platform
- Ensure proper relative path resolution for all imports
- Use "bak" naming convention for backups (consistent with flake.nix)

## Technical Design

### Before Structure
```
├── nix/
│   ├── flake.nix
│   ├── flake.lock
│   ├── run.sh
│   ├── home/
│   │   ├── home.nix
│   │   ├── user-host.nix
│   │   └── ...
│   ├── darwin/
│   │   └── run.sh
│   ├── ubuntu/
│   │   └── run.sh
│   └── kelasa-al2/
│       ├── run.sh
│       └── fix-ssl-trusted-user.sh
```

### After Structure
```
├── flake.nix
├── flake.lock
├── home/
│   ├── home.nix
│   ├── user-host.nix
│   └── ...
└── nix-install-scripts/
    ├── run.sh
    ├── darwin-run.sh
    ├── ubuntu-run.sh
    ├── kelasa-al2-run.sh
    └── kelasa-al2-fix-ssl-trusted-user.sh
```

## Implementation Tasks

### Phase 1: Directory Restructuring
1. Create `nix-install-scripts/` directory
2. Move `nix/flake.nix` → `./flake.nix`
3. Move `nix/flake.lock` → `./flake.lock` (if exists)
4. Move `nix/home/` → `./home/`
5. Copy `nix/run.sh` → `nix-install-scripts/run.sh`
6. Copy `nix/darwin/run.sh` → `nix-install-scripts/darwin-run.sh`
7. Copy `nix/ubuntu/run.sh` → `nix-install-scripts/ubuntu-run.sh`
8. Copy `nix/kelasa-al2/run.sh` → `nix-install-scripts/kelasa-al2-run.sh`
9. Copy `nix/kelasa-al2/fix-ssl-trusted-user.sh` → `nix-install-scripts/kelasa-al2-fix-ssl-trusted-user.sh`

### Phase 2: Path Reference Updates
1. Update `home/home.nix`:
   - Line 5: `../../gittree/gittree-module.nix` → `../gittree/gittree-module.nix`
   - Line ~178: `../../emacs` → `../emacs`

2. Update `nix-install-scripts/darwin-run.sh`:
   - Line 21: `/../run.sh` → `/run.sh` (same directory reference)

3. Update `nix-install-scripts/ubuntu-run.sh`:
   - Line 20: `/../run.sh` → `/run.sh` (same directory reference)

4. Update `nix-install-scripts/kelasa-al2-run.sh`:
   - Update source path to reference `run.sh` in same directory

### Phase 3: Documentation Updates
1. Update `README.md`:
   - Change flake reference from `nix-darwin/flake.nix` to `flake.nix`
   - Update platform run script paths to `./nix-install-scripts/platform-run.sh`
   - Update SSL fix script path for kelasa-al2

### Phase 4: Verification
1. Test flake evaluation: `nix flake check`
2. Test platform configurations:
   - `nix eval .#darwinConfigurations.darwin.system`
   - `nix eval .#homeConfigurations.ubuntu.system`
   - `nix eval .#homeConfigurations.kelasa-al2.system`
3. Verify run script syntax
4. Test gittree module and emacs symlink resolution

## Expected Outcome

- Cleaner, more intuitive project structure
- Main flake easily accessible at root
- Install scripts clearly organized and labeled
- All three platforms (darwin, ubuntu, kelasa-al2) continue to work identically
- Improved maintainability and documentation
- Consistent backup naming using "bak" convention

## Success Criteria

- [ ] All nix flake evaluations pass
- [ ] All platform configurations build successfully
- [ ] All run scripts execute without syntax errors
- [ ] Gittree and emacs configurations resolve correctly
- [ ] Documentation accurately reflects new structure
- [ ] No functionality regression on any platform