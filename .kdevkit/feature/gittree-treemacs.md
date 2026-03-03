# gittree-treemacs

## Feature Brief
A lazygit-inspired Emacs mode combining treemacs navigation with git status visualization and intelligent file comparison. Selecting a file automatically shows git-aware panels with the working file always editable.

## Requirements
- Tree navigation with git status prefixes on filenames
- Selecting a file shows git-aware panels: single panel for clean/untracked, dual vdiff panels for modified files
- Working file is always editable in dual-panel view
- File change detection with reload prompt when files change externally
- Manual git status refresh
- Focus returns to treemacs after vdiff setup
- Deployed via Nix (home-manager) with shell alias `emacs-gittree`

## Design

### Window Layout
```
+------------------+------------------------+------------------------+
|                  |                        |                        |
|    treemacs      |     left panel         |     right panel        |
|    (file tree    |     (read-only         |     (working file,     |
|     with git     |      git ref)          |      editable)         |
|     status       |                        |                        |
|     prefixes)    |                        |                        |
|                  |                        |                        |
+------------------+------------------------+------------------------+
       resizable          vdiff highlights differences
```

### Data Flow
```
gittree-mode (global minor mode)
├── enable:  gittree--enable()
│   ├── treemacs setup (project, file-name-transformer, visit-action)
│   ├── hooks (find-file → file watcher, kill-buffer → unwatch)
│   ├── advice (vdiff--diff-refresh-finish → refocus treemacs)
│   └── keybinding (g → refresh status)
│
└── disable: gittree--disable()
    └── teardown (treemacs quit, restore defaults, remove hooks/advice/watches)

[User selects file in treemacs]
└── gittree-visit-node()
    ├── gittree--get-file-git-status()     → git status --porcelain
    ├── gittree--find-status-config()      → lookup in gittree--status-configs
    ├── gittree-cleanup-panels()           → reset content area
    ├── gittree-cleanup-file-buffers()     → kill stale git buffers
    └── dispatch:
        ├── left-ref nil  → gittree-show-single-panel()
        └── left-ref set  → gittree-show-dual-panel() + vdiff
                             └── gittree--refocus-treemacs (via advice)
```

### Git Status Config
Declarative mapping from `git status --porcelain` output to panel behavior:

| Pattern | Left Panel | Right Panel | vdiff | Description |
|---------|------------|-------------|-------|-------------|
| (clean) | - | working file | no | Clean file |
| `??` | - | working file | no | Untracked |
| ` M` | HEAD (read-only) | working file | yes | Modified in worktree |
| `M ` | HEAD (read-only) | staged | yes | Staged for commit |
| `MM` | staged (read-only) | working file | yes | Staged + modified |
| ` D` | HEAD (read-only) | empty | yes | Deleted in worktree |

### Git Status Display
4-character prefix before filenames in treemacs via `treemacs-file-name-transformer`:
```
 M  modified-file.el      (modified in worktree)
MM  staged-and-mod.el     (staged + modified)
??  new-file.el           (untracked)
A   added-file.el         (staged/added)
    clean-file.el         (no changes)
```

### File Change Detection
- `file-notify` watches opened files for external changes
- Prompts "Reload? (y/n)" immediately when file changes on disk
- Only prompts for unmodified buffers (avoids overwriting unsaved edits)

### Key Bindings (in treemacs)
| Key | Action |
|-----|--------|
| `g` | Refresh git status |
| `Enter` | Select file (git-aware panels) / expand directory |

### Deployment
- `core-gittree.el` loaded by `core.el` (same directory)
- `emacs/` directory deployed to `~/.config/emacs/` via `home.file` in Nix
- Shell alias: `emacs-gittree` → `emacs --eval "(gittree-mode 1)"`

### Dependencies
| Package | Role |
|---------|------|
| treemacs | File tree sidebar |
| vdiff | Side-by-side diff with highlighting |
| ediff | Commit-to-commit comparison |
| diffview | Diff viewing utilities |
