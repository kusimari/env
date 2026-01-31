# all the input overlays to be used in home-manager
{inputs}:
  [ inputs.nix-vscode-extensions.overlays.default
    inputs.alacritty-theme.overlays.default 
    inputs.claude-code.overlays.default
  ]
