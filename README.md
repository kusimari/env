Follow the rabbit in flake.nix

For system configuration:
  - Darwin: ./nix-install-scripts/darwin-run.sh
  - Ubuntu: ./nix-install-scripts/ubuntu-run.sh
  - Amazon Linux 2:
    - First run SSL fixes: ./nix-install-scripts/kelasa-al2-fix-ssl-trusted-user.sh
    - Then setup: ./nix-install-scripts/kelasa-al2-run.sh

For a new project folder:
  - mkdir <folder>; cd <folder>
  - nix flake init --template templates#utils-generic
  - echo "use flake" >> .envrc
  - direnv allow
  - use language specific builders or use one of nix-templates which generalizes them
