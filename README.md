Follow the rabbit in flake.nix

For system configuration:
  - Darwin: ./build-nix/darwin.sh
  - Ubuntu: ./build-nix/ubuntu.sh
  - Amazon Linux 2:
    - First run SSL fixes: ./build-nix/al2-fix-ssl.sh
    - Then setup: ./build-nix/al2.sh

To check flake evaluates without building:
  - ./build-nix/test.sh

For a new project folder:
  - mkdir <folder>; cd <folder>
  - nix-init
  - use language specific builders or use one of nix-templates which generalizes them
