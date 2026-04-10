# SSH Setup Module - Universal pattern for all systems
# Provides:
# - Nix-managed SSH config (config_nix) with GitHub settings
# - Nix-managed known_hosts (known_hosts_nix) with GitHub keys
# - Activation script that ensures Include directive and creates SSH keys
{ config, lib, pkgs, ... }:
{
  # Nix-managed SSH configuration
  # System ~/.ssh/config includes this file via "Include ~/.ssh/config_nix" directive
  # This allows system processes to write to the main config while nix manages its own section
  home.file.".ssh/config_nix".text = ''
    # Nix-managed GitHub SSH configuration
    Host github.com
      IdentityFile ~/.ssh/github_id
      # Check both system and nix known_hosts files
      UserKnownHostsFile ~/.ssh/known_hosts ~/.ssh/known_hosts_nix
  '';

  # Nix-managed known_hosts with GitHub public keys
  home.file.".ssh/known_hosts_nix".text = ''
    github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl
    github.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCj7ndNxQowgcQnjshcLrqPEiiphnt+VTTvDP6mHBL9j1aNUkY4Ue1gvwnGLVlOhGeYrnZaMgRK6+PKCUXaDbC7qtbW8gIkhL7aGCsOr/C56SJMy/BCZfxd1nWzAOxSDPgVsmerOBYfNqltV9/hWCqBywINIR+5dIg6JTJ72pcEpEjcYgXkE2YEFXV1JHnsKgbLWNlhScqb2UmyRkQyytRLtL+38TGxkxCflmO+5Z8CSSNY7GidjMIZ7Q4zMjA2n1nGrlTDkzwDCsw+wqFPGQA179cnfGWOWRVruj16z6XyvxvjJwbz0wQZ75XK5tKSb7FNyeIEs4TT4jk+S4dhPeAUC5y+bDYirYgM4GC7uEnztnZyaVWQ7B381AK4Qdrwt51ZqExKbQpTUNn+EjqoTwvqNj4kqx5QUCI0ThS/YkOxJCXmPUWZbhjpCg56i+2aB6CmK2JGhn57K5mj0MNdBXA4/WnwH6XoPWJzK5Nyu2zB3nAZp+S5hpQs+p1vN1/wsjk=
    github.com ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEmKSENjQEezOmxkZMy7opKgwFB9nkt5YRrYMjNuG5N87uRgg6CLrbo5wAdT/y6v0mKV0U2w0WZ2YB/++Tpockg=
  '';

  # Activation script - runs ssh-setup.sh directly from nix store
  # This runs on ALL systems during home-manager switch
  # No need to copy script to home directory - keeps it clean
  home.activation.setupSSHConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
    bash ${./ssh-setup.sh}
  '';
}
