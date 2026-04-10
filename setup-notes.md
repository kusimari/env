# Post-install setup notes

- Add google drive remote: `rclone-env add`
- Add ssh remote for desktop-aka: `rclone-env add`
- Configure claude to use kdevkit
- Setup GitHub SSH: `ssh-keygen -t ed25519 -f ~/.ssh/github_id -C "your-email@example.com"`, copy `~/.ssh/github_id.pub` to GitHub Settings → SSH keys, test with `ssh -T git@github.com`
