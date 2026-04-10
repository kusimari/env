# Post-install setup notes

- Add google drive remote: `rclone-env add`
- Add ssh remote for desktop-aka: `rclone-env add`
- Configure claude to use kdevkit
- GitHub SSH: Setup script will prompt to create and upload key automatically (or manual: `ssh-keygen -t ed25519 -f ~/.ssh/github_id -C "your-email@example.com"` → upload to github.com/settings/ssh)
