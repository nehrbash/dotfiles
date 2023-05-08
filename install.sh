#! /bin/bash
sudo pacman -S --needed base-devel
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si
cd -
rm -r paru

paru --needed -S - < pkglist.txt

mkdir -p "$HOME/.zsh"
git clone https://github.com/sindresorhus/pure.git "$HOME/.zsh/pure"

sudo mkdir -p /etc/systemd/system/getty@tty1.service.d
sudo touch /etc/systemd/system/getty@tty1.service.d/override.conf

systemctl --user enable hyprland.service
systemctl --user enable hyprpaper.service
systemctl --user enable hyprshell.service
systemctl --user enable eww.service
# systemctl --user enable swaync.service

spicetify config current_theme Onepunch color_scheme light
spicetify apply

xdg-mime default emacsclient.desktop application/pdf
xdg-mime default emacsclient.desktop inode/directory

mkdir -p ~/doc/Roam/Journal
touch ~/doc/inbox.org
