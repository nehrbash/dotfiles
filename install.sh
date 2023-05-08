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

spicetify config current_theme Onepunch color_scheme light
spicetify apply

xdg-mime default emacsclient.desktop application/pdf
xdg-mime default emacsclient.desktop inode/directory

mkdir -p ~/doc/Roam/Journal
touch ~/doc/inbox.org
