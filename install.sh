#! /bin/bash
sudo pacman -S --needed base-devel
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si
cd -
rm -r paru

paru --needed -S - < pkglist.txt

xdg-mime default emacsclient.desktop application/pdf
xdg-mime default emacsclient.desktop inode/directory

mkdir -p ~/doc/Roam/Journal
touch ~/doc/inbox.org
