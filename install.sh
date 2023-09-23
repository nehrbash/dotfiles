#! /bin/bash
set -e
sudo pacman -S --needed base-devel
if ! command -v paru &> /dev/null; then
    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si
    cd -
    rm -r paru
fi

paru --needed -S - < pkglist.txt



systemctl --user enable hyprland.service
systemctl --user enable hyprpaper.service
systemctl --user enable hyprshell.service
systemctl --user enable eww.service
systemctl --user enable swaync.service

go install github.com/nehrbash/hyprshell@latest

chsh -s $(which zsh)

PROFILE=$(awk -F= -v section="$install_section" '$1 == "Default" && found {print $2; exit} $1 == section {found=1}' ~/.mozilla/firefox/profiles.ini)
# Create the symlink
ln -sfn ~/.dotfiles/.config/chrome/ ~/.mozilla/firefox/${PROFILE}/

spicetify config current_theme Onepunch color_scheme light
spicetify backup
spicetify apply

xdg-mime default emacsclient.desktop application/pdf
xdg-mime default emacsclient.desktop inode/directory

mkdir -p ~/doc/Roam/Journal
if [ ! -f ~/doc/inbox.org ]; then
    cat << EOF > ~/doc/inbox.org
#+CATEGORY: INBOX
#+FILETAGS: INBOX
EOF
fi
if [ ! -f ~/doc/projects.org ]; then
    cat << EOF > ~/doc/projects.org
#+CATEGORY: PROJECT
#+FILETAGS: PROJECT
EOF
fi
if [ ! -f ~/doc/repeater.org ]; then
    cat << EOF > ~/doc/repeater.org
#+CATEGORY: REPEATER
#+FILETAGS: REPEATER
EOF
fi
if [ ! -f ~/doc/gcal.org ]; then
    touch ~/doc/gcal.org
fi
