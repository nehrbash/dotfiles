#! /bin/bash
set -e
cd ~
if [ -d ".dotfiles" ]; then
    echo ".dotfiles directory exists. Pulling latest updates."
    cd .dotfiles
    git pull
    git submodule update --init --recursive
else
    echo ".dotfiles directory does not exist. Cloning repo."
    git clone https://github.com/snehrbass/dotfiles.git .dotfiles --recurse-submodules
fi
cd .dotfiles
stow .
cd -

sudo pacman -S --needed base-devel
if ! command -v paru &> /dev/null; then
    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si
    cd -
    rm -r paru
fi
echo "Updating packages..."
paru -Syu

read -p "Do you want to update packages? (yes/no): " choice

if [[ "$choice" == "yes" ]]; then
    paru --needed -S - < pkglist.txt
else
    echo "Skipping package update."
fi

read -p "Do you want to install greetd.service? (yes/no): " choice
if [[ "$choice" == "yes" ]]; then
	cd ~/.dotfiles/ || exit
	sudo cp -r greetd/ /etc/greetd/ 
	sudo systemctl enable greetd.service
else
    echo "Skipping Spicetify update."
fi

systemctl --user enable swaync.service

go install github.com/nehrbash/hyprshell@latest

read -p "Do you want install/rebuild Eww? (yes/no): " choice
if [[ "$choice" == "yes" ]]; then
	mkdir -p ~/src
	cd ~/src/ || exit
	git clone https://github.com/nehrbash/eww.git
	cd eww || exit
	cargo build --release --no-default-features --features=wayland && cargo build --release --no-default-features --features=wayland
	cd ~
else
    echo "Skipping Eww 😲."
fi

if [[ "$SHELL" == *"/zsh" ]]; then
    echo "The current shell is already zsh. Skipping shell change."
else
    read -p "Do you want to change the shell to zsh? (yes/no): " choice

    if [[ "$choice" == "yes" ]]; then
        chsh -s $(which zsh)
        echo "Shell changed to zsh."
    else
        echo "Skipping shell change."
    fi
fi

read -p "Do you want to update Firefox CSS? (yes/no): " choice

if [[ "$choice" == "yes" ]]; then
	PROFILE=$(awk -F= -v section="$install_section" '$1 == "Default" && found {print $2; exit} $1 == section {found=1}' ~/.mozilla/firefox/profiles.ini)
	# Create the symlink
	ln -sfn ~/.dotfiles/.config/chrome/ ~/.mozilla/firefox/${PROFILE}/
else
    echo "Skipping Firefox CSS update."
fi

read -p "Do you want to update Spicetify? (yes/no): " choice
if [[ "$choice" == "yes" ]]; then
	spicetify config current_theme Onepunch color_scheme light
	spicetify restore backup
	spicetify backup
	spicetify apply
else
    echo "Skipping Spicetify update."
fi

read -p "Do you want install/rebuild Emacs? (yes/no): " choice
if [[ "$choice" == "yes" ]]; then
	mkdir -p ~/src
	cd ~/src/ || exit
	git clone https://aur.archlinux.org/emacs-git.git
	cd emacs-git || exit
	git apply < ~/.dotfiles/emacs_build.patch && makepkg -si
	cd ~ || exit
else
    echo "Skipping Emacs 😞."
fi
