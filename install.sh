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

read -p "Do you want to update packages? (yes/no): " choice

if [[ "$choice" == "yes" ]]; then
    paru --needed -S - < pkglist.txt
else
    echo "Skipping package update."
fi

systemctl enable greetd.service

systemctl --user enable swaync.service

go install github.com/nehrbash/hyprshell@latest

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
	spicetify backup
	spicetify config current_theme Onepunch color_scheme light
	spicetify restore backup
	spicetify backup
	spicetify apply
else
    echo "Skipping Spicetify update."
fi

systemctl --user enable dropbox.service

xdg-mime default emacsclient.desktop application/pdf
xdg-mime default emacsclient.desktop inode/directory
