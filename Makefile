.PHONY: all update-channels update-quickshell update-system update-home update update-quickshell-upstream

DOTFILES_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
HOSTNAME     := $(shell hostname)

# Update everything in the correct order
all: update

update: update-channels update-quickshell update-home

# 1. Pull latest Guix channels
update-channels:
	guix pull -C $(DOTFILES_DIR)/channels.scm

# 2. Update quickshell submodule (fetch + rebase personal commits onto upstream/main)
update-quickshell-upstream:
	git -C $(DOTFILES_DIR)/files/quickshell fetch upstream
	git -C $(DOTFILES_DIR)/files/quickshell rebase upstream/main
	git -C $(DOTFILES_DIR)/files/quickshell push --force-with-lease
	git -C $(DOTFILES_DIR) add files/quickshell
	git -C $(DOTFILES_DIR) commit -m "chore: update shell to latest upstream"

# 2. Update quickshell submodule to the pinned commit (normal pull)
update-quickshell:
	git -C $(DOTFILES_DIR) submodule update --remote --rebase files/quickshell
	git -C $(DOTFILES_DIR) add files/quickshell
	git -C $(DOTFILES_DIR) commit -m "chore: update quickshell submodule"

# 3. Reconfigure Guix Home (rebuilds quickshell + caelestia-shell packages)
update-home:
	guix home reconfigure $(DOTFILES_DIR)/home/redfish.scm -L $(DOTFILES_DIR)

# 4. Reconfigure Guix System (run as root)
update-system:
	sudo -E guix system reconfigure $(DOTFILES_DIR)/systems/$(HOSTNAME).scm
