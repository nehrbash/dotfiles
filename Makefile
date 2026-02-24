.PHONY: all update-channels update-quickshell update-system update-home update

DOTFILES_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
HOSTNAME     := $(shell hostname)

# Update everything in the correct order
all: update

update: update-channels update-quickshell update-home

# 1. Pull latest Guix channels
update-channels:
	guix pull -C $(DOTFILES_DIR)/channels.scm

# 2. Pull latest quickshell source (used as local-file in packages/quickshell.scm)
update-quickshell:
	git -C $(HOME)/src/quickshell pull

# 3. Reconfigure Guix Home (rebuilds quickshell + caelestia-shell packages)
update-home:
	guix home reconfigure $(DOTFILES_DIR)/home/redfish.scm -L $(DOTFILES_DIR)

# 4. Reconfigure Guix System (run as root)
update-system:
	sudo -E guix system reconfigure $(DOTFILES_DIR)/systems/$(HOSTNAME).scm
