.PHONY: all update-channels update-system update-home update \
        quickshell-upstream-fetch quickshell-upstream-log quickshell-upstream-diff \
        quickshell-upstream-show quickshell-upstream-bump

DOTFILES_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
HOSTNAME     := $(shell hostname)
CORES        := $(shell nproc)

# Map hostnames to config names (add new machines here).
HOST_MAP_redfish     := redfish
HOST_MAP_dell-nehrbash := bluefish
HOST_CONFIG := $(or $(HOST_MAP_$(HOSTNAME)),$(HOSTNAME))

# Substitute URLs read from substitute-urls.txt (one per line, # comments ok).
# Edit that file to add/remove mirrors — applies on the very next make target,
# no system reconfigure needed.
SUBSTITUTE_URLS := $(shell sed -e 's/#.*$$//' -e '/^[[:space:]]*$$/d' $(DOTFILES_DIR)/substitute-urls.txt | tr '\n' ' ')
SUB_FLAG        := --substitute-urls="$(SUBSTITUTE_URLS)"

# Update everything in the correct order
all: update

update: update-channels update-home

# 1. Pull latest Guix channels
update-channels:
	guix pull -C $(DOTFILES_DIR)/channels.scm --cores=$(CORES) $(SUB_FLAG)

# 2. Reconfigure Guix Home (uses HOST_CONFIG mapping)
update-home:
	guix home reconfigure $(DOTFILES_DIR)/home/$(HOST_CONFIG).scm -L $(DOTFILES_DIR) --cores=$(CORES) --fallback $(SUB_FLAG)

# 3. Reconfigure Guix System (run as root)
update-system:
	sudo -E guix system reconfigure $(DOTFILES_DIR)/systems/$(HOST_CONFIG).scm --cores=$(CORES) --fallback $(SUB_FLAG)

# ---------------------------------------------------------------------------
# Quickshell upstream tracking
#
# files/quickshell/ is a vendored copy of caelestia-dots/shell. The SHA we
# diverged from lives in files/quickshell/.upstream-base. These targets fetch
# upstream into a local cache and show what's changed since that pin so you
# can decide what to port. Bump the pin with `make quickshell-upstream-bump`
# after porting changes.
QUICKSHELL_UPSTREAM_URL  := https://github.com/caelestia-dots/shell.git
QUICKSHELL_UPSTREAM_DIR  := $(DOTFILES_DIR)/files/quickshell
QUICKSHELL_UPSTREAM_BASE := $(shell cat $(QUICKSHELL_UPSTREAM_DIR)/.upstream-base)
QUICKSHELL_CACHE         := $(DOTFILES_DIR)/.cache/caelestia-shell.git

$(QUICKSHELL_CACHE):
	mkdir -p $(dir $(QUICKSHELL_CACHE))
	git clone --bare $(QUICKSHELL_UPSTREAM_URL) $(QUICKSHELL_CACHE)

quickshell-upstream-fetch: $(QUICKSHELL_CACHE)
	git -C $(QUICKSHELL_CACHE) fetch --quiet origin '+refs/heads/*:refs/heads/*'
	@echo "upstream HEAD: $$(git -C $(QUICKSHELL_CACHE) rev-parse --short main)"
	@echo "pinned base:   $$(echo $(QUICKSHELL_UPSTREAM_BASE) | cut -c1-12)"

# List upstream commits since the pinned base.
quickshell-upstream-log: quickshell-upstream-fetch
	@git -C $(QUICKSHELL_CACHE) log --oneline $(QUICKSHELL_UPSTREAM_BASE)..main

# Full diff of upstream changes since the pinned base (paths relative to repo root).
quickshell-upstream-diff: quickshell-upstream-fetch
	@git -C $(QUICKSHELL_CACHE) diff $(QUICKSHELL_UPSTREAM_BASE)..main

# Show one upstream commit by SHA: make quickshell-upstream-show SHA=abc123
quickshell-upstream-show: quickshell-upstream-fetch
	@test -n "$(SHA)" || { echo "usage: make quickshell-upstream-show SHA=<sha>"; exit 1; }
	@git -C $(QUICKSHELL_CACHE) show $(SHA)

# Bump the pin to current upstream main. Run AFTER porting all wanted changes.
quickshell-upstream-bump: quickshell-upstream-fetch
	@new=$$(git -C $(QUICKSHELL_CACHE) rev-parse main); \
	echo $$new > $(QUICKSHELL_UPSTREAM_DIR)/.upstream-base; \
	echo "bumped pin to $$new"
