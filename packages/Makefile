# Makefile for updating git-fetched custom Guix packages.
#
# Usage:
#   make update-all          # update every git package
#   make update-emacs-eca
#   make update-emacs-gptel-git
#   make update-emacs-gptel-agent
#   make update-font-rubik

PACKAGES_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# ── helpers ───────────────────────────────────────────────────────────────────

# Fetch the latest commit SHA from a GitHub/git URL.
# Usage: $(call latest-commit,<url>)
define latest-commit
$(shell git ls-remote $(1) HEAD | cut -f1)
endef

# Fetch the Guix base32 hash for a git-fetch origin.
# Usage: $(call guix-hash-git,<url>,<commit>)
define guix-hash-git
$(shell guix hash --serializer=nar \
  <(guix download --no-substitutes "git+$(1)#$(2)" 2>/dev/null) 2>/dev/null \
  || guix hash -x --serializer=nar \
     "$$(nix-prefetch-git --quiet --rev $(2) $(1) 2>/dev/null | jq -r .path)" 2>/dev/null)
endef

# In-place sed that works on both GNU and BSD sed.
# Usage: $(call inplace,<expr>,<file>)
define inplace
sed -i 's|$(1)|$(2)|g' $(3)
endef

# ── per-package targets ───────────────────────────────────────────────────────

.PHONY: update-emacs-eca
update-emacs-eca:
	@echo "==> emacs-eca"
	$(eval URL   := https://github.com/editor-code-assistant/eca-emacs)
	$(eval FILE  := $(PACKAGES_DIR)emacs-eca.scm)
	$(eval OLD   := $(shell grep -oP '(?<=commit ")[0-9a-f]+' $(FILE)))
	$(eval NEW   := $(call latest-commit,$(URL)))
	@if [ "$(OLD)" = "$(NEW)" ]; then \
	  echo "  already up to date ($(OLD))"; \
	else \
	  echo "  $(OLD) -> $(NEW)"; \
	  NEW_SHORT=$$(echo $(NEW) | cut -c1-7); \
	  OLD_SHORT=$$(echo $(OLD) | cut -c1-7); \
	  sed -i "s|$(OLD)|$(NEW)|g" $(FILE); \
	  sed -i "s|$$OLD_SHORT|$$NEW_SHORT|g" $(FILE); \
	  NEW_HASH=$$(guix hash --serializer=nar -x \
	    $$(guix download "git+$(URL)#$(NEW)" 2>/dev/null)); \
	  OLD_HASH=$$(grep -oP '(?<=base32 ")[^"]+' $(FILE)); \
	  sed -i "s|$$OLD_HASH|$$NEW_HASH|g" $(FILE); \
	  echo "  hash -> $$NEW_HASH"; \
	fi

.PHONY: update-emacs-gptel-git
update-emacs-gptel-git:
	@echo "==> emacs-gptel-git"
	$(eval URL   := https://github.com/karthink/gptel)
	$(eval FILE  := $(PACKAGES_DIR)emacs-gptel-git.scm)
	$(eval OLD   := $(shell grep -oP '(?<=commit ")[0-9a-f]+' $(FILE)))
	$(eval NEW   := $(call latest-commit,$(URL)))
	@if [ "$(OLD)" = "$(NEW)" ]; then \
	  echo "  already up to date ($(OLD))"; \
	else \
	  echo "  $(OLD) -> $(NEW)"; \
	  NEW_SHORT=$$(echo $(NEW) | cut -c1-7); \
	  OLD_SHORT=$$(echo $(OLD) | cut -c1-7); \
	  sed -i "s|$(OLD)|$(NEW)|g" $(FILE); \
	  sed -i "s|$$OLD_SHORT|$$NEW_SHORT|g" $(FILE); \
	  NEW_HASH=$$(guix hash --serializer=nar -x \
	    $$(guix download "git+$(URL)#$(NEW)" 2>/dev/null)); \
	  OLD_HASH=$$(grep -oP '(?<=base32 ")[^"]+' $(FILE)); \
	  sed -i "s|$$OLD_HASH|$$NEW_HASH|g" $(FILE); \
	  echo "  hash -> $$NEW_HASH"; \
	fi

.PHONY: update-emacs-gptel-agent
update-emacs-gptel-agent:
	@echo "==> emacs-gptel-agent"
	$(eval URL   := https://github.com/karthink/gptel-agent)
	$(eval FILE  := $(PACKAGES_DIR)emacs-gptel-agent.scm)
	$(eval OLD   := $(shell grep -oP '(?<=commit ")[0-9a-f]+' $(FILE)))
	$(eval NEW   := $(call latest-commit,$(URL)))
	@if [ "$(OLD)" = "$(NEW)" ]; then \
	  echo "  already up to date ($(OLD))"; \
	else \
	  echo "  $(OLD) -> $(NEW)"; \
	  NEW_SHORT=$$(echo $(NEW) | cut -c1-7); \
	  OLD_SHORT=$$(echo $(OLD) | cut -c1-7); \
	  sed -i "s|$(OLD)|$(NEW)|g" $(FILE); \
	  sed -i "s|$$OLD_SHORT|$$NEW_SHORT|g" $(FILE); \
	  NEW_HASH=$$(guix hash --serializer=nar -x \
	    $$(guix download "git+$(URL)#$(NEW)" 2>/dev/null)); \
	  OLD_HASH=$$(grep -oP '(?<=base32 ")[^"]+' $(FILE)); \
	  sed -i "s|$$OLD_HASH|$$NEW_HASH|g" $(FILE); \
	  echo "  hash -> $$NEW_HASH"; \
	fi

.PHONY: update-font-rubik
update-font-rubik:
	@echo "==> font-rubik"
	$(eval URL   := https://github.com/googlefonts/rubik)
	$(eval FILE  := $(PACKAGES_DIR)fonts.scm)
	$(eval OLD   := $(shell grep -A5 'font-rubik' $(FILE) | grep -oP '(?<=commit ")[0-9a-f]+'))
	$(eval NEW   := $(call latest-commit,$(URL)))
	@if [ "$(OLD)" = "$(NEW)" ]; then \
	  echo "  already up to date ($(OLD))"; \
	else \
	  echo "  $(OLD) -> $(NEW)"; \
	  NEW_SHORT=$$(echo $(NEW) | cut -c1-7); \
	  OLD_SHORT=$$(echo $(OLD) | cut -c1-7); \
	  sed -i "s|$(OLD)|$(NEW)|g" $(FILE); \
	  sed -i "s|$$OLD_SHORT|$$NEW_SHORT|g" $(FILE); \
	  NEW_HASH=$$(guix hash --serializer=nar -x \
	    $$(guix download "git+$(URL)#$(NEW)" 2>/dev/null)); \
	  OLD_HASH=$$(grep -A10 'font-rubik' $(FILE) | grep -oP '(?<=base32 ")[^"]+'); \
	  sed -i "s|$$OLD_HASH|$$NEW_HASH|g" $(FILE); \
	  echo "  hash -> $$NEW_HASH"; \
	fi

# ── convenience ───────────────────────────────────────────────────────────────

.PHONY: update-all
update-all: update-emacs-eca update-emacs-gptel-git update-emacs-gptel-agent update-font-rubik
	@echo "==> all git packages checked"

.PHONY: status
status:
	@echo "==> checking for updates (no changes made)"
	@for pkg_url in \
	  "emacs-eca.scm https://github.com/editor-code-assistant/eca-emacs" \
	  "emacs-gptel-git.scm https://github.com/karthink/gptel" \
	  "emacs-gptel-agent.scm https://github.com/karthink/gptel-agent" \
	  "fonts.scm https://github.com/googlefonts/rubik"; do \
	  file=$$(echo $$pkg_url | cut -d' ' -f1); \
	  url=$$(echo $$pkg_url | cut -d' ' -f2); \
	  pinned=$$(grep -oP '(?<=commit ")[0-9a-f]+' $(PACKAGES_DIR)$$file | head -1); \
	  latest=$$(git ls-remote $$url HEAD | cut -f1); \
	  if [ "$$pinned" = "$$latest" ]; then \
	    echo "  ✓ $$file ($$pinned)"; \
	  else \
	    echo "  ✗ $$file  pinned=$$(echo $$pinned | cut -c1-7)  latest=$$(echo $$latest | cut -c1-7)"; \
	  fi; \
	done

.PHONY: help
help:
	@echo "Targets:"
	@echo "  update-all              update all git-fetched packages"
	@echo "  update-emacs-eca        update emacs-eca commit + hash"
	@echo "  update-emacs-gptel-git  update emacs-gptel-git commit + hash"
	@echo "  update-emacs-gptel-agent update emacs-gptel-agent commit + hash"
	@echo "  update-font-rubik       update font-rubik commit + hash"
	@echo "  status                  show which packages are behind HEAD"
	@echo "  help                    this message"
