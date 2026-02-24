PACKAGES_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
UPDATE       := $(PACKAGES_DIR)update-pkg.sh

.PHONY: update-all
update-all: update-emacs-eca update-emacs-gptel-git update-emacs-gptel-agent update-font-rubik
	@echo "==> done"

.PHONY: update-emacs-eca
update-emacs-eca:
	@echo "==> emacs-eca"
	@bash $(UPDATE) \
	  $(PACKAGES_DIR)emacs-eca.scm \
	  https://github.com/editor-code-assistant/eca-emacs

.PHONY: update-emacs-gptel-git
update-emacs-gptel-git:
	@echo "==> emacs-gptel-git"
	@bash $(UPDATE) \
	  $(PACKAGES_DIR)emacs-gptel-git.scm \
	  https://github.com/karthink/gptel

.PHONY: update-emacs-gptel-agent
update-emacs-gptel-agent:
	@echo "==> emacs-gptel-agent"
	@bash $(UPDATE) \
	  $(PACKAGES_DIR)emacs-gptel-agent.scm \
	  https://github.com/karthink/gptel-agent

.PHONY: update-font-rubik
update-font-rubik:
	@echo "==> font-rubik"
	@bash $(UPDATE) \
	  $(PACKAGES_DIR)fonts.scm \
	  https://github.com/googlefonts/rubik

.PHONY: status
status:
	@echo "==> checking for updates (no changes made)"
	@for entry in \
	  "emacs-eca.scm|https://github.com/editor-code-assistant/eca-emacs" \
	  "emacs-gptel-git.scm|https://github.com/karthink/gptel" \
	  "emacs-gptel-agent.scm|https://github.com/karthink/gptel-agent" \
	  "fonts.scm|https://github.com/googlefonts/rubik"; do \
	  file=$$(echo "$$entry" | cut -d'|' -f1); \
	  url=$$(echo "$$entry" | cut -d'|' -f2); \
	  pinned=$$(grep -oE '[0-9a-f]{40}' "$(PACKAGES_DIR)$$file" | head -1); \
	  latest=$$(git ls-remote "$$url" HEAD | cut -f1); \
	  if [ "$$pinned" = "$$latest" ]; then \
	    printf "  \033[32m✓\033[0m $$file ($${pinned:0:7})\n"; \
	  else \
	    printf "  \033[33m✗\033[0m $$file  pinned=$${pinned:0:7}  latest=$${latest:0:7}\n"; \
	  fi; \
	done

.PHONY: help
help:
	@echo "Targets:"
	@echo "  update-all               update all git-fetched packages"
	@echo "  update-emacs-eca         update emacs-eca"
	@echo "  update-emacs-gptel-git   update emacs-gptel-git"
	@echo "  update-emacs-gptel-agent update emacs-gptel-agent"
	@echo "  update-font-rubik        update font-rubik"
	@echo "  status                   show which packages are behind upstream HEAD"
