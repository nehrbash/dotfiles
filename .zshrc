# /bin/zsh
# Dependancies You Need for this Config
# zsh-syntax-highlighting - syntax highlighting for ZSH in standard repos
# zsh-autosuggestions - Suggestions based on your history

# # very simple prompt for when zsh cacuse problems
# [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Enable colors and change prompt:
autoload -U colors && colors

export PS1="> "

# Pure Prompt
fpath+=$HOME/.zsh/pure
autoload -U promptinit; promptinit
prompt pure

# Bash-like navigation
autoload -U select-word-style
select-word-style bash

# highlight colors
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.sh 2>/dev/null

## History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh/history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)     # Include hidden files.

# Load plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source /usr/share/autojump/autojump.zsh 2>/dev/null

# export variables are in $HOME/.zshenv

# Load aliases and shortcuts if existent.
[ -f "$HOME/.zsh/aliasrc" ] && source "$HOME/.zsh/aliasrc"

# Load utility functions
[ -f "$HOME/.zsh/functions" ] && source "$HOME/.zsh/functions"

# Load emacs functions
[ -f "$HOME/.zsh/emacs_functions" ] && source "$HOME/.zsh/emacs_functions"

