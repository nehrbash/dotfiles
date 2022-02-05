#.zshrc
# Dependancies You Need for this Config
# zsh-syntax-highlighting - syntax highlighting for ZSH in standard repos
# zsh-autosuggestions - Suggestions based on your history

# Initial Setup
# mkdir -p "$HOME/.zsh"
# git clone https://github.com/sindresorhus/pure.git "$HOME/.zsh/pure"
# Setup Alias in $HOME/.zsh/aliasrc

# Enable colors and change prompt:
autoload -U colors && colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "


# Pure Prompt
fpath+=$HOME/.zsh/pure
autoload -U promptinit; promptinit
prompt pure
# zstyle 'prompt:continuation' color '#b294bb'


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

# Load aliases and shortcuts if existent.
[ -f "$HOME/.zsh/aliasrc" ] && source "$HOME/.zsh/aliasrc"


# Load plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=3'
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source /usr/share/autojump/autojump.zsh 2>/dev/null


# Load emacs functions for vterm and restarting daemon.
[ -f "$HOME/.zsh/emacs_functions" ] && source "$HOME/.zsh/emacs_functions"

# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
# cat ~/.cache/wal/sequences
# clear
# source ~/.cache/wal/colors-tty.sh

# set dual monitors
dual () {
    xrandr --output DP1 --mode 3840x2160 --primary --left-of eDP1 --output eDP1 --auto
}

# set single monitor
single () {
    xrandr --output eDP1 --off
}

if type swiperd > /dev/null; then
    cd () {
        builtin cd $@ && swiperd -a
    }
    sd () {
        builtin cd "$(swiperd)" && swiperd -a
    }
fi

# put in function bc it keeps getting in the way of system packages.
conda_setup () {
    __conda_setup="$('/usr/local/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/usr/local/anaconda3/etc/profile.d/conda.sh" ]; then
            . "/usr/local/anaconda3/etc/profile.d/conda.sh"
        else
            export PATH="/usr/local/anaconda3/bin:$PATH"
        fi
    fi
    unset __conda_setup
    # <<< conda initialize <<<
}


export EDITOR=emacs
export TERM=xterm-256color
