export GOPATH=$HOME/src/go
export GOROOT=/usr/lib/go
export PATH=$PATH:$GOROOT/bin
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.config/eww/bin
export PATH=$PATH:~/.cargo/bin
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export EDITOR="emacs -nw"
export TERM=xterm-256color
export PKG_CONFIG_PATH=/usr/lib/pkgconfig:$PKG_CONFIG_PATH

# default pip env
[ -f "$HOME/aidrenv" ] && source ~/aidrenv/bin/activate
