# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# XDG Base Directory
# Default settings
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
# WSLGによって設定済み
# XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir

export HISTFILE="${XDG_CACHE_HOME}/bash/history"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set Aliases
alias ls='ls --color=auto --show-control-chars'
alias grep='grep --color=auto'
if type eza > /dev/null 2>&1 ; then
    # use eza
    alias l='eza'
    alias la='eza --all'
    alias ll='eza --long --classify --group --git --time-style=long-iso'
    alias lla='ll --all'
else
    # use ls
    alias l='ls'
    alias la='ls --all'
    alias ll='ls -l --classify --human-readable'
    alias lla='ll --all'
fi
if type vim > /dev/null 2>&1 ; then
    alias vi='vim'
    alias lessv='/usr/share/vim/vim82/macros/less.sh'
    alias vless='lessv'
fi
if type emacs > /dev/null 2>&1 ; then
    alias spacemacs='env HOME=${HOME}/.spacemacs emacs'
    alias emacsc='emacs -Q --batch -f batch-byte-compile'
fi
if type gitkraken > /dev/null 2>&1 ; then
    alias gitkraken="GDK_SCALE=2 gitkraken 1>/dev/null 2>/dev/null"
fi

# GPG
export GPG_TTY=$(tty)

# EDITOR
if type vim > /dev/null 2>&1 ; then
    export EDITOR=vim
fi

# rlwrap home
export RLWRAP_HOME="${XDG_CACHE_HOME}/rlwrap"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# OPAM configuration
if [ -f "$HOME/.opam/opam-init/init.sh" ] ; then
    . /home/kei/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

# OchaCaml
if [ -d "${HOME}/.ochacaml" ]; then
    PATH="$PATH:$HOME/.ochacaml/OchaCaml"
fi

# SML/NJ PATH
if [ -d "$HOME/.smlnj" ] ; then
    PATH="$HOME/.smlnj/bin:$PATH"
fi

# WSL
if [ -f "/mnt/c/Program Files/Mozilla Firefox/firefox.exe" ] ; then
    alias firefox-win="/mnt/c/Program\ Files/Mozilla\ Firefox/firefox.exe"
fi
PATH="$PATH:/mnt/c/Windows"          # for wsl deb package
PATH="$PATH:/mnt/c/Windows/System32" # for wsl deb package

# Rust and cargo
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
if [ -d "${CARGO_HOME}" ] ; then
    PATH="${CARGO_HOME}/bin:$PATH"
fi

# Starship
# cargo install starship
if [ -n "$INSIDE_EMACS" ] ; then # Emacs shell
    # Emacs内ではTERM=dumpでシェルが開かれる
    # これにより、starshipがdumpモードで起動するのを防ぐ
    export TERM=xterm-256color
    eval "$(starship init bash)"
    export TERM=dump
else
    eval "$(starship init bash)"
fi


# Ruby
export RBENV_ROOT="${XDG_DATA_HOME}/rbenv"
if [ -d "$RBENV_ROOT" ] ; then
    PATH="$RBENV_ROOT/bin:$PATH"
    eval "$(rbenv init -)"
fi
export SOLARGRAPH_CACHE="${XDG_CACHE_HOME}/solargraph"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# This loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# ts-node
export TS_NODE_HISTORY="${XDG_CACHE_HOME}/ts-node/history"

# Docker
PATH="/usr/libexec/docker/cli-plugins:$PATH"

# all pip package upgrade
if type pip > /dev/null 2>&1 ; then
    alias pip-upgrade-all="pip list -o | tail -n +3 | awk '{ print \$1 }' | xargs pip install -U"
fi

if type bat > /dev/null 2>&1 ; then
    help() {
        "$@" --help 2>&1 | bat --plain --language=help
    }
fi

# fzf
[ -f ~/.fzf.bash ]  && source ~/.fzf.bash

# ripgrep
export RIPGREP_CONFIG_PATH="${XDG_CONFIG_HOME}/ripgrep.conf"

# Wakatime
export WAKATIME_HOME="${XDG_CONFIG_HOME}/wakatime"

# screen
export SCREENRC="${XDG_CONFIG_HOME}/screenrc"

# PostgreSQL
export PSQL_HISTORY="${XDG_CACHE_HOME}/psql/history"
