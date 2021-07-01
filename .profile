# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# all pip package upgrade
if type pip > /dev/null 2>&1 ; then
    alias pip-upgrade-all="pip list -o | tail -n +3 | awk '{ print \$1 }' | xargs pip install -U"
fi

# GPG
export GPG_TTY=$(tty)

# EDITOR
if type vim > /dev/null 2>&1 ; then
    export EDITOR=vim
fi

# OPAM configuration
if [ -f "$HOME/.opam/opam-init/init.sh" ] ; then
    . /home/kei/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
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
if [ -d "$HOME/.cargo" ] ; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# Ruby
if [ -d "$HOME/.rbenv" ] ; then
    PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# This loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# OchaCaml
if [ -d "${HOME}/.ochacaml" ]; then
    PATH="$PATH:$HOME/.ochacaml/OchaCaml"
fi

#=====================================================================
# GitKraken
#   https://www.it-swarm.dev/ja/git/%E3%83%AD%E3%82%B0%E3%82%A4%E3%83%B3%E6%99%82%E3%81%ABsshagent%E3%82%92%E8%B5%B7%E5%8B%95%E3%81%99%E3%82%8B/1042011156/
#   http://mah.everybody.org/docs/ssh
#=====================================================================

SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

# set Aliases
alias ls='ls --color=auto --show-control-chars'
alias grep='grep --color=auto'
if type exa > /dev/null 2>&1 ; then
    # use exa
    alias l='exa'
    alias la='exa -a'
    alias ll='exa -lFg --git'
    alias lla='exa -alFg --git'
else
    # use ls
    alias l='ls'
    alias la='ls -A'
    alias ll='ls -lFh'
    alias lla='ls -AlFh'
fi
if type vim > /dev/null 2>&1 ; then
    alias vi='vim'
    alias lessv='/usr/share/vim/vim81/macros/less.sh'
    alias vless='lessv'
fi
if type emacs > /dev/null 2>&1 ; then
    alias spacemacs='env HOME=${HOME}/.spacemacs emacs'
    alias emacsc='emacs -Q --batch -f batch-byte-compile'
fi
if type gitkraken > /dev/null 2>&1 ; then
    alias gitkraken="GDK_SCALE=2 gitkraken 1>/dev/null 2>/dev/null"
fi
