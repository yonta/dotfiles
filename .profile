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
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
# WSLGによって設定済み
# XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir

# if running bash
if [ -n "${BASH_VERSION}" ]; then
    export HISTFILE="${XDG_CONFIG_HOME}/bash/history"
    export INPUTRC="${XDG_CONFIG_HOME}/bash/inputrc"

    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
        . "${HOME}/.bashrc"
    fi
fi

# Rust and cargo
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
if [ -d "${CARGO_HOME}" ] ; then
    # PATH="${CARGO_HOME}/bin:${PATH}"
    . "/home/kei/.local/share/cargo/env"
fi

# set Aliases
alias grep='grep --color=auto'
if type eza > /dev/null 2>&1 ; then
    # use eza
    alias ls='eza'
    alias l='eza'
    alias la='eza --all'
    alias ll='eza --long --classify --group --git --time-style=long-iso'
    alias lla='ll --all'
else
    # use ls
    alias ls='ls --color=auto --show-control-chars'
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
    alias emacsc='emacs -Q --batch -f batch-byte-compile'
fi
if type gitkraken > /dev/null 2>&1 ; then
    alias gitkraken="GDK_SCALE=2 gitkraken 1>/dev/null 2>/dev/null"
fi

# apt package bat/fd alias
if ! type bat > /dev/null 2>&1 && dpkg -l bat > /dev/null 2>&1 ; then
    alias bat='batcat'
fi
if ! type fd > /dev/null 2>&1 && dpkg -l fd-find > /dev/null 2>&1 ; then
    alias fd='fdfind'
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
if [ -d "${HOME}/bin" ] ; then
    PATH="${HOME}/bin:${PATH}"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/.local/bin" ] ; then
    PATH="${HOME}/.local/bin:${PATH}"
fi

# OPAM configuration
export OPAMROOT="${XDG_DATA_HOME}/opam"
if [ -f "${OPAMROOT}/opam-init/init.sh" ] ; then
    . ${OPAMROOT}/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

# OchaCaml
if [ -d "${HOME}/.ochacaml" ]; then
    PATH="${PATH}:${HOME}/.ochacaml/OchaCaml"
fi

# SML/NJ PATH
if [ -d "${HOME}/.smlnj" ] ; then
    PATH="${HOME}/.smlnj/bin:${PATH}"
fi

# WSL
if [ -n "${WSLENV}" ] ; then
    if [ -f "/mnt/c/Program Files/Mozilla Firefox/firefox.exe" ] ; then
        alias firefox-win="/mnt/c/Program\ Files/Mozilla\ Firefox/firefox.exe"
    fi
    # for wsl tool deb package
    PATH="${PATH}:/mnt/c/Windows"
    PATH="${PATH}:/mnt/c/Windows/System32"
fi

# Starship
# cargo install starship
if type starship > /dev/null 2>&1 ; then
    if [ -n "${INSIDE_EMACS}" ] ; then # Emacs shell
        # Emacs内ではTERM=dumpでシェルが開かれる
        # これにより、starshipがdumpモードで起動するのを防ぐ
        export TERM=xterm-256color
        eval "$(starship init bash)"
        export TERM=dump
    else
        eval "$(starship init bash)"
    fi
fi

# Ruby
export RBENV_ROOT="${XDG_DATA_HOME}/rbenv"
if [ -d "${RBENV_ROOT}" ] ; then
    PATH="${RBENV_ROOT}/bin:${PATH}"
    eval "$(rbenv init -)"
fi
export BUNDLE_USER_HOME="${XDG_CONFIG_HOME}/bundle"
export SOLARGRAPH_CACHE="${XDG_CACHE_HOME}/solargraph"

# nvm
export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
# This loads nvm
[ -s "${NVM_DIR}/nvm.sh" ] && \. "${NVM_DIR}/nvm.sh"
# This loads nvm bash_completion
[ -s "${NVM_DIR}/bash_completion" ] && \. "${NVM_DIR}/bash_completion"

# npm
# export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npmrc"
export NPM_CONFIG_CACHE="${XDG_CACHE_HOME}/npm"

# node
export NODE_REPL_HISTORY="${XDG_CACHE_HOME}/node/history"

# ts-node
export TS_NODE_HISTORY="${XDG_CACHE_HOME}/ts-node/history"

# bun
export BUN_INSTALL="${XDG_DATA_HOME}/bun"
if [ -d "${BUN_INSTALL}" ] ; then
    export PATH="${BUN_INSTALL}/bin:${PATH}"
fi

# all pip package upgrade
if type pip > /dev/null 2>&1 ; then
    alias pip-upgrade-all="pip list -o | tail -n +3 | awk '{ print \$1 }' | xargs pip install -U"
fi

# bat help
if type bat > /dev/null 2>&1 ; then
    help() {
        "$@" --help 2>&1 | bat --plain --language=help
    }
fi
alias h='help'

# grip
export GRIPHOME="${XDG_CONFIG_HOME}/grip"

# fzf
if [ -f "${XDG_CONFIG_HOME}/fzf/fzf.bash" ]; then
    source "${XDG_CONFIG_HOME}/fzf/fzf.bash"

    # fzf-tab-completion
    # https://github.com/lincheney/fzf-tab-completion
    #
    # tab補完にfzfを利用する
    # GitHubからbash用ファイルを取得した
    source ~/.config/bash/fzf-bash-completion.sh

    # 端末のときのみ
    if [ -t 1 ] ; then
        # TABに割り当て
        bind -x '"\t": fzf_bash_completion'
        # Shift TABに割り当て
        # bind -x '"\e[Z": fzf_bash_completion'
    fi
fi

# ripgrep
export RIPGREP_CONFIG_PATH="${XDG_CONFIG_HOME}/ripgrep.conf"

# Wakatime
export WAKATIME_HOME="${XDG_CONFIG_HOME}/wakatime"

# screen
export SCREENRC="${XDG_CONFIG_HOME}/screenrc"

# PostgreSQL
export PSQL_HISTORY="${XDG_CACHE_HOME}/psql/history"

# GnuPG
export GNUPGHOME="${XDG_CONFIG_HOME}/gnupg"

# aptitude
if [ -f "${XDG_CONFIG_HOME}/aptitude/config" ]; then
    export APT_CONFIG="${XDG_CONFIG_HOME}/aptitude/config"
fi
