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

# Rust and cargo
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
if [ -d "${CARGO_HOME}" ] ; then
    PATH="${CARGO_HOME}/bin:${PATH}"
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

# SML/NJ PATH
if [ -d "${HOME}/.smlnj" ] ; then
    PATH="${HOME}/.smlnj/bin:${PATH}"
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

# bat help
if type bat > /dev/null 2>&1 ; then
    help() {
        "$@" --help 2>&1 | bat --plain --language=help
    }
fi

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

# WSLのみの設定
if [ -n "${WSLENV}" ] ; then
    # for wsl tool deb package
    PATH="${PATH}:/mnt/c/Windows"
    PATH="${PATH}:/mnt/c/Windows/System32"

    # SSHログインじゃないとき
    if [ -z "${SSH_CLIENT}" ] ; then
        # WSL2のGUIでキーボード配列がUSになる暫定対処
        setxkbmap -layout jp -model pc105
        export BROWSER=wslview

        # ディスプレイが存在しVSCode WSLじゃない
        if xrandr > /dev/null 2>&1 &&
                echo "${WSLENV}" | grep -v "VSCODE" > /dev/null 2>&1  ; then
            # 全角半角キーが連打されるのを防ぐ
            xset -r 49
        fi
    fi

    # WSL1のみ
    if uname -a | grep -e 'Microsoft' > /dev/null 2>&1 ; then
        # Change default file and directory permission for WSL
        umask 0022
        export DOCKER_HOST='tcp://localhost:2375'
        if [ -z "${SSH_CLIENT}" ]; then # not via ssh
            export DISPLAY=localhost:0
        fi
    fi
fi

# if running bash
if [ -n "${BASH_VERSION}" ]; then
    export HISTFILE="${XDG_CONFIG_HOME}/bash/history"
    export INPUTRC="${XDG_CONFIG_HOME}/bash/inputrc"

    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
        . "${HOME}/.bashrc"
    fi
fi
