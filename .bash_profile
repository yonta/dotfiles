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

# rustup/cargo completions
if type rustup > /dev/null 2>&1 ; then
    eval "$(rustup completions bash)"
    eval "$(rustup completions bash cargo)"
fi

# GPG
GPG_TTY=$(tty)
export GPG_TTY

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
    # shellcheck disable=SC1091
    ". ${OPAMROOT}/opam-init/init.sh" > /dev/null 2> /dev/null || true
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

# Bundler completions
if type complete_bundle_bash_command > /dev/null 2>&1 ; then
    eval "$(complete_bundle_bash_command init)"
fi

# nvm
export NVM_DIR="${XDG_CONFIG_HOME}/nvm"

# This loads nvm
# shellcheck disable=SC1091
[ -s "${NVM_DIR}/nvm.sh" ] && \. "${NVM_DIR}/nvm.sh"

# This loads nvm bash_completion
# shellcheck disable=SC1091
[ -s "${NVM_DIR}/bash_completion" ] && \. "${NVM_DIR}/bash_completion"

# npm
# export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npmrc"
export NPM_CONFIG_CACHE="${XDG_CACHE_HOME}/npm"

# npm completion
if type npm > /dev/null 2>&1 ; then
    eval "$(npm completion)"
fi

# node
export NODE_REPL_HISTORY="${XDG_CACHE_HOME}/node/history"

# ts-node
export TS_NODE_HISTORY="${XDG_CACHE_HOME}/ts-node/history"

# bun
export BUN_INSTALL="${XDG_DATA_HOME}/bun"
if [ -d "${BUN_INSTALL}" ] ; then
    PATH="${BUN_INSTALL}/bin:${PATH}"
fi

# bat help
if type bat > /dev/null 2>&1 ; then
    bathelp() {
        "$@" --help 2>&1 | bat --plain --language=help
    }
    # command completionをきかせる
    complete -A command bathelp
    complete -A command batman
fi

# Python
if type pipenv > /dev/null 2>&1 ; then
    eval "$(_PIPENV_COMPLETE=bash_source pipenv)"
fi

# pip completions
if type pip > /dev/null 2>&1 ; then
    eval "$(pip completion --bash)"
fi

# Ruff completions
#
# venvやpipenv環境のため、コマンドをevalで評価できない。
# 以下コマンドにて事前に生成したファイルを読み込む
# ruff generate-shell-completion bash > ~/.config/bash/completions/ruff
# shellcheck disable=SC1091
source "${XDG_CONFIG_HOME}/bash/completions/ruff"

# grip
export GRIPHOME="${XDG_CONFIG_HOME}/grip"

# fzf

# 補完の大文字小文字を無視
#
# MEMO:
# fzfはinputrcの設定と競合する
# これはキーバインディング以外にも影響する
#   https://github.com/junegunn/fzf/issues/2365
# そのため、inputrcファイルではなく、bindで設定する必要がある
bind "set completion-ignore-case on"
bind "set bell-style none"

if [ -f "${XDG_CONFIG_HOME}/fzf/fzf.bash" ]; then
    # shellcheck disable=SC1091
    source "${XDG_CONFIG_HOME}/fzf/fzf.bash"

    # fzf-tab-completion
    # https://github.com/lincheney/fzf-tab-completion
    #
    # tab補完にfzfを利用する
    # GitHubからbash用ファイルを取得した
    source "${HOME}/.config/bash/completions/fzf-bash-completion.sh"
    FZF_COMPLETION_AUTO_COMMON_PREFIX="true"
    FZF_COMPLETION_AUTO_COMMON_PREFIX_PART="true"

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

# direnv
if type direnv > /dev/null 2>&1 ; then
    eval "$(direnv hook bash)"
fi

# flyctl
export FLYCTL_INSTALL="${XDG_DATA_HOME}/fly"
export FLY_CONFIG_DIR="${XDG_CONFIG_HOME}/fly"
if [ -d "${FLYCTL_INSTALL}" ] ; then
    PATH="${FLYCTL_INSTALL}/bin:${PATH}"
fi

# fly completions
if type flyctl > /dev/null 2>&1 ; then
    eval "$(fly completion bash)"
fi

# ollama completions
if type ollama > /dev/null 2>&1 ; then
    # shellcheck disable=SC1091
    source "${XDG_CONFIG_HOME}/bash/completions/ollama"
fi

# sshやsu後に端末タイトルを戻す
# https://unix.stackexchange.com/questions/40830/fix-terminal-title-after-ssh-remote-logging-to-another-machine
function resettitle()
{
    # change the title to default of the current window or tab
    (source /etc/lsb-release; echo -ne "\033]0;${DISTRIB_DESCRIPTION}\007")
}

function ssh()
{
    /usr/bin/ssh "$@"
    # revert the window title after the ssh command
    resettitle
}

function su()
{
    # shellcheck disable=SC2117
    /bin/su "$@"
    # revert the window title after the su command
    resettitle
}

# WSLのみの設定
if [ -n "${WSLENV}" ] ; then
    # for wsl tool deb package
    PATH="${PATH}:/mnt/c/Windows"
    PATH="${PATH}:/mnt/c/Windows/System32"

    # SSHログインじゃないとき
    if [ -z "${SSH_CLIENT}" ] ; then
        export BROWSER=wslview
        # WSLg用にHi-DPIに対応させる
        export GDK_SCALE=2

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

    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ]; then
        . "${HOME}/.bashrc"
    fi
fi

if type aws > /dev/null 2>&1 ; then
    # completionの設定
    complete -C '/usr/local/bin/aws_completer' aws

    # aws-cliはXDG CONFIGに対応していない
    # 暫定で環境変数をセットする
    # https://github.com/aws/aws-cli/issues/9031#issuecomment-2448119520
    # aws config
    export AWS_CONFIG_FILE="${XDG_CONFIG_HOME}/aws/config"
    # aws cache
    AWS_DATA_HOME="${XDG_DATA_HOME}/aws"
    export AWS_CLI_HISTORY_FILE="${AWS_DATA_HOME}/history"
    export AWS_CREDENTIALS_FILE="${AWS_DATA_HOME}/credentials"
    export AWS_WEB_IDENTITY_TOKEN_FILE="${AWS_DATA_HOME}/token"
    export AWS_SHARED_CREDENTIALS_FILE="${AWS_DATA_HOME}/shared-credentials"
fi

if type aws-vault > /dev/null 2>&1 ; then
    # completionの設定
    # なんと、curlでとってこいとあった
    # トラブルがおきたらローカルにライセンスとともにコピーも考える
    # https://github.com/99designs/aws-vault/blob/master/USAGE.md#shell-completion
    eval "$(curl -fs https://raw.githubusercontent.com/99designs/aws-vault/master/contrib/completions/bash/aws-vault.bash)"

    # 認証バックエンドにpassを使う
    export AWS_VAULT_BACKEND=pass
    # 認証バックエンドのキー保存先の名前
    export AWS_VAULT_PASS_PREFIX=aws-vault
    # 認証期限切れ時間、デフォルトは1h、最大12hらしい
    export AWS_SESSION_TOKEN_TTL=3h
fi

if type terraform > /dev/null 2>&1 ; then
    # completionの設定
    complete -C /usr/bin/terraform terraform
fi

# AWS copilot
if type copilot > /dev/null 2>&1 ; then
    # completionの設定
    eval "$(copilot completion bash)"
fi

if type diesel > /dev/null 2>&1 ; then
    # completionの設定
    eval "$(diesel completions bash)"
fi

# zig
ZIG_HOME="${XDG_DATA_HOME}/zig"
if [ -d "${ZIG_HOME}" ] ; then
    PATH="${ZIG_HOME}:${PATH}"
fi

# mold, which is YA ld linker
MOLD_HOME="${XDG_DATA_HOME}/mold"
if [ -d "${MOLD_HOME}" ] ; then
    PATH="${MOLD_HOME}/bin:${PATH}"
fi
