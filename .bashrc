# shellcheck shell=bash
# ~/.bashrc: executed by bash(1) for interactive non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

if [ -f "${HOME}/.bash_env" ]; then
    # shellcheck disable=SC1091
    source "${HOME}/.bash_env"
fi

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoredups
export HISTFILE="${XDG_CONFIG_HOME}/bash/history"

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=20000
HISTFILESIZE=20000

# bash history を複数 terminal で同期する
# MEMO: direnv が PROMPT_COMMAND を書き換えるので、そのあとにセットする
__sync_history() {
    history -a
    history -c
    history -r
}
# PROMPT_COMMAND が配列か文字列かで追加の仕方が変わる
# そのため shellcheck をオフにしている
case "$(declare -p PROMPT_COMMAND 2>/dev/null)" in
    # 配列
    declare\ -a*)
        PROMPT_COMMAND+=('__sync_history')
        ;;
    # 文字列
    *)
        # shellcheck disable=SC2178,SC2128
        PROMPT_COMMAND="${PROMPT_COMMAND:+${PROMPT_COMMAND}; }__sync_history"
        ;;
esac
# bash 終了時に履歴を追記せず上書き保存する
# 上記で history 同期しているので追記の必要がない
shopt -u histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
if [ -x /usr/bin/lesspipe ] ; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ]; then
    if [ -r /etc/debian_chroot ]; then
        debian_chroot=$(cat /etc/debian_chroot)
    fi
fi

# Starship
# require: cargo install starship
if type starship > /dev/null 2>&1 ; then
    if [ -n "${INSIDE_EMACS}" ] ; then
        if [ "${TERM}" = "dumb" ] ; then
            # Emacs内ではTERM=dumbでシェルが開かれる
            # これにより、starshipがdumbモードで起動するのを防ぐ
            __saved_term="${TERM}"
            export TERM=xterm-256color
            eval "$(starship init bash)"
            export TERM="${__saved_term}"
            unset __saved_term
        fi
    else
        if [ "${TERM}" != "dumb" ] ; then
            eval "$(starship init bash)"
        fi
    fi

    # 端末タイトルを変更する
    # https://starship.rs/advanced-config/?utm_source=chatgpt.com#change-window-title
    __set_terminal_title() {
        local dir
        case "$PWD" in
            "$HOME")
                dir="~"
                ;;
            "/")
                dir="/"
                ;;
            *)
                dir="$(basename "$PWD")"
                ;;
        esac
        echo -ne "\033]0;${debian_chroot:+($debian_chroot)}${HOSTNAME}: ${dir}\007"
    }
    case "$(declare -p PROMPT_COMMAND 2>/dev/null)" in
        # 配列
        declare\ -a*)
            PROMPT_COMMAND+=('__set_terminal_title')
            ;;
        # 文字列
        *)
            # shellcheck disable=SC2178,SC2128
            PROMPT_COMMAND="${PROMPT_COMMAND:+${PROMPT_COMMAND}; }__set_terminal_title"
            ;;
    esac
else
    # set a fancy prompt (non-color, unless we know we "want" color)
    case "$TERM" in
        xterm-color|*-256color) color_prompt=yes;;
    esac

    # uncomment for a colored prompt, if the terminal has the capability; turned
    # off by default to not distract the user: the focus in a terminal window
    # should be on the output of commands, not on the prompt
    #force_color_prompt=yes

    if [ -n "$force_color_prompt" ]; then
        if [ -x /usr/bin/tput ]; then
            if tput setaf 1 >&/dev/null; then
                # We have color support; assume it's compliant with Ecma-48
                # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
                # a case would tend to support setf rather than setaf.)
                color_prompt=yes
            else
                color_prompt=
            fi
        else
            color_prompt=
        fi
    fi

    # git ps1 settings
    export GIT_PS1_SHOWDIRTYSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUPSTREAM=auto
    # shellcheck disable=SC1091
    source /usr/lib/git-core/git-sh-prompt

    if [ "$color_prompt" = yes ]; then
        # shellcheck disable=SC2016
        PS1_1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h'
        OSNAME=''
        # shellcheck disable=SC2016
        PS1_2='\[\033[00m\]:\[\033[01;34m\]\w\[\033[35m\]$(__git_ps1)\[\033[00m\]\$ '
        # WSLでubuntu/openSUSEの両方がある場合、OS名をいれる
        if uname -a | grep -e 'Microsoft' -e 'microsoft' > /dev/null 2>&1 ; then
            if type "ubuntu.exe" > /dev/null 2>&1 ; then
                if type "openSUSE-42.exe" > /dev/null 2>&1 ; then
                    if grep 'Ubuntu' /etc/os-release > /dev/null 2>&1; then
                        OSNAME='-ubuntu'
                    elif grep 'openSUSE' /etc/os-release > /dev/null 2>&1; then
                        OSNAME='-openSUSE'
                    fi
                fi
            fi
        fi
        PS1="${PS1_1}${OSNAME}${PS1_2}"
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi
    unset color_prompt force_color_prompt

    # If this is an xterm set the title to user@host:dir
    case "$TERM" in
        xterm*|rxvt*)
            PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
            ;;
        *)
            ;;
    esac
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
# shellcheck disable=SC1091
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        source /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    fi
fi

# --RAW-CONTROL-CHARS: カラーシーケンスを処理する
# --no-init: less終了時に画面をクリアしない
# --quiet: lessでビープ音を鳴らさない
export LESS='--RAW-CONTROL-CHARS --no-init --quiet'

# GPG
# -t 0 は標準入力が端末
if [ -t 0 ] ; then
    GPG_TTY=$(tty)
    export GPG_TTY
fi

# mise
# MEMO: rust-analyzer と yarn は mise を優先
#       mise activate を PATH 設定よりも後にする
#       または、mise settings activate_aggressive=true を実行する
if type mise > /dev/null 2>&1 ; then
    eval "$(mise activate bash)"

    # completion の設定
    if ! type usage > /dev/null 2>&1 ; then
        # usage がなければ mise でいれる
        mise use --global usage
    fi
    eval "$(mise completions --include-bash-completion-lib bash)"
fi

# nvm
# MEMO:mise よりも優先するため、mise の設定よりも後におく

# This loads nvm
# shellcheck disable=SC1091
if [ -n "${NVM_DIR}" ] && [ -s "${NVM_DIR}/nvm.sh" ] ; then
    source "${NVM_DIR}/nvm.sh"
fi

# This loads nvm bash_completion
# shellcheck disable=SC1091
if [ -n "${NVM_DIR}" ] && [ -s "${NVM_DIR}/bash_completion" ] ; then
    source "${NVM_DIR}/bash_completion"
fi

# npm completion
if type npm > /dev/null 2>&1 ; then
    eval "$(npm completion)"
fi

# rustup/cargo completions
if type rustup > /dev/null 2>&1 ; then
    eval "$(rustup completions bash)"
    eval "$(rustup completions bash cargo)"
fi

# OPAM configuration
if [ -n "${OPAMROOT}" ] && [ -f "${OPAMROOT}/opam-init/init.sh" ] ; then
    # shellcheck disable=SC1091
    source "${OPAMROOT}/opam-init/init.sh" 2>&1 /dev/null
fi

# Ruby
if type rbenv > /dev/null 2>&1 ; then
    eval "$(rbenv init -)"
fi

# Bundler completions
if type complete_bundle_bash_command > /dev/null 2>&1 ; then
    eval "$(complete_bundle_bash_command init)"
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
if type ruff > /dev/null 2>&1 && [ -r "${XDG_CONFIG_HOME}/bash/completions/ruff" ] ; then
    # shellcheck disable=SC1091
    source "${XDG_CONFIG_HOME}/bash/completions/ruff"
fi

# fzf

# MEMO:
# fzfはinputrcの設定と競合する
# これはキーバインディング以外にも影響する
#   https://github.com/junegunn/fzf/issues/2365
# そのため、inputrcファイルではなく、bindで設定する必要がある
#
# 補完の大文字小文字を無視
bind "set completion-ignore-case on"
bind "set bell-style none"

if type fzf > /dev/null 2>&1 ; then
    eval "$(fzf --bash)"

    # fzf-tab-completion
    # https://github.com/lincheney/fzf-tab-completion
    #
    # tab補完にfzfを利用する
    # GitHubからbash用ファイルを取得した
    if [ -r "${HOME}/.config/bash/completions/fzf-bash-completion.sh" ] ; then
        # shellcheck disable=SC1091
        source "${HOME}/.config/bash/completions/fzf-bash-completion.sh"
        # shellcheck disable=SC2034
        FZF_COMPLETION_AUTO_COMMON_PREFIX="true"
        # shellcheck disable=SC2034
        FZF_COMPLETION_AUTO_COMMON_PREFIX_PART="true"

        # 端末のときのみ
        if [ -t 1 ] ; then
            # TABに割り当て
            bind -x '"\t": fzf_bash_completion'
            # Shift TABに割り当て
            # bind -x '"\e[Z": fzf_bash_completion'
        fi
    fi
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

# direnv
if type direnv > /dev/null 2>&1 ; then
    eval "$(direnv hook bash)"
fi

# fly completions
if type flyctl > /dev/null 2>&1 ; then
    eval "$(flyctl completion bash)"
    # HACK: 短縮 fly コマンドも補完
    complete -o default -F __start_flyctl fly
fi

# ollama completions
if type ollama > /dev/null 2>&1 && [ -r "${XDG_CONFIG_HOME}/bash/completions/ollama" ] ; then
    # shellcheck disable=SC1091
    source "${XDG_CONFIG_HOME}/bash/completions/ollama"
fi

# AWS CLI
if type aws > /dev/null 2>&1 ; then
    # completionの設定、mise のパスを使う
    complete -C '/home/kei/.local/share/mise/installs/aws-cli/latest/aws/dist/aws_completer' aws
fi

# aws-vault
if type aws-vault > /dev/null 2>&1 &&
        [ -r "${XDG_CONFIG_HOME}/bash/completions/aws-vault.bash" ] ; then
    # completionの設定
    # curlで毎回取得せず、必要なら以下に保存したものを読み込む
    # https://github.com/99designs/aws-vault/blob/master/USAGE.md#shell-completion
    # shellcheck disable=SC1091
    source "${XDG_CONFIG_HOME}/bash/completions/aws-vault.bash"
fi

# Terraform
if type terraform > /dev/null 2>&1 ; then
    # completionの設定
    complete -C /usr/bin/terraform terraform
fi

# AWS copilot
if type copilot > /dev/null 2>&1 ; then
    # completionの設定
    eval "$(copilot completion bash)"
fi

# Diesel, RustのORMツール
if type diesel > /dev/null 2>&1 ; then
    # completionの設定
    eval "$(diesel completions bash)"
fi

# actdk
if type actdk > /dev/null 2>&1 ; then
    # completionの設定
    eval "$(actdk generate shell-completion --shell bash)"
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f "${HOME}/.bash_aliases" ]; then
    # shellcheck disable=SC1091
    . "${HOME}/.bash_aliases"
fi
