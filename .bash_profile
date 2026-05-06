# shellcheck shell=bash
# ~/.bash_profile: executed by bash(1) for login shells.

if [ -f "$HOME/.bash_env" ]; then
    source "$HOME/.bash_env"
fi

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# WSLのみ
if [ -n "${WSLENV}" ] ; then
    # SSH ログインじゃない、画面がある、VSCode Remote じゃない
    if [ -z "${SSH_CLIENT}" ] &&
           xrandr > /dev/null 2>&1 &&
           [[ ":${WSLENV:-}:" != "*:VSCODE:*" ]] ; then
        # 全角半角キーが連打されるのを防ぐ
        xset -r 49
    fi

    # WSL1のみ
    if uname -a | grep -e 'Microsoft' > /dev/null 2>&1 ; then
        # Change default file and directory permission for WSL
        umask 0022
    fi
fi


# インタラクティブ bash シェルであれば ~/.bashrc を読み込む
if [ -n "${BASH_VERSION:-}" ] && [[ $- == *i* ]] && [ -f "${HOME}/.bashrc" ] ; then
    # shellcheck disable=SC1091
    source "${HOME}/.bashrc"
fi
