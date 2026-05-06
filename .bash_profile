# shellcheck shell=bash
# ~/.bash_profile: executed by bash(1) for login shells.

if [ -f "${HOME}/.bash_env" ]; then
    source "${HOME}/.bash_env"
fi

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# WSLのみ
if [ -n "${WSLENV}" ] ; then
    # SSH ログインじゃない、画面がある、VSCode Remote じゃない
    if [ -z "${SSH_CLIENT}" ] &&
           xrandr > /dev/null 2>&1 &&
           [[ ":${WSLENV:-}:" != *:VSCODE:* ]] ; then
        # WSLg用にHi-DPIに対応させる
        export GDK_SCALE=2
        # 全角半角キーが連打されるのを防ぐ
        xset -r 49
    fi

    # WSL1のみ
    if uname -a | grep -e 'Microsoft' > /dev/null 2>&1 ; then
        # Change default file and directory permission for WSL
        umask 0022
        export DOCKER_HOST='tcp://localhost:2375'
        # SSHログインじゃないとき
        if [ -z "${SSH_CLIENT}" ]; then
            export DISPLAY=localhost:0
        fi
    fi
fi


# インタラクティブ bash シェルであれば ~/.bashrc を読み込む
if [ -n "${BASH_VERSION:-}" ] && [[ $- == *i* ]] && [ -f "${HOME}/.bashrc" ] ; then
    source "${HOME}/.bashrc"
fi
