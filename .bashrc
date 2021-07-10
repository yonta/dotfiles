# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

# git/hg ps1 function
function show_branch {
    if [ -n "$(__git_ps1)" ]
    then
        echo "$(__git_ps1)"
    else
        vcprompt -f "(%b%u%m)"
    fi
}

if [ "$color_prompt" = yes ]; then
    PS1_1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h'
    OSNAME=''
    PS1_2='\[\033[00m\]:\[\033[01;34m\]\w\[\033[35m\]$(show_branch)\[\033[00m\]\$ '
    # WSLでubuntu/openSUSEの両方がある場合、OS名をいれる
    if uname -a | grep -e 'Microsoft' -e 'microsoft' > /dev/null 2>&1 &&
           type "ubuntu.exe" > /dev/null 2>&1 &&
           type "openSUSE-42.exe" > /dev/null 2>&1 ; then
        if grep 'Ubuntu' /etc/os-release > /dev/null 2>&1; then
            OSNAME='-ubuntu'
        elif grep 'openSUSE' /etc/os-release > /dev/null 2>&1; then
            OSNAME='-openSUSE'
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

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# by User

# --RAW-CONTROL-CHARS: カラーシーケンスを処理する
# --no-init: less終了時に画面をクリアしない
# --quiet: lessでビープ音を鳴らさない
export LESS='--RAW-CONTROL-CHARS --no-init --quiet'

# git ps1 settings
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUPSTREAM=auto

# WSL1
if uname -a | grep -e 'Microsoft' > /dev/null 2>&1 ; then
    # Change default file and directory permission for WSL
    umask 0022
    export DOCKER_HOST='tcp://localhost:2375'
    if [ -z "$SSH_CLIENT" ]; then # not via ssh
        export DISPLAY=localhost:0
    fi
fi

# WSL2
if uname -a | grep -e 'microsoft' > /dev/null 2>&1 ; then
    if [ -z "$SSH_CLIENT" ]; then # not via ssh
        export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0.0
    fi
fi

# WSL1 or WSL2
if uname -a | grep -e 'Microsoft' -e 'microsoft' > /dev/null 2>&1 &&
       [ -z "$SSH_CLIENT" ]; then
    # ディスプレイが存在しVSCode WSLじゃない
    if xrandr > /dev/null 2>&1 &&
           echo "${WSLENV}" | grep -v "VSCODE" > /dev/null 2>&1  ; then
        export GTK_IM_MODULE=fcitx
        export QT_IM_MODULE=fcitx
        export XMODIFIERS=@im=fcitx
        export DefaultIMModule=fcitx
        export NO_AT_BRIDGE=1
        xset -r 49              # 全角半角キーが連打されるのを防ぐ
        # fcitxが起動してなければ
        if ! ps aux | grep "fcitx" | grep -v "grep" > /dev/null 2>&1 ; then
            fcitx-autostart > /dev/null 2>&1
        fi
        # Scale for High DPI Display
        export GDK_SCALE=2
        export QT_AUTO_SCREEN_SCALE_FACTOR=1
    fi
fi
