# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    # alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# ls
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

# vim
if type vim > /dev/null 2>&1 ; then
    alias vi='vim'
    alias lessv='/usr/share/vim/vim82/macros/less.sh'
    alias vless='lessv'
fi

# emacs
if type emacs > /dev/null 2>&1 ; then
    alias emacsc='emacs -Q --batch -f batch-byte-compile'
fi

# apt package bat/fd alias
if ! type bat > /dev/null 2>&1 && dpkg -l bat > /dev/null 2>&1 ; then
    alias bat='batcat'
fi
if ! type fd > /dev/null 2>&1 && dpkg -l fd-find > /dev/null 2>&1 ; then
    alias fd='fdfind'
fi

# bat help
alias h='bathelp'
alias hh='batman'

# all pip package upgrade
if type pip > /dev/null 2>&1 ; then
    pip-upgrade-all() {
        pip list -o | tail -n +3 | awk '{ print $1 }' | xargs pip install -U
    }
fi

# gitkraken
alias gitkraken="GDK_SCALE=2 gitkraken 1>/dev/null 2>/dev/null"

# GitHub Copilot CLI
# HACK: aws-copilotとgithub-copilotが同じコマンド名でコンフリクトする
#       mise execをaliasして回避する
alias github-copilot='mise exec npm:github-copilot -- copilot'

# WSLでWindowsのFirefoxを設定
if [ -f "/mnt/c/Program Files/Mozilla Firefox/firefox.exe" ] ; then
    alias firefox-win="/mnt/c/Program\ Files/Mozilla\ Firefox/firefox.exe"
fi

# WSL で Mac の pbcopy/pbpaste を使う
alias pbcopy='/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -command "[Console]::InputEncoding = [System.Text.Encoding]::UTF8; Set-Clipboard -Value ([Console]::In.ReadToEnd())"'
alias pbpaste='/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -command "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8; Get-Clipboard"'
