#===========================================
# COMMON
#===========================================

#-----------------------------------------
# function
#-----------------------------------------

function show_branch {
    if [ -n "$(__git_ps1)" ]
    then
        echo "$(__git_ps1)"
    else
        vcprompt -f "(%b%u%m)"
    fi
}

#-------------------------------------
# source
#-------------------------------------

source /usr/share/git/completion/git-completion.bash
source /usr/share/git/completion/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUPSTREAM=auto

#-------------------------------------
# alias
#-------------------------------------

alias ls='ls --color=auto --show-control-chars'
alias la='ls -A'
alias ll='ls -lh'
alias l='ls -CF'
alias vi='vim'
alias grep='grep --color=auto'

# 色付きlessコマンドオプション`less -R`を使う
# -X: less終了時に画面をクリアしない
# -q: lessでビープ音を鳴らさない
export LESS='-R -X -q'

#-------------------------------------
# env
#-------------------------------------

PATH=/c/Program\ Files/Java/jdk1.8.0_92/bin/:${PATH}

# Anaconda
CONDA_BASE=/c/Users/${USER}/Anaconda3
CONDA_PATH=${CONDA_BASE}
# if other conda env
# conda_env=/envs/ml4se
conda_env=''
PATH=${CONDA_PATH}/Scripts:${PATH} # conda command
PATH=${CONDA_PATH}${conda_env}/Library/bin:${PATH}
PATH=${CONDA_PATH}${conda_env}/Scripts:${PATH}
PATH=${CONDA_PATH}${conda_env}:${PATH}

export PATH

PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
export PKG_CONFIG_PATH

LIBRARY_PATH=/usr/local/lib
export LIBRARY_PATH

LD_LIBRARY_PATH=/usr/local/lib
export LIBRARY_PATH

# pip bash completion start
_pip_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                             COMP_CWORD=$COMP_CWORD \
                             PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip
# pip bash completion end

#===========================================
# Emacs
#===========================================
if [ "$EMACS" ]; then

    #-------------------------------------
    # lang
    #-------------------------------------

    export LANG=C.UTF8

    #-------------------------------------
    # prompt
    #-------------------------------------

    PS1='\033[0mkei@dn:\w$(__git_ps1 "(%s)")$(show_branch)\$ '

fi
#===========================================
# msys2 or mingw64
#===========================================
if [ -e "/msys2.ico" ]; then # msys2

    #-------------------------------------
    # prompt
    #-------------------------------------

    PS1='\[\033[31m\]kei@dn\[\033[30m\]:\[\033[36m\]\w\[\033[35m\]$(show_branch)\[\033[00m\]\$ '

fi
#===========================================
# msys2 only
#===========================================
# if [ "$MSYSTEM" = "MSYS" ]; then
# fi
#===========================================
# mingw64 only
#===========================================
# if [ "$MSYSTEM" = "MINGW64" ]; then
# fi
