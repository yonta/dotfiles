@echo off

rem set current directory to folder in this script
cd /d %~dp0
set current=%CD%

set home=e:\home
set repopath=%current%

rem directories
mklink /D %home%\.config\git %repopath%\.config\git
mklink /D %home%\.config\hg %repopath%\.config\hg
mklink /D %home%\.config\emacs %repopath%\.config\emacs

rem files
mklink %home%\.profile %repopath%\.profile
mklink %home%\.config\bash\.inputrc %repopath%\.config\bash\.inputrc
mklink %home%\.vimrc %repopath%\.vimrc
mklink %home%\.config\screenrc %repopath%\.config\screenrc
mklink %home%\.config\starship.toml %repopath%\.config\starship.toml
mklink %home%\.config\ripgrep.conf %repopath%\.config\ripgrep.conf
