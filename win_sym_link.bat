@echo off

rem set current directory to folder in this script
cd /d %~dp0
set current=%CD%

set home=e:\home
set repopath=%current%

rem directories
mklink /D %home%\.config\bash %repopath%\.config\bash
mklink /D %home%\.config\emacs %repopath%\.config\emacs
mklink /D %home%\.config\git %repopath%\.config\git
mklink /D %home%\.config\hg %repopath%\.config\hg
mklink /D %home%\.config\solargraph %repopath%\.config\solargraph
mklink /D %home%\.config\bat %repopath%\.config\bat

rem files
mklink %home%\.bashrc %repopath%\.bashrc
mklink %home%\.bash_profile %repopath%\.bash_profile
mklink %home%\.vimrc %repopath%\.vimrc
mklink %home%\.config\screenrc %repopath%\.config\screenrc
mklink %home%\.config\starship.toml %repopath%\.config\starship.toml
mklink %home%\.config\ripgrep.conf %repopath%\.config\ripgrep.conf
