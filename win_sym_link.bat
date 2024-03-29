@echo off

rem set current directory to folder in this script
cd /d %~dp0
set current=%CD%

set home=e:\home
set repopath=%current%

mklink %home%\.profile %repopath%\.profile
mklink %home%\.config\bash\.inputrc %repopath%\.config\bash\.inputrc
mklink %home%\.config\git\config %repopath%\.config\git\config
mklink %home%\.config\git\ignore %repopath%\.config\git\ignore
mklink %home%\.hgrc %repopath%\.hgrc
mklink %home%\.hgignore_global %repopath%\.hgignore_global
mklink /D %home%\.emacs.d %repopath%\.emacs.d
mklink %home%\.vimrc %repopath%\.vimrc
mklink %home%\.config/screenrc %repopath%\.config/screenrc
mklink %home%\.config\starship.toml %repopath%\.config\starship.toml
mklink %home%\.config\ripgrep.conf %repopath%\.config\ripgrep.conf
