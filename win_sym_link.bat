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
mklink %home%\.config\hg\hgrc %repopath%\.config\hg\hgrc
mklink %home%\.config\hg\ignore %repopath%\.config\hg\ignore
mklink /D %home%\.config\emacs %repopath%\.config\emacs
mklink %home%\.vimrc %repopath%\.vimrc
mklink %home%\.config\screenrc %repopath%\.config\screenrc
mklink %home%\.config\starship.toml %repopath%\.config\starship.toml
mklink %home%\.config\ripgrep.conf %repopath%\.config\ripgrep.conf
