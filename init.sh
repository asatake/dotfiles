#!/bin/bash

function exists() {
    which "$1" > /dev/null 2>&1
    return $?
}

function install_package() {
    if exists "apt";then
        sudo apt install -y $1
    elif exists "brew";then
        brew install $1
    elif exists "pacman";then
        sudo pacman -Syu $1
    fi
    return $?
}

function install_if_not_exists() {
    if exists $1;then
        echo "$1 is already installed."
    else
        install_package $1
    fi
}

function install_fzf() {
    echo "Start installing fzf."
    success=install_package fzf
    if -n success;then
        git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
        ~/.fzf/install
    fi
    echo "End installing fzf."
}

function install_ghq() {
    echo "Start installing ghq."
    go get github.com/motemen/ghq
    echo "End installing ghq"
}

function install_nvm() {
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash
}

function install_rust() {
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    rustup update
}


install_if_not_exists git
install_if_not_exists zsh
install_if_not_exists tmux
install_if_not_exists vim
install_if_not_exists emacs
install_if_not_exists curl
install_if_not_exists alacritty
install_if_not_exists go

install_fzf
install_ghq
install_nvm
install_rust

# tmux setting
cp ./.tmux.conf $HOME/.tmux.conf
echo 'alias tmux="tmux -f ~/.tmux.conf"' >> $HOME/.zshrc

# fzf setting
echo <<EOS >> $HOME/.zshrc
## fzf
function fzf-history-widget() {
    local tac=${commands[tac]:-"tail -r"}
    BUFFER=$( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | sed 's/ *[0-9]* *//' | eval $tac | awk '!a[$0]++' | fzf +s)
    CURSOR=$#BUFFER
    zle clear-screen
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget

EOS

echo <<EOS >> $HOME/.zshenv
## fzf
export FZF_TMUX=1
export FZF_DEFAULT_OPTS="--height 40% --extended --cycle --reverse --border --bind ctrl-k:kill-line"

EOS

# ghq setting
echo <<EOS >> $HOME/.zshrc
alias g='cd $(ghq list --full-path | fzf)'
alias gh='hub browse $(ghq list | fzf | cut -d "/" -f 2,3)'

EOS

# emacs setting
echo "alias ec='emacsclient -n'" >> $HOME/.zshrc
mkdir $HOME/.emacs.d
cp ./emacs/initialize.el $HOME/.emacs.d/init.el


echo "==============="
echo "=  INSTALLED  ="
echo "==============="
