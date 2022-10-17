#!/usr/bin/env bash

set -eu
shopt -s expand_aliases

function exists() {
    which "$1" > /dev/null 2>&1
    return $?
}

function install_rust() {
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    rustup update
}

echo "MacOS install script start..."
cd ~/

# install xcode developer tools
if exists "xcode-select"; then
    xcode-select --install
else
    echo "Require Xcode. Please install via AppStore."
    exit 2
fi

# install Homebrew
if exists "brew"; then
    echo "Homebrew is already installed."
else
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Download Brewfile, and install
curl -OL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/Brewfile
brew bundle

# config
## tmux
mkdir -p ~/.config/tmux
wget https://raw.githubusercontent.com/asatake/dotfiles/main/shared/.tmux.conf -o ~/.config/tmux/.tmux.conf

## starship
wget https://raw.githubusercontent.com/asatake/dotfiles/main/shared/starship.toml -o ~/.config/starship.toml

## alacritty
mkdir -p ~/.config/alacritty
wget https://raw.githubusercontent.com/asatake/dotfiles/main/shared/alacritty.yml -o ~/.config/alacritty/alacritty.yml

## asdf
asdf plugin add direnv https://github.com/asdf-community/asdf-direnv
asdf install direnv 2.32.0
asdf plugin add golang https://github.com/kennyp/asdf-golang.git
asdf install golang 1.9
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf install nodejs lts
asdf plugin add python https://github.com/asdf-community/asdf-python.git
asdf install python 3.10.8
asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf install ruby 3.0.1
asdf plugin add rust https://github.com/asdf-community/asdf-rust.git

# dot config
wget https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.zshrc -o ~/.zshrc
wget https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.zshenv -o ~/.zshenv
wget https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.profile -o ~/.profile

# emacs
export LIBRARY_PATH=/usr/local/opt/gcc/lib/gcc/12:/usr/local/opt/libgccjit/lib/gcc/12:/usr/local/opt/gcc/lib/gcc/12/gcc/x86_64-apple-darwin21/12
export LSP_USE_PLISTS=true
../shared/emacs/install.sh
mkdir ~/.emacs.d
wget https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/early-init.el -o ~/.emacs.d/early-init.el
/Applications/Emacs.app/Contents/MacOS/Emacs -Q -batch -f batch-byte-compile ~/.emacs.d/*.el
wget https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/init.el -o ~/.emacs.d/init.el

# # install Rust (prompt input required)
# install_rust
