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
    echo "xcode-select has installed."
else
    echo "Require Xcode. Please install via AppStore."
    exit 2
fi

# install Homebrew
if exists "brew"; then
    echo "Homebrew is already installed."
else
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Download Brewfile, and install
curl -OL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/Brewfile
brew bundle

# config
## tmux
mkdir -p ~/.config/tmux
curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/.tmux.conf > ~/.config/tmux/.tmux.conf
if [ -e ~/.tmux/plugins/tpm ]; then
    echo "tpm has already installed."
else
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

## starship
curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/starship.toml > ~/.config/starship.toml

## alacritty
mkdir -p ~/.config/alacritty
curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/alacritty.yml > ~/.config/alacritty/alacritty.yml

## asdf

if [[ -z $(asdf list direnv) ]]; then
    asdf plugin add direnv https://github.com/asdf-community/asdf-direnv
    asdf install direnv 2.32.0
    asdf global direnv 2.32.0
fi
if [[ -z $(asdf list golang) ]]; then
    asdf plugin add golang https://github.com/kennyp/asdf-golang.git
    asdf install golang 1.19.2
    asdf global golang 1.19.2
fi
if [[ -z $(asdf list nodejs) ]]; then
    asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
    asdf install nodejs 16.18.0
    asdf global nodejs 16.18.0
fi
if [[ -z $(asdf list python) ]]; then
    asdf plugin add python https://github.com/asdf-community/asdf-python.git
    asdf install python 3.10.8
    asdf global python 3.10.8
fi
if [[ -z $(asdf list ruby) ]]; then
    asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
    asdf install ruby 3.0.1
fi
# asdf plugin add rust https://github.com/asdf-community/asdf-rust.git

# dot config
if [ -e "$HOME/.zshrc" ]; then
    curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.zshrc > ~/.zshrc
fi
if [ -e "$HOME/.zshenv" ]; then
    curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.zshenv > ~/.zshenv
fi
if [ -e "$HOME/.profile" ]; then
    curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.profile > ~/.profile
fi

# emacs
export LIBRARY_PATH=/usr/local/opt/gcc/lib/gcc/12:/usr/local/opt/libgccjit/lib/gcc/12:/usr/local/opt/gcc/lib/gcc/12/gcc/x86_64-apple-darwin21/12
export LSP_USE_PLISTS=true
curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/install.sh | sh
mkdir ~/.emacs.d
curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/early-init.el > ~/.emacs.d/early-init.el
/Applications/Emacs.app/Contents/MacOS/Emacs -Q -batch -f batch-byte-compile ~/.emacs.d/*.el
curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/init.el > ~/.emacs.d/init.el

# # install Rust (prompt input required)
# install_rust
