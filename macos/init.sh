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
# ## tmux
# if [ ! -d "$HOME/.config/tmux" ]; then
#   mkdir -p ~/.config/tmux
# fi
# curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/.tmux.conf > ~/.config/tmux/.tmux.conf
# if [ -e ~/.tmux/plugins/tpm ]; then
#   echo "tpm has already installed."
# else
#   git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
# fi

## starship
if [ ! -e "$HOME/.config/starship.toml" ]; then
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/starship.toml > ~/.config/starship.toml
fi

## alacritty
if [ ! -d "$HOME/.config/alacritty" ]; then
  mkdir -p ~/.config/alacritty
fi
if [ -e "$HOME/.config/alacritty/alacritty.toml" ]; then
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/alacritty.toml > ~/.config/alacritty/alacritty.toml
fi

## karabiner
if [ ! -d "$HOME/.config/karabiner" ]; then
  mkdir -p ~/.config/karabiner
fi
if [ -e "$HOME/.config/karabiner/karabiner.json" ]; then
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/keymap/karabiner.json > ~/.config/karabiner/karabiner.json
fi

## zellij
if [ ! -d "$HOME/.config/zellij" ]; then
  mkdir -p ~/.config/zellij
fi
if [ -e "$HOME/.config/zellij/config.kdl" ]; then
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/zellij/config.kdl > ~/.config/zellij/config.kdl
fi

## asdf
if [[ -z $(asdf list direnv) ]]; then
  asdf plugin add direnv https://github.com/asdf-community/asdf-direnv
  asdf direnv setup --shell zsh --version latest
fi
if [[ -z $(asdf list golang) ]]; then
  asdf plugin add golang https://github.com/kennyp/asdf-golang.git
  asdf install golang 1.24.3
  asdf global golang 1.24.3

  # install development tools
  go install golang.org/x/tools/gopls@latest
  go install golang.org/x/tools/cmd/goimports@latest
  go install github.com/nametake/golangci-lint-langserver@latest
fi
if [[ -z $(asdf list nodejs) ]]; then
  asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
  asdf install nodejs 22.9.0
  asdf global nodejs 22.9.0

  npm install -g \
      eslint \
      prettier \
      textlint \
      typescript-language-server \
      tsc \
      yaml-language-server \
      sql-formatter \
      @anthropic-ai/claude-code \
      @mermaid-js/mermaid-cli
fi
if [[ -z $(asdf list python) ]]; then
  asdf plugin add python https://github.com/asdf-community/asdf-python.git
  asdf install python 3.13.5
  asdf global python 3.13.5
fi
if [[ -z $(asdf list ruby) ]]; then
  asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
  asdf install ruby 3.3.7
  asdf global ruby 3.3.7
fi
if [[ -z $(asdf list ecspresso)]]; then
  asdf plugin add ecspresso
  asdf install ecspresso 2.5.0
  asdf global ecspresso 2.5.0
fi
if [[ -z $(asdf list terraform)]]; then
  asdf plugin add terraform
  asdf install terraform 1.12.2
  asdf global terraform 1.12.2
fi
if [[ -z $(asdf list java)]]; then
  asdf plugin add java
  asdf install java openjdk-21
  asdf global java openjdk-21
fi

# dot config
if [ -e "$HOME/.zshrc" ]; then
  echo ".zshrc already exists."
else
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.zshrc > ~/.zshrc
fi
if [ -e "$HOME/.zshenv" ]; then
  echo ".zshenv already exists."
else
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.zshenv > ~/.zshenv
fi
if [ -e "$HOME/.profile" ]; then
  echo ".profile already exists."
else
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/.profile > ~/.profile
fi

# emacs
if [ ! -d "/Application/Emacs.app" ]; then
  export LSP_USE_PLISTS=true
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/install.sh | sh
  if [ ! -d "$HOME/.emacs.d" ]; then
    mkdir ~/.emacs.d
  fi
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/early-init.el > ~/.emacs.d/early-init.el
  cp -r "$HOME/repos/emacs/nextstep/Emacs.app" /Applications/
  /Applications/Emacs.app/Contents/MacOS/Emacs -Q -batch -f batch-byte-compile ~/.emacs.d/init.el
  curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/shared/emacs/init.el > ~/.emacs.d/init.el
fi
# # install Rust (prompt input required)
# install_rust
