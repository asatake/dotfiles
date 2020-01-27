#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

# ja (for hyper.js)
export LESSCHARSET=utf-8
export LANG=ja_JP.UTF-8
export PATH=$PATH:$USER_BASE_PATH/bin

## fzf
export FZF_TMUX=1
export FZF_DEFAULT_OPTS="--height 40% --extended --cycle --reverse --border --bind ctrl-k:kill-line"

## rust
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
#export RUST_SRC_PATH="/home/manome/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
#export RUST_SRC_PATH=/usr/local/src/rust/src

# PATH
export GOPATH=$HOME/dev/go-workspace
export PATH=$PATH:$GOPATH/bin
export PATH="/usr/local/opt/libxslt/bin:$PATH"


