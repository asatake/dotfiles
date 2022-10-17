# . "$HOME/.cargo/env"

export HISTFILE=$HOME/history
export HISTSIZE=1000000
export SAVEHIST=10000000
setopt hist_ignore_dups
setopt share_history
setopt inc_append_history

export PATH="/usr/local/sbin:$PATH"

export GUILE_TLS_CERTIFICATE_DIRECTORY=/usr/local/etc/gnutls/
# export LIBRARY_PATH="/usr/local/Cellar/gcc/10.2.0_4/lib/gcc/10/:/usr/local/Cellar/gcc/10.2.0_4/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0/"

export PATH="$PATH:/usr/local/bin"

#export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
# export PATH=$HOME/.nodebrew/current/bin:$PATH

## docker settings
export DOCKER_BUILDKIT=1
export COMPOSE_DOCKER_CLI_BUILD=1

## aws
export PATH=$PATH:$USER_BASE_PATH/bin

## fzf
export FZF_TMUX=1
export FZF_DEFAULT_OPTS="--height 40% --extended --cycle --reverse --border --bind ctrl-k:kill-line"

# ## rust
# export PATH="$HOME/.cargo/bin:$PATH"
# export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

## asdf
export PATH=$PATH:$HOME/.asdf/shims
export PATH=$PATH:$HOME/.asdf/bin

# ## serverless
# export PATH="$HOME/.serverless/bin:$PATH"

## for emacs
export LSP_USE_PLISTS=true
