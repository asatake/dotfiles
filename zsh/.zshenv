
# starship
export STARSHIP_CACHE=~/.starship/cache

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

#export GOPATH=$HOME/go
#export PATH=$PATH:$GOPATH/bin
#export PATH=$HOME/.nodebrew/current/bin:$PATH

## docker settings
export DOCKER_BUILDKIT=1
export COMPOSE_DOCKER_CLI_BUILD=1

## Set path for pyenv
#export PYENV_ROOT="${HOME}/.pyenv"

## nvm settings
#export NVM_DIR="$HOME/.nvm"
#export NVM_SYMLINK_CURRENT=true
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
#export PATH="$NVM_DIR/current/bin:$PATH"

## nodejs settings
#export PATH=$PATH:$ASDF_USER_SHIMS


## rbenv settings
# eval "$(rbenv init -)"

# ## pyenv settings
# if [ -d "${PYENV_ROOT}" ]; then
#     export PATH=${PYENV_ROOT}/bin:$PATH
#     eval "$(pyenv init -)"
#     eval "$(pyenv virtualenv-init -)"
# fi

# ## haskell stack settings
# export PATH=~/.local/bin:$PATH

# ## aws
# export PATH=$PATH:$USER_BASE_PATH/bin

# ## fzf
# export FZF_TMUX=1
# export FZF_DEFAULT_OPTS="--height 40% --extended --cycle --reverse --border --bind ctrl-k:kill-line"

# ## rust
# export PATH="$HOME/.cargo/bin:$PATH"
# export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
#export RUST_SRC_PATH="/home/manome/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
#export RUST_SRC_PATH=/usr/local/src/rust/src
