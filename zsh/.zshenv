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

# PATH
export GOPATH=$HOME/dev/go-workspace
export PATH=$PATH:$GOPATH/bin
export PATH="/usr/local/opt/libxslt/bin:$PATH"

