#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs..
# emacs
alias emacs='emacsclient -n'

# env
eval "$(pyenv init -)"
eval "$(rbenv init -)"

# asdf
 . $HOME/.asdf/asdf.sh
 . $HOME/.asdf/completions/asdf.bash
