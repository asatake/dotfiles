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
alias tmux="tmux -f ~/.tmux.conf"
# alias g='cd $(ghq root)/$(ghq list | peco)'
alias g='cd $(ghq list --full-path | fzf)'
alias gh='hub browse $(ghq list | fzf | cut -d "/" -f 2,3)'
alias emacs='emacsclient -n'
alias plantuml='plantuml -config ~/tools/plantuml-scheme.txt'
alias ssm='cd ~/ghq/github.com/itandi/aws-ssm && docker-compose run aws bash'
## fzf
function fzf-history-widget() {
    local tac=${commands[tac]:-"tail -r"}
    BUFFER=$( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | sed 's/ *[0-9]* *//' | eval $tac | awk '!a[$0]++' | fzf +s)
    CURSOR=$#BUFFER
    zle clear-screen
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget


# env
eval "$(pyenv init -)"
eval "$(rbenv init -)"

# asdf
 . $HOME/.asdf/asdf.sh
 . $HOME/.asdf/completions/asdf.bash
