eval "$(starship init zsh)"

autoload -Uz compinit && compinit
autoload -Uz colors && colors
zstyle ':completion:*:default' menu select=2

setopt complete_in_word

alias ls="exa"
alias ll="ls -l"
alias la="ls -la"
alias tmux="tmux -f ~/.config/tmux/.tmux.conf"
alias E="open -a Emacs.app"
alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias ecbatch="cd $HOME/.emacs.d && emacs -Q -batch -f batch-byte-compile *.el"
alias ec='emacsclient -n'

alias g='cd $(ghq list --full-path | fzf)'
alias hrg='hgrep'

## fzf
function fzf-history-widget() {
    local tac=${commands[tac]:-"tail -r"}
    BUFFER=$( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | sed 's/ *[0-9]* *//' | eval $tac | awk '!a[$0]++' | fzf +s)
    CURSOR=$#BUFFER
    zle clear-screen
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget


# asdf
. /usr/local/opt/asdf/asdf.sh

# direnv
direnv() { asdf exec direnv "$@"; }
eval "$(direnv hook zsh)"

alias lzd='lazydocker'

alias norg="gron --ungron"
alias ungron="gron --ungron"
