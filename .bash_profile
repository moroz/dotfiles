eval "$(rbenv init -)"
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[35;1m\]\w\[\033[m\]\$ "
export FZF_DEFAULT_COMMAND='fd --type f'
export TMPDIR=/tmp

alias tmux="TERM=screen-256color-bce tmux"
alias mc='mc -b'
alias vim="nvim"
alias ga='git add .'
alias gs='git status'
alias gc='git commit'
alias gd='git diff'
