if [ -f ~/.bashrc ]; then
source ~/.bashrc
fi

PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[35;1m\]\w\[\033[m\]\$ "
export TMPDIR="/tmp"
export FZF_DEFAULT_COMMAND="ag -g ."

alias tmux="env TERM=xterm-256color tmux"
alias dc='sudo docker-compose'
alias docker='sudo docker'
alias crspec='COVERAGE=true bundle exec rspec'
alias rspec='bundle exec rspec'
alias dep='git push origin master && cap production deploy'
alias mc='mc -b'
alias vim="nvim"
alias ga='git add .'
alias gs='git status'
alias gc='git commit'
alias gd='git diff'
set -o vi
