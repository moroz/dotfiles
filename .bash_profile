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
alias injobs='ssh deployer@injobs.pl'
alias vim="nvim"
alias ga='git add .'
alias gs='git status'
alias gc='git commit'
alias gf='git checkout'
alias gfo='git fetch origin'
alias gd='git diff'
alias gp='git pull'
alias gpu='git push'
alias cdb='cd ~/working/buddy'
alias prc='pry -r ./config/environment'
alias vol='~/bin/vol.rb'
alias his='history 10'
alias imix="iex -S mix"
set -o vi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

