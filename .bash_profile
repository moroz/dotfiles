source $HOME/.bashrc
PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[35;1m\]\w\[\033[m\]\$ "
export TMPDIR="/tmp"
export FZF_DEFAULT_COMMAND="ag -g ."
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_TMUX=1
export EDITOR=nvim

alias dc='docker-compose'
alias crspec='COVERAGE=true bundle exec rspec'
alias rspec='bundle exec rspec'
alias dep='git push origin master && cap production deploy'
alias vim="nvim"
alias ga='git add .'
alias gs='git status'
alias gc='git commit'
alias gf='git checkout'
alias gfo='git fetch origin'
alias gd='git diff'
alias gp='git pull'
alias gpu='git push'
alias ls='ls --color=auto'
alias vol='~/bin/vol.rb'
alias his='history 10'
alias imix="iex -S mix"
alias drw="docker-compose run web"
set -o vi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

