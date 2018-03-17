# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/arch/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U promptinit; promptinit
prompt pure
RPROMPT='%(0?,,%?)'
ENABLE_CORRECTION="true"

# Initialize rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

export QUOTING_STYLE=literal
export FZF_DEFAULT_COMMAND="ag -g ."
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_TMUX=1
export EDITOR=nvim

alias dc='docker-compose'
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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
