# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
zstyle :compinstall filename "$HOME/.zshrc"
autoload edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

system=$(uname -s)

autoload -Uz compinit
compinit
# End of lines added by compinstall

PS1=$'%{\e[01;32m%}%m%{\e[m%}:%{\e[01;34m%}%~%{\e[m%}$ '
ENABLE_CORRECTION="true"

if [ "$system" = "Linux" ]; then
  alias ls="ls --color=auto -las"
else;
  alias ls="ls -G -las"
fi

export QUOTING_STYLE=literal
export EDITOR=nvim
export DOCKER_BUILDKIT=1
export ERL_AFLAGS="-kernel shell_history enabled"

alias ga='git add -A'
alias giac="git init && git add -A && git commit -m 'Initial commit'"
alias gac='git add -A; git commit'
alias gc='git commit'
alias gca='git commit --amend'
alias gd='git diff'
alias gf='git checkout'
alias glol="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gm="git merge"
alias gp='git push'
alias gs='git status'
alias vi="nvim"
