# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall

PS1=$'%{\e[34m%}%m%{\e[m%}:%{\e[32m%}%~%{\e[m%}$ '
RPROMPT='%(0?,,%?)'
ENABLE_CORRECTION="true"

if [ "$(uname)" = "Linux" ]; then
  alias ls="ls --color=auto"
else;
  alias ls="ls -G"
fi

export QUOTING_STYLE=literal
export FZF_DEFAULT_COMMAND="ag -g ."
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_TMUX=1
export EDITOR=nvim
export ERL_AFLAGS="-kernel shell_history enabled"
export LC_CTYPE=zh_TW.UTF-8

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

alias cdw="cd ~/working"
alias cde="cd ~/elixir"

alias im="iex -S mix"
alias ims="iex -S mix phx.server"
alias ms="mix phx.server"
alias mm="mix ecto.migrate"
alias md="mix deps.get"
alias mt="mix test"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

magit() {
  emacsclient -t --eval "(call-interactively #'magit-status)" -a "emacs -nw --eval \"(call-interactively #'magit-status)\""
}
