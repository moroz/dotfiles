# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"

system=$(uname -s)

autoload -Uz compinit
compinit
# End of lines added by compinstall

PS1=$'%{\e[92;1m%}%m%{\e[m%}:%{\e[96;1m%}%~%{\e[m%}$ '
RPROMPT='%(0?,,%?)'
ENABLE_CORRECTION="true"

if [ "$system" = "Linux" ]; then
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
export LANG=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8
# export LC_CTYPE=zh_TW.UTF-8

alias vim="nvim"
alias ga='git add -A'
alias gs='git status'
alias gc='git commit'
alias gf='git checkout'
alias gfo='git fetch origin'
alias gd='git diff'
alias gp='git push'
alias gpu='git pull'
alias gr='git rev-parse HEAD'

ee() {
    emacsclient --create-frame "${1:-.}"
}

alias cd..="cd .."

alias cdw="cd ~/working"
alias cde="cd ~/elixir"
alias c="code ."

alias ims="iex -S mix phx.server"
alias mtf="mix test --trace --failed"
alias imtf="iex -S mix test --failed"
alias ml="mix compile"
alias ya="yarn add "
alias yad="yarn add -D "
alias ap="ansible-playbook -v site.yml"

mm() {
    if [ -f mix.exs ]; then
        mix ecto.migrate
    elif [ -f composer.json ]; then
      php artisan migrate
    elif [ -f package.json ]; then
        yarn db:migrate
    fi
}

mr() {
    if [ -f mix.exs ]; then
        mix ecto.rollback
    elif [ -f package.json ]; then
        yarn db:rollback
    fi
}

mt() {
  if [ -f mix.exs ]; then
    mix test --trace
  elif [ -f package.json ]; then
    yarn test
  fi
}

im() {
  if [ -f mix.exs ]; then
    iex -S mix
  elif [ -f package.json ]; then
    yarn run ts-node
  fi
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

md() {
  if [ -f mix.exs ]; then
    mix deps.get
  elif [ -f Gemfile ]; then 
    bundle
  elif [ -f package.json ]; then
    yarn
  fi
}

ms() {
  if [ -f mix.exs ]; then
    mix phx.server
  elif [ -f Gemfile ]; then 
    rails server
  elif [ -f composer.json ]; then
    php artisan serve
  elif [ -f package.json ]; then
    yarn start
  fi
}

magit() {
  emacsclient -t --eval "(call-interactively #'magit-status)" -a "emacs -nw --eval \"(call-interactively #'magit-status)\""
}

if [ "$system" = "Darwin" ]; then
  # enable Erlang builds on Catalina
  export CFLAGS="-O2 -g -fno-stack-check"
  # Uncomment this line to build (takes too much time when not building)
  # export KERL_CONFIGURE_OPTIONS="--disable-hipe --with-ssl=$(brew --prefix openssl)"
fi
