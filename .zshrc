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

PS1=$'%{\e[34m%}%m%{\e[m%}:%{\e[32m%}%~%{\e[m%}$ '
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
export LC_CTYPE=zh_TW.UTF-8

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
alias mr="mix ecto.rollback"

mm() {
    if [ -f mix.exs ]; then
        mix ecto.migrate
    fi
    if [ -f package.json ]; then
        yarn db:migrate
    fi
}

mt() {
  if [ -f mix.exs ]; then
    mix test --trace
  fi
  if [ -f package.json ]; then
    yarn test
  fi
}

im() {
  if [ -f mix.exs ]; then
    iex -S mix
  fi
  if [ -f package.json ]; then
    yarn run ts-node
  fi
}

serve() {
  DIR=${1:-.}
  nginx -p $DIR -c ~/.dotfiles/nginx_cwd.conf
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

md() {
  if [ -f Gemfile ]; then 
    bundle
  fi
  if [ -f mix.exs ]; then
    mix deps.get
  fi
}

ms() {
  if [ -f Gemfile ]; then 
    rails server
  fi
  if [ -f mix.exs ]; then
    mix phx.server
  fi
  if [ -f package.json ]; then
    yarn start
  fi
}

magit() {
  emacsclient -t --eval "(call-interactively #'magit-status)" -a "emacs -nw --eval \"(call-interactively #'magit-status)\""
}

serve() {
  nginx -p . -c ~/.dotfiles/nginx-serve-cwd.conf
}

if [ "$system" = "Darwin" ]; then
  # enable Erlang builds on Catalina
  export CFLAGS="-O2 -g -fno-stack-check"
  # Uncomment this line to build (takes too much time when not building)
  # export KERL_CONFIGURE_OPTIONS="--disable-hipe --with-ssl=$(brew --prefix openssl)"
fi
