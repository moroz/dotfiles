# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"
autoload edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

system=$(uname -s)

autoload -Uz compinit
compinit
# End of lines added by compinstall

PS1=$'%{\e[01;32m%}%m%{\e[m%}:%{\e[01;34m%}%~%{\e[m%}$ '
RPROMPT='%(0?,,%?)'
ENABLE_CORRECTION="true"
export PATH="$HOME/bin:$HOME/.emacs.d/bin:$HOME/.fzf/bin:$PATH"
export GPG_TTY=$(tty)

if which eza >/dev/null; then
  alias ls="eza -la"
elif [ "$system" = "Linux" ]; then
  alias ls="ls --color=auto -las"
else;
  alias ls="ls -G -las"
fi

if which bat >/dev/null; then
  alias cat="bat"
fi

alias rg="rg -S"

export QUOTING_STYLE=literal
export FZF_DEFAULT_COMMAND="rg --files --hidden --ignore -g '!.git'"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_TMUX=1
export EDITOR=nvim
export ERL_AFLAGS="-kernel shell_history enabled"
export DOCKER_BUILDKIT=1

# https://mobile.twitter.com/bernheisel/status/1358201158507061250
# compile erlang with docs using asdf/kerl
export KERL_BUILD_DOCS="yes"

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

alias cdw="cd ~/working"
alias cdd="cd ~/Downloads"

alias ims="iex -S mix phx.server"
alias imtf="iex --dbg pry -S mix test --failed"
alias ml="mix compile"
alias ap="ansible-playbook -v site.yml"
alias up="docker-compose up"
alias down="docker-compose down"
alias tf="terraform"
alias build="docker-compose build"

mm() {
    if [ -f mix.exs ]; then
        mix ecto.migrate
    elif [ -f Gemfile ]; then
        bundle exec rake db:migrate $@
    elif [ -f package.json ]; then
        pnpm db:migrate $@
    elif [ -f Cargo.toml ]; then
        diesel migration run
    fi
}

mr() {
    if [ -f mix.exs ]; then
        mix ecto.rollback $@
    elif [ -f Gemfile ]; then
        bundle exec rake db:rollback $@
    elif [ -f package.json ]; then
        pnpm db:rollback
    elif [ -f Cargo.toml ]; then
      diesel migration revert
    fi
}

mt() {
  if [ -f mix.exs ]; then
    mix test --trace $@
  elif [ -f Gemfile ]; then
    bundle exec rspec $@
  elif [ -f package.json ]; then
    pnpm test $@
  elif [ -f Cargo.toml ]; then
    cargo test $@
  fi
}

alias mtw="cargo watch -x test"

mtf() {
  if [ -f mix.exs ]; then
    mix test --trace --failed $@
  elif [ -f Gemfile ]; then
    bundle exec rspec --only-failures $@
  fi
}

im() {
  if [ -f mix.exs ]; then
    iex -S mix
  elif [ -f Gemfile ]; then
    bundle exec rails c
  elif [ -f package.json ]; then
    pnpm run ts-node
  fi
}

md() {
  if [ -f mix.exs ]; then
    mix deps.get
  elif [ -f Gemfile ]; then 
    bundle
  elif [ -f package.json ]; then
    pnpm
  fi
}

ms() {
  if [ -f mix.exs ]; then
    mix phx.server $@
  elif [ -f next.config.js ] || [ -f vite.config.ts ] || [ -f vite.config.js ] || [ -f vite.config.mjs ]; then
    pnpm dev $@
  elif [ -f book.toml ]; then
    mdbook serve -p 3001 $@
  elif [ -f Gemfile ]; then 
    bundle exec rails server $@
  elif [ -f Procfile ]; then
    foreman start
  elif [ -f vue.config.js ]; then
    pnpm serve
  elif [ -f package.json ]; then
    pnpm start $@
  elif [ -f Cargo.toml ]; then
    cargo watch -x run $@
  elif [ -f modd.conf ]; then
    modd $@
  elif [ -f go.mod ]; then
    air $@
  fi
}

magit() {
  emacsclient -t --eval "(call-interactively #'magit-status)" -a "emacs -nw --eval \"(call-interactively #'magit-status)\""
}

gensecret() {
  LENGTH="${1:-32}"
  SECRET="$(openssl rand -base64 $LENGTH)"

  if [ "$system" = "Darwin" ]; then
    echo $SECRET | tr -d '\n' | pbcopy
  else
    echo $SECRET | tr -d '\n' | xclip -sel c
  fi
}

gpd() {
  branch="$(git rev-parse --abbrev-ref HEAD)"
  git push -u origin "$branch"
}

glc() {
  git log --format=%B -n 1 | tr -d '\n'
}

if [ "$system" = "Darwin" ]; then
  # enable Erlang builds on Catalina
  export CFLAGS="-O2 -g -fno-stack-check"
  # Uncomment this line to build (takes too much time when not building)
  # export KERL_CONFIGURE_OPTIONS="--disable-hipe --with-ssl=$(brew --prefix openssl)"
fi
