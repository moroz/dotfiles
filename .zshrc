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

PROMPT='%m:%~$ '
ENABLE_CORRECTION="true"
export PATH="$HOME/bin:$HOME/.emacs.d/bin:$HOME/.fzf/bin:$PATH"
export GPG_TTY=$(tty)

if which eza >/dev/null 2>&1; then
  alias ls="eza -la"
elif [ "$system" = "Linux" ]; then
  alias ls="ls --color=auto -las"
else;
  alias ls="ls -G -las"
fi

if which bat >/dev/null 2>&1; then
  alias cat="bat"
fi

alias rg="rg -S"
alias ...="cd ../.."

export QUOTING_STYLE=literal
export FZF_DEFAULT_COMMAND="rg --files --hidden --ignore -g '!.git' -g '!.jj'"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_TMUX=1
export EDITOR=nvim
export ERL_AFLAGS="-kernel shell_history enabled"
export DOCKER_BUILDKIT=1
export BAT_THEME=zenburn

# https://mobile.twitter.com/bernheisel/status/1358201158507061250
# compile erlang with docs using asdf/kerl
export KERL_BUILD_DOCS="yes"

# TODO: Find an equivalent commit chain for jj
alias giac="git init && git add -A && git commit -m 'Initial commit'"
alias gpd="jj git push --allow-new"
alias vi="nvim"
alias r=". ~/.zshrc"

alias cdw="cd ~/working"
alias cdd="cd ~/Downloads"
alias cdf="cd ~/.dotfiles"

alias ims="iex --dbg pry -S mix phx.server"
alias imtf="iex --dbg pry -S mix test --failed"
alias up="docker compose up"
alias down="docker compose down"
alias build="docker compose build"
alias tf="terraform"
alias mc="mc -b"

is_jj_repo() {
  dir="$(pwd)"

  while [[ "$dir" != "/" ]]; do
    if [[ -d "$dir/.jj" ]]; then
      return 0
    fi
    dir="$(dirname "$dir")"
  done

  return 1
}

gs() {
  if is_jj_repo; then 
    jj st $@
  else
    git status $@
  fi
}

gf() {
  if is_jj_repo; then 
    jj new $@
  else
    git checkout $@
  fi
}

gd() {
  if is_jj_repo; then
    jj diff $@
  else
    git diff $@
  fi
}

gc() {
  if is_jj_repo; then
    jj commit $@
  else
    git commit $@
  fi
}

alias gb="jj bookmark set -r @-"
alias gci="jj commit -i"

ap() {
  playbook="${1:-site.yml}"
  ansible-playbook -v $playbook
}

mm() {
    if [ -f mix.exs ]; then
        mix ecto.migrate
    elif [ -f Gemfile ]; then
        bundle exec rake db:migrate $@
    elif [ -f Cargo.toml ]; then
        diesel migration run
    elif [ -n "${GOOSE_DBSTRING}" ]; then
        goose up
    fi
}

mr() {
    if [ -f mix.exs ]; then
        mix ecto.rollback $@
    elif [ -f Gemfile ]; then
        bundle exec rake db:rollback $@
    elif [ -f Cargo.toml ]; then
      diesel migration revert
    elif [ -n "${GOOSE_DBSTRING}" ]; then
        goose down
    fi
}

mt() {
  if [ -f mix.exs ]; then
    mix test --trace $@
  elif [ -f Gemfile ]; then
    bundle exec rspec $@
  elif [ -f Cargo.toml ]; then
    cargo test $@
  elif [ -f Makefile ]; then
    make test $@
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
  fi
}

md() {
  if [ -f mix.exs ]; then
    mix deps.get
  elif [ -f Gemfile ]; then 
    bundle install $@
  elif [ -f package.json ]; then
    pnpm install $@
  elif [ -f Makefile ] && grep -q '^install:' Makefile; then
    make install
  fi
}

ms() {
  if [ -f mix.exs ]; then
    mix phx.server $@
  elif [ -f next.config.js ] || [ -f vite.config.ts ] || [ -f vite.config.js ] || [ -f vite.config.mjs ]; then
    pnpm dev $@
  elif [ -f modd.conf ]; then
    modd $@
  elif [ -f hugo.toml ]; then
    hugo server -D
  elif [ -f book.toml ]; then
    mdbook serve -p 3001 $@
  elif [ -f Gemfile ]; then 
    bundle exec rails server $@
  elif [ -f Procfile ]; then
    foreman start
  elif [ -f package.json ]; then
    pnpm start $@
  elif [ -f Cargo.toml ]; then
    cargo watch -x run $@
  elif [ -f go.mod ]; then
    go run . $@
  fi
}

msa() {
  if [ -f vite.config.ts ]; then
    pnpm dev $@
  else
    cd assets && pnpm dev $@
  fi
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

if [ "$system" = "Darwin" ]; then
  # enable Erlang builds on Catalina
  export CFLAGS="-O2 -g -fno-stack-check"
  # Uncomment this line to build (takes too much time when not building)
  # export KERL_CONFIGURE_OPTIONS="--disable-hipe --with-ssl=$(brew --prefix openssl)"
fi

alias dark="dconf write /org/gnome/desktop/interface/color-scheme \"'prefer-dark'\""
alias light="dconf write /org/gnome/desktop/interface/color-scheme \"'prefer-light'\""
