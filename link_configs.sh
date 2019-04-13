#!/bin/bash

if [[ ! -f $HOME/.config/nvim/init.vim ]]; then
  mkdir -p $HOME/.config/nvim
  ln -s $HOME/.dotfiles/init.vim $HOME/.config/nvim/init.vim
fi

[[ -f $HOME/.tmux.conf ]] || ln -s $HOME/.dotfiles/.tmux.conf $HOME/.tmux.conf
[[ -f $HOME/.prettierrc.js ]] || ln -s $HOME/.dotfiles/.prettierrc.js $HOME/.prettierrc.js
[[ -f $HOME/.spacemacs ]] || ln -s $HOME/.dotfiles/.spacemacs $HOME/.spacemacs
[[ -f $HOME/.zshrc ]] || echo ". ~/.dotfiles/.zshrc" > ~/.zshrc
[[ -f $HOME/.gitignore ]] || ln -s $HOME/.dotfiles/.gitignore $HOME/.gitignore
git config --global core.excludesfile ~/.gitignore

mkdir -p ~/.ctags.d
[[ -f $HOME/.ctags.d/default.ctags ]] || ln -s $HOME/.dotfiles/.ctags $HOME/.ctags.d/default.ctags

os="`uname`"
if [[ "$os" == 'Darwin' ]]; then
  mkdir -p "$HOME/Library/Application Support/Code/User"
  ln -sf $HOME/.dotfiles/Code/User/settings.json "$HOME/Library/Application Support/Code/User/settings.json"
  ln -sf $HOME/.dotfiles/Code/User/keybindings.json "$HOME/Library/Application Support/Code/User/keybindings.json"
elif [[ "$os" == 'Linux' ]]; then
  mkdir -p $HOME/.config/Code/User/
  ln -sf $HOME/.dotfiles/Code/User/settings.json $HOME/.config/Code/User/settings.json
  ln -sf $HOME/.dotfiles/Code/User/keybindings.json $HOME/.config/Code/User/keybindings.json
fi
