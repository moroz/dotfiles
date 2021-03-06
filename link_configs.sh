#!/bin/bash

mkdir -p $HOME/.config/fontconfig

ln -sf $HOME/.dotfiles/nvim $HOME/.config/nvim
[[ -f $HOME/.tmux.conf ]] || ln -s $HOME/.dotfiles/.tmux.conf $HOME/.tmux.conf
[[ -f $HOME/.prettierrc.js ]] || ln -s $HOME/.dotfiles/.prettierrc.js $HOME/.prettierrc.js
[[ -f $HOME/.eslintrc.js ]] || ln -s $HOME/.dotfiles/.eslintrc.js $HOME/.eslintrc.js
[[ -f $HOME/.spacemacs ]] || ln -s $HOME/.dotfiles/.spacemacs $HOME/.spacemacs
[[ -f $HOME/.zshrc ]] || echo ". ~/.dotfiles/.zshrc" > ~/.zshrc
[[ -f $HOME/.psqlrc ]] || ln -s $HOME/.dotfiles/.psqlrc $HOME/.psqlrc
[[ -f $HOME/.gitignore ]] || ln -s $HOME/.dotfiles/.gitignore $HOME/.gitignore
[[ -f $HOME/.config/fontconfig/fonts.confi ]] || ln -s $HOME/.dotfiles/fonts.conf $HOME/.config/fontconfig/fonts.conf
git config --global core.excludesfile ~/.gitignore

mkdir -p ~/.ctags.d
[[ -f $HOME/.ctags.d/default.ctags ]] || ln -s $HOME/.dotfiles/.ctags $HOME/.ctags.d/default.ctags

[[ -d $HOME/.doom.d ]] || ln -s $HOME/.dotfiles/.doom.d $HOME/.doom.d

os="`uname`"
if [[ "$os" == 'Darwin' ]]; then
  mkdir -p "$HOME/Library/Application Support/Code/User"
  ln -sf $HOME/.dotfiles/Code/User/settings.json "$HOME/Library/Application Support/Code/User/settings.json"
  ln -sf $HOME/.dotfiles/Code/User/keybindings.json "$HOME/Library/Application Support/Code/User/keybindings.json"
  if [ -x $(which brew) ]; then
    brew install zsh tmux reattach-to-user-namespace mc ag neovim wget curl postgresql python3 nodejs direnv aws-vault ripgrep universal-ctags
    brew install --cask firefox google-chrome iterm2 spectacle karabiner-elements signal slack tableplus
    sudo echo $(which zsh) >> /etc/shells
    sudo chsh -s $(which zsh) $USER
  fi
  sudo cp "$HOME/.dotfiles/Custom Dvorak.keylayout" /Library/Keyboard\ Layouts
  sudo cp "$HOME/.dotfiles/Dvorak Esperanto.keylayout" /Library/Keyboard\ Layouts
  defaults write -g ApplePressAndHoldEnabled -bool false
  mkdir -p $HOME/Pictures/screenshots
  defaults write com.apple.screencapture location $HOME/Pictures/screenshots
elif [[ "$os" == 'Linux' ]]; then
  mkdir -p $HOME/.config/Code/User/
  ln -sf $HOME/.dotfiles/Code/User/settings.json $HOME/.config/Code/User/settings.json
  ln -sf $HOME/.dotfiles/Code/User/keybindings.json $HOME/.config/Code/User/keybindings.json
fi
