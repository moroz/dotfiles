#!/bin/bash -eu

mkdir -p $HOME/.config/fontconfig

[[ -L $HOME/.config/nvim ]] || ln -s $HOME/.dotfiles/nvim $HOME/.config/nvim
[[ -L $HOME/.tmux.conf ]] || ln -s $HOME/.dotfiles/.tmux.conf $HOME/.tmux.conf
[[ -L $HOME/.prettierrc.js ]] || ln -s $HOME/.dotfiles/.prettierrc.js $HOME/.prettierrc.js
[[ -L $HOME/.eslintrc.js ]] || ln -s $HOME/.dotfiles/.eslintrc.js $HOME/.eslintrc.js
[[ -L $HOME/.spacemacs ]] || ln -s $HOME/.dotfiles/.spacemacs $HOME/.spacemacs
[[ -f $HOME/.zshrc ]] || echo ". ~/.dotfiles/.zshrc" > ~/.zshrc
[[ -L $HOME/.psqlrc ]] || ln -s $HOME/.dotfiles/.psqlrc $HOME/.psqlrc
[[ -L $HOME/.gitignore ]] || ln -s $HOME/.dotfiles/.gitignore $HOME/.gitignore
[[ -L $HOME/.config/fontconfig/fonts.conf ]] || ln -s $HOME/.dotfiles/fonts.conf $HOME/.config/fontconfig/fonts.conf
git config --global core.excludesfile ~/.gitignore

mkdir -p ~/.ctags.d
[[ -f $HOME/.ctags.d/default.ctags ]] || ln -s $HOME/.dotfiles/.ctags $HOME/.ctags.d/default.ctags

[[ -d $HOME/.doom.d ]] || ln -s $HOME/.dotfiles/.doom.d $HOME/.doom.d

if [[ "$(uname)" == 'Darwin' ]]; then
  if which brew >/dev/null; then
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
  [[ -L $HOME/.config/alacritty ]] || ln -s $HOME/.dotfiles/alacritty $HOME/.config/alacritty
fi
