#!/bin/sh

mkdir -p $HOME/.config/nvim
ln -s $HOME/.dotfiles/init.vim $HOME/.config/nvim/init.vim
ln -s $HOME/.dotfiles/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/.dotfiles/.spacemacs $HOME/.spacemacs

echo ". ~/.dotfiles/.zshrc" > ~/.zshrc
