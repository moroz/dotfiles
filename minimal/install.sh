#!/usr/bin/env -S bash -e

DIR="$( cd "$( dirname "$0" )" && pwd )"

mkdir -p $HOME/.config
[[ -d $HOME/.config/nvim ]] || ln -s $DIR/nvim $HOME/.config/nvim
[[ -f $HOME/.zshrc ]] || ln -s $DIR/.zshrc $HOME/.zshrc
