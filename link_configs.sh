#!/usr/bin/env -S bash -eu

mkdir -p $HOME/.config/{fontconfig,rofi,powershell}

[[ -e $HOME/.config/nvim ]] || ln -s $HOME/.dotfiles/nvim $HOME/.config/nvim
[[ -e $HOME/.config/nvim-minimal ]] || ln -s $HOME/.dotfiles/nvim-minimal $HOME/.config/nvim-minimal

[[ -L $HOME/.tmux.conf ]] || ln -s $HOME/.dotfiles/.tmux.conf $HOME/.tmux.conf
[[ -L $HOME/.prettierrc.json ]] || ln -s $HOME/.dotfiles/.prettierrc.json $HOME/.prettierrc.json
[[ -L $HOME/.eslintrc.js ]] || ln -s $HOME/.dotfiles/.eslintrc.js $HOME/.eslintrc.js
[[ -L $HOME/.spacemacs ]] || ln -s $HOME/.dotfiles/.spacemacs $HOME/.spacemacs
[[ -L $HOME/.psqlrc ]] || ln -s $HOME/.dotfiles/.psqlrc $HOME/.psqlrc
[[ -L $HOME/.gitignore ]] || ln -s $HOME/.dotfiles/.gitignore $HOME/.gitignore
[[ -L $HOME/.config/fontconfig/fonts.conf ]] || ln -s $HOME/.dotfiles/fonts.conf $HOME/.config/fontconfig/fonts.conf
[[ -L $HOME/.config/i3 ]] || ln -s $HOME/.dotfiles/i3 $HOME/.config/i3
[[ -L $HOME/.Xresources ]] || ln -s $HOME/.dotfiles/.Xresources $HOME/.Xresources
[[ -L $HOME/.config/rofi/config.rasi ]] || ln -s $HOME/.dotfiles/config.rasi $HOME/.config/rofi/config.rasi
[[ -L $HOME/.doom.d ]] || ln -s $HOME/.dotfiles/.doom.d $HOME/.doom.d
git config --global core.excludesfile ~/.gitignore

[[ -f $HOME/.zshrc ]] || echo ". ~/.dotfiles/.zshrc" > ~/.zshrc
[[ -f $HOME/.config/powershell/Microsoft.PowerShell_profile.ps1 ]] || ln -s $HOME/.dotfiles/Microsoft.PowerShell_profile.ps1 $HOME/.config/powershell/Microsoft.PowerShell_profile.ps1

if [[ ! -L ~/.config/jj/config.toml ]]; then
  rm -f ~/.config/jj/config.toml
  mkdir -p ~/.config/jj
  ln -s $HOME/.dotfiles/jj/config.toml ~/.config/jj/config.toml
fi

mkdir -p ~/.ctags.d
[[ -f $HOME/.ctags.d/default.ctags ]] || ln -s $HOME/.dotfiles/.ctags $HOME/.ctags.d/default.ctags


if [[ "$(uname)" == 'Darwin' ]]; then
  [[ -f "/Library/Keyboard Layouts/Custom Dvorak.keylayout" ]] || sudo cp "$HOME/.dotfiles/Custom Dvorak.keylayout" /Library/Keyboard\ Layouts
  [[ -f "/Library/Keyboard Layouts/Dvorak Esperanto.keylayout" ]] || sudo cp "$HOME/.dotfiles/Dvorak Esperanto.keylayout" /Library/Keyboard\ Layouts
  defaults write -g ApplePressAndHoldEnabled -bool false
  mkdir -p $HOME/Pictures/screenshots
  defaults write com.apple.screencapture location $HOME/Pictures/screenshots
fi
