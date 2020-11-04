#!/bin/bash

DAYTIME_WALLPAPER="$HOME/wallpapers/10-15-Day.jpg"
NIGHT_WALLPAPER="$HOME/wallpapers/10-15-Night.jpg"

DAYTIME_GTK_THEME="McOS-MJV-Cinnamon-Edition-2.0"
NIGHT_GTK_THEME="McOS-MJV-Dark-mode-Gnome-3.30"

DAYTIME_WINDOW_THEME="McOS-CTLina-Mint"
NIGHT_WINDOW_THEME="McOS-CTLina-Mint-Dark"

REAL_DAYTIME=$($HOME/.dotfiles/daytime)
DAYTIME="${DAYTIME:-$REAL_DAYTIME}"
echo $DAYTIME

if [ "$DAYTIME" = "DAYTIME" ]; then
  WALLPAPER="${DAYTIME_WALLPAPER}"
  GTK_THEME="${DAYTIME_GTK_THEME}"
  WINDOW_THEME="${DAYTIME_WINDOW_THEME}"
else
  WALLPAPER="${NIGHT_WALLPAPER}"
  GTK_THEME="${NIGHT_GTK_THEME}"
  WINDOW_THEME="${NIGHT_WINDOW_THEME}"
fi

PID=$(pgrep -o "cinnamon-sess|gnome-sess|mate-sess")
export DBUS_SESSION_BUS_ADDRESS=$(grep -z DBUS_SESSION_BUS_ADDRESS /proc/$PID/environ|cut -d= -f2-)

gsettings set org.cinnamon.desktop.background picture-uri  "file://$WALLPAPER"
gsettings set org.cinnamon.desktop.wm.preferences theme "${WINDOW_THEME}"
gsettings set org.cinnamon.desktop.interface gtk-theme "${GTK_THEME}"
