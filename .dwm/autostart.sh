#! /bin/bash 
picom &
nitrogen --restore &
dwmblocks &
xiccd & 
xfce4-notifyd &
polkit-gnome-authentication-agent-1 &
gnome-keyring-daemon --start --components=pkcs11,secrets,ssh &
xfce4-power-manager &
xfce4-screensaver &
#xscreensaver -no-splash &
#light-locker &
export SSH_AUTH_SOCK
