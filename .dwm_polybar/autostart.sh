#! /bin/bash 
picom &
nitrogen --restore &
polybar main &
xiccd & 
notify-osd &
polkit-gnome-authentication-agent-1 &
gnome-keyring-daemon --start --components=pkcs11,secrets,ssh &
xfce4-power-manager &
xfce4-screensaver &
export SSH_AUTH_SOCK
