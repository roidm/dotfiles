#! /bin/bash 
picom --experimental-backends &
nitrogen --restore &
xiccd & 
notify-osd &
polkit-gnome-authentication-agent-1 &
gnome-keyring-daemon --start --components=pkcs11,secrets,ssh &
xfce4-power-manager &
export SSH_AUTH_SOCK
