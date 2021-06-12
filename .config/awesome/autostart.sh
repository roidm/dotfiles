#! /bin/bash 
picom -b &
nitrogen --restore &
killall "xiccd"
xiccd & 
polkit-gnome-authentication-agent-1 &
xfce4-power-manager &
killall "emacs"
emacs --daemon &
sleep 4 && pamixer --set-volume 70
