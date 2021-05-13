#! /bin/bash 
picom -b &
nitrogen --restore &
killall "xiccd"
xiccd & 
notify-osd &
polkit-gnome-authentication-agent-1 &
xfce4-power-manager &
pamixer --set-volume 70
