#! /bin/bash 
picom -b --experimental-backends &
nitrogen --restore &
killall "xiccd"
xiccd & 
notify-osd &
polkit-gnome-authentication-agent-1 &
xfce4-power-manager &
xsetroot -cursor_name left_ptr &
killall "unclutter"
