#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Update monitor info
xrandr --auto
# Setup monitor layout
sh ~/.screenlayout/normal-1-1.sh

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --transparent true --tint 0x5f5f5f --height 25 &

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr

# Set a nice background
# feh --bg-fill --no-fehbg ~/Pictures/Wallpapers/penguin.jpg &

# Change color pallete based on the background
# wal -i ~/Pictures/Wallpapers/penguin.jpg -s &

# Set keyboard layout
setxkbmap -option grp:alt_shift_toggle us,ru &

# Fire up screensaver
#xscreensaver -no-splash &

# Power Management
xfce4-power-manager &

# Lock on suspend and stuff
xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &

# Turn on xbindkeys
xbindkeys

# Picom
picom &

# twm &
# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x50+494+51 &
# xterm -geometry 80x20+494-0 &
# exec xterm -geometry 80x66+0+0 -name login

exec xmonad
