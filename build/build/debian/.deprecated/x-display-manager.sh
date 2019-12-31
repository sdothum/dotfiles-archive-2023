#!/bin/bash
# manage x display manager

case $1 in
	remove)
		# remove x display manager for manual startx
		sudo update-rc.d gdm remove
		sudo update-rc.d x11-common remove

		[ -f /etc/inittab.original ] || sudo cp /etc/inittab /etc/inittab.original
		sudo cp ~/.config/etc/inittab.startx /etc/inittab
		;;
	install)
		# install x display manager
		sudo update-rc.d x11-common defaults
		sudo update-rc.d gdm defaults

		sudo cp /etc/inittab.original /etc/inittab
		;;
	*)
		echo "x-display-manager.sh [ remove | install ]"
		;;
esac
