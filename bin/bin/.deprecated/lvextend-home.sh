#!/bin/sh
# lvextend /home +size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvextend-home.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ $1 -gt 0 ]; then
	umount /home
	/sbin/lvextend -v -L $1 /dev/mapper/vg1-home
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-home
	/sbin/resize2fs /dev/mapper/vg1-home 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-home
	mount /home
	df -h
fi
