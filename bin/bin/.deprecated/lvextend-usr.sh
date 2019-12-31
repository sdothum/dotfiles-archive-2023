#!/bin/sh
# lvextend /usr +size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvextend-usr.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /usr
	/sbin/lvextend -v -L $1 /dev/mapper/vg1-usr
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-usr
	/sbin/resize2fs /dev/mapper/vg1-usr 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-usr
	mount /usr
	df -h
fi
