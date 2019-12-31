#!/bin/sh
# lvextend /srv +size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvextend-srv.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /srv
	/sbin/lvextend -v -L $1 /dev/mapper/vg1-srv
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-srv
	/sbin/resize2fs /dev/mapper/vg1-srv 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-srv
	mount /srv
	df -h
fi
