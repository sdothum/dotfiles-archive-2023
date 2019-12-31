#!/bin/sh
# lvextend /var +size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvextend-var.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /var
	/sbin/lvextend -v -L $1 /dev/mapper/vg1-var
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-var
	/sbin/resize2fs /dev/mapper/vg1-var 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-var
	mount /var
	df -h
fi
