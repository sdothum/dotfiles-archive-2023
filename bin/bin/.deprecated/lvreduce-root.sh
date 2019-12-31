#!/bin/sh
# lvreduce /root size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvreduce-root.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /
	e2fsck -f /dev/mapper/vg1-root
	/sbin/resize2fs -p /dev/mapper/vg1-root $1
	/sbin/lvreduce -v -L $1 /dev/mapper/vg1-root
	e2fsck -f /dev/mapper/vg1-root
	/sbin/resize2fs -p /dev/mapper/vg1-root 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-root
	e2fsck -f /dev/mapper/vg1-root
	mount /
	df -h
fi
