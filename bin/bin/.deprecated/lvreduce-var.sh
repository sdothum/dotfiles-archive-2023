#!/bin/sh
# lvreduce /var size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvreduce-var.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /var
	e2fsck -f /dev/mapper/vg1-var
	/sbin/resize2fs -p /dev/mapper/vg1-var $1
	/sbin/lvreduce -v -L $1 /dev/mapper/vg1-var
	e2fsck -f /dev/mapper/vg1-var
	/sbin/resize2fs -p /dev/mapper/vg1-var 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-var
	e2fsck -f /dev/mapper/vg1-var
	mount /var
	df -h
fi
