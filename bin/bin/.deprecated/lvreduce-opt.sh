#!/bin/sh
# lvreduce /opt size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvreduce-opt.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /opt
	e2fsck -f /dev/mapper/vg1-opt
	/sbin/resize2fs -p /dev/mapper/vg1-opt $1
	/sbin/lvreduce -v -L $1 /dev/mapper/vg1-opt
	e2fsck -f /dev/mapper/vg1-opt
	/sbin/resize2fs -p /dev/mapper/vg1-opt 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-opt
	e2fsck -f /dev/mapper/vg1-opt
	mount /opt
	df -h
fi
