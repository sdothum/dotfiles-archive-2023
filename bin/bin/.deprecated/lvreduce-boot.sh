#!/bin/sh
# lvreduce /boot size (in Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvreduce-boot.sh size (in Mbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /boot
	e2fsck -f /dev/mapper/vg1-boot
	/sbin/resize2fs -p /dev/mapper/vg1-boot ${1}G
	/sbin/lvreduce -v -L ${1}G /dev/mapper/vg1-boot
	e2fsck -f /dev/mapper/vg1-boot
	/sbin/resize2fs -p /dev/mapper/vg1-boot 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-boot
	e2fsck -f /dev/mapper/vg1-boot
	mount /boot
	df -h
fi
