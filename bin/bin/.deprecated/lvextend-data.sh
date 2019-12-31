#!/bin/sh
# lvextend /net +size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
    echo "lvextend-data.sh size (in Mbytes or Gbytes)"
    exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
    umount /net
    /sbin/lvextend -v -L $1 /dev/mapper/vg1-data
    /sbin/fsck.ext4 -v -f /dev/mapper/vg1-data
    /sbin/resize2fs /dev/mapper/vg1-data 
    /sbin/fsck.ext4 -v -f /dev/mapper/vg1-data
    mount /net
    df -h
fi
