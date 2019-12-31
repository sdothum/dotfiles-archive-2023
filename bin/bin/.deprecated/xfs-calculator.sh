#!/bin/sh
BLOCKSIZE=4096 # Make sure this is in bytes
CHUNKSIZE=256  # Make sure this is in KiB
NUMSPINDLES=8
RAID_TYPE=6
RAID_DEVICE_NAME="/dev/md0" # Specify device name for your RAID device
FSLABEL="mythtv" # specify filesystem label for generating mkfs line here

case "$RAID_TYPE" in
0)
		RAID_DISKS=${NUMSPINDLES};
		;;
1)
		RAID_DISKS=${NUMSPINDLES};
		;;
10)
		RAID_DISKS=${NUMSPINDLES};
		;;
5)
		RAID_DISKS=`echo "${NUMSPINDLES} - 1" | bc`;
		;;
6)
		RAID_DISKS=`echo "${NUMSPINDLES} - 2" | bc`;
		;;
*)
		echo "Please specify RAID_TYPE as one of: 0, 1, 10, 5, or 6."
		exit
		;;
esac

SUNIT=`echo "${CHUNKSIZE} * 1024 / 512" | bc`
SWIDTH=`echo "$RAID_DISKS * ${SUNIT}" | bc`

echo "System blocksize=${BLOCKSIZE}"
echo "Chunk Size=${CHUNKSIZE} KiB"
echo "NumSpindles=${NUMSPINDLES}"
echo "RAID Type=${RAID_TYPE}"
echo "RAID Disks (usable for data)=${RAID_DISKS}"
echo "Calculated values:"
echo "Stripe Unit=${SUNIT}"
echo -e "Stripe Width=${SWIDTH}\n"
echo "mkfs line:"
echo -e "mkfs.xfs -b size=${BLOCKSIZE} -d sunit=${SUNIT},swidth=${SWIDTH} -L ${FSLABEL} ${RAID_DEVICE_NAME}\n"
echo "mount line:"
echo -e "mount -o remount,sunit=${SUNIT},swidth=${SWIDTH}\n"
echo "Add these options to your /etc/fstab to make permanent:"
echo "sunit=${SUNIT},swidth=${SWIDTH}"
