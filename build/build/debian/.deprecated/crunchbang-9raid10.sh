#!/bin/bash
# setup raid 10 configuration
# ..not really an automated script
exit

original() {
	if [ ! -f $1.original ]; then
		if [ -f $1 ]; then
			cp $1 $1.original
		fi
	fi
	cp /home/shum/.config$1 $1 2>/dev/null
	echo "cp /home/shum/.config$1 $1" >>/home/shum/bin/crunchbang/.crunchbang-9raid10.log
	echo ----------------------------------------------- >>/home/shum/bin/crunchbang/.crunchbang-9raid10.log
	echo edit this file: $1 >>/home/shum/bin/crunchbang/.crunchbang-9raid10.log
	echo ----------------------------------------------- >>/home/shum/bin/crunchbang/.crunchbang-9raid10.log
}

install() { 
	echo
	echo "::: install ::: $@"
	echo "--- install --- $@" >>/home/shum/bin/crunchbang/.crunchbang-9raid10.log
	echo
	apt-get $yes install $@ 2>>/home/shum/bin/crunchbang/.crunchbang-9raid10.log 
}

# install mdadm, "none" option to load after boot (see mdadm.conf below)
install mdadm 
original /etc/fstab
cp -fv /home/shum/.config/etc/fstab /etc/fstab
original /etc/mdadm/mdadm.conf
cp -fv /home/shum/.config/etc/mdadm/mdadm.conf /etc/mdadm/mdadm.conf

# insert module
#modprobe raid10

# rebuild an existing array?
if-continue.sh "Stop array" mdadm --stop /dev/md0
if-continue.sh "Delete existing array" mdadm --remove /dev/md0

# verify available disks
fdisk -l
if-continue.sh "Exit this build" exit

# allocate hard disk partitions
echo gdisk prompts..
echo command: "n"
echo partition number: <enter>
echo first sector: <enter>
echo last sector: <enter>
echo partition type: "fd00"
echo command: "w"
echo proceed: "y"
#gdisk /dev/sdb
gdisk /dev/sdc
gdisk /dev/sdd
gdisk /dev/sde
gdisk /dev/sdf
#gdisk /dev/sdg
gdisk /dev/sdh
gdisk /dev/sdi

# -------------------------------------------------------------------------------------------------------------------
# suggested disk configuration enhancements (not inserted into rc.local as no effective performance increase noticed)
#
# tune drives for raid
#blockdev --setra 16384 /dev/sdb
blockdev --setra 16384 /dev/sdc
blockdev --setra 16384 /dev/sdd
blockdev --setra 16384 /dev/sde
blockdev --setra 16384 /dev/sdf
#blockdev --setra 16384 /dev/sdg
blockdev --setra 16384 /dev/sdh
blockdev --setra 16384 /dev/sdi
# default read ahead set 8192
#cat /sys/block/sdb/queue/read_ahead_kb
cat /sys/block/sdc/queue/read_ahead_kb
cat /sys/block/sdd/queue/read_ahead_kb
cat /sys/block/sde/queue/read_ahead_kb
cat /sys/block/sdf/queue/read_ahead_kb
#cat /sys/block/sdg/queue/read_ahead_kb
cat /sys/block/sdh/queue/read_ahead_kb
cat /sys/block/sdi/queue/read_ahead_kb
#echo 1024 > /sys/block/sdb/queue/read_ahead_kb
echo 1024 > /sys/block/sdc/queue/read_ahead_kb
echo 1024 > /sys/block/sdd/queue/read_ahead_kb
echo 1024 > /sys/block/sde/queue/read_ahead_kb
echo 1024 > /sys/block/sdf/queue/read_ahead_kb
#echo 1024 > /sys/block/sdg/queue/read_ahead_kb
echo 1024 > /sys/block/sdh/queue/read_ahead_kb
echo 1024 > /sys/block/sdi/queue/read_ahead_kb
# default queue set 128
#cat /sys/block/sdb/queue/nr_requests
cat /sys/block/sdc/queue/nr_requests
cat /sys/block/sdd/queue/nr_requests
cat /sys/block/sde/queue/nr_requests
cat /sys/block/sdf/queue/nr_requests
#cat /sys/block/sdg/queue/nr_requests
cat /sys/block/sdh/queue/nr_requests
cat /sys/block/sdi/queue/nr_requests
#echo 256 > /sys/block/sdb/queue/nr_requests
echo 256 > /sys/block/sdc/queue/nr_requests
echo 256 > /sys/block/sdd/queue/nr_requests
echo 256 > /sys/block/sde/queue/nr_requests
echo 256 > /sys/block/sdf/queue/nr_requests
#echo 256 > /sys/block/sdg/queue/nr_requests
echo 256 > /sys/block/sdh/queue/nr_requests
echo 256 > /sys/block/sdi/queue/nr_requests
# Disable NCQ on all disks, default set to 31
#cat /sys/block/sdb/device/queue_depth
cat /sys/block/sdc/device/queue_depth
cat /sys/block/sdd/device/queue_depth
cat /sys/block/sde/device/queue_depth
cat /sys/block/sdf/device/queue_depth
#cat /sys/block/sdg/device/queue_depth
cat /sys/block/sdh/device/queue_depth
cat /sys/block/sdi/device/queue_depth
#echo 1 > /sys/block/sdb/device/queue_depth
echo 1 > /sys/block/sdc/device/queue_depth
echo 1 > /sys/block/sdd/device/queue_depth
echo 1 > /sys/block/sde/device/queue_depth
echo 1 > /sys/block/sdf/device/queue_depth
#echo 1 > /sys/block/sdg/device/queue_depth
echo 1 > /sys/block/sdh/device/queue_depth
echo 1 > /sys/block/sdi/device/queue_depth
# -------------------------------------------------------------------------------------------------------------------

# quick check of disk i/o for potential bad drive
#hdparm -tT /dev/sdb
hdparm -tT /dev/sdc
hdparm -tT /dev/sdd
hdparm -tT /dev/sde
hdparm -tT /dev/sdf
#hdparm -tT /dev/sdg
hdparm -tT /dev/sdh
hdparm -tT /dev/sdi

# initialize raid device (now managed by md)
#mknod /dev/md0 b 9 0

# -------
# raid 10
# -------
mdadm --create /dev/md0 --raid-devices=8 --level=raid10 --chunk=256 /dev/sdb1 /dev/sdc1 /dev/sdd1 /dev/sde1 /dev/sdf1 /dev/sdg1 /dev/sdh1 /dev/sdi1 
mkfs.xfs -f -b size=4096 -d sunit=512,swidth=2048 /dev/md0
xfs_check /dev/md0

#-------
# raid 0
#-------
mdadm --create /dev/md0 --raid-devices=6 --level=raid0 --chunk=256 /dev/sdb1 /dev/sdc1 /dev/sdd1 /dev/sde1 /dev/sdh1 /dev/sdi1 
mkfs.xfs -f -b size=4096 -d sunit=512,swidth=3072 /dev/md0
xfs_check /dev/md0

# ------
# raid 6
# ------
mdadm --create /dev/md0 --raid-devices=6 --level=raid6 --chunk=256 /dev/sdb1 /dev/sdc1 /dev/sdd1 /dev/sde1 /dev/sdh1 /dev/sdi1 
mkfs.xfs -f -b size=4096 -d sunit=512,swidth=2048 /dev/md0
mdadm --create /dev/md0 --raid-devices=8 --level=raid6 --chunk=256 /dev/sdb1 /dev/sdc1 /dev/sdd1 /dev/sde1 /dev/sdh1 /dev/sdi1 /dev/sdf1 /dev/sdg1
mkfs.xfs -f -b size=4096 -d sunit=512,swidth=3072 /dev/md0
xfs_check /dev/md0

# ------
# raid 5
# ------
mdadm --create /dev/md0 --raid-devices=6 --level=raid5 --chunk=256 /dev/sdb1 /dev/sdc1 /dev/sdd1 /dev/sde1 /dev/sdh1 /dev/sdi1 
mkfs.xfs -f -b size=4096 -d sunit=512,swidth=2560 /dev/md0
xfs_check /dev/md0

# ------------------------------------------
# mdadm config file and performance settings
# ------------------------------------------
mdadm --detail /dev/md0
watch -d cat /proc/mdstat
# may have to run assemble if problems after reboot
#mdadm --assemble --scan
mdadm --detail --scan --verbose

mdadm --detail --scan --verbose >>/etc/mdadm/mdadm.conf
if [ ! -f /etc/mdadm/mdadm.conf.original ]; then
	cp /etc/mdadm/mdadm.conf /etc/mdadm/mdadm.conf.original
fi
cp /etc/mdadm/mdadm.conf.original /etc/mdadm/mdadm.conf 
mdadm --detail --scan --verbose >>/etc/mdadm/mdadm.conf

# note: stripe cache size not applicable to raid 10
echo 8192 > /sys/block/md0/md/stripe_cache_size
#echo 16384 > /sys/block/md0/md/stripe_cache_size
# read ahead cache
blockdev --setra 65536 /dev/md0
# setup stripe and read ahead caches for boot initialization
if [ ! -f /etc/rc.local.original ]; then
	cp /etc/rc.local /etc/rc.local.original
fi
cp /home/shum/.config/etc/rc.local /etc/rc.local
# to speed up sync
#echo 50000 > /proc/sys/dev/raid/speed_limit_min
sysctl dev.raid.speed_limit_min
sysctl dev.raid.speed_limit_max
if-continue.sh "Set new min 50000 limit" sysctl -w dev.raid.speed_limit_min=50000
# watch md process..cd 
watch -d cat /proc/mdstat
# add bitmap usage for increased resync performance (can only be added "after" creating a stable "clean" array)
#mdadm -G /dev/md0 --bitmap=internal
#mdadm -G /dev/md0 --bitmap=none
hdparm -tT /dev/md0

# --------------
# data structure
# --------------
# mount array
mkdir /data >/dev/null
#mount -t ext4 /dev/md0 /data
mount -t xfs -o sunit=512,swidth=2560 /dev/md0 /data

mkdir /data/depot >/dev/null
mkdir /data/downloads >/dev/null
mkdir -p /data/downloads/nzbs/completed >/dev/null
mkdir /data/downloads/nzbs/queue >/dev/null
mkdir /data/downloads/nzbs/tmp >/dev/null
mkdir /data/downloads/torrents >/dev/null
mkdir -p /data/media/images >/dev/null
mkdir /data/media/music >/dev/null
mkdir -p /data/media/videos/animation >/dev/null
mkdir /data/media/videos/anime >/dev/null
mkdir /data/media/videos/anime\ -\ serial >/dev/null
mkdir /data/media/videos/movies >/dev/null
mkdir /data/media/videos/movies\ -\ foreign >/dev/null
mkdir /data/media/videos/television >/dev/null
chown -R shum /data

[ ! -f /etc/fstab.original ] && cp /etc/fstab /etc/fstab.original
cp /home/shum/.config/etc/fstab /etc/fstab
#if-continue.sh "mount raid" mount -a

