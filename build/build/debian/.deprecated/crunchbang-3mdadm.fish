#!/usr/bin/fish
# install raid sw
# perform this before crunchbang-4install.fish if /data/depot installs desired

set numdisks (ll /dev/sd*1 | wc -l)
if [ $numdisks -gt 2 -a ! -f /sbin/mdadm ]
    install mdadm
    etc-config /etc/fstab
    etc-config /etc/rc.local
    etc-config /etc/default/mdadm

    if [ -f /home/shum/.config/etc/mdadm/mdadm.conf ]
        etc-config /etc/mdadm/mdadm.conf
    else
        original /etc/mdadm/mdadm.conf
        sudo mdadm --assemble --scan
        sudo mdadm --detail --scan --verbose >>/etc/mdadm/mdadm.conf
        v /etc/mdadm/mdadm.conf
    end

    sudo mkdir -v /data 2>/dev/null
    sudo dpkg-reconfigure mdadm
    sudo mount -a
    sudo mkdir -pv /data/backup/Dropbox 2>/dev/null
    sudo mkdir -pv /data/backup/vps 2>/dev/null
    sudo mkdir -pv /data/depot/ 2>/dev/null
    sudo mkdir -pv /data/distros/archlinux 2>/dev/null
    sudo mkdir -pv /data/distros/crunchbang 2>/dev/null
    sudo mkdir -pv /data/media/ebooks/Calibre 2>/dev/null
    sudo mkdir -pv /data/media/music 2>/dev/null
    sudo mkdir -pv /data/media/images/fuji\ x100 2>/dev/null
    sudo mkdir -pv /data/media/images/Lightroom 2>/dev/null
    sudo mkdir -pv /data/media/images/sigma\ dp1 2>/dev/null
    sudo mkdir -pv /data/media/video/anime 2>/dev/null
    sudo mkdir -pv /data/media/video/anime\ serial 2>/dev/null
    sudo mkdir -pv /data/media/video/movies 2>/dev/null
    sudo mkdir -pv /data/media/video/tv\ shows 2>/dev/null
    sudo chown -R shum:shum /data/*
    ~/bin/install/crunchbang-3install.fish
end
