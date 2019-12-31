#!/bin/bash
#set -x

# for future install decisions based on available ram and disks instead of hostname
kbram=$(cat /proc/meminfo | head -1 | awk '{print $2}')
numdisks=$(ll /dev/sd*1 | wc -l)

#-----------
# messaging
#-----------
log() {
    option=
    # suppress line break?
    if [ "$1". = "-n." ]; then
        option=$1
        shift
    fi
    echo $option "$@" >>/home/shum/tmp/install.log
}

annotate() {
    annotation=">>>   $@   <<<"
    log $(head -c $(echo "$annotation" | wc -L) < /dev/zero | tr '\0' '-')
    log "$annotation"
    log $(head -c $(echo "$annotation" | wc -L) < /dev/zero | tr '\0' '-')
}

distro="$(head -1 /etc/apt/apt.conf | sed 's/^.*"\(.*\)".*/\1/')"
squeezy=
if [ "$distro" = "squeeze" ]; then
    squeezy="/wheezy"
fi
log ""
log ""
log "Starting $0 $@ ... $(date '+%Y.%m.%d %H:%S:%M') "
annotate "using debian $distro"
log ""

#-------------------------------------------
# existing user configuration substitutions
#-------------------------------------------
original() {
    if [ ! -f $1.original ]; then
        if [ -f $1 ]; then
            cp $1 $1.original
        fi
    fi
    cp /home/shum/.config$1 $1 2>/dev/null
    log "cp /home/shum/.config$1 $1"
    annotate "check configuration .. $1"
}

#-----------------
# apt-get install
#-----------------
install() { 
    echo
    echo "::: installing ::: $@"
    log -n "... installing $@"
    echo
    # install [-t target] [install options] package
    target=
    if [ $1. = -t. ]; then
        target="$1 $2"
        shift 2
    fi
    #aptitude $yes $target install $@ 2>.error
    apt-get $yes $target install $@ 2>.error
    error=$(cat .error)
    #[ ! "$error." = "." ] && log "   ! error !    $(head -c $(echo "$@" | wc -L) < /dev/zero | tr '\0' '^') $error"
    if [ ! "$error." = "." ]; then
        log -n " /////////   $error   /////////"
        # build exception install script
        echo $error | grep 'E:' && echo "sudo aptitude \$@ install $@" >>/home/shum/bin/install/install-exceptions.sh
    fi
    log ""
}

#----------------
# apt-get remove
#----------------
uninstall() { 
    echo
    echo "::: removing ::: $@"
    log -n "... removing $@"
    echo
    apt-get remove $@ 2>.error
    error=$(cat .error)
    #[ ! "$error." = "." ] && log "   ! error !    $(head -c $(echo "$@" | wc -L) < /dev/zero | tr '\0' '^') $error"
    if [ ! "$error." = "." ]; then
        log -n " /////////   $error   /////////"
        # build exception install script
        echo $error | grep 'E:' && echo "sudo aptitude \$@ remove $@" >>/home/shum/bin/install/install-exceptions.sh
    fi
    log ""
}

#-----------------
# root user check
#-----------------
yes=$1
if [ "$yes". != "." -a "$yes". != "-y." ]; then
    echo "sudo ./crunchbang-3install.sh [-y]"
    exit
fi
if [ "$HOME" != "/root" ]; then
    echo "!! must run as root... aborting"
    exit
fi  
echo "running as: apt-get $yes"
killall xscreensaver

#--------------------------
# initialize fresh install
#--------------------------
/home/shum/bin/if-continue.sh "install crunchbang distro (cb-welcome)" cb-welcome
/usr/bin/terminator -x tail -f /home/shum/tmp/install.log &

# sudoers config
original /etc/sudoers.d/README

#if [ ! -f /usr/bin/launchy ]; then
#   if [ -f /home/shum/depot/system/install-launchy.sh ]; then
#       log "... installing launchy"
#       /home/shum/depot/system/install-launchy.sh
#       touch /home/shum/.config/launchy/history.db /home/shum/.config/launchy/launchy.db /home/shum/.config/launchy/launchy.ini
#       ln -s /home/shum/.config/launchy/history.db /home/shum/
#       ln -s /home/shum/.config/launchy/launchy.db /home/shum/
#       ln -s /home/shum/.config/launchy/launchy.ini /home/shum/
#       chown shum:shum /home/shum/history.db /home/shum/launchy.db /home/shum/launchy.ini
#   fi
#fi 

# capture log (script) of install exceptions (see install)
echo "#!/bin/bash
# crunchbang-3install exceptions
# run as: ./install-exceptions.sh [options ..]
# example: ./install-exceptions.sh -t wheezy
set -x
" >/home/shum/bin/install/install-exceptions.sh
chmod 755 /home/shum/bin/install/install-exceptions.sh
/usr/bin/terminator -x tail -f /home/shum/bin/install/install-exceptions.sh &

#-----------
# astronomy
#-----------
if [ "$(hostname)" = "luna" ]; then
    install celestia 
    install kstars
fi
install stellarium

#------
# chat
#------
install bitlbee
install irssi irssi-scripts
install libtime-duration-perl
install pidgin finch pidgin-hotkeys pidgin-libnotify
[ -d /data/depot ] && /home/shum/bin/install/install-pidgin.sh

#---------
# desktop
#---------
#if [ "$(hostname)" = "luna" ]; then
# install gnome-do
#fi
#install gnome-backgrounds
install launchy launchy-plugins launchy-skins

#if [ "$(hostname)" = "luna" -a "$distro" = "squeeze"]; then
    # for memory leak in xinerama when invoked by zoomed full screen desktop (wallpaper) 
    # note: causes problems with audio synchronization when in xinerama full screen mode
# install nitrogen/wheezy
#fi

install rss-glx
install screenruler

#if [ "$(hostname)" = "luna" ]; then
    install synapse gnome-activity-journal
#fi

#---------
# editors
#---------
install bluefish
install focuswriter
install retext retext-wpgen
install vim-nox vim-gtk exuberant-ctags
#git clone http://github.com/gmarik/vundle.git /home/shum/.vim/vundle.git
original /etc/vim/vimrc

#------
# java
#------
# note: needed for tiddlywiki javascript edit (save)
install -t squeeze sun-java6-jre sun-java6-plugin

#--------
# kernel
#--------
if [ "$distro" = "sid" -a "$(hostname)" = "luna" ]; then
    install --force-yes liquorix-keyrings
    install --force-yes linux-image-liquorix-amd64
fi

#------
# mail
#------
install abook  
install antiword 
install archivemail  
#install icedove icedove-gcontactsync icedove-quotecolors iceowl-extension

install imapfilter  
mkdir -p /home/shum/.mail/gmail
install lynx  
original /etc/lynx-cur/lynx.cfg

install msmtp-mta 
cp /usr/share/doc/msmtp/examples/msmtpqueue/msmtp*.sh /usr/local/bin/
chmod 755 /usr/local/bin/msmtp*.sh
if [ -L /home/shum/.msmtprc ]; then
    chmod 600 /home/shum/.config/mail/msmtprc
else
    chmod 600 /home/shum/.msmtprc
fi
# initialize crontab for sendmail (and all other user cron processes)
cp /home/shum/.config/crontab/crontab /var/spool/cron/crontabs/shum
chmod 600 /var/spool/cron/crontabs/shum
chown shum:crontab /var/spool/cron/crontabs/shum

#install mutt mutt-patched mutt-vc-query muttprint muttprint-manual 
install mutt mutt-patched muttprint muttprint-manual 
mkdir /home/shum/attachments 2>/dev/null
chown shum:shum /home/shum/attachments

#install notmuch/wheezy 
install notmuch$squeezy 
mkdir -p /home/shum/.mail/.notmuch
mkdir -p /home/shum/.cache/mutt_results
annotate "initialize notmuch by running: notmuch setup && notmuch new"
[ -d /data/depot ] && /home/shum/bin/install/install-mutt-notmuch.sh
# not part of the #! backports distribution?
install libmail-box-perl

#install offlineimap/wheezy
install offlineimap$squeezy

install slrn slrnpull
original /etc/default/slrnpull
original /etc/news/server
original /etc/news/slrnpull.conf
mv /etc/cron.daily/slrnpull /etc/cron.hourly/
cd /var/spool/ ; ln -sf slrnpull/news news ; cd -
cp /home/shum/.config/var/spool/slrnpull/authinfo /var/spool/slrnpull/authinfo
chmod 600 /var/spool/slrnpull/authinfo
chown -R news:news /var/spool/slrnpull
#chmod ugo+rwxt /var/spool/slrnpull/out.going
cp /home/shum/.config/slrn/jnewsrc.initialize /home/shum/.config/slrn/jnewsrc 2>/dev/null

install urlscan
install urlview

#-------
# media
#-------
if [ "$(hostname)" = "luna" ]; then
    install abcde
    install asunder
fi
    
install calibre
install comix

if [ "$(hostname)" = "luna" ]; then
    install coriander 
    #install vloopback-source

    install fbreader 
    install ffmpeg
fi

install flashplugin-nonfree

if [ "$(hostname)" = "luna" ]; then
    install gnome-sutitles
    install handbrake-cli handbrake-gtk
    #install moovida 
fi


#install mpd/wheezy mpc/wheezy ncmpcpp/wheezy
#install mpd
install mpd
#cp /home/shum/.config/etc/mpd.conf.$(hostname) /home/shum/.config/etc/mpd.conf
cp /home/shum/.config/etc/mpd.conf.$(hostname).$(distro) /home/shum/.config/etc/mpd.conf
original /etc/mpd.conf
annotate "verify ~/.config/mpd.conf/* with /etc/mpd.conf"
install mpc ncmpcpp
mpc update

install mplayer/sid
original /etc/mplayer/mplayer.conf

install mupdf
if [ "$(hostname)" = "luna" ]; then
    install rubyripper
    install sonata 
fi

install xbmc/sid xbmc-skin-confluence/sid
[ ! -f /usr/bin/xbmc.original ] && mv /usr/bin/xbmc /usr/bin/xbmc.original
ln -s /home/shum/bin/xbmc /usr/bin/xbmc

install zathura 

#---------------------
# monitor and testing
#---------------------
install bmon 
install bonnie++
install htop 
install iftop 
install iotop 
install memtester
install smartmontools gsmartcontrol
install tcptrack
install traceroute 

#---------
# network
#---------
if [ "$(hostname)" = "luna" ]; then
    install airport-utils
fi

install dnsmasq 
original /etc/dnsmasq.conf
/etc/init.d/dnsmasq restart
install dnsutils

install polipo 
original /etc/polipo/config
/etc/init.d/polipo stop
/etc/init.d/polipo force-reload

#install privoxy/wheezy 
install privoxy$squeezy 
original /etc/privoxy/config
original /etc/privoxy/default.action
original /etc/privoxy/user.action
annotate "configure browser proxies to localhost:8118"

install openssh-server
install vsftpd
original /etc/vsftpd.conf

#-------------
# photography
#-------------
install darktable

if [ "$(hostname)" = "luna" ]; then
    install digikam showfoto
    #install dispcalgu 
    #install qtpfsgui 
    install rawstudio 
    install rawtherapee
    #install ufraw gimp-ufraw
fi

#-------------------------
# formatting and printing
#-------------------------
install csstidy
install enscript

install cups cups-pdf cups-bsd 
sed 's/\(^lpadmin:.*$\)/\1,shum/' /etc/group >group.sed
cp -v group.sed /etc/group

install system-config-printer 
install hpijs

#--------------
# productivity
#--------------
install alarm-clock
install artha 
install autokey-gtk gir1.2-notify-0.7
#install blogilo 
install dict
install gcal
install gcalcli
install ktouch
install qalc

#--------
# system
#--------
install arandr
install autocutsel
install cpufrequtils 
install fish 
annotate "run ./crunchbang-6fishshell.sh for user account"
install fonts-inconsolata
install fortunes fortunes-off
install hfsplus hfsprogs hfsutils hfsutils-tcltk 
install keepassx

#if [ "$(hostname)" = "luna" -a ! -f /sbin/mdadm ]; then
if [ $numdisks -gt 2 -a ! -f /sbin/mdadm ]; then
    install mdadm 
    original /etc/fstab
    #original /etc/mdadm/mdadm.conf
    cp /etc/mdadm/mdadm.conf /etc/mdadm/mdadm.conf.original
    mdadm --assemble --scan
    mdadm --detail --scan --verbose >>/etc/mdadm/mdadm.conf
    vi /etc/mdadm/mdadm.conf
    mkdir /data 2>/dev/null
    dpkg-reconfigure mdadm
fi

install ntpdate 
install qshutdown
ntpdate time.nrc.ca

install rlwrap 
install screen 
install startupmanager
#install tmux/wheezy 
install tmux$squeezy 

#if [ "$(hostname)" = "luna" ]; then
if [ $kbram -gt 2048 ]; then
    install linux-headers-$(uname -r)
    install virtualbox virtualbox-guest-additions-iso
fi

#-----------
# utilities
#-----------
install ack-grep 
sudo ln -s /usr/bin/ack-grep /usr/bin/ack 2>/dev/null
#install aptitude-gtk 
install apt-file
install deborphan 
install filelight
install fontmatrix
install gdisk 
install guake 
install locate
#install nautilus
install ncdu 
install ranger
install telnet 

if [ "$distro" = "squeeze" ]; then
    install thunar/wheezy
fi
install tree
#install unetbootin
install vifm 

#-----------------
# version control
#-----------------

install bzr bzrtools 
install cvs
install git 
install mercurial 
install patch
install subversion subversion-tools 

#-----
# web
#-----
install curl
install elinks 
#install iceweasel 

#install luakit/wheezy 
install luakit$squeezy 
annotate "compare customizations in ~/.luakit/* with /etc/xdg/luakit/* sources"
# afterwards replace with luajit build (see /data/depot/luakit/install-luakit.sh) - still need apt install for luakit.desktop file
mv /usr/bin/luakit /usr/bin/luakit.apt 
[ -d /data/depot ] && /home/shum/bin/install/install-luajit.sh
[ -d /data/depot ] && /home/shum/bin/install/install-luakit.sh
ln -s /home/shum/bin/luakit /usr/bin/luakit

install newsbeuter 

install nzbget  
install par2 pypar2
mkdir /home/shum/nzbs 2>/dev/null

install rtorrent 
mkdir /home/shum/.session 2>/dev/null
mkdir /home/shum/torrents 2>/dev/null

#install sabnzbdplus
#original /etc/default/sabnzbdplus
install surfraw 
install ttytter
#install uzbl 
install w3m

#------
# wiki
#------
# debian dokuwiki uses apache - boo
# nginx not used at present because of php5 package conflicts not allowing php5-fpm to be installed (required for nginx)
# note: php5-cli must be installed 1st
#install php5-cli php5-common php5-suhosin
#install php5-fpm php5-cgi

#install lighttpd
#install dokuwiki

#install php5-cgi php5
#lighty-enable-mod fastcgi fastcgi-php dokuwiki
#/etc/init.d/lighttpd force-reload
#touch /etc/dokuwiki/plugins.local.php
#mkdir -p /srv/www/dokuwiki
#cp -pvr /var/lib/dokuwiki/data /srv/www/dokuwiki/data
#rm -rf /var/lib/dokuwiki/data
#ln -s /srv/www/dokuwiki/data /var/lib/dokuwiki/data
#chown -R www-data:www-data /etc/dokuwiki
#chown -R www-data:www-data /usr/share/dokuwiki
#chown -R www-data:www-data /var/lib/dokuwiki
#chown -R www-data:www-data /srv/www/dokuwiki

#uninstall apache2-mpm-prefork apache2-utils apache2.2-bin apache2.2-common libapache2-mod-php5
#uninstall lighttpd

# installed for nanoki and google reader css
install nginx
original /etc/nginx/nginx.conf
original /etc/nginx/sites-available/default

[ -d /data/depot ] && /home/shum/bin/install/install-nanoki.sh
if [ ! -f /etc/hosts.original ]; then
    cp /etc/hosts /etc/hosts.original
fi
echo >>/etc/hosts
echo "127.0.0.1       thedarnedestthing" >>/etc/hosts

#install sputnik
#install xavante xavante-doc

#-----
# x11
#-----
/home/shum/bin/install/install-compton.sh

#if [ "$(hostname)" = "luna" ]; then
# dpkg -i /home/shum/depot/x11/ion3_20090110-2_amd64.deb
#fi
dpkg -i /data/depot/x11/notion/notion_3+2012042300-1_amd64.deb
install xterm xfce4-terminal
/home/shum/bin/install/install-trayion.sh
mkdir -p /home/shum/.x11/notion/default-session--0
chown -R shum:shum /home/shum/.x11

install xfce4

#-------------------------------------
# apps that need to be installed last
#-------------------------------------
#if [ "$(hostname)" = "luna" -a ! -f /usr/bin/calibre ]; then
#   log "... installing calibre" 
#   sudo python -c "import sys; py3 = sys.version_info[0] > 2; u = __import__('urllib.request' if py3 else 'urllib', fromlist=1); exec(u.urlopen('http://status.calibre-ebook.com/linux_installer').read()); main(install_dir='/opt')"
#fi

if [ "$(hostname)" = "luna" -a ! -f /usr/sbin/start_pms ]; then
    #install plexmediaserver
    install --force-yes plexmediaserver
    original /etc/default/plexmediaserver
    mkdir -p /srv/plexmediaserver/Library/Application\ Support 2>/dev/null
    chown -R plex /srv/plexmediaserver/
    /etc/init.d/plexmediaserver restart
fi

#if [ ! "$(head -1 /etc/apt/apt.conf | sed 's/^.*"\(.*\)".*/\1/')" = "wheezy" ]; then
# /home/shum/bin/crunchbang/crunchbang-3install-wheezy.sh
#fi

# the following gackages conflict with the wheezy installs if preceding
#if [ "$(hostname)" = "luna" ]; then
# dpkg -i /home/shum/depot/system/powerpanel_1.2_amd64.deb
#fi

#if [ "$(hostname)" = "luna" ]; then
# dpkg -i /home/shum/depot/astronomy/google-earth-stable_current_amd64.deb
#fi

#if [ "$(hostname)" = "luna" ]; then
# install ia32-libs-gtk
# dpkg -i /home/shum/depot/www/google-talkplugin_current_amd64.deb
#fi

#--------------------
# install exceptions
#--------------------
annotate "logged install exceptions"
cat /home/shum/bin/install/install-exceptions.sh
log ""
log "Ending $0 ... $(date '+%Y.%m.%d %H:%S:%M')"
log ""
chown shum:shum /home/shum/tmp/install.log

#---------------------------
# manual installation tweaks
#---------------------------
# reinstall power manager which seems to get disabled by wheezy upgrade
#if [ "$(head -1 /etc/apt/apt.conf | sed 's/^.*"\(.*\)".*/\1/')" = "wheezy" ]; then
    install --reinstall xfce4-power-manager
#fi
/home/shum/bin/if-continue.sh "install kernel sensor modules for conky" sudo sensors-detect
/etc/init.d/module-init-tools start

# remove tumbler until apparmor in kernel to deny thunar thumbnail generation of video directories
# see http://forums.opensuse.org/english/get-technical-help-here/applications/471701-tumblerd-large-files.html
#if [ "$(hostname)" = "luna" ]; then
# /home/shum/bin/if-continue.sh "remove tumbler - thunar jpeg creator" apt-get remove tumbler
#fi

# disable display manager
update-rc.d -f gdm remove
original /etc/inittab

#-----------------------
# complete installation
#-----------------------
su - shum -c "xscreensaver -no-splash &"
/home/shum/bin/if-continue.sh "install application builds" /home/shum/bin/crunchbang/crunchbang-4builds.sh
/home/shum/bin/if-continue.sh "install language and development packages" /home/shum/bin/crunchbang/crunchbang-5development.sh
/home/shum/bin/if-continue.sh "set default applications" update-alternatives --all
/home/shum/bin/if-continue.sh "set default shell" /home/shum/bin/crunchbang/crunchbang-6fishshell.sh
/home/shum/bin/if-continue.sh "set common symlinks" /home/shum/bin/crunchbang/crunchbang-7symcommon.sh
/home/shum/bin/if-continue.sh "set config symlinks" /home/shum/bin/crunchbang/crunchbang-8symlinks.sh
if [ "$(hostname)" = "luna" ]; then
    /home/shum/bin/if-continue.sh "configure raid10" /home/shum/bin/crunchbang/crunchbang-9raid10.sh
fi
/home/shum/bin/if-continue.sh "reboot" reboot
