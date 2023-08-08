#!/bin/sh -e
#
# $Id: build-updates.sh 18638 2021-07-04 23:59:30Z NiLuJe $
#

HACKNAME="stuff"
HACKDIR="KoboStuff"
PKGNAME="${HACKNAME}"
PKGVER="1.6.N"

# Setup xz multi-threading...
export XZ_DEFAULTS="-T 0"

# Create the VERSION tag file
SVN_REV="$(svnversion -c .. | awk '{print $NF}' FS=':' | tr -d 'P')"
PKG_DATE="$(date +'%Y-%b-%d @ %H:%M')"
echo "${PKGVER} @ r${SVN_REV} on ${PKG_DATE}" > ../VERSION
echo "${SVN_REV}" > ../REVISION

# Pull all our crap, and put it in a single place, that place being a staging rootfs
mkdir -p ../KOBOROOT
cd ../KOBOROOT

###
# Core

# Pull the actual Core scripts from the src folder
cp -av ../src/. .

mkdir -p mnt/onboard/.niluje
# Plop the version file down here, too, so it's visible on device
cp -av ../VERSION mnt/onboard/.niluje/
cp -av ../REVISION mnt/onboard/.niluje/
# USBNet stuff
cp -av ../../USBNetwork/src/usbnet mnt/onboard/.niluje/
# Fonts stuff
cp -av ../../Fonts/src/linkfonts mnt/onboard/.niluje/
# ScreenSavers stuff
cp -av ../../ScreenSavers/src/linkss mnt/onboard/.niluje/
# Don't forget xzdec
cp -av ../../Common/bin/xzdec mnt/onboard/.niluje/usbnet/bin/
# As well as zstd-decompress
cp -av ../../Common/bin/zstd-decompress mnt/onboard/.niluje/usbnet/bin/
# And lz4
cp -av ../../Common/bin/lz4 mnt/onboard/.niluje/usbnet/bin/
# And the busybox symlinks
mkdir -p usr/local/niluje/usbnet/bin usr/local/niluje/usbnet/lib usr/local/niluje/usbnet/etc usr/local/niluje/usbnet/run usr/local/niluje/usbnet/libexec
tar -xvf ../../KoboStuff/build/busybox-links.tar -C usr/local/niluje/usbnet

# Create symlinks for what's not already shipped by Kobo...
mkdir -p usr/bin usr/libexec usr/sbin bin sbin

# ScreenSavers stuff
for my_bin in convert mogrify identify inotifywait ; do
	ln -sf /mnt/onboard/.niluje/linkss/bin/${my_bin} usr/bin/${my_bin}
done

# Fonts stuff (not terribly useful, skip)
#for my_bin in fc-list fc-scan ; do
#	ln -sf /mnt/onboard/.niluje/linkfonts/bin/${my_bin} usr/bin/${my_bin}
#done

# USBNet stuff
for my_bin in eu-nm eu-objdump eu-readelf eu-strings fbgrab htop kindletool kindle_usbnet_addr ltrace mosh-client mosh-server rsync rsync-ssl sshfs strace nano ag tmux gdb gcore gdb-add-index gdbserver objdump gprof fbink fbdepth curl evemu-describe evemu-device evemu-event evemu-play evemu-record xzdec zstd-decompress lz4 fusermount3 scanelf lddtree symtree trace perf lsblk choom lsipc lslocks jq bsdtar lftp lftpget tree dircolors b2sum file dtc ; do
	ln -sf /mnt/onboard/.niluje/usbnet/bin/${my_bin} usr/bin/${my_bin}
done

# Kobo ships busybox w/ lsof, as well as evtest, and so do we, so prefer the full versions...
for my_bin in lsof evtest ; do
	ln -sf /mnt/onboard/.niluje/usbnet/bin/${my_bin} usr/local/niluje/usbnet/usr/bin/${my_bin}
done

# Symlink ZSH in /bin
for my_bin in zsh ; do
	ln -sf /mnt/onboard/.niluje/usbnet/bin/${my_bin} bin/${my_bin}
done

# Symlink nologin in /sbin
ln -sf /usr/local/niluje/usbnet/bin/busybox sbin/nologin

# NOTE: Special handling of busybox, dropbear, & OpenSSH, to make them live in the internal memory.
for my_bin in busybox dropbearmulti scp sftp ssh ssh-add ssh-agent ssh-keygen ssh-keyscan ; do
	mv -v mnt/onboard/.niluje/usbnet/bin/${my_bin} usr/local/niluje/usbnet/bin/${my_bin}
done
for my_bin in scp sftp ssh ssh-add ssh-agent ssh-keygen ssh-keyscan ; do
	ln -sf /usr/local/niluje/usbnet/bin/${my_bin} usr/bin/${my_bin}
done
# OpenSSH's libexec
for my_bin in sftp-server ssh-keysign ssh-pkcs11-helper ssh-sk-helper ; do
	mv -v mnt/onboard/.niluje/usbnet/libexec/${my_bin} usr/local/niluje/usbnet/libexec/${my_bin}
	ln -sf /usr/local/niluje/usbnet/libexec/${my_bin} usr/libexec/${my_bin}
done
# OpenSSH's sbin
for my_bin in sshd ; do
	mv -v mnt/onboard/.niluje/usbnet/sbin/${my_bin} usr/local/niluje/usbnet/sbin/${my_bin}
	ln -sf /usr/local/niluje/usbnet/sbin/${my_bin} usr/sbin/${my_bin}
done
# We also have dosfstools, which we move to the rootfs (because, duh), but which we do *NOT* symlink, so as not to confuse the stock tools...
for my_bin in fatlabel fsck.fat mkfs.fat ; do
	mv -v mnt/onboard/.niluje/usbnet/sbin/${my_bin} usr/local/niluje/usbnet/sbin/${my_bin}
	#ln -sf /usr/local/niluje/usbnet/sbin/${my_bin} usr/sbin/${my_bin}
done
# We also need libz
cp -av mnt/onboard/.niluje/usbnet/lib/libz.so.1 usr/local/niluje/usbnet/lib/libz.so.1
# And OpenSSL for OpenSSH
cp -av mnt/onboard/.niluje/usbnet/lib/libcrypto.so.1.1 usr/local/niluje/usbnet/lib/libcrypto.so.1.1
# And don't forget the DropBear configs...
for my_conf in dropbear_dss_host_key dropbear_ecdsa_host_key dropbear_ed25519_host_key dropbear_rsa_host_key ; do
	mv -v mnt/onboard/.niluje/usbnet/etc/${my_conf} usr/local/niluje/usbnet/etc/${my_conf}
done
# And OpenSSH configs....
for my_conf in moduli sshd_config ssh_config ssh_host_dsa_key ssh_host_dsa_key.pub ssh_host_ecdsa_key ssh_host_ecdsa_key.pub ssh_host_ed25519_key ssh_host_ed25519_key.pub ssh_host_rsa_key ssh_host_rsa_key.pub ; do
	mv -v mnt/onboard/.niluje/usbnet/etc/${my_conf} usr/local/niluje/usbnet/etc/${my_conf}
done
mkdir -p usr/local/niluje/usbnet/run usr/local/niluje/usbnet/empty usr/local/niluje/usbnet/etc/dot.ssh

# DropBear
for my_db_symlinks in usr/sbin/dropbearmulti usr/bin/dropbearmulti usr/bin/dropbear usr/bin/dbclient usr/bin/dropbearkey usr/bin/dropbearconvert usr/bin/dbscp ; do
	ln -sf /usr/local/niluje/usbnet/bin/dropbearmulti ${my_db_symlinks}
done

# fuse
for my_sbin in mount.fuse3 ; do
	ln -sf /mnt/onboard/.niluje/usbnet/sbin/${my_sbin} usr/sbin/${my_sbin}
done

# gawk
mkdir -p mnt/onboard/.niluje/extensions/gawk
tar -I pigz -xvf ../../USBNetwork/src/usbnet/bin/gawk-KOBO.tar.gz -C mnt/onboard/.niluje/extensions/gawk
ln -sf /mnt/onboard/.niluje/extensions/gawk/bin/gawk usr/bin/gawk
rm -f mnt/onboard/.niluje/usbnet/bin/gawk-KOBO.tar.gz

# Symlink the OTA scripts
for my_bin in update-kobostuff python-setup usbnet-toggle ; do
	ln -sf /usr/local/stuff/bin/${my_bin}.sh usr/bin/${my_bin}
done

# And finally create the update package...
rm -f ../KoboRoot.tgz
tar --hard-dereference --owner=root --group=root -cvf ../KoboRoot.tar .
pigz -k ../KoboRoot.tar
mv -v ../KoboRoot.tar.gz ../KoboRoot.tgz

# Create a ZSTD variant for the updater script...
rm -f ../../${HACKDIR}-Core-r*.tar.zst
zstd --rm -f -c -T0 --ultra -20 ../KoboRoot.tar -o ../../${HACKDIR}-Core-r"${SVN_REV}".tar.zst

# Clear our crap...
rm -rf mnt etc usr bin sbin


###
# Python 3
PY3VER="3.9"

mkdir -p mnt/onboard/.niluje usr/bin
# Unpack
tar -xvJf ../../Python/src/python3.tar.xz -C mnt/onboard/.niluje/
# Symlink
for my_bin in sqlite3 ; do
	ln -sf /mnt/onboard/.niluje/python3/bin/${my_bin} usr/bin/${my_bin}
done
for my_bin in python${PY3VER} python3 python ; do
	ln -sf /mnt/onboard/.niluje/python3/bin/python${PY3VER} usr/bin/${my_bin}
done
# HTTPie
for my_bin in http https ; do
	ln -sf /mnt/onboard/.niluje/python3/bin/${my_bin} usr/bin/${my_bin}
done
# Create the VERSION tag file
PY_SVN_REV="$(svnversion -c ../../Python | awk '{print $NF}' FS=':' | tr -d 'P')"
PY_PKG_DATE="$(date +'%Y-%b-%d @ %H:%M')"
echo "${PKGVER} @ r${PY_SVN_REV} on ${PY_PKG_DATE}" > mnt/onboard/.niluje/python3/VERSION
echo "${PY_SVN_REV}" > mnt/onboard/.niluje/python3/REVISION

# Package...
tar --hard-dereference --owner=root --group=root -cvf ../../${HACKDIR}-Python3.tar .
# Remove old version
rm -f ../../${HACKDIR}-Python3-r*.tar.zst
zstd --rm -f -c -T0 --ultra -20 ../../${HACKDIR}-Python3.tar -o ../../${HACKDIR}-Python3-r"${PY_SVN_REV}".tar.zst

# Cleanup...
rm -rf mnt usr


###
# Python 2

mkdir -p mnt/onboard/.niluje usr/bin
# Unpack
tar -xvJf ../../Python/src/python.tar.xz -C mnt/onboard/.niluje/
# Symlink
for my_bin in sqlite3 ; do
	ln -sf /mnt/onboard/.niluje/python/bin/${my_bin} usr/bin/${my_bin}
done
for my_bin in python2.7 python2 ; do
	ln -sf /mnt/onboard/.niluje/python/bin/python2.7 usr/bin/${my_bin}
done
# Create the VERSION tag file
PY_SVN_REV="$(svnversion -c ../../Python | awk '{print $NF}' FS=':' | tr -d 'P')"
PY_PKG_DATE="$(date +'%Y-%b-%d @ %H:%M')"
echo "${PKGVER} @ r${PY_SVN_REV} on ${PY_PKG_DATE}" > mnt/onboard/.niluje/python/VERSION
echo "${PY_SVN_REV}" > mnt/onboard/.niluje/python/REVISION

# Package...
tar --hard-dereference --owner=root --group=root -cvf ../../${HACKDIR}-Python2.tar .
# Remove old version
rm -f ../../${HACKDIR}-Python2-r*.tar.zst
zstd --rm -f -c -T0 --ultra -20 ../../${HACKDIR}-Python2.tar -o ../../${HACKDIR}-Python2-r"${PY_SVN_REV}".tar.zst

# Cleanup...
rm -rf mnt usr

cd -


# We also want to package our uninstaller...
cd ../uninstall
	rm -f KoboRoot.tgz
	tar --hard-dereference -I pigz --owner=root --group=root -cvf KoboRoot.tgz ./usr
cd -

# Clear staging rootfs...
rm -rfv ../KOBOROOT

# Package it...
cd ../..
# Update ChangeLog
svn2cl --linelen=80 --break-before-msg=2 -i -a -o ${HACKDIR}/ChangeLog ${HACKDIR}
# Remove old version
rm -f kobo-${HACKNAME}-*.tar.xz
# Package it
tar --exclude-vcs -cvJf kobo-${HACKNAME}-${PKGVER}-r"${SVN_REV}".tar.xz ${HACKDIR}

# We're done
cd -

