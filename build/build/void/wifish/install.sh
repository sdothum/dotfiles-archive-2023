#!/bin/sh -e
install -D -m0644 awk/wscanparse.awk /usr/local/share/wifish/wscanparse.awk
install -D -m0644 awk/wlistparse.awk /usr/local/share/wifish/wlistparse.awk
install -D -m0644 awk/wscan2menu.awk /usr/local/share/wifish/wscan2menu.awk
install -D -m0644 awk/iwparse.awk /usr/local/share/wifish/iwparse.awk
install -D -m0755 wifish /usr/local/bin
if [ ! -d /etc/sv/wpa_supplicant ];then
	echo "Installing /etc/sv/wpa_supplicant service"
	install -D -d -m0755 /etc/sv/wpa_supplicant
	install -D -m0644 sv/wpa_supplicant/conf /etc/sv/wpa_supplicant/conf
	install -D -m0755 sv/wpa_supplicant/run /etc/sv/wpa_supplicant/run
	install -D -m0755 sv/wpa_supplicant/log/run /etc/sv/wpa_supplicant/log/run
fi
echo ".. Wifish is installed .."
