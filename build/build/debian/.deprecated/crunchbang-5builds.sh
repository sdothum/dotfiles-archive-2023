#!/bin/bash
set -x

# editors
~/depot/editors/install-pyroom.sh
~/depot/editors/install-scribes.sh
#~/depot/editors/install-textroom.sh
# git
# jumanji
#~/depot/git/install-jumanji.sh

# mail
~/depot/mail/install-mutt-notmuch.sh
#~/depot/mail/install-mu.sh

# media
# calibre
sudo python -c "import sys; py3 = sys.version_info[0] > 2; u = __import__('urllib.request' if py3 else 'urllib', fromlist=1); exec(u.urlopen('http://status.calibre-ebook.com/linux_installer').read()); main()"
~/depot/media/install-vobsub2srt.sh

# productivity
~/depot/productivity/install-todo.sh

# system
~/depot/system/install-launchy.sh
#~/depot/system/install-obapps.sh

# themes
#~/depot/themes/install-pidgin.sh

# web
#~/depot/www/install-luajit-luakit.sh

# x11
#~/depot/x11/install-tint2.sh
#~/depot/x11/install-euphoria.sh

# zeitgeist
sudo bzr branch lp:zeitgeist-datasources
#apt-get install libtool automake autoconf libzeitgeist-dev libglib2.0-dev xulrunner-1.9.2-dev 
sudo apt-get install libtool automake autoconf libzeitgeist-dev libglib2.0-dev xulrunner-dev 
# for geany
sudo apt-get install geany
# for tootem 
sudo apt-get install valac libtotem-plparser-dev 
# for tomboy 
sudo apt-get install libzeitgeist-cil-dev libgtk2.0-cil-dev  
cd zeitgeist-datasources
#./autogen.sh --enable-all-plugins
./autogen.sh 
cd {package}
make
sudo make install
