#!/bin/bash
# rebuild symbolic links
#
# ll | a '\-\>' | awk '{ printf "ln -sf %s %s \n", $11, $9; }'
set -x

move() { [ -d $1 -a ! -d $1.save ] && mv -f $1 $1.save ; }

# rebuild symbolic links for application .config and .rc files
cd ~
move .abook
ln -sf .config/mail/abook .abook 
ln -sf ~/Dropbox/debian/.config/autokey/autokey.json .config/autokey/autokey.json
ln -sf ~/Dropbox/debian/.config/autokey/data .config/autokey/data
ln -sf .config/bash_aliases .bash_aliases 
#ln -sf .config/asoundrc .asoundrc 
ln -sf .config/conkyrc .conkyrc 
move .elinks
ln -sf .config/elinks .elinks 
ln -sf ~/Dropbox/debian/.config/fish/config.fish .config/fish/config.fish
ln -sf ~/Dropbox/debian/.config/fish/user.fish .config/fish/user.fish
ln -sf ~/Dropbox/debian/.config/fish/functions .config/fish/functions
move .fonts
ln -sf .config/fonts .fonts 
move .imapfilter
ln -sf .config/mail/imapfilter .imapfilter 
ln -sf .config/inputrc .inputrc 
move .ion3
ln -sf .config/ion3 .ion3 
move .irssi
ln -sf .config/irssi .irssi 
ln -sf .config/launchy.ini .launchy.ini
ln -sf .config/slrn/jnewsrc .jnewsrc 
ln -sf .config/slrn/jnewsrc.dsc .jnewsrc.dsc 
ln -sf .config/luarc .luarc
ln -sf .config/mail/mailcap .mailcap 
ln -sf .config/mail/msmtprc .msmtprc 
chmod 600 .config/mail/msmtprc
move .mutt
ln -sf .config/mail/mutt .mutt 
ln -sf .config/mail/muttrc .muttrc 
ln -sf .config/nanorc .nanorc 
move .ncmpcpp
ln -sf .config/ncmpcpp .ncmpcpp 
ln -sf ~/Dropbox/debian/.config/notion/autostart .config/notion/autostart
ln -sf ~/Dropbox/debian/.config/notion/cfg_kludges.lua .config/notion/cfg_kludges.lua
ln -sf ~/Dropbox/debian/.config/notion/cfg_notioncore.lua .config/notion/cfg_notioncore.lua
ln -sf ~/Dropbox/debian/.config/notion/cfg_notion.lua .config/notion/cfg_notion.lua
ln -sf ~/Dropbox/debian/.config/notion/cfg_sp.lua .config/notion/cfg_sp.lua
ln -sf ~/Dropbox/debian/.config/notion/cfg_statusbar.lua .config/notion/cfg_statusbar.lua
ln -sf ~/Dropbox/debian/.config/notion/look_greenlight.lua .config/notion/look_greenlight.lua
ln -sf ~/Dropbox/debian/.config/notion/look_mygreenlight.lua .config/notion/look_mygreenlight.lua
ln -sf ~/Dropbox/debian/.config/notion/look_parallel.lua .config/notion/look_parallel.lua
ln -sf .config/notion/autostart .notion/autostart
ln -sf .config/notion/cfg_kludges.lua .notion/cfg_kludges.lua
ln -sf .config/notion/cfg_notioncore.lua .notion/cfg_notioncore.lua
ln -sf .config/notion/cfg_notion.lua .notion/cfg_notion.lua
ln -sf .config/notion/cfg_sp.lua .notion/cfg_sp.lua
ln -sf .config/notion/cfg_statusbar.lua .notion/cfg_statusbar.lua
ln -sf .config/notion/look_greenlight.lua .notion/look_greenlight.lua
ln -sf .config/notion/look_mygreenlight.lua .notion/look_mygreenlight.lua
ln -sf .config/notion/look_parallel.lua .notion/look_parallel.lua
ln -sf .config/nzbget/nzbget.conf .config/nzbget.conf 
ln -sf .config/nzbget.conf .nzbget.conf 
ln -sf .config/mail/offlineimaprc .offlineimaprc 
ln -sf .config/pentadactylrc .pentadactylrc 
ln -sf .config/pyradio .pyradio 
ln -sf ~/Dropbox/debian/.config/ReText\ project .config/ReText\ project
ln -sf .config/rtorrent.rc .rtorrent.rc 
ln -sf .config/screenrc .screenrc 
ln -sf .config/screensaver .screensaver 
ln -sf .config/mail/signature .signature 
ln -sf .config/slrn/slrnrc .slrnrc 
ln -sf .config/themes .themes
ln -sf .config/tmux.conf .tmux.conf 
move .ssh
ln -sf .config/ssh/config .
move .todo
ln -sf Dropbox/.todo .todo 
ln -sf .config/ttytterrc .ttytterrc
ln -sf .config/mail/urlview .urlview 
move .vifm
ln -sf .config/vifm .vifm 
move .vim
ln -s ~/Dropbox/debian/.config/vim/* ~/.config/vim/
ln -sf .config/vim .vim 
ln -sf .config/vim/vimrc .vimrc 
ln -sf .config/vromerc .vromerc 
move .w3m
ln -sf .config/w3m .w3m 
ln -sf .config/xscreensaver .xscreensaver 
ln -sf .config/xsession .xsession 

# todo symlinks
ln -sf ~/Dropbox/.todo/todo.sh ~/bin/todo
ln -sf ~/Dropbox/.todo/todo-screen.sh ~/bin/todo-screen

# launchy symlinks 
#ln -sf .config/launchy/history.db
#ln -sf .config/launchy/launchy.db
#ln -sf .config/launchy/launchy.ini

# create mailbox links for mutt listing
#cd ~/.mail/gmail
#ln -sf Inbox In:box 
#cd ~/.mail/gmail/system
#ln -sf Important Flag:ged 
#ln -sf Spam Junk 
#cd ~/.mail/gmail/user
#ln -sf Comments Com:ments 
#ln -sf Community Communit:y 
#ln -sf Computing Comp:uting 
#ln -sf Dailies Dai:lies 
#ln -sf Services Ser:vices 
#ln -sf Store St:ore 
#ln -sf Travel Trav:el 
#ln -sf Unix Uni:x 
#cd ~/.mail/.notmuch
#ln -sf /home/shum/.cache/mutt_results =Search 

