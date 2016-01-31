function v
  set SHELL /bin/sh
  set BROWSER /usr/local/bin/luakit
  set -g root ~/sync/dist
  [ -z "$DISPLAY" -a (tty) = /dev/tty1 ]; or set gui -g
  # [ -z "$DISPLAY" -a (tty) = /dev/tty1 ]; or set -g term "terminator --profile=xmonad -x"
  # update fish bundle comment string (append space after # like bash)
  grep -q '#%s' ~/sync/user/.vim/bundle/vim-fish/ftplugin/fish.vim
    and sed -i 's/\(commentstring\)=#%s/\1=#\\\\ %s/' ~/sync/user/.vim/bundle/vim-fish/ftplugin/fish.vim

  function uservim
    # echo uservim
    /usr/bin/vim $argv 2> /tmp/vim.error
    # eval $term /usr/bin/vim $argv 2> /tmp/vim.error
    sudo rm -fv $root/.vim/backups/*tmp*sup.* 2>/dev/null
  end

  function sudovim
    # echo sudovim
    sudo -s /usr/bin/vim $argv 2> /tmp/vim.error
    for i in $argv
      [ -f $i ]; and sudo chmod -v 644 $root/.vim/backups/*(basename $i)
    end
    own-sync
  end

  switch "$argv[1]"
    case apt;               set distro (head -1 /etc/apt/apt.conf | sed 's/^.*"\(.*\)".*/\1/')
                            v -f $root/etc/apt/$distro/* $root/etc/apt/$distro/*/*
                            apt a
    case crontab cron;      v -f ~/sync/dist/var/spool/cron/crontabs/$USER ~/.config/logrotate/logrotate.conf
                            cat ~/sync/dist/var/spool/cron/crontabs/$USER | sudo crontab -u $USER -
    case css;               v -f /srv/http/thedarnedestthing.com/application/public/css/*.css
                            rsync -rltuv /srv/http/thedarnedestthing.com/application/public/css ~/sync/thedarnedestthing.com/application/public/
                            rsync vps
    case group;             v -f $root/etc/group; dist-config /etc/group
    case grub;              v -f /etc/default/grub; sudo update-grub
    case mpd;               v -f ~/.mpdconf; mpd --kill; mpd
    case msmtp;             v -f ~/.msmtprc; chmod 600 ~/.msmtprc
    case nginx;             v -f $root/etc/nginx/sites-available/default; dist-config /etc/nginx/sites-available/default; restart nginx
    case privoxy;           v -f $root/etc/privoxy/default.action
                            dist-config /etc/privoxy/default.action; restart privoxy
    case prompt;            v -f ~/.config/fish/functions/fish_prompt_*.fish; prompt 2
    case slrn news usenet;  v -f ~/.slrnrc $root/etc/news/slrnpull.conf ~/.config/slrn/jnewsrc
                            dist-config /etc/news/slrnpull.conf
    case sudoers sudo;      v -f $root/etc/sudoers.d/README; dist-config /etc/sudoers.d/README
    case vimwiki wiki;      v '+call OpenWikis()'
    case ttrss;             v -f /srv/http/tt-rss/themes/myredder*
                            rsync -rltuv /srv/http/tt-rss/themes/myredder* ~/sync/tt-rss/themes/
                            rsync ttrss
    case www;               v -f /srv/http/thedarnedestthing.com/application/*.rb /srv/http/thedarnedestthing.com/application/lib/*.rb /srv/http/thedarnedestthing.com/application/views/*.slim
                            rsync -rltuv /srv/http/thedarnedestthing.com/application ~/sync/thedarnedestthing.com/
                            rsync vps
    case x;                 v -f ~/.Xresources; xrdb -merge ~/.Xresources

                            # edit executable
    case '!';               v -f $argv[2]; sudo chmod 755 $argv[2]

                            # edit fish function
    case ',';               cd ~/.config/fish/functions; v $argv[2].fish; cd -

                            # edit by search
    case '.';               [ (f . "$argv[2]" | wc -l) -gt 0 ]; and v (f . "$argv[2]")

                            # edit by content
    case '/';               [ (g -l $argv[2] * | wc -l) -gt 0 ]; and v (g -l $argv[2] *)

                            # note: sudo vim uses /etc/vim/vimrc for syntax highlighting unless HOME env specified in sudoers
                            # must define '.*' to catch hidden files
                            # file lists are assumed to be group owned, new files by the directory owner
    case '*' '.*';          [ "$argv[1]" = "-f" ]; and set file $argv[2]; or set file $argv[1]
                            if [ -f "$file" ]
                              if [ (stat -c \%U $file) = $USER ]; uservim $gui $argv; else; sudovim $gui $argv; end
                            else; if [ "$argv" != "" ]
                                if [ (stat -c \%U (dirname $file)) = $USER ]; uservim $gui $argv; else; sudovim $gui $argv; end
                              else
                                if [ (stat -c \%U .) = $USER ]; uservim $gui $argv; else; sudovim $gui $argv; end
                              end
                            end
                            command rm -f ~/.viminf*.tmp
  end
end
