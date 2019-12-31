function rsync
  # run "config-sync.sh --delete" to delete extraneous files
  set exclude_cache "--delete-excluded --exclude='cache' --exclude='.cache' --exclude='*.cache'"
  set exclude_http_local "--exclude='thedarnedestthing.db' --exclude='log'"
  # huge virtual machine images can cause zfs to freeze on 100% memory usage. manually cp instead
  # set exclude_virtualbox "--exclude='VirtualBox VMs' --filter='protect VirtualBox*'"

  switch "$argv[1]"
    case [1];       echo ".. rsync  backup | bkup | data | depot | home | sync | ttrss | vps | [--delete] command"

    # zfs may freeze with liquorix kernel, using all available memory
    case backup;    if [ (hostname) = luna ]
                      if-yes "backup system to /net/backup and /net/archive"; and begin;
                        # mkdir -p /net/backup/{home,srv,usr} 2>/dev/null
                        # trace "updating => /backup$HOME"; rsync HOME
                        # trace "updating => /backup/opt"; rsync --delete /opt /net/backup/
                        # trace "updating => /backup/usr/local"; rsync --delete /usr/local /net/backup/usr/
                        # trace "updating => /backup/srv/http"; rsync --delete $exclude_cache /srv/http /net/backup/srv/
                        # trace "updating => /backup/srv/lib"; rsync --delete /srv/lib /net/backup/srv/
                        # rsync bkup
                        terminator  -T "rsync /net/backup" --profile=xmonad -x fish -c "rsync --delete /net/media/* /net/backup/media/" 2>/dev/null &
                        terminator  -T "rsync /net/archive" --profile=xmonad -x fish -c "rsync --delete /net/media/* /net/archive/media/" 2>/dev/null &
                      end
                    end
    case bkup;      if [ (hostname) = luna ]
                      if-yes "backup /net to /bkup"; and begin
                        rsync --delete $exclude_virtualbox --delete-excluded --exclude='downloads' /net/* /bkup/
                      end
                    end
    case data;      if [ (hostname) = luna ]
                      if-no "restore /net from /bkup"; or begin
                        rsync $exclude_virtualbox /bkup/* /net/
                      end
                    end
    case home;      if-yes "backup shum to /net/backup"; and rsync HOME
    case HOME;      rsync --delete $exclude_virtualbox $exclude_cache --exclude='Trash' --exclude='Logs' ~ /net/backup/home/

    case depot;     if [ (hostname) = luna ]
                      if-yes "backup depot to monad"; and rsync --delete /net/depot monad:/net/
                    else
                      if-no "update depot to luna"; or rsync /net/depot luna:/net/
                    end

    case music;     if [ (hostname) = monad ]
                      if-yes "replicate mpd music database from luna"; and begin
                        killall mpd
                        if [ ! -d /net/media ]
                          sudo mkdir /net/media
                          sudo chown shum:users /net/media
                        end
                        rsync --delete shum@luna:.mpd ~/
                        rsync --delete luna:/net/media/music /net/media/
                        mpd 2>/dev/null &
                      end
                    else
                      echo ".. initiate update request from monad"
                    end

    case sync;      if [ (hostname) = luna ]
                      if-yes "backup sync to monad"; and rsync --delete --exclude='.SyncArchive' ~/sync shum@monad:~/
                    else
                      if-no "update sync to luna"; or rsync --exclude='.SyncArchive' ~/sync shum@luna:~/
                    end

    case ttrss;     if [ (hostname) = luna ]
                      rsync /srv/http/tt-rss/themes/my*.css monad:/srv/http/tt-rss/themes/
                    else
                      rsync /srv/http/tt-rss/themes/my*.css luna:/srv/http/tt-rss/themes/
                    end

    case vps;       sudo touch /srv/http/thedarnedestthing.com/application/log/restart.txt
                    trace "updating => thedarnedestthing.com:/application";
                      rsync --delete --exclude='vimwiki' $exclude_http_local \
                        /srv/http/thedarnedestthing.com/application thedarnedestthing.com:/srv/http/thedarnedestthing.com/
                    trace "updating => thedarnedestthing.com:/vimwiki";
                      rsync -rt --delete ~/sync/vimwiki thedarnedestthing.com:/srv/http/thedarnedestthing.com/application/public/
                    if [ (hostname) = luna ]
                      trace "updating => monad:/srv/http/thedarnedestthing.com";
                        rsync --delete $exclude_http_local /srv/http/thedarnedestthing.com monad:/srv/http/
                      trace "syncing timestamps => monad:$HOME/sync/vimwiki";
                        rsync -rt ~/sync/vimwiki monad:~/sync/
                      # trace "updating => /backup/srv/http/thedarnedestthing.com";
                      #   rsync --delete /srv/http/thedarnedestthing.com /net/backup/srv/http/
                    else
                      ping -c 1 -W 1 luna >/dev/null; and begin;
                        trace "updating => luna:/srv/http/thedarnedestthing.com";
                          rsync $exclude_http_local /srv/http/thedarnedestthing.com luna:/srv/http/
                        # trace "updating => luna:/backup/srv/http/thedarnedestthing.com";
                        #   rsync $exclude_http_local /srv/http/thedarnedestthing.com luna:/net/backup/srv/http/
                      end
                    end
    case VPS;       if-no "restore vimwiki from <= thedarnedestthing.com:/vimwiki";
                      or rsync -rt thedarnedestthing.com:/srv/http/thedarnedestthing.com/application/public/vimwiki ~/sync/

    case '*';       # sudo rsync -avu --exclude='lost+found' $argv
                    command rsync -av --exclude='lost+found' $argv
  end
end
