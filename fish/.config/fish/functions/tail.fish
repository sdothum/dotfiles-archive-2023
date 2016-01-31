function tail
  switch "$argv[1]"
    case [1];         echo ".. tail imap | log | luakit | nzb | sup | syslog | www | {file}"
    case imap;        imap l
    case log;         command tail -f ~/logs/session.log
    case luakit;      command tail -f /tmp/luakit.log
    case nzb;         nzb l
    case sickbeard;   command tail -f /opt/sickbeard/Logs/sickbeard.log
    case sup;         sup l
    case syslog;      [ -f /var/log/syslog ]; and !p 'tail.*/var/log/syslog'; and sudo tail -f /var/log/syslog
    case www;         command tail -f /srv/http/thedarnedestthing.com/application/log/thedarnedestthing.log
    case '*';         command tail $argv
  end
end
