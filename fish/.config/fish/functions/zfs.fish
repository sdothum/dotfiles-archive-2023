function zfs
  switch "$argv[1]"
    case destroy;   if-no "$argv"; or sudo zfs $argv
    case '*';       sudo zfs $argv
  end
end
