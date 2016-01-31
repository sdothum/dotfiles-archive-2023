function zpool
  # since 0.6.1 initial zpool mounts often are not mounted with a mount point
  function mount
    set ls (ls $argv[2])
    attn "mount $argv[1] :: $argv[2] -> $ls"
    if [ (ls $argv[2] | wc -w) -eq 0 -a $argv[3] -lt 2 ]
      # klugde is to export and import the pool
      zpool status $argv[1] 2>/dev/null | grep ONLINE; and zpool export $argv[1]; or zpool import -f $argv[1]
      mount $argv[1] $argv[2] (echo "$argv[3]+1" | bc)
    end
  end

  switch "$argv[1]"
    [1];            sudo zpool status
    case destroy;   if-no "$argv"; or sudo zpool $argv
    case mount;     mount tank /data 0
                    mount pond /bkup 0
    case '*';       sudo zpool $argv
  end
end
