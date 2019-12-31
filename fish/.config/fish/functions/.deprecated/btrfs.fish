function btrfs
  [ -z "$argv" ]; and sudo btrfs filesystem show  --all-devices; or sudo btrfs $argv
end
