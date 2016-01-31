function devid
  switch "$argv[1]"
    case [1];   ll /dev/disk/by-id/ | g scsi-SATA_
    case '*';   ll /dev/disk/by-id/ | g scsi-SATA_ | g $argv[1]
  end
end
