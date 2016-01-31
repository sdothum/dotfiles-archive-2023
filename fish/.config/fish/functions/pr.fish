function pr
  set printfile (mktemp).ps
  set format --pretty-print --color --fancy-header --font=Courier8 --header-font=Helvetica12 --line-numbers --media=Letter --portrait --tabsize=4
  switch "$argv[1]"
    case [1];   echo ".. pr [prev'iew] filelist.."
                # note: header font spec doesn't appear to do anything
    case v;     enscript $format --output=$printfile $argv[2]; evince $printfile
                # not using enscript print option because lpr doesn't see printer despite lpstat
    case '*';   for file in $argv; enscript $format --output=$printfile $file; lp $printfile; end
  end
  command rm -f $printfile
end
