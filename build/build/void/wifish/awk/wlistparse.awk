$1~/^[[:digit:]]/ {
  num=$1
  $1=""
  $(NF)=""
  if(NF==4) {
    print "4 Fields"
    $(NF-1)=""
    sub(/  $/,"", $0)
  }
  if(NF==3) 
    print "3 Fields"
    sub(/ $/, "", $0)
  sub(/^ /,"", $0)
  print num":\""$0"\""
}
