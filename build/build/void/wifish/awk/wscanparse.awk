BEGIN {
  longest_ssid = 0
}
/Selected interface/ {
  print
}
/^..:..:/ {
  line[NR]["mac"] = $1
  line[NR]["freq"] = $2
  line[NR]["signal"] = $3
  line[NR]["caps"] = $4
  $1 = ""
  $2 = ""
  $3 = ""
  $4 = ""
  sub(/^[[:space:]]+/,"", $0)
  line[NR]["ssid"] = "\""$0"\""
  if(length(line[NR]["ssid"]) > longest_ssid)
    longest_ssid = length(line[NR]["ssid"])

}
END {
  printf "%-"longest_ssid"s\t%-6s\t%-19s\t%-6s\t%s\n","SSID", "SIGNAL", "MAC", "FREQ", "CAPABILITIES"
  for(i in line) {
    ssid=line[i]["ssid"]
    if(ssid ~ /^""$/)
      ssid="HIDDEN"
    printf "%-"longest_ssid"s\t%-6s\t%-19s\t%-6s\t%s\n", ssid, line[i]["signal"]+100,line[i]["mac"], line[i]["freq"], line[i]["caps"]
  }
}
