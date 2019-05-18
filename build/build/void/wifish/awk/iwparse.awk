function cmp_signal(i1, v1, i2, v2) 
{
  return(v2["signal"] - v1["signal"])
}

/^BSS / {
    MAC = $2
}
/SSID/ {
    sub($1,"",$0)
    sub(/^[[:space:]]+/,"",$0)
    wifi[MAC]["SSID"] = "\""$0"\""
    if(length(wifi[MAC]["SSID"]) > length(longest)) {
      longest = wifi[MAC]["SSID"]
    }
}
/primary channel/ {
    wifi[MAC]["channel"] = $NF
}

/RSN/ {
    current = "RSN"
}

/signal:/ {
    wifi[MAC]["signal"] = $2
}

/WPS/ {
    wifi[MAC]["WPS"] = $NF
    current = "WPS"
}

/WPA/ {
    wifi[MAC]["WPA"] = $NF
    current = "WPA"
}

/Authentication suite/ {
    wifi[MAC][current] = $NF
}

END {
    len = length(longest)
    if(len == 0)
      exit
    printf "%-"len"s\t\t%-3s\t%-6s\t%-4s\t%s\n","SSID","CH","SIGNAL", "ENC", "AUTH"

    PROCINFO["sorted_in"] = "cmp_signal"
    ws = asort(wifi, nwifi)
    for (w in nwifi) {
        ssid = nwifi[w]["SSID"]
        if(ssid ~ /^""$/) 
          ssid = "HIDDEN"
        printf "%-"len"s\t\t%-3s\t%-6s\t",ssid,nwifi[w]["channel"],nwifi[w]["signal"]
        if(nwifi[w]["WPA"])
          printf "%-4s\t%s\n", "WPA", nwifi[w]["WPA"]
        else if(nwifi[w]["RSN"])
          printf "%-4s\t%s\n", "RSN", nwifi[w]["RSN"]
        else if(nwifi[w]["WPS"])
          printf "%-4s\t%s\n", "WPS", "Push Button?"
        else
          printf "%-4s\t%s\n", "NONE", "N/A"
    }
}
