if message.header.from.first.address == "sdothum@gmail.com"
  account = "gmail"
else
  account = (Person.from_address message.header.from.first.address).shortname
end
system("touch ~/.sup/.check_sent")
log "Sending to #{account}"
#cmd = "msmtp --account=#{account} -t --"
cmd = "/usr/local/bin/msmtp-enqueue.sh --account=#{account} -t --"
IO.popen(cmd, "w") { |p| p.puts message }
return $? == 0
