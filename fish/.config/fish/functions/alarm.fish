function alarm
  function timer
    [ (count $argv) -lt 2 ]; and echo ".. alarm minutes message"; and return
    set minutes (echo "$argv[1] * 60" | bc)
    set message $argv[(seq 2 (count $argv))]
    [ (hostname) = luna ]; and set device --device=alsa_output.usb-Burr-Brown_from_TI_USB_Audio_DAC-00-DAC.analog-stereo
    echo "..$message alarm in $argv[1] minutes => "(date -d (echo now + $argv[1] minutes) '+%I:%M:%S%P')
    sleep $minutes
    notify "$argv[1] minutes" "$message"
    paplay --volume=35687 $device --property=media.role=event /usr/share/alarmclock/ring.wav
  end

  switch "$argv[1]"
    case bacon;     timer 20 bacon
    case cooker;    timer 10 pressure cooker
    case dryer;     timer 60 dryer
    case hard;      timer 5 hard boiled eggs
    case laundry;   timer 30 laundry
                    echo ..put laundry in dryer, press enter to continue; read
                    alarm dryer
    case lentils;   timer 10 lentils
                    echo ..add spices, press enter to continue; read
                    timer 5 lentils
    case noodles;   timer 5 noodles
    case poached;   timer 2 poached eggs
    case rice;      timer 40 rice
    case simmer;    timer 90 simmer soup
    case soft;      timer 5 soft boiled eggs
    case '*';       timer $argv
  end
end
