function cmpdir
  # posix "<(command)" syntax replaced with fish psub named pipe
  if [ (count $argv) -eq 2 ]
    comm (ls $argv[1] | sort | psub) (ls $argv[2] | sort | psub)
  else
    echo ".. cmpdir dir1 dir2"
  end
end
