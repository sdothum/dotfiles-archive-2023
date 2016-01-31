function ups
  sudo pwrstat -test; sleep 3; sudo pwrstat -config; sudo pwrstat -status; 
end
