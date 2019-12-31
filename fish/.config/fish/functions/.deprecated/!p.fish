function !p
  return (p $argv | wc -l); 
  # [ (p $argv | wc -l) -eq 0 ]; and false; or true
end
