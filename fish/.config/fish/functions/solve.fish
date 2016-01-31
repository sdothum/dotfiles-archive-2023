function solve
  # bypass inbuilt fish echo function
  command echo -e "scale=4\n$argv\nquit" | bc
end
