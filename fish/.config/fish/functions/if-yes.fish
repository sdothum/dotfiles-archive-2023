function if-yes
  function prompt
    echo "continue? [yes]/no : "
  end

  underline "$argv"
  while true
    read -p prompt CONTINUE
    if [ "$CONTINUE." = "." -o "$CONTINUE." = "y." -o "$CONTINUE." = "Y." ]
      return 0
    else 
      if [ "$CONTINUE." = "n." -o "$CONTINUE." = "N." ]
        return 1
      end
    end
  end
end
