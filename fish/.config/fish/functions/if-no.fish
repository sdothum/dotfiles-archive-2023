function if-no
  function prompt
    echo "continue? [no]/yes : "
  end

  underline "$argv"
  while true
    read -p prompt CONTINUE
    if [ "$CONTINUE." = "y." -o "$CONTINUE." = "Y." ]
      return 1
    else 
      if [ "$CONTINUE." = "." -o "$CONTINUE." = "n." -o "$CONTINUE." = "N." ]
        return 0
      end
    end
  end
end
