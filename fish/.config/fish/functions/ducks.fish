function ducks
  du -cks * | sort -rn | egrep -v '^0|total'; 
end
