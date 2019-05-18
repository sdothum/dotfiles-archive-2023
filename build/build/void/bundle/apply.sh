#!/usr/bin/dash
for f in *-* ;do 
  while read i j <&3 ;do 
    sed -i "/ $i */s/$i/$j/" $f 
  done 3< package.missing.tovoid 
done
