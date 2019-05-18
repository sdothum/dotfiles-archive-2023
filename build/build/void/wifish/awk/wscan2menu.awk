$1~/^..:..:..:../ {
  strength=$3
  enc=$4
  $1=""
  $2=""
  $3=""
  $4=""
  sub(/^..../,"",$0)
  printf "\""$0"\" "
  printf "\""strength+100"|  "enc"\"\n"
}
