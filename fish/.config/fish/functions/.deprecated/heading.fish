function heading
	if [ (count $argv) -eq 1 -o (expr length "$argv[1]") -gt 1 ]
    set char ':'
    set message $argv
  else
    set char $argv[1]
    set message $argv[(seq 2 (count $argv))]
  end
  set leader (head -c (echo '('(tput cols)' - '(echo "   $message   " | wc -L)') / 2' | bc) </dev/zero | tr '\0' $char)
  [ (echo "$leader   $message   $leader" | wc -L) -lt (tput cols) ]; and set pad $char
  # attn (set_color -o red)"$leader   "(set_color -o yellow)"$message   "(set_color -o red)"$leader$pad"
  attn "$leader   $message   $leader$pad"
end
