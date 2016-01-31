function unknown
  switch "$argv"
    case c;     a '^Cc:' ~/.mail/sdothum/unknown/* | sed 's/.*C[Cc]: *\(.*\)/\1/' | sort | m
    case f;     a '^From:' ~/.mail/sdothum/unknown/* | sed 's/.*From: *\(.*\)/\1/' | sort | m
    case s;     a '^Subject:.*\[.*\]' ~/.mail/sdothum/unknown/* | sed 's/.*Subject: *\(.*\)/\1/' | sort | m
    case t;     a '^To:' ~/.mail/sdothum/unknown/* | sed 's/.*To: *\(.*\)/\1/' | sort | m
    case '*';   echo ".. unknown(mail)  c'c | f'rom | s'ubject | t'o"
  end
end
