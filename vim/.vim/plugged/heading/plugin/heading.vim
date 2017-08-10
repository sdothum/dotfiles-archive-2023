" sdothum - 2016 (c) wtfpl

" Heading
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Heading styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let g:linedrawing = '▁─▔▂'            " declare multibytes, see info.vim

    " ................................................................ Underline

      " example: draw underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      imap <silent>,,- <C-o>:silent call heading#Underline('▔')<CR>
      nmap <silent>,,- :silent call heading#Underline('▔')<CR>
      imap <silent>,,_ <C-o>:silent call heading#Underline('▂')<CR>
      nmap <silent>,,_ :silent call heading#Underline('▂')<CR>

    " .................................................................... Ruler

      " example: draw ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      imap <silent>,,-- <C-o>:silent call heading#Drawline('▔')<CR>
      nmap <silent>,,-- :silent call heading#Drawline('▔')<CR>
      imap <silent>,,__ <C-o>:silent call heading#Drawline('▂')<CR>
      nmap <silent>,,__ :silent call heading#Drawline('▂')<CR>

    " .................................................................. Trailer

      " example: append trailer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

      imap <silent>,.. <C-o>:silent call heading#AppendTrailer('.')<CR>
      nmap <silent>,.. :silent call heading#AppendTrailer('.')<CR>
      imap <silent>,-- <C-o>:silent call heading#AppendTrailer('─')<CR>
      nmap <silent>,-- :silent call heading#AppendTrailer('─')<CR>
      imap <silent>,__ <C-o>:silent call heading#AppendTrailer('▁')<CR>
      nmap <silent>,__ :silent call heading#AppendTrailer('▁')<CR>

      imap <silent>,?? <C-o>:call heading#InputTrailer()<CR>
      nmap <silent>,?? :call heading#InputTrailer()<CR>

    " ................................................................... Leader

      " ................................................. example: insert leader

      imap <silent>,. <C-o>:silent call heading#InsertLeader('.')<CR>
      nmap <silent>,. :silent call heading#InsertLeader('.')<CR>
      imap <silent>,_ <C-o>:silent call heading#InsertLeader('▁')<CR>
      nmap <silent>,_ :silent call heading#InsertLeader('▁')<CR>

      imap <silent>,? <C-o>:call heading#InputLeader()<CR>
      nmap <silent>,? :call heading#InputLeader()<CR>

    " .................................................................. Justify

      "                                                         example: justify

      imap <silent>,<Right> <C-o>:silent call heading#Justify()<CR>
      nmap <silent>,<Right> :silent call heading#Justify()<CR>

" heading.vim
