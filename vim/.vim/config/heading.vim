" sdothum - 2016 (c) wtfpl

" Heading
" ══════════════════════════════════════════════════════════════════════════════

  " Heading styles _____________________________________________________________

    " .................................................................... Setup

      let g:over       = ','  " leaders
      let g:under      = '!'  " shift comma on beakl pi layout

      let g:bullet     = '·'  " U+00b7
      let g:dot        = '.'
      let g:equal      = '═'  " U+2550
      let g:dash       = '─'  " U+2500
      let g:underscore = '_'

    " ................................................................ Underline

      " example: draw underline
      " ═══════════════════════
      execute 'imap <silent>' . g:under . g:under . '-' '<C-o>:silent Underline ' . g:dash  . '<CR><C-o>$'
      execute 'nmap <silent>' . g:under . g:under . '-'      ':silent Underline ' . g:dash  . '<CR>'
      execute 'imap <silent>' . g:under . g:under . '=' '<C-o>:silent Underline ' . g:equal . '<CR><C-o>$'
      execute 'nmap <silent>' . g:under . g:under . '='      ':silent Underline ' . g:equal . '<CR>'

    " .................................................................... Ruler

      " example: draw ruler
      " ════════════════════════════════════════════════════════════════════════
      execute 'imap <silent>' . g:under . g:under . '--' '<C-o>:silent Drawline ' . g:dash  . '<CR>'
      execute 'nmap <silent>' . g:under . g:under . '--'      ':silent Drawline ' . g:dash  . '<CR>'
      execute 'imap <silent>' . g:under . g:under . '==' '<C-o>:silent Drawline ' . g:equal . '<CR>'
      execute 'nmap <silent>' . g:under . g:under . '=='      ':silent Drawline ' . g:equal . '<CR>'

    " .................................................................. Trailer

      " example: append trailer ................................................

      execute 'imap <silent>' . g:over . '..' '<C-o>:silent AppendTrailer ' . g:dot        . '<CR>'
      execute 'nmap <silent>' . g:over . '..'      ':silent AppendTrailer ' . g:dot        . '<CR>'
      execute 'imap <silent>' . g:over . '__' '<C-o>:silent AppendTrailer ' . g:underscore . '<CR>'
      execute 'nmap <silent>' . g:over . '__'      ':silent AppendTrailer ' . g:underscore . '<CR>'

      execute 'imap <silent>' . g:over . '??' '<C-o>:InputTrailer<CR>'
      execute 'nmap <silent>' . g:over . '??'      ':InputTrailer<CR>'

    " ................................................................... Leader

      " ................................................. example: insert leader
 
      execute 'imap <silent>' . g:over . '.' '<C-o>:silent InsertLeader ' . g:dot        . '<CR><C-o>$'
      execute 'nmap <silent>' . g:over . '.'      ':silent InsertLeader ' . g:dot        . '<CR>'
      execute 'imap <silent>' . g:over . '_' '<C-o>:silent InsertLeader ' . g:underscore . '<CR><C-o>$'
      execute 'nmap <silent>' . g:over . '_'      ':silent InsertLeader ' . g:underscore . '<CR>'

      execute 'imap <silent>' . g:over . '?' '<C-o>:InputLeader<CR>'
      execute 'nmap <silent>' . g:over . '?'      ':InputLeader<CR>'

    " .................................................................. Justify

      "                                                   example: right justify

      execute 'imap <silent>' . g:over . '<Right>' '<C-o>:silent Justify<CR>'
      execute 'nmap <silent>' . g:over . '<Right>'      ':silent Justify<CR>'

" heading.vim
