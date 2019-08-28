" sdothum - 2016 (c) wtfpl

" Heading
" ══════════════════════════════════════════════════════════════════════════════

  " Heading styles _____________________________________________________________

    " .................................................................... Setup

      let s:leader     = [',', '!!']  " leaders [comma, shift comma (on beakl pi layout)]

      let s:bullet     = '·'  " U+00b7
      let s:dot        = '.'
      let s:equal      = '═'  " U+2550
      let s:dash       = '─'  " U+2500
      let s:underscore = '_'

    " ................................................................ Underline

      " example: draw underline
      " ═══════════════════════
      execute 'imap <silent>' . s:leader[1] . '-' '<C-o>:silent Underline ' . s:dash  . '<CR><C-o>$'
      execute 'nmap <silent>' . s:leader[1] . '-'      ':silent Underline ' . s:dash  . '<CR>'
      execute 'imap <silent>' . s:leader[1] . '=' '<C-o>:silent Underline ' . s:equal . '<CR><C-o>$'
      execute 'nmap <silent>' . s:leader[1] . '='      ':silent Underline ' . s:equal . '<CR>'

    " .................................................................... Ruler

      " example: draw ruler
      " ════════════════════════════════════════════════════════════════════════
      execute 'imap <silent>' . s:leader[1] . '--' '<C-o>:silent Drawline ' . s:dash  . '<CR>'
      execute 'nmap <silent>' . s:leader[1] . '--'      ':silent Drawline ' . s:dash  . '<CR>'
      execute 'imap <silent>' . s:leader[1] . '==' '<C-o>:silent Drawline ' . s:equal . '<CR>'
      execute 'nmap <silent>' . s:leader[1] . '=='      ':silent Drawline ' . s:equal . '<CR>'

    " .................................................................. Trailer

      " example: append trailer ................................................

      execute 'imap <silent>' . s:leader[0] . '..' '<C-o>:silent AppendTrailer ' . s:dot        . '<CR>'
      execute 'nmap <silent>' . s:leader[0] . '..'      ':silent AppendTrailer ' . s:dot        . '<CR>'
      execute 'imap <silent>' . s:leader[0] . '__' '<C-o>:silent AppendTrailer ' . s:underscore . '<CR>'
      execute 'nmap <silent>' . s:leader[0] . '__'      ':silent AppendTrailer ' . s:underscore . '<CR>'

      execute 'imap <silent>' . s:leader[0] . '??' '<C-o>:InputTrailer<CR>'
      execute 'nmap <silent>' . s:leader[0] . '??'      ':InputTrailer<CR>'

    " ................................................................... Leader

      " ................................................. example: insert leader
 
      execute 'imap <silent>' . s:leader[0] . '.' '<C-o>:silent InsertLeader ' . s:dot        . '<CR><C-o>$'
      execute 'nmap <silent>' . s:leader[0] . '.'      ':silent InsertLeader ' . s:dot        . '<CR>'
      execute 'imap <silent>' . s:leader[0] . '_' '<C-o>:silent InsertLeader ' . s:underscore . '<CR><C-o>$'
      execute 'nmap <silent>' . s:leader[0] . '_'      ':silent InsertLeader ' . s:underscore . '<CR>'

      execute 'imap <silent>' . s:leader[0] . '?' '<C-o>:InputLeader<CR>'
      execute 'nmap <silent>' . s:leader[0] . '?'      ':InputLeader<CR>'

    " .................................................................. Justify

      "                                                   example: right justify

      execute 'imap <silent>' . s:leader[0] . '<Right>' '<C-o>:silent Justify<CR>'
      execute 'nmap <silent>' . s:leader[0] . '<Right>'      ':silent Justify<CR>'

" heading.vim
