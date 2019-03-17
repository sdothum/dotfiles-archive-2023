" sdothum - 2016 (c) wtfpl

" Heading
" ══════════════════════════════════════════════════════════════════════════════

  " Heading styles _____________________________________________________________

    " .................................................................... Setup

      if exists("g:loaded_heading") | finish | endif
      let g:loaded_heading = 1
      let s:save_cpo       = &cpo
      set cpo&vim

      let g:over       = ','  " leaders
      let g:under      = '`'  " shift comma on beakl si layout

      let g:bullet     = '·'  " U+00b7
      let g:dot        = '.'
      let g:equal      = '═'  " U+2550
      let g:dash       = '─'  " U+2500
      let g:underscore = '_'

    " ................................................................ Underline

      " example: draw underline
      " ═══════════════════════
      execute 'imap <silent>' . g:under . g:under . '-'  " <C-o>:silent call heading#Underline(g:dash)<CR><C-o>$"
      execute 'nmap <silent>' . g:under . g:under . '-'  " :silent call heading#Underline(g:dash)<CR>"
      execute 'imap <silent>' . g:under . g:under . '='  " <C-o>:silent call heading#Underline(g:equal)<CR><C-o>$"
      execute 'nmap <silent>' . g:under . g:under . '='  " :silent call heading#Underline(g:equal)<CR>"

    " .................................................................... Ruler

      " example: draw ruler
      " ════════════════════════════════════════════════════════════════════════
      execute 'imap <silent>' . g:under . g:under . '--'  " <C-o>:silent call heading#Drawline(g:dash)<CR>"
      execute 'nmap <silent>' . g:under . g:under . '--'  " :silent call heading#Drawline(g:dash)<CR>"
      execute 'imap <silent>' . g:under . g:under . '=='  " <C-o>:silent call heading#Drawline(g:equal)<CR>"
      execute 'nmap <silent>' . g:under . g:under . '=='  " :silent call heading#Drawline(g:equal)<CR>"

    " .................................................................. Trailer

      " example: append trailer ................................................

      execute 'imap <silent>' . g:over . '..'  " <C-o>:silent call heading#AppendTrailer(g:dot)<CR>"
      execute 'nmap <silent>' . g:over . '..'  " :silent call heading#AppendTrailer(g:dot)<CR>"
      execute 'imap <silent>' . g:over . '__'  " <C-o>:silent call heading#AppendTrailer(g:underscore)<CR>"
      execute 'nmap <silent>' . g:over . '__'  " :silent call heading#AppendTrailer(g:underscore)<CR>"

      execute 'imap <silent>' . g:over . '??'  " <C-o>:call heading#InputTrailer()<CR>"
      execute 'nmap <silent>' . g:over . '??'  " :call heading#InputTrailer()<CR>"

    " ................................................................... Leader

      " ................................................. example: insert leader
 
      execute 'imap <silent>' . g:over . '.'  " <C-o>:silent call heading#InsertLeader(g:dot)<CR><C-o>$"
      execute 'nmap <silent>' . g:over . '.'  " :silent call heading#InsertLeader(g:dot)<CR>"
      execute 'imap <silent>' . g:over . '_'  " <C-o>:silent call heading#InsertLeader(g:underscore)<CR><C-o>$"
      execute 'nmap <silent>' . g:over . '_'  " :silent call heading#InsertLeader(g:underscore)<CR>"

      execute 'imap <silent>' . g:over . '?'  " <C-o>:call heading#InputLeader()<CR>"
      execute 'nmap <silent>' . g:over . '?'  " :call heading#InputLeader()<CR>"

    " .................................................................. Justify

      "                                                   example: right justify

      execute 'imap <silent>' . g:over . "<Right>"  " <C-o>:silent call heading#Justify()<CR>"
      execute 'nmap <silent>' . g:over . "<Right>"  " :silent call heading#Justify()<CR>"

      let &cpo = s:save_cpo
      unlet s:save_cpo

" heading.vim
