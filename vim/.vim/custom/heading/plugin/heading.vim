" sdothum - 2016 (c) wtfpl

" Heading
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Heading styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_heading")
        finish
      endif
      let g:loaded_heading = 1
      let s:save_cpo       = &cpo
      set cpo&vim

      if !exists('g:heading_leader')
        let g:heading_leader = ','
      endif

    " ................................................................ Underline

      " example: draw underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      execute 'imap <silent>' . g:heading_leader . g:heading_leader . '-' "<C-o>:silent call heading#Underline('▔')<CR>"
      execute 'nmap <silent>' . g:heading_leader . g:heading_leader . '-' ":silent call heading#Underline('▔')<CR>"
      execute 'imap <silent>' . g:heading_leader . g:heading_leader . '_' "<C-o>:silent call heading#Underline('▂')<CR>"
      execute 'nmap <silent>' . g:heading_leader . g:heading_leader . '_' ":silent call heading#Underline('▂')<CR>"

    " .................................................................... Ruler

      " example: draw ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      execute 'imap <silent>' . g:heading_leader . g:heading_leader . '--' "<C-o>:silent call heading#Drawline('▔')<CR>"
      execute 'nmap <silent>' . g:heading_leader . g:heading_leader . '--' ":silent call heading#Drawline('▔')<CR>"
      execute 'imap <silent>' . g:heading_leader . g:heading_leader . '__' "<C-o>:silent call heading#Drawline('▂')<CR>"
      execute 'nmap <silent>' . g:heading_leader . g:heading_leader . '__' ":silent call heading#Drawline('▂')<CR>"

    " .................................................................. Trailer

      " example: append trailer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

      execute 'imap <silent>' . g:heading_leader . '..' "<C-o>:silent call heading#AppendTrailer('.')<CR>"
      execute 'nmap <silent>' . g:heading_leader . '..' ":silent call heading#AppendTrailer('.')<CR>"
      execute 'imap <silent>' . g:heading_leader . '--' "<C-o>:silent call heading#AppendTrailer('─')<CR>"
      execute 'nmap <silent>' . g:heading_leader . '--' ":silent call heading#AppendTrailer('─')<CR>"
      execute 'imap <silent>' . g:heading_leader . '__' "<C-o>:silent call heading#AppendTrailer('▁')<CR>"
      execute 'nmap <silent>' . g:heading_leader . '__' ":silent call heading#AppendTrailer('▁')<CR>"

      execute 'imap <silent>' . g:heading_leader . '??' "<C-o>:call heading#InputTrailer()<CR>"
      execute 'nmap <silent>' . g:heading_leader . '??' ":call heading#InputTrailer()<CR>"

    " ................................................................... Leader

      " ................................................. example: insert leader

      execute 'imap <silent>' . g:heading_leader . '.' "<C-o>:silent call heading#InsertLeader('.')<CR>"
      execute 'nmap <silent>' . g:heading_leader . '.' ":silent call heading#InsertLeader('.')<CR>"
      execute 'imap <silent>' . g:heading_leader . '_' "<C-o>:silent call heading#InsertLeader('▁')<CR>"
      execute 'nmap <silent>' . g:heading_leader . '_' ":silent call heading#InsertLeader('▁')<CR>"

      execute 'imap <silent>' . g:heading_leader . '?' "<C-o>:call heading#InputLeader()<CR>"
      execute 'nmap <silent>' . g:heading_leader . '?' ":call heading#InputLeader()<CR>"

    " .................................................................. Justify

      "                                                   example: right justify

      execute 'imap <silent>' . g:heading_leader . "<Right>" "<C-o>:silent call heading#Justify()<CR>"
      execute 'nmap <silent>' . g:heading_leader . "<Right>" ":silent call heading#Justify()<CR>"

      let &cpo = s:save_cpo
      unlet s:save_cpo

" heading.vim
