" sdothum - 2016 (c) wtfpl

" Heading
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Heading styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_heading")
        finish
      endif
      let g:loaded_heading = 1
      let s:save_cpo = &cpo
      set cpo&vim

      let g:multibytes = '▁─▔▂'             " multibyte line characters in use, see info.vim

      if !exists('g:heading_map_prefix')
        let g:heading_map_prefix = ','
      endif

    " ................................................................ Underline

      " example: draw underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      execute 'imap <silent>'g:heading_map_prefix . g:heading_map_prefix . '-' "<C-o>:silent call heading#Underline('▔')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . g:heading_map_prefix . '-' ":silent call heading#Underline('▔')<CR>"
      execute 'imap <silent>'g:heading_map_prefix . g:heading_map_prefix . '_' "<C-o>:silent call heading#Underline('▂')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . g:heading_map_prefix . '_' ":silent call heading#Underline('▂')<CR>"

    " .................................................................... Ruler

      " example: draw ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      execute 'imap <silent>'g:heading_map_prefix . g:heading_map_prefix . '--' "<C-o>:silent call heading#Drawline('▔')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . g:heading_map_prefix . '--' ":silent call heading#Drawline('▔')<CR>"
      execute 'imap <silent>'g:heading_map_prefix . g:heading_map_prefix . '__' "<C-o>:silent call heading#Drawline('▂')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . g:heading_map_prefix . '__' ":silent call heading#Drawline('▂')<CR>"

    " .................................................................. Trailer

      " example: append trailer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

      execute 'imap <silent>'g:heading_map_prefix . '..' "<C-o>:silent call heading#AppendTrailer('.')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . '..' ":silent call heading#AppendTrailer('.')<CR>"
      execute 'imap <silent>'g:heading_map_prefix . '--' "<C-o>:silent call heading#AppendTrailer('─')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . '--' ":silent call heading#AppendTrailer('─')<CR>"
      execute 'imap <silent>'g:heading_map_prefix . '__' "<C-o>:silent call heading#AppendTrailer('▁')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . '__' ":silent call heading#AppendTrailer('▁')<CR>"

      execute 'imap <silent>'g:heading_map_prefix . '??' "<C-o>:call heading#InputTrailer()<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . '??' ":call heading#InputTrailer()<CR>"

    " ................................................................... Leader

      " ................................................. example: insert leader

      execute 'imap <silent>'g:heading_map_prefix . '.' "<C-o>:silent call heading#InsertLeader('.')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . '.' ":silent call heading#InsertLeader('.')<CR>"
      execute 'imap <silent>'g:heading_map_prefix . '_' "<C-o>:silent call heading#InsertLeader('▁')<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . '_' ":silent call heading#InsertLeader('▁')<CR>"

      execute 'imap <silent>'g:heading_map_prefix . '?' "<C-o>:call heading#InputLeader()<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . '?' ":call heading#InputLeader()<CR>"

    " .................................................................. Justify

      "                                                         example: justify

      execute 'imap <silent>'g:heading_map_prefix . "<Right>" "<C-o>:silent call heading#Justify()<CR>"
      execute 'nmap <silent>'g:heading_map_prefix . "<Right>" ":silent call heading#Justify()<CR>"

      let &cpo = s:save_cpo
      unlet s:save_cpo

" heading.vim
