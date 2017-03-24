" sdothum - 2016 (c) wtfpl

" Lines
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Comment line styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................. Leaders

      " macros assume the first word is the comment delimiter
      let g:linedrawing = '▁─▔▂'            " declare multibytes, see status.vim

    " ................................................................ Underline

      " example: draw underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      function! Underline(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          execute 'normal yypwv$r' . a:delimiter
        endif
        normal $
      endfunction

      imap <silent>,,- <C-o>:silent call Underline('▔')<CR>
      nmap <silent>,,- :silent call Underline('▔')<CR>
      imap <silent>,,_ <C-o>:silent call Underline('▂')<CR>
      nmap <silent>,,_ :silent call Underline('▂')<CR>

    " .................................................................... Ruler

      " example: draw ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      function! Drawline(delimiter)
        call Underline(a:delimiter)
        if virtcol('.') < g:linewidth       " for mirrored left/right margin spacing
          " let l:col = g:linewidth - virtcol('.') - l:col + 1
          let l:col   = g:linewidth - virtcol('.')
          execute 'normal ' . l:col . 'a' . a:delimiter
        endif
        normal $
      endfunction

      imap <silent>,,-- <C-o>:silent call Drawline('▔')<CR>
      nmap <silent>,,-- :silent call Drawline('▔')<CR>
      imap <silent>,,__ <C-o>:silent call Drawline('▂')<CR>
      nmap <silent>,,__ :silent call Drawline('▂')<CR>

    " .................................................................. Trailer

      " example: append trailer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
      function! AppendTrailer(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing trailer
          if matchstr(getline(line('.')), '\s[' . a:delimiter . ']\+$') > ''
            if g:mnle == 0
              normal $bhD
            else
              normal $bmD
            endif
          endif
          normal $
          let l:col = g:linewidth - virtcol('.') - 1
          if l:col > 0
            set formatoptions-=c            " suppress potential comment line wrapping
            execute 'normal a '
            execute 'normal ' . l:col . 'a' . a:delimiter
            set formatoptions+=c
          endif
          normal $
        endif
      endfunction

      imap <silent>,.. <C-o>:silent call AppendTrailer('.')<CR>
      nmap <silent>,.. :silent call AppendTrailer('.')<CR>
      imap <silent>,-- <C-o>:silent call AppendTrailer('─')<CR>
      nmap <silent>,-- :silent call AppendTrailer('─')<CR>
      imap <silent>,__ <C-o>:silent call AppendTrailer('▁')<CR>
      nmap <silent>,__ :silent call AppendTrailer('▁')<CR>

      " prompted trailer
      function! InputTrailer()
        let l:delimiter = input('Line character: ')
        if l:delimiter > ''
          call AppendTrailer(l:delimiter[0])
        endif
      endfunction

      imap <silent>,?? <C-o>:call InputTrailer()<CR>
      nmap <silent>,?? :call InputTrailer()<CR>

    " ................................................................... Leader

      " ................................................. example: insert leader
      function! InsertLeader(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing leader
          if matchstr(getline(line('.')), '\S\s\+[' . a:delimiter . ']\+\s') > ''
            if g:mnle == 0
              execute 'normal ^wdf '
            else
              execute 'normal ^wdt x'
            endif
          endif
          call AppendTrailer(a:delimiter)
          " cut trailer and insert as leader!
          if g:mnle == 0
            normal $bhD^whP
          else
            normal $bmD^wmP
          endif
          normal $
        endif
      endfunction

      imap <silent>,. <C-o>:silent call InsertLeader('.')<CR>
      nmap <silent>,. :silent call InsertLeader('.')<CR>
      imap <silent>,_ <C-o>:silent call InsertLeader('▁')<CR>
      nmap <silent>,_ :silent call InsertLeader('▁')<CR>

      " prompted leader
      function! InputLeader()
        let l:delimiter = input('Line character: ')
        if l:delimiter > ''
          if l:delimiter == ' '
            call RightJustify()
          else
            call InsertLeader(l:delimiter[0])
          endif
        endif
      endfunction

      imap <silent>,? <C-o>:call InputLeader()<CR>
      nmap <silent>,? :call InputLeader()<CR>

    " .................................................................. Justify

      "                                                         example: justify
      function! Justify()
        call InsertLeader('▔')
        execute ':s/▔/ /'
        normal $
      endfunction

      imap <silent>,<Right> <C-o>:silent call Justify()<CR>
      nmap <silent>,<Right> :silent call Justify()<CR>

" lines.vim
