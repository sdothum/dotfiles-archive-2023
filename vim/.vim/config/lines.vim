" sdothum - 2016 (c) wtfpl

" Lines
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Comment line styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    let g:linedrawing = '▁▔▂'             " declare multibytes, see statusline.vim

    " ................................................................ Underline

      " example: draw underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔

      " for simplification, assumes first word of line is comment delimiter
      function! Underline(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          execute 'normal yypwv$r' . a:delimiter
        endif
      endfunction

      imap <silent>,,- <C-o>:silent call Underline('▔')<CR><C-Return>
      nmap <silent>,,- :silent call Underline('▔')<CR><Down>
      imap <silent>,,_ <C-o>:silent call Underline('▂')<CR><C-Return>
      nmap <silent>,,_ :silent call Underline('▂')<CR><Down>

    " .................................................................... Ruler

      " example: draw ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      " note: to insert a ruler on an empty line (virtual column position), add a <space>
      " character, else in insert mode, the ruler will positioned using column 1

      function! Drawline(delimiter)
        call Underline(a:delimiter)
        normal $
        if virtcol('.') < g:linewidth       " for mirrored left/right margin spacing
          " let l:col = g:linewidth - virtcol('.') - l:col + 1
          let l:col   = g:linewidth - virtcol('.')
          execute 'normal ' . l:col . 'a' . a:delimiter
        endif
        normal o
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
          normal o
        endif
      endfunction

      imap <silent>,.. <C-o>:silent call AppendTrailer('.')<CR>
      nmap <silent>,.. :silent call AppendTrailer('.')<CR>
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
          normal o
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

      function! RightJustify()
        if matchstr(getline(line('.')), '\S') > ''
          if matchstr(getline(line('.')), '^ *') > '' " remove existing leader
            if g:mnle == 0
              execute 'normal 0vwhd'
            else
              execute 'normal 0vwmd'
            endif
          endif
          normal $
          let l:col = g:linewidth - virtcol('.') - 1
          if l:col > 0
            set formatoptions-=c            " suppress potential comment line wrapping
            normal ^
            execute 'normal ' . l:col . 'i '
            execute 'normal a '
            set formatoptions+=c
          endif
        endif
        normal o
      endfunction

      imap <silent>,<Right> <C-o>:silent call RightJustify()<CR>
      nmap <silent>,<Right> :silent call RightJustify()<CR>

" lines.vim
