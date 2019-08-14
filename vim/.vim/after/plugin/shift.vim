" sdothum - 2016 (c) wtfpl

" Shift
" ══════════════════════════════════════════════════════════════════════════════

  " Text shift _________________________________________________________________

    " .............................................................. Select text

      function! s:paragraphAbove()
        if NonBlankLine
          normal! {
          if BlankLine()
            normal! j
          endif
        endif
        normal! }kV{
      endfunction
       
      command! ParagraphAbove silent! call <SID>paragraphAbove()

      function! s:paragraphBelow()
        if NonBlankLine
          normal! }
          if BlankLine()
            normal! k
          endif
        endif
        normal! {jV}
      endfunction
       
      command! ParagraphBelow silent! call <SID>paragraphBelow()

    " ............................................................ Shift up down

      " see https://github.com/gorkunov/vimconfig.git
      function! s:moveLineOrVisualUpOrDown(move)
        let l:col = virtcol('.')
        execute 'silent! ' . a:move
        execute 'normal! ' . l:col . '|'
      endfunction

      function! s:moveLineOrVisualUp(line_getter, range)
        let l:line = line(a:line_getter)
        if l:line - v:count1 - 1 < 0 | let l:move = '0'
        else                         | let l:move = a:line_getter . ' -' . (v:count1 + 1) | endif
        call s:moveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! s:moveLineOrVisualDown(line_getter, range)
        let l:line = line(a:line_getter)
        if l:line + v:count1 > line('$') | let l:move = '$'
        else                             | let l:move = a:line_getter . ' +' . v:count1 | endif
        call s:moveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! s:moveLineUp()
        call s:moveLineOrVisualUp('.', '')
      endfunction
       
      command! MoveLineUp silent! call <SID>moveLineUp()

      function! s:moveLineDown()
        call s:moveLineOrVisualDown('.', '')
      endfunction
       
      command! MoveLineDown silent! call <SID>moveLineDown()

      function! s:moveVisualUp()
        call s:moveLineOrVisualUp("'<", "'<,'>")
        normal! gv
      endfunction
       
      command! MoveVisualUp silent! call <SID>moveVisualUp()

      function! s:moveVisualDown()
        call s:moveLineOrVisualDown("'>", "'<,'>")
        normal! gv
      endfunction
       
      command! MoveVisualDown silent! call <SID>moveVisualDown()

" shift.vim


