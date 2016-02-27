" sdothum - 2016 (c) wtfpl

" Coding
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Visual aids ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ....................................................... Trailing highlight

      " toggle trailing whitespace highlight
      function! ToggleSpaces()
        set list!
        if &list == 0
          match ExtraWhitespace /\%x00$/    " nolist by failing match with null character :-)
          echo ''
        else
          match ExtraWhitespace /\s\+$/
          echo 'List invisibles ON'
        end
      endfunction

      " imap <silent><F8>          <C-o>:call ToggleSpaces()<CR>
      " nmap <silent><F8>          :call ToggleSpaces()<CR>
      nmap <silent><leader><Space> :call ToggleSpaces()<CR>

    " ................................................. Colorcolumns / line wrap

      let g:linewidth = &textwidth          " default to &textwidth, see Ruler
      " column list must end in [0]
      let s:margins = [45, 72, &textwidth, g:linewidth, 0]

      " toggle colorcolumns
      function! ToggleColumn()
        " add 1st non-blank character in current line to margins list :-)
        if getline(line('.')) != ''
          if index(s:margins, col('.')) == -1
            let s:margins = uniq(SortNumbers([col('.')]+s:margins[:-2])) + [0]
            let &colorcolumn = col('.')
            return
          endif
        endif
        if &colorcolumn == ''
          let &colorcolumn = col('.')
        endif
        execute 'let l:index = index(s:margins,' . &colorcolumn . ') + 1'
        " weird problem with if test so simply loop list! :-o
        " if l:index > len(s:margins)
        "   let l:index = 0
        " endif
        " let &colorcolumn = s:margins[l:index]
        let &colorcolumn = (s:margins+s:margins)[l:index]
      endfunction

      " imap <F7> <C-o>:call ToggleColumn()<CR>
      " nmap <F7> :call ToggleColumn()<CR>
      nmap <Bar>  :call ToggleColumn()<CR>

      " toggle line wrap
      function! ToggleWrap()
        if &formatoptions == 'tqwan1'
          " NoPencil
          let &formatoptions = g:codeoptions
          " echo PencilMode() . ' - Automatic line wrap OFF'
          echo 'Automatic line wrap OFF'
        elseif &formatoptions == g:codeoptions
          " Pencil
          set formatoptions=tqwan1
          " echo PencilMode() . ' - Automatic line wrap ON'
          echo 'Automatic line wrap ON'
        else
          set formatoptions
        endif
      endfunction

      " imap <silent><F4>    <C-o>:call ToggleWrap()<CR>
      " nmap <silent><F4>    :call ToggleWrap()<CR>
      nmap <silent><leader><CR> :call ToggleWrap()<CR>

      " " toggle pencil
      " function! TogglePencil()
      "   TogglePencil
      "   echo PencilMode()
      " endfunction
      "
      " imap <C-F4> <C-o>:call TogglePencil()<CR>
      " nmap <C-F4> :call TogglePencil()<CR>

    " .............................................................. Select text

      function! ParagraphAbove()
        if matchstr(getline(line('.')), '\S') == ''
          normal {
          if matchstr(getline(line('.')), '\S') == ''
            normal j
          endif
        endif
        normal }kV{
      endfunction

      function! ParagraphBelow()
        if matchstr(getline(line('.')), '\S') == ''
          normal }
          if matchstr(getline(line('.')), '\S') == ''
            normal k
          endif
        endif
        normal {jV}
      endfunction

      " select paragragh
      nmap <A-PageUp>   :call ParagraphAbove()<CR>
      nmap <A-PageDown> :call ParagraphBelow()<CR>
      " extend paragraph selection
      vmap <A-PageUp>   {
      vmap <A-PageDown> }

  " Text manipulation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Comment text

      " toggle comment, see T-comment plugins.vim
      function! ToggleComment()
        let l:col = virtcol('.')
        " suffix empty line from successive ToggleComment's
        " (for cycles: empty commented -> uncommented -> empty commented..)
        if matchstr(getline(line('.')), '\s') > ''
          let l:mark = l:col
          normal aMark
        else
          let l:mark = 0
        endif
        " comment line
        execute "normal :TComment\<CR>"
        " reposition cursor when uncommenting autocomment line (creates "" line)
        if matchstr(getline(line('.')), '\S') == ''
          execute 'normal ' . l:col . 'a '
          execute "normal a\<BS>"
        else
          normal $
          " remove empty comment suffix
          if l:mark > 0
            normal xxxx
          endif
        endif
      endfunction

      imap <leader><leader>c <C-o>:call ToggleComment()<CR>

    " .......................................................... Code block text

      " convert wiki text lines into code block lines
      function! CodeBlock()
        execute "silent! normal :s/\\(.*\\)/`\\1`/\<CR>"
        " preserve leading spaces with wiki markdown
        execute "silent! normal gv:s/^` /`^ /\<CR>"
        execute "silent! normal gv:s/^``/`^ `/e\<CR>"
        " convert [[ test ]], see thedarnedestthing markdown
        execute "silent! normal gv:s/ \\[\\[ / [[] /e\<CR>"
        execute "silent! normal gv:s/ \\]\\] / []] /e\<CR>"
      endfunction

    " ...................................................... Vertical text shift

      " see editing.vim for left/right key mappings
      " see https://github.com/gorkunov/vimconfig.git
      function! s:MoveLineUp()
        call s:MoveLineOrVisualUp('.', '')
      endfunction

      function! s:MoveLineDown()
        call s:MoveLineOrVisualDown('.', '')
      endfunction

      function! s:MoveVisualUp()
        call s:MoveLineOrVisualUp("'<", "'<,'>")
        normal gv
      endfunction

      function! s:MoveVisualDown()
        call s:MoveLineOrVisualDown("'>", "'<,'>")
        normal gv
      endfunction

      function! s:MoveLineOrVisualUp(line_getter, range)
        let l_num = line(a:line_getter)
        if l_num - v:count1 - 1 < 0
          let move_arg = '0'
        else
          let move_arg = a:line_getter . ' -' . (v:count1 + 1)
        endif
        call s:MoveLineOrVisualUpOrDown(a:range . 'move ' . move_arg)
      endfunction

      function! s:MoveLineOrVisualDown(line_getter, range)
        let l_num = line(a:line_getter)
        if l_num + v:count1 > line('$')
          let move_arg = '$'
        else
          let move_arg = a:line_getter . ' +' . v:count1
        endif
        call s:MoveLineOrVisualUpOrDown(a:range . 'move ' . move_arg)
      endfunction

      function! s:MoveLineOrVisualUpOrDown(move_arg)
        let col_num = virtcol('.')
        execute 'silent! ' . a:move_arg
        execute 'normal! ' . col_num . '|'
      endfunction

      " shift text up / down
      imap <silent><S-Up>   <ESC>:call <SID>MoveLineUp()<CR>a
      imap <silent><S-Down> <ESC>:call <SID>MoveLineDown()<CR>a
      nmap <silent><S-Up>   <ESC>:call <SID>MoveLineUp()<CR>
      nmap <silent><S-Down> <ESC>:call <SID>MoveLineDown()<CR>
      vmap <silent><S-Up>   <ESC>:call <SID>MoveVisualUp()<CR>
      vmap <silent><S-Down> <ESC>:call <SID>MoveVisualDown()<CR>

" coding.vim
