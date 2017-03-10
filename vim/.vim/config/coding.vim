" sdothum - 2016 (c) wtfpl

" Coding
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Visual aids ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

      augroup coding
        autocmd!
      augroup END

    " ........................................................... Line numbering

      " toggle relative number, line number and no numbering
      function! ToggleNumber()
        if (&relativenumber == 1 && &number == 1)
          set norelativenumber
        else
          if (&relativenumber == 0 && &number == 1)
            set nonumber
          else
            set relativenumber
            set number
          endif
        endif
      endfunction

      nmap <silent># :call ToggleNumber()<CR>

    " ............................................................. Column ruler

      " see plugins.vim IndentTheme()
      let g:ruler = 0

      " toggle colorcolumn modes
      function! ToggleColumn()
        if g:ruler == 0
          let g:ruler = 1
          let &colorcolumn = col('.')
          autocmd coding CursorMoved,CursorMovedI * let &colorcolumn = col('.')
        else
          if g:ruler == 1
            let g:ruler = 2
            autocmd! coding
          else
            let g:ruler = 0
            let &colorcolumn = 0
          endif
        endif
        call IndentTheme()
      endfunction

      nmap <silent><Bar>      :call ToggleColumn()<CR>
      nmap <silent><Bar><Bar> :IndentGuidesToggle<CR>:call IndentTheme()<CR>

    " ....................................................... Trailing highlight

      " toggle trailing whitespace highlight and indentation levels
      function! ToggleSpaces()
        set list!
        if &list == 0
          match ExtraWhitespace /\%x00$/    " nolist by failing match with null character :-)
          " echo ''
          let g:matchspace = ''
        else
          match ExtraWhitespace /\s\+$/
          " echo 'List invisibles ON'
          let g:matchspace = '■'
        end
      endfunction

      nmap <silent><leader><Space> :call ToggleSpaces()<CR>

  " Line wrap ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Insert line wrap

      " insert line while disabling auto-commenting
      function! InsertWrap()
        let l:formatoptions = &formatoptions
        set formatoptions-=c
        set formatoptions-=r
        set formatoptions-=o
        normal ^
        let l:pos = col('.')
        normal o
        " align line indentation
        execute 'normal a' . repeat(' ', l:pos)
        let &formatoptions = l:formatoptions
      endfunction

      inoremap <silent><C-Return> <C-o>:call InsertWrap()<CR>

    " ......................................................... Toggle line wrap

      function! ToggleWrap()
        if &formatoptions =~ 't'
          " NoPencil
          let &formatoptions = g:codeoptions
          echo 'Automatic line wrap OFF'
        elseif &formatoptions == g:codeoptions
          " Pencil
          set formatoptions=tqwan1
          echo 'Automatic line wrap ON'
        else
          set formatoptions
        endif
      endfunction

      nmap <silent><leader><CR> :call ToggleWrap()<CR>

  " Text shift ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Select text

      function! ParagraphAbove()
        if matchstr(getline(line('.')), '\S') == ''
          normal {
          if matchstr(getline(line('.')), '\S') == ''
            normal j
          endif
        endif
        normal }lV{
      endfunction

      function! ParagraphBelow()
        if matchstr(getline(line('.')), '\S') == ''
          normal }
          if matchstr(getline(line('.')), '\S') == ''
            normal k
          endif
        endif
        normal {nV}
      endfunction

      " select paragragh
      nmap <silent><A-PageUp>   :call ParagraphAbove()<CR>
      nmap <silent><A-PageDown> :call ParagraphBelow()<CR>
      " extend paragraph selection
      vmap <A-PageUp>   {
      vmap <A-PageDown> }

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
        let l:line = line(a:line_getter)
        if l:line - v:count1 - 1 < 0
          let l:move = '0'
        else
          let l:move = a:line_getter . ' -' . (v:count1 + 1)
        endif
        call s:MoveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! s:MoveLineOrVisualDown(line_getter, range)
        let l:line = line(a:line_getter)
        if l:line + v:count1 > line('$')
          let l:move = '$'
        else
          let l:move = a:line_getter . ' +' . v:count1
        endif
        call s:MoveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! s:MoveLineOrVisualUpOrDown(move)
        let l:col = virtcol('.')
        execute 'silent! ' . a:move
        execute 'normal! ' . l:col . '|'
      endfunction

      " shift text up / down
      imap <silent><S-Up>   <ESC>:call <SID>MoveLineUp()<CR>a
      imap <silent><S-Down> <ESC>:call <SID>MoveLineDown()<CR>a
      nmap <silent><S-Up>   :call <SID>MoveLineUp()<CR>
      nmap <silent><S-Down> :call <SID>MoveLineDown()<CR>
      vmap <silent><S-Up>   <ESC>:call <SID>MoveVisualUp()<CR>
      vmap <silent><S-Down> <ESC>:call <SID>MoveVisualDown()<CR>

      " move visual block with automatic alignment!
      vnoremap L :m '<-2<CR>gv=gv
      vnoremap N :m '>+1<CR>gv=gv

  " Text manipulation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Comment text

      " toggle comment, see T-comment plugins.vim
      function! ToggleComment()
        let l:col = virtcol('.')
        let l:len = virtcol('$')
        " suffix empty line from successive ToggleComment's
        " (for cycles: empty commented -> uncommented -> empty commented..)
        if matchstr(getline(line('.')), '\s') > ''
          let l:mark = l:col
        else
          let l:mark = 0
        endif
        " toggle comment line
        execute "normal :TComment\<CR>"
        " reposition cursor when uncommenting autocomment line (creates "" line)
        if matchstr(getline(line('.')), '\S') == ''
          execute 'normal ' . l:col . 'a '
          execute "normal a\<BS>"
        endif
        " loose column repositioning calculation for beginning and end of line
        " but not all near edge corner cases are accounted for
        if virtcol('.') > 1 && virtcol('.') < virtcol('$') - 1
          if virtcol('$') > l:len
            execute 'normal ' . (virtcol('$') - l:len) . 'e'
          else
            if virtcol('$') < l:len
              execute 'normal ' . (l:len - virtcol('$')) . 'm'
            endif
          endif
        endif
      endfunction

      imap <silent>,c <C-o>:call ToggleComment()<CR>

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

      " markup wiki code blocks
      nnoremap <silent><leader>` V:call CodeBlock()<CR>
      vmap <silent><leader>`     :call CodeBlock()<CR>

" coding.vim
