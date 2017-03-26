" sdothum - 2016 (c) wtfpl

" Edit
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Line ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Insert line

      " continue inserting in new line a la textmate command-enter
      " ctrl-enter only works with gvim due to terminal limitation :-(
      " inoremap <C-CR> <C-o>o
      " similarly, open curly braces and continue inserting in indented body
      inoremap <S-CR>   <CR><C-o>O<Tab>

      " break line (in .wiki)
      nnoremap <silent><leader><Enter> :silent set paste<CR>i<CR><ESC>:silent set nopaste<CR>i

      " insert blank line above/below
      nnoremap <silent><leader><Up>    :silent set paste<CR>m`O<Esc>``:silent set nopaste<CR>
      nnoremap <silent><leader><Down>  :silent set paste<CR>m`o<Esc>``:silent set nopaste<CR>

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

    " ...................................................... Reformat paragraghs

      " select all
      nnoremap <A-End>           ggVG
      " retain cursor position for insert mode reformatting
      inoremap <silent><F4> <Esc>lmZ{jv}kJvgq`Z:delmarks Z<CR>i
      " otherwise advance cursor to next paragraph
      nnoremap <F4>              {jv}kJvgq}}{j
      vnoremap <F4>              Jvgqj

    " .............................................................. Delete line

      " delete blank line above/below
      nnoremap <silent><C-Up>   m`:silent -g/\m^\s*$/d<CR>``:silent nohlsearch<CR>
      nnoremap <silent><C-Down> m`:silent +g/\m^\s*$/d<CR>``:silent nohlsearch<CR>

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

    " ....................................................... Shift left / right

      nnoremap <S-Left>  <<
      nnoremap <S-Right> >>
      inoremap <S-Left>  <C-d>
      inoremap <S-Right> <C-t>
      " preserve selection when indenting
      vnoremap <S-Right> >gv
      vnoremap <S-Left>  <gv

    " .......................................................... Shift up / down

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
      nmap <silent><S-Up>        :call <SID>MoveLineUp()<CR>
      nmap <silent><S-Down>      :call <SID>MoveLineDown()<CR>
      vmap <silent><S-Up>   <ESC>:call <SID>MoveVisualUp()<CR>
      vmap <silent><S-Down> <ESC>:call <SID>MoveVisualDown()<CR>

      " move visual block with automatic alignment!
      vnoremap L :m '<-2<CR>gv=gv
      vnoremap N :m '>+1<CR>gv=gv

  " Text manipulation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Convert tabs

      :command! -range=% -nargs=0 Tab2Space execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'
      :command! -range=% -nargs=0 Space2Tab execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'

      nmap <silent><leader><tab>         :silent retab<CR>
      nmap <silent><leader><leader><tab> :silent Space2Tab<CR>
      vmap <silent><leader><leader><tab> :silent Space2Tab<CR>
      nmap <silent><leader><tab><Space>  :silent Tab2Space<CR>
      vmap <silent><leader><tab><Space>  :silent Tab2Space<CR>

    " .................................................. Quote enclose selection

      " extend enclosing %V 1 char right to enclose last character of block
      vnoremap ' :s/\%V\(.*\%V.\)/'\1'/<CR>:noh<CR>`>l
      vnoremap " :s/\%V\(.*\%V.\)/"\1"/<CR>:noh<CR>`>l

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
      vmap     <silent><leader>` :call CodeBlock()<CR>

" edit.vim
