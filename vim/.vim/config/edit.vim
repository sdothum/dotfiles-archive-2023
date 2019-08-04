" sdothum - 2016 (c) wtfpl

" Edit
" ══════════════════════════════════════════════════════════════════════════════

  " Line _______________________________________________________________________

    " .............................................................. Insert line

      " insert line while disabling auto-commenting OR break (prose) line
      function! s:smartWrap()
        if core#Prose()  " override Pencil mode (the default state for prose)
          set paste
          execute "normal! i\<CR>"
          set nopaste
          execute 'startinsert'
        else  " append EOL wrap from any col position
          let l:formatoptions = &formatoptions  " disable auto commenting
          set formatoptions-=c
          set formatoptions-=r
          set formatoptions-=o
          normal! ^
          let l:pos = col('.')
          normal! o
          " align line indentation
          execute 'normal! a' . repeat(' ', l:pos)
          let &formatoptions = l:formatoptions
        endif
      endfunction

      inoremap <silent><C-CR> <C-o>:call <SID>smartWrap()<CR>

      " insert blank line above/below
      nnoremap <silent><leader><Up>   :silent set paste<CR>m`O<Esc>``:silent set nopaste<CR>
      nnoremap <silent><leader><Down> :silent set paste<CR>m`o<Esc>``:silent set nopaste<CR>

      " duplicate line
      " nnoremap <C-CR> :t.<CR>
      " inoremap <C-CR> <C-o>:t.<CR>

    " .............................................................. Delete line

      " delete blank line above/below
      nnoremap <silent><C-S-Up>   m`:silent -g/\m^\s*$/d<CR>``:silent nohlsearch<CR>
      nnoremap <silent><C-S-Down> m`:silent +g/\m^\s*$/d<CR>``:silent nohlsearch<CR>

    " ......................................................... Strip whitespace
    
      nmap <silent><F4> :call core#StripTrailingWhitespaces()<CR>
      vmap <silent><F4> :<C-u>call core#StripTrailingWhitespaces()<CR>

      augroup edit | autocmd! | augroup END

      " " pre-write formatting
      " autocmd edit BufWritePre * call core#StripTrailingWhitespaces()
      " " focus oriented formatting
      " autocmd edit BufLeave    * call core#StripTrailingWhitespaces()
      " autocmd edit FocusLost   * call core#StripTrailingWhitespaces()

  " Text manipulation __________________________________________________________

    " ...................................................... Reformat paragraghs

      " retain cursor position for insert mode reformatting
      inoremap <silent><C-r> <Esc>lmZ{jv}kJvgq`Z:delmarks Z<CR>i
      inoremap <silent><F5>  <Esc>lmZ{jv}kJvgq`Z:delmarks Z<CR>i
      vnoremap <C-r>         lmZJvgq`Z:delmarks Z<CR>i
      " reformat at cursor position
      nnoremap <C-r>         lmZ{jv}kJvgq`Z:delmarks Z<CR>
      " otherwise advance cursor to next paragraph
      nnoremap <F5>          {jv}kJvgq}}{j
      vnoremap <F5>          Jvgqj

    " ............................................................. Convert tabs

      :command! -range=% -nargs=0 Tab2Space execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'
      :command! -range=% -nargs=0 Space2Tab execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'

      nmap <silent><leader><tab>        :retab<CR>
      nmap <silent><leader><Space><tab> :Space2Tab<CR>
      vmap <silent><leader><Space><tab> :Space2Tab<CR>
      nmap <silent><leader><tab><Space> :Tab2Space<CR>
      vmap <silent><leader><tab><Space> :Tab2Space<CR>

    " .................................................. Quote enclose selection

      " extend enclosing %V 1 char right to enclose last character of block
      vnoremap ' :s/\%V\(.*\%V.\)/'\1'/<CR>:noh<CR>`>l
      vnoremap " :s/\%V\(.*\%V.\)/"\1"/<CR>:noh<CR>`>l

    " .......................................................... Code block text

      " markup wiki code blocks
      nnoremap <silent><leader>` V:call core#CodeBlock()<CR>
      vmap     <silent><leader>` :call core#CodeBlock()<CR>

" edit.vim
