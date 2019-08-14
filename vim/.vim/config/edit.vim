" sdothum - 2016 (c) wtfpl

" Edit
" ══════════════════════════════════════════════════════════════════════════════

  " Line _______________________________________________________________________

    " .............................................................. Insert line

      inoremap <silent><C-CR> <C-o>:SmartWrap<CR>

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
    
      nmap <silent><F4> :StripTrailingWhitespaces<CR>
      vmap <silent><F4> :<C-u>StripTrailingWhitespaces<CR>

      " augroup edit | autocmd! | augroup END

      " " pre-write formatting
      " autocmd edit BufWritePre * StripTrailingWhitespaces
      " " focus oriented formatting
      " autocmd edit BufLeave    * StripTrailingWhitespaces
      " autocmd edit FocusLost   * StripTrailingWhitespaces

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
      nnoremap <silent><leader>` V:CodeBlock<CR>
      vmap     <silent><leader>` :CodeBlock<CR>

" edit.vim
