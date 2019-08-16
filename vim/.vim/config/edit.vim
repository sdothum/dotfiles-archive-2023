" sdothum - 2016 (c) wtfpl

" Edit
" ══════════════════════════════════════════════════════════════════════════════

  " Command ____________________________________________________________________

    " .......................................................... Command history

      nmap <F1> q:
      imap <F1> <C-o>q:
      vmap <F1> <C-u>q:

      " easier redo
      nnoremap U <C-r>

  " Line _______________________________________________________________________

    " .............................................................. Insert line

      inoremap <silent><C-CR> <C-o>:SmartWrap<CR>

      " insert blank line above/below
      nnoremap <silent><leader><Up>   :silent set paste<CR>m`O<Esc>``:silent set nopaste<CR>
      nnoremap <silent><leader><Down> :silent set paste<CR>m`o<Esc>``:silent set nopaste<CR>

      " " duplicate line
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

  " Text shift _________________________________________________________________

    " .............................................................. Select text

      " select all
      nnoremap <C-a>    ggVG
      " extend paragraph selection
      vmap <C-PageUp>   {
      vmap <C-PageDown> }

      " select paragragh
      nmap <silent><C-PageUp>   :ParagraphAbove<CR>
      nmap <silent><C-PageDown> :ParagraphBelow<CR>

    " ......................................................... Shift left right

      " softwidth shifts, preserve selection when indenting
      nnoremap <S-Left>  <<
      inoremap <S-Left>  <C-d>
      vnoremap <S-Left>  <gv
      nnoremap <S-Right> >>
      inoremap <S-Right> <C-t>
      vnoremap <S-Right> >gv

      " byte shift left / right
      nnoremap <leader><Left>  :s/^ //<CR>:silent nohlsearch<CR>
      nnoremap <leader><Right> :s/^/ /<CR>:silent nohlsearch<CR>
      vnoremap <leader><Left>  :s/^ //<CR>:silent nohlsearch<CR>gv
      vnoremap <leader><Right> :s/^/ /<CR>:silent nohlsearch<CR>gv

      " move visual block with automatic alignment!
      vnoremap L :m '<-2<CR>gv=gv
      vnoremap N :m '>+1<CR>gv=gv

    " ............................................................ Shift up down

      nmap <silent><S-Up>   :MoveLineUp<CR>
      imap <silent><S-Up>   <ESC>:MoveLineUp<CR>a
      vmap <silent><S-Up>   <ESC>:MoveVisualUp<CR>

      nmap <silent><S-Down> :MoveLineDown<CR>
      imap <silent><S-Down> <ESC>:MoveLineDown<CR>a
      vmap <silent><S-Down> <ESC>:MoveVisualDown<CR>

" edit.vim
