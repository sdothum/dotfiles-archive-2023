" sdothum - 2016 (c) wtfpl

" Shift
" ══════════════════════════════════════════════════════════════════════════════

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

" shift.vim


