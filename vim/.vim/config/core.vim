" sdothum - 2016 (c) wtfpl

" Core
" ══════════════════════════════════════════════════════════════════════════════

  " System _____________________________________________________________________

    " ............................................................ Open terminal

      nmap <silent><C-t>      :Term<CR>
      imap <silent><C-t>      <C-o>:Term<CR>

      nmap <silent><C-t><C-t> :term fish<CR>
      imap <silent><C-t><C-t> <C-o>:term fish<CR>

    " ............................................................... Dictionary

      " lookup word under cursor
      " nnoremap <silent><C-k> :silent call system('term "vimterm" STACK SHELL "dict <C-r><C-w> \| less"')<CR>
      " inoremap <silent><C-k> <C-o>:silent call system('term "vimterm" STACK SHELL "dict <C-r><C-w> \| less"')<CR>

    " .................................................................... Debug

      nnoremap <silent><S-F10> :let g:trace = g:trace == 0 ? 1 : 0<CR>

" core.vim
