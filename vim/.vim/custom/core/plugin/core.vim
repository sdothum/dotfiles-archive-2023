" sdothum - 2016 (c) wtfpl

" Core
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Primitive ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_core")
        finish
      endif
      let g:loaded_core = 1
      let s:save_cpo    = &cpo
      set cpo&vim

      augroup core
        autocmd!
      augroup END

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Config file

      " when updates won't break the current vim session!
      command! Vimrc call core#Vimrc()

      " handy searchable lists
      command! Hi  enew | put=execute('hi')  | normal gg
      command! Map enew | put=execute('map') | normal gg

    " ....................................................... Error message trap

      command! -nargs=1 Quietly call core#Quietly(<f-args>)

    " ............................................................ Open terminal

      " !term fails on shell error 1 (?)
      command! Term :call system('term "vimterm" STACK')

      nmap <silent><C-t>      :Term<CR>
      nmap <silent><C-t><C-t> :term fish<CR>
      imap <silent><C-t>      <C-o>:Term<CR>
      imap <silent><C-t><C-t> <C-o>:term fish<CR>

    " ............................................................... Dictionary

      " lookup word under cursor
      " nnoremap <silent><C-k> :silent call system('term "vimterm" STACK SHELL "dict <C-r><C-w> \| less"')<CR>
      " inoremap <silent><C-k> <C-o>:silent call system('term "vimterm" STACK SHELL "dict <C-r><C-w> \| less"')<CR>

    " ............................................................... Print file

      command! Hardcopy silent call core#Hardcopy()

    " .................................................................... Debug

      command! -nargs=1 Trace call core#Trace(<f-args>)

      nnoremap <silent><F10> :let g:trace = g:trace == 0 ? 1 : 0<CR>

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " environment variable
      if $COLEMAK > ''
        call core#Colemak()
      endif

  " Text ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Strip whitespace
    
      nmap <silent><F4>   :call core#StripTrailingWhitespaces()<CR>
      vmap <silent><F4>   :<C-u>call core#StripTrailingWhitespaces()<CR>

      " pre-write formatting
      " autocmd buffer BufWritePre * call core#StripTrailingWhitespaces()
      " " focus oriented formatting
      " autocmd buffer BufLeave    * call core#StripTrailingWhitespaces()
      " autocmd buffer FocusLost   * call core#StripTrailingWhitespaces()

    " .......................................................... Code block text

      " markup wiki code blocks
      nnoremap <silent><leader>` V:call core#CodeBlock()<CR>
      vmap     <silent><leader>` :call core#CodeBlock()<CR>

    " .......................................................... CSS block align

      nnoremap <silent><S-F4>    :call core#CssBlockAlign()<CR>

  " Filetype ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Modifiable

      " check filetype on open
      " autocmd core BufNewFile,BufRead * call core#CheckFiletype()

    " ................................................................... E-mail

      " position cursor for email reply or new message, see .sup/config.yaml and bin/dcompose
      autocmd core Filetype mail call core#ComposeMail()

    " ..................................................................... Wiki
  
     " set touch date
     command! -nargs=1 Wiki execute ':silent !wikitouch "' . expand('%:p') . '" ' . <f-args>

      let &cpo = s:save_cpo
      unlet s:save_cpo

" core.vim
