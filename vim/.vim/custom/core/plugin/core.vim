" sdothum - 2016 (c) wtfpl

" Core
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Primitive ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_core")
        finish
      endif
      let g:loaded_core = 1
      let s:save_cpo = &cpo
      set cpo&vim

      let g:ruler = 0                       " colorcolumn mode
      let g:repo  = $HOME . '/stow/'

      augroup core
        autocmd!
      augroup END

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Open terminal

      " open shell session in buffer directory
      command! Term silent call system('term "vimterm" STACK')

      nnoremap <silent><C-t> :Term<CR>
      inoremap <silent><C-t> <C-o>:Term<CR>

    " ............................................................... Dictionary

      " lookup word under cursor
      nnoremap <silent><C-k> :silent call system('term "vimterm" STACK SHELL "dict <C-r><C-w> \| less"')<CR>
      inoremap <silent><C-k> <C-o>:silent call system('term "vimterm" STACK SHELL "dict <C-r><C-w> \| less"')<CR>

    " ............................................................... Print file

      command! Hardcopy silent call core#Hardcopy()<CR>:echo 'Printing..'

    " .................................................................... Debug

      nnoremap <silent><S-F12>     :let g:trace = g:trace == 0 ? 1 : 0<CR>

    " .............................................................. Auto backup

      autocmd core VimEnter * let g:queue = system('date "+%y%m%d%H%M%S"')
      autocmd core VimLeave * call system('dash -c "vhg ' . g:queue . ' &"')

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " environment variable
      if $COLEMAK > ''
        call core#Colemak()
      endif

  " GUI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................... Gvim menu and toolbar

      if $DISPLAY > ''
        autocmd core VimEnter * call core#RedrawGui()
      endif

      nnoremap <silent><F12>       :call core#ToggleGui()<CR>
      inoremap <silent><F12>       <C-o>:call core#ToggleGui()<CR>
      vnoremap <silent><F12>       <C-o>:call core#ToggleGui()<CR>

    " .................................................. Column and line numbers

      nmap <silent><Bar>           :call core#ToggleColumn()<CR>
      nmap <silent><leader><Bar>   :IndentGuidesToggle<CR>
      nmap <silent>#               :call core#ToggleNumber()<CR>

    " ...................................................... White space markers

      nmap <silent><leader><Space> :call core#ToggleSpaces()<CR>

  " Buffer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Line wrap

      nmap <silent><leader><CR> :call core#ToggleWrap()<CR>

    " ..................................................... Save cursor position

      " only works for simple :buffer actions (not plugin pane selection)
      autocmd core BufWinLeave * let b:winview = winsaveview()
      autocmd core BufWinEnter * if exists('b:winview') | call winrestview(b:winview) | endif

  " Edit ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Shift up / down

      " shift text up / down
      imap <silent><S-Up>         <ESC>:call core#MoveLineUp()<CR>a
      imap <silent><S-Down>       <ESC>:call core#MoveLineDown()<CR>a
      nmap <silent><S-Up>         :call core#MoveLineUp()<CR>
      nmap <silent><S-Down>       :call core#MoveLineDown()<CR>
      vmap <silent><S-Up>         <ESC>:call core#MoveVisualUp()<CR>
      vmap <silent><S-Down>       <ESC>:call core#MoveVisualDown()<CR>

    " ......................................................... Insert line wrap

      inoremap <silent><C-Return> <C-o>:call core#InsertWrap()<CR>

  " Text ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Select text

      " select paragragh
      nmap <silent><A-PageUp>     :call core#ParagraphAbove()<CR>
      nmap <silent><A-PageDown>   :call core#ParagraphBelow()<CR>

    " .......................................................... Code block text

      " markup wiki code blocks
      nnoremap <silent><leader>`  V:call core#CodeBlock()<CR>
      vmap     <silent><leader>`  :call core#CodeBlock()<CR>

    " .......................................................... CSS block align

      nnoremap <silent><F5>       :call core#CssBlockAlign()<CR>

  " Filetype ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Modifiable

      " check filetype on open
      autocmd core BufNewFile,BufRead * call core#CheckFiletype()

    " ................................................................... E-mail

      " position cursor for email reply or new message, see .sup/config.yaml and bin/dcompose
      autocmd core Filetype mail call core#ComposeMail()

      let &cpo = s:save_cpo
      unlet s:save_cpo

" core.vim
