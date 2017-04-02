" sdothum - 2016 (c) wtfpl

" Keyboard
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Keyboard (re)mappings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let mapleader   = "\<Space>"          " remap <leader> a la spacemacs
      let g:mapleader = "\<Space>"

      " unshift command mode
      nnoremap ; :
      vnoremap ; :

      augroup kbd
        autocmd!
      augroup END

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " Note: scripts are affected by the mappings below!
      "       e.g. "h" becomes "m", "f" becomes "t" etc.
      "       see thedarnedestthing.com

      " hjkl mapping (0) hjkl (1) mnle
      let s:mapping = 0
      let g:mnle    = 0

      " up/down by screen lines, not file lines
      nnoremap j          gj
      nnoremap k          gk

      function! Colemak()
        if s:mapping == 0
          " map home row (cluster) cursor movement
          nnoremap k      gk
          vnoremap k      gk
          nnoremap j      gj
          vnoremap j      gj
        else
          let g:mnle = 1
          " map home row (cluster) cursor movement
          nnoremap u      gk
          vnoremap u      gk
          nnoremap n      h
          vnoremap n      h
          nnoremap e      gj
          vnoremap e      gj
          nnoremap i      l
          vnoremap i      l

          " recover vi keys (including caps for consistency)
          nnoremap f      e
          vnoremap f      e
          nnoremap F      E
          vnoremap F      E
          nnoremap h      m
          vnoremap h      m
          nnoremap k      n
          vnoremap k      n
          nnoremap K      N
          vnoremap K      N

          " combine find and till commands
          nnoremap t      f
          vnoremap t      f
          nnoremap T      F
          vnoremap T      F
          nnoremap <A-t>  t
          vnoremap <A-t>  t
          nnoremap <C-t>  T
          vnoremap <C-t>  T
        endif
      endfunction

      " environment variable
      if $COLEMAK_DH == 'true'
        call Colemak()
      endif

  " Cursor ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ....................................................... Backspace settings

      set backspace=indent,eol,start        " allow backspace in insert mode
      set whichwrap=b,s,h,l,<,>,[,]         " backspace and cursor keys wrap

    " ......................................................... Cursor movements

      " up/down by paragraph sentence
      nmap <C-S-Left>     {{)
      nmap <C-S-Right>    })

      " insert mode local region cursor movements
      " <C-h> is overridden by auto-pairs delete <BS> when enabled
      " imap <C-h>        <Left>
      " imap <C-j>        <Down>
      " imap <C-k>        <Up>
      " imap <C-l>        <Right>

    " ............................................................. Disable keys

      " affirm vim modal usage but these keys are remapped below anyway :-)
      " (re-enabled for colemak keyboard as qwerty key cluster no longer valid)
      " imap <down>       <nop>
      " imap <left>       <nop>
      " imap <right>      <nop>
      " imap <up>         <nop>
      " nmap <down>       <nop>
      " nmap <left>       <nop>
      " nmap <right>      <nop>
      " nmap <up>         <nop>

  " Keyboard shortcuts ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Copy / paste

      " prevent cascading paste insert
      set pastetoggle=<F3>
      autocmd kbd InsertLeave * set nopaste     " disable paste mode when leaving Insert Mode

      " yank from the cursor to the end of the line, to be consistent with C and D.
      " see yankring for plugin equivalent
      nnoremap Y          y$
      " reselect/reyank text just pasted
      nnoremap <leader>v  gv
      nnoremap <leader>V  gvy
      map      <leader>p  pgvy

      " highlight last inserted text
      nnoremap <leader>i  `[v`]

    " ...................................................... Sentence operations

      " use "as" suffix for outer sentence
      " change sentence
      nnoremap <leader>cc cis
      " cut sentence
      nnoremap <leader>dd dis
      " yank sentence
      nnoremap <leader>yy yis

    " .................................................... Clipboard cut / paste

      " visual mode yank/cut clipboard actions
      " "+Y yank to clipboard
      vnoremap <C-F2>     "+y
      vnoremap <S-F2>     "+Y
      " "+D cut to clipboard
      vnoremap <C-F2>     "+d
      vnoremap <S-F2>     "+D

      " " normal/insert mode paste actions
      " " "+P pads space after insert
      " " note: to enter visual block mode type v<C-v>
      imap      <F2> <ESC>"+pli
      nmap      <F2>      h"+pl
      " command mode insertion (paste) of current yank buffer
      cmap      <F2> <C-r>"

" keyboard.vim
