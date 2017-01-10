" sdothum - 2016 (c) wtfpl

" Keyboard
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " Note: scripts are affected by the mappings below!
      " e.g. "h" becomes "m", "f" becomes "t" etc.
      " see thedarnedestthing.com

      " hjkl mapping (0) hjkl (1) mnle
      let s:mapping = 0
      let g:mnle = 0

      function! Colemak()
        if s:mapping == 0
          " map home row (cluster) cursor movement
          nnoremap k gk
          vnoremap k gk
          nnoremap j gj
          vnoremap j gj
        else
          let g:mnle = 1
          " map home row (cluster) cursor movement
          nnoremap u gk
          vnoremap u gk
          nnoremap n h
          vnoremap n h
          nnoremap e gj
          vnoremap e gj
          nnoremap i l
          vnoremap i l

          " recover vi keys (including caps for consistency)
          nnoremap f e
          vnoremap f e
          nnoremap F E
          vnoremap F E
          nnoremap h m
          vnoremap h m
          nnoremap k n
          vnoremap k n
          nnoremap K N
          vnoremap K N

          " combine find and till commands
          nnoremap t f
          vnoremap t f
          nnoremap T F
          vnoremap T F
          nnoremap <A-t> t
          vnoremap <A-t> t
          nnoremap <C-t> T
          vnoremap <C-t> T
        endif
      endfunction

      " environment variable
      if $COLEMAK_DH == 'true'
        call Colemak()
      endif

" keyboard.vim
