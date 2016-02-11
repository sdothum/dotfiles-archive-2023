" sdothum - 2016 (c) wtfpl

" Colemak-shift-dh layout, see thedarnedestthing.com
" ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

" Note: scripts are affected by the mappings below!
" e.g. "h" becomes "m", "f" becomes "t" etc.
" see functions.vim

" environment variable
if $COLEMAK_DH == 'true'
  " map home row (cluster) cursor movement
  nnoremap l gk
  vnoremap l gk
  nnoremap m h
  vnoremap m h
  nnoremap n gj
  vnoremap n gj
  nnoremap e l
  vnoremap e l

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

" colemak.vim
