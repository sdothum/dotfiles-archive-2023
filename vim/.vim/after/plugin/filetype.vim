" sdothum - 2016 (c) wtfpl

" Filetypes
" ══════════════════════════════════════════════════════════════════════════════

    " ..................................................................... Wiki
  
      " set touch date
      command! -nargs=1 Wiki execute ':silent !wikitouch "' . expand('%:p') . '" ' . <f-args>

" filetype.vim
