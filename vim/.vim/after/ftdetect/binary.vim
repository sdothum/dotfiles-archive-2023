" sdothum - 2016 (c) wtfpl

" ftdetect
" ══════════════════════════════════════════════════════════════════════════════

    " ................................................................... Binary
  
      autocmd BufRead * if system('file -i ' . expand('%') . '|cut -d: -f2') =~ 'binary' | set filetype=binary | endif

" binary.vim
