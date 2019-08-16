" sdothum - 2016 (c) wtfpl

" Filetypes
" ══════════════════════════════════════════════════════════════════════════════

    " ................................................................. Markdown
  
      set spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72

      " set touch date
      command! -nargs=1 Wiki execute ':silent !wikitouch "' . expand('%:p') . '" ' . <f-args>

" markdown.vim
