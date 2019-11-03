" sdothum - 2016 (c) wtfpl

" ftplugin
" ══════════════════════════════════════════════════════════════════════════════

" ..................................................................... Markdown
setlocal spell
setlocal wrap
setlocal enc=utf-8
setlocal formatoptions=tqwan1
setlocal textwidth=72

" set touch date
command! -nargs=1 Wiki execute ':silent !wikitouch "' . expand('%:p') . '" ' . <f-args>

" markdown.vim
