" sdothum - 2016 (c) wtfpl

" ftdetect
" ══════════════════════════════════════════════════════════════════════════════

" ....................................................................... Binary
" apply default rules to linked (inode) files
autocmd BufRead * if system('file -i ' . expand('%') . '| cut -d: -f2 | grep -v inode') =~ 'binary' | set filetype=binary | endif

" binary.vim
