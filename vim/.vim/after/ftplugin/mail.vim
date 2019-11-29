" sdothum - 2016 (c) wtfpl

" ftplugin
" ══════════════════════════════════════════════════════════════════════════════

" ......................................................................... Mail
setlocal spell
setlocal wrap
setlocal enc=utf-8
setlocal formatoptions=tqwan1
setlocal textwidth=72
setlocal filetype=mail
setlocal syntax=mail

" ...................................................................... Compose
" email has blank lines inserted externally (via sed) for replys, see dmenu compose
function! s:composeMail()
  setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72 syntax=mail
  execute 'normal! gg'
  execute 'normal! ' . (search('^\(\(Subject\|From\|To\|Cc\):.*\n\(Subject\|From\|To\|Cc\):.*\n\(Subject\|From\|To\|Cc\):.*\n\)') + 4) . 'G'
  execute 'startinsert'
endfunction

autocmd Filetype mail silent! call <SID>composeMail()

" mail.vim
