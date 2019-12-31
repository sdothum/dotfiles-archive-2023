" sdothum - 2016 (c) wtfpl

" StatusLine
" ══════════════════════════════════════════════════════════════════════════════

" Format _______________________________________________________________________

augroup statusline | autocmd! | augroup END

" ..................................................................... Settings
set laststatus=2                 " always show status line
set ruler                        " show cursor position in status line

let g:pad = ['      ', '     ']  " statusline padding [inner, outer]
"             123456    12345

" ....................................................................... Glyphs
" buffer g:icon [0] unmodified [1] unmodifiable [2] modified [3] inactive [4] insert mode
if empty($DISPLAY) | let g:icon = ['•', '-', '+', 'x', '^']  " console font
elseif g:mono      | let g:icon = ['', '', '', '', '']  " nerd-font utf-8 mono symbols
else               | let g:icon = ['', '', '', '', '']  " nerd-font utf-8 double width symbols
endif

" ............................................................. Expanded details
let g:detail = 0  " default expanded detail (0) tag (1) atom, see F7 map

" toggle tag / line details
nmap <silent><S-F7>      :let g:detail = !g:detail<CR>
imap <silent><S-F7> <C-o>:let g:detail = !g:detail<CR>

" .............................................................. Column position
let g:show_column = 0  " statusline current column

" trigger autocmd to flash column position (does not work for BOF)
nnoremap <silent><C-c> hl

autocmd statusline CursorHold  * let g:show_column = 0
autocmd statusline CursorMoved * let g:show_column = 1

" ................................................................. Syntax group
nnoremap <silent><F10> :Atom<CR>

" statusline.vim
