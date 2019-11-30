" sdothum - 2016 (c) wtfpl

" Theme
" ══════════════════════════════════════════════════════════════════════════════

" The look _____________________________________________________________________

augroup theme | autocmd! | augroup END

" .................................................................. Colorscheme
colorscheme duochrome
if has('gui_running') && empty(glob('~/.session/vim:dark')) | set background=light
else                                                        | set background=dark | endif

autocmd theme InsertEnter * Background
autocmd theme InsertLeave * Background

" ................................................................ Switch colour
nmap <silent><S-F8>      :LiteSwitch<CR>
imap <silent><S-F8> <C-o>:LiteSwitch<CR>

" theme.vim
