" sdothum - 2016 (c) wtfpl

" Theme
" ══════════════════════════════════════════════════════════════════════════════

" The look _____________________________________________________________________

" .................................................................. Colorscheme
colorscheme duochrome
if has('gui_running')
  if empty(glob('~/.session/vim:dark'))
    set background=light
  else
    set background=dark
  endif
else
  set background=dark
endif

augroup theme | autocmd! | augroup END

autocmd theme InsertEnter * Background
autocmd theme InsertLeave * Background

" ................................................................ Switch colour
nmap <silent><S-F8>      :LiteSwitch<CR>
imap <silent><S-F8> <C-o>:LiteSwitch<CR>

" theme.vim
