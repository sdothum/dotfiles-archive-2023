" sdothum - 2016 (c) wtfpl

" Theme
" ══════════════════════════════════════════════════════════════════════════════

" The look _____________________________________________________________________

" .................................................................. Colorscheme
if has('gui_running')
  if &diff           " diff highlights the SignColumn which can only be cleared afterwards..
    colorscheme one  " diff mode doesn't work well with reverse (block) highlighting
    autocmd theme CursorHold * hi! link SignColumn NonText
  else 
    colorscheme duochrome
    if empty(glob('~/.session/vim:dark'))
      set background=light
    else
      set background=dark
    endif
  endif
else
  colorscheme duochrome
  set background=dark
endif

augroup theme | autocmd! | augroup END

autocmd theme FocusGained,BufEnter * Background
autocmd theme InsertEnter          * Background
autocmd theme InsertLeave          * Background

" ................................................................ Switch colour
nmap <silent><S-F8>      :LiteSwitch<CR>
imap <silent><S-F8> <C-o>:LiteSwitch<CR>

" theme.vim
