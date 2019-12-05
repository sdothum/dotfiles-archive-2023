" sdothum - 2016 (c) wtfpl

" Theme
" ══════════════════════════════════════════════════════════════════════════════

" The look _____________________________________________________________________

augroup theme | autocmd! | augroup END

" .................................................................. Colorscheme
colorscheme duochrome
if has('gui_running') && empty(glob('~/.session/vim:dark')) | set background=light
else                                                        | set background=dark
endif

autocmd theme InsertEnter * Background
autocmd theme InsertLeave * Background

" wm timing requires FocusGained+sleep with VimResized to consistently set margins, see Background
autocmd theme VimEnter,VimResized,FocusGained * WaitFor | Background

" ................................................................ Switch colour
nmap <silent><S-F8>      :LiteSwitch<CR>
imap <silent><S-F8> <C-o>:LiteSwitch<CR>

" ................................................................ Split windows
let g:active = 0   " active window tag

" for active window highlighting
autocmd theme WinEnter,TerminalOpen,BufWinEnter,VimEnter * let g:active = g:active + 1 | let w:tagged = g:active
autocmd theme WinEnter,TerminalOpen                      * SplitColors

" theme.vim
