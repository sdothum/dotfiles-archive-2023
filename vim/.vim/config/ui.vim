" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

" The view _____________________________________________________________________

" ........................................................................ Setup
let g:detail    = 0   " default expanded detail (0) tag (1) atom, see F7 map
let g:active    = 0   " active window tag

" Iosevka custom compiled, with nerd-fonts awesome patches, see make_install/iosevka
let g:font      = ['Iosevka' . $MONO . '\', 'Iosevka-proof' . $MONO . '\']  " family [source, prose]
let g:font_type = -1                                                        " current font setting (0) source (1) prose
let g:font_step = empty(glob('~/.session/font++')) ? 1 : 2                  " increase (point size) for prose

augroup ui | autocmd! | augroup END

" Display ______________________________________________________________________

" ....................................................................... Redraw
nmap <silent><F9>      :Refresh<CR>
imap <silent><F9> <C-o>:Refresh<CR>

" wm timing requires FocusGained+sleep with VimResized to consistently set margins
autocmd ui VimEnter,VimResized,FocusGained * sleep 10m | Background

" ................................................................... Initialize
" intial view mode: source code or prose, plugin windows inherit current theme (avoids thrashing)
autocmd ui BufWinEnter * Layout

" ..................................................................... Messages
" recover last error message
nmap <leader>e :echo errmsg<CR>

" clear messages after awhile to keep screen clean and distraction free!
autocmd ui CursorHold * echo

" Highlighting _________________________________________________________________

" .......................................................... Syntax highlighting
set omnifunc=syntaxcomplete#Complete
syntax on  " turn on syntax highlighting
 
" refresh highlighting on arm
" autocmd ui CursorHold * if !Prose() && !&diff && !empty(&filetype) | execute 'set filetype=' . &filetype | endif

" .......................................................... White space markers
set nolist  " display tabs and trailing spaces visually
set listchars="tab:▸\<Space>"

" set listchars+=trail:_
set listchars+=trail:·
set listchars+=nbsp:.
set listchars+=extends:>
set listchars+=precedes:<
" set listchars+=eol:¬

" ......................................................... Trailing white space
nmap <silent><leader><Space> :ToggleWhiteSpace<CR>

" UI ___________________________________________________________________________

" ............................................................ Toggle statusline
" toggle statusline details
nmap <silent><F7>        :ToggleInfo<CR>
imap <silent><F7>   <C-o>:ToggleInfo Prose()<CR>

" show info in balanced diff windows
autocmd ui VimEnter * if &diff | execute "normal! \<C-w>=" | ToggleInfo | endif

" toggle tag, line details
nmap <silent><S-F7>      :let g:detail = !g:detail<CR>
imap <silent><S-F7> <C-o>:let g:detail = !g:detail<CR>

" for active window highlighting
autocmd ui WinEnter,TerminalOpen,BufWinEnter,VimEnter * let g:active = g:active + 1 | let w:tagged = g:active
autocmd ui WinEnter,TerminalOpen                      * SplitColors

" .................................................................... View mode
nmap <silent><C-F7>      :ToggleProof<CR>
imap <silent><C-F7> <C-o>:ToggleProof<CR>

if has('gui_running')
  autocmd ui InsertEnter * ToggleProof | SignifyDisable
  autocmd ui InsertLeave * ToggleProof | SignifyEnable
endif

" ............................................................. Switch font size
nmap <silent><S-F9>      :Font !g:font_type<CR>
imap <silent><S-F9> <C-o>:Font !g:font_type<CR>

" ui.vim
