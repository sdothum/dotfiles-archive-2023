" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

" Fonts ________________________________________________________________________

augroup ui | autocmd! | augroup END

" ................................................................. Code / Prose
" Iosevka custom compiled, with nerd-fonts awesome patches, see make_install/iosevka
let s:mono = g:mono ? '-mono' : ''                           " font name extension
let g:font = ['Iosevka' . s:mono, 'Iosevka-proof' . s:mono]  " family [code, prose]

" Display ______________________________________________________________________

" ................................................................... Initialize
" intial view mode: source code or prose, plugin windows inherit current theme (avoids thrashing)
autocmd ui VimEnter,BufWinEnter * Layout

" ....................................................................... Redraw
nmap <silent><F11>      :Refresh<CR>
imap <silent><F11> <C-o>:Refresh<CR>

" ..................................................................... Messages
" clear messages after awhile to keep screen clean and distraction free!
autocmd ui CursorHold * echo

" Highlighting _________________________________________________________________

" .......................................................... Syntax highlighting
set omnifunc=syntaxcomplete#Complete
syntax on  " turn on syntax highlighting
 
" ftplugin set syntax is overridden by vim runtime Syntax autocmd
autocmd ui Syntax <buffer> execute 'set syntax=' . &filetype
" refresh highlighting on arm
" autocmd ui CursorHold * if !Prose() && !&diff && !empty(&filetype) | execute 'set filetype=' . &filetype | endif

" UI ___________________________________________________________________________

" ............................................................ Toggle statusline
" toggle statusline details
nmap <silent><F7>        :ToggleInfo<CR>
imap <silent><F7>   <C-o>:ToggleInfo Prose()<CR>

" show info+sleep in balanced diff windows
autocmd ui VimEnter * if &diff | ToggleInfo | WaitFor | execute "normal! \<C-w>=" | endif

" .................................................................... View mode
nmap <silent><S-F11>      :ToggleProof<CR>
imap <silent><S-F11> <C-o>:ToggleProof<CR>

if has('gui_running')
  autocmd ui InsertEnter * ToggleProof | SignifyDisable
  autocmd ui InsertLeave * ToggleProof | SignifyEnable
endif

" ............................................................. Switch font size
" prose font is by (writing preference) default set 1px larger than code font
nmap <silent><S-F9>      :Font !g:fonttype<CR>
imap <silent><S-F9> <C-o>:Font !g:fonttype<CR>

" ui.vim
