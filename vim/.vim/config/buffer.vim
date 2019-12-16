" sdothum - 2016 (c) wtfpl

" Buffers
" ══════════════════════════════════════════════════════════════════════════════

" File buffers _________________________________________________________________

augroup buffer | autocmd! | augroup END

" ..................................................................... Settings
set autoread     " reload files changed outside vim
" set autowrite  " automatically write a modified buffer on leaving
set hidden       " allow hidden background buffers

" ......................................................................... Help
autocmd buffer BufWinEnter *.txt,*.txt.gz if &filetype == 'help' | wincmd _ | endif

" list my function and leader key assignments
nmap <silent><S-F1>      :silent !term 'vmap' vmap<CR>
imap <silent><S-F1> <C-o>:silent !term 'vmap' vmap<CR>
vmap <silent><S-F1> :<C-u>silent !term 'vmap' vmap<CR>

" File actions _________________________________________________________________

" .......................................................... Buffer close / save
" save buffers
nmap <silent><leader>w  :silent write!<CR>
nmap <leader>W          :silent write !sudo tee % >/dev/null<CR>
nmap <silent><leader>ww :silent wqall!<CR>

" close buffers
nmap <silent><leader>d  :CloseUnique<CR>
nmap <silent><leader>DD :CloseDiff<CR>:%bdelete!<CR>
nmap <leader>D          :Singleton<CR>
" discard quit
nmap <silent><leader>qq :quitall!<CR>

" .................................................................. Auto backup
" auto backup
autocmd buffer BufWrite  * QueueFile
" save on losing focus, :wall on FocusLost does not trigger s:queueFile() (?)
autocmd buffer FocusLost * QueueBuffers

" Buffer handling ______________________________________________________________

" ............................................................... Initialization
" always switch to the current file directory, unless uri
autocmd buffer BufEnter    * if bufname('') !~ '^[A-Za-z0-9]*://' | lcd %:p:h | echo | endif
" return to last edit position when opening files (You want this!)
autocmd buffer BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') | execute 'normal! g`"' | endif

" ................................................................... Modifiable
" toggle modifiable attribute
nmap <silent><leader>- :let &modifiable = !&modifiable<CR>

" protected help
autocmd buffer BufWinEnter *.txt,*.txt.gz if &filetype == 'help' | set nomodifiable | endif
" mode check for fzf terminal window
autocmd buffer BufWinEnter * if !Protected() | set modifiable | endif

" ................................................................ Switch buffer
nmap <leader>B   :echo '[' . bufnr('%') . '] ' . expand('%:p')<CR>

" beakl si layout specific buffer navigation key assignments, note silent -> silent
if !empty($BEAKL)  " see config.fish
  " don't wait for statusline refresh to set split colors, see ui.vim s:showInfo()
  nmap <silent><Delete> :CloseDiff<CR>:silent bprevious<CR>:SplitColors<CR>
  nmap <silent><Enter>  :Enter<CR>
else
  nmap <silent>-        :CloseDiff<CR>:silent bprevious<CR>:SplitColors<CR>
  nmap <silent>+        :CloseDiff<CR>:silent bnext<CR>:SplitColors<CR>
endif
" switch to previously edited/viewed buffer
nmap <silent><BS>       :CloseDiff<CR>:silent edit #<CR>:SplitColors<CR>

" buffer.vim
