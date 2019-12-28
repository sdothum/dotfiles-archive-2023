" sdothum - 2016 (c) wtfpl

" GUI
" ══════════════════════════════════════════════════════════════════════════════

" Display ______________________________________________________________________

augroup gui | autocmd! | augroup END

" .................................................................... Scrolling
set sidescroll=1  " smooth scrolling by 1 column
set sidescrolloff=1

" horizontal scrolling
noremap <C-S-Left>  zL
noremap <C-S-Right> zH

" ......................................................... Save cursor position
" only works for simple :buffer actions (not plugin pane selection)
autocmd gui BufWinLeave * let b:winview = winsaveview()
autocmd gui BufWinEnter * if exists('b:winview') | call winrestview(b:winview) | endif

" Terminal _____________________________________________________________________

" ......................................................................... Font
scriptencoding utf-8
set encoding=utf-8      " necessary to show unicode glyphs
set ambiwidth="double"  " for double width glyph handling
set guifont=monospace

" ....................................................................... Cursor
set cursorline          " highlight current line
" set cursorlineopt=screenline  " waiting on v8.1 patch

set guicursor=a:block   " mode aware cursors
set guicursor+=o:hor50-Cursor
set guicursor+=n:Cursor
set guicursor+=i-ci-sm:ver25-InsertCursor
set guicursor+=r-cr:hor15-ReplaceCursor
set guicursor+=c:CommandCursor
set guicursor+=v-ve:VisualCursor
set guicursor+=a:blinkon0

" ................................... Gvim Options (make it look like terminal!)
set guioptions+=LlRrb  " hide scrollbars
set guioptions-=LlRrb
set guioptions-=m      " no menubar
set guioptions-=T      " no toolbar

" .......................................................... White space markers
set nolist  " display tabs and trailing spaces visually
set listchars="tab:▸\<Space>"

" set listchars+=trail:_
set listchars+=trail:·
set listchars+=nbsp:.
set listchars+=extends:>
set listchars+=precedes:<
" set listchars+=eol:¬

command! ToggleWhiteSpace call gui#ToggleWhiteSpace()

nmap <silent><leader><Space> :ToggleWhiteSpace<CR>

" Search _______________________________________________________________________

" ........................................................... Incremental search
set incsearch  " find the next match as we type the search

command! ToggleWrapSearch call gui#ToggleWrapSearch()

nmap <silent><F6> :ToggleWrapSearch<CR>

" ........................................................... Search and replace
" toggle magic and case sensitivity, \m to append magic tokens
cmap %%     \v
cmap ^^     \C

command! -nargs=1 SearchReplace silent! call gui#SearchReplace(<f-args>)

" replace current word!
nnoremap \\      :SearchReplace :%s/\C\<<C-r><C-w>\>/<CR>
" see magic settings
nnoremap //      :SearchReplace :%s/<CR>
vnoremap // :<C-u>SearchReplace :'<,'>s/<CR>

" Window actions _______________________________________________________________

" .............................................................. Window handling
" kill (close) current window
noremap <leader>q  <C-w>q
" close all other windows
noremap <leader>Q  <C-w>o

" ................................................................ Split windows
" horizontal / vertical split
noremap <leader>Z  <C-w>v<C-w>l
noremap <leader>z  <C-w>s<C-w>l
" maximize left:right / top:bottom
noremap <leader>ZZ <C-w><Bar>
noremap <leader>zz <C-w>_
" adjust all splits to the same size
noremap <leader>=  <C-w>=

nnoremap <C-Up>    :resize +5<CR>
nnoremap <C-Down>  :resize -5<CR>
nnoremap <C-Left>  :vertical resize -5<CR>
nnoremap <C-Right> :vertical resize +5<CR>

" ............................................................... Switch windows
" switch to left / right split
noremap <Left>     <C-w>h
noremap <Right>    <C-w>l
" switch to top / bottom split
noremap <Up>       <C-w>k
noremap <Down>     <C-w>j
" " switch windows
" noremap <C-w>    <C-w><C-w>

" gui.vim
