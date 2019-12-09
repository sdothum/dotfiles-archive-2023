" sdothum - 2016 (c) wtfpl

" Defaults
" ══════════════════════════════════════════════════════════════════════════════

" Mode _________________________________________________________________________

augroup defaults | autocmd! | augroup END

" ....................................................................... Insert
" don't linger in insert mode indefinitely (updatetime=ms)
autocmd defaults InsertEnter * let s:updatetime = &updatetime | set updatetime=60000
autocmd defaults InsertLeave * let &updatetime  = s:updatetime
autocmd defaults CursorHoldI * stopinsert

" ........................................................................ Debug
nnoremap <silent><S-F10> :let g:trace = !g:trace<CR>
" recover last error message
nmap <leader>e           :echo errmsg<CR>

" ..................................................................... Terminal
nmap <silent><C-t>           :Term<CR>
imap <silent><C-t>      <C-o>:Term<CR>

nmap <silent><C-t><C-t>      :term fish<CR>
imap <silent><C-t><C-t> <C-o>:term fish<CR>

" Registers ____________________________________________________________________

" ........................................................................ Marks
set viminfo='100,f1  " save up to 100 marks, enable capital marks
set viminfo^=%       " remember info about open buffers on close

" " delete all marks in current buffer, see signature plugin
" nmap <silent><leader>'' :delmarks!<CR>

" ....................................................................... Macros
" replay q macro
nnoremap <C-q>             @q
" edit q macro
nnoremap <leader>Q         :<C-u><C-r><C-r>='let @q = '. string(getreg('q'))<CR><C-f><Left>

" Folding ______________________________________________________________________

" ................................................................. Fold methods
set foldenable            " fold by default
set foldlevelstart=10     " open most folds by default
" set foldlevelstart=1
set foldnestmax=10        " 10 nested fold max
set foldmethod=indent     " fold based on indent (faster than syntax)
" set foldmethod=syntax   " folding based on syntax

let javaScript_fold=1     " JavaScript
let perl_fold=1           " Perl
let php_folding=1         " PHP
let r_syntax_folding=1    " R
let ruby_fold=1           " Ruby
let sh_fold_enabled=1     " sh
let vimsyn_folding='af'   " Vim script
let xml_syntax_folding=1  " XML

" " toggle fold tag / open all
" noremap <leader>z         za
" noremap <leader>Z         zA
" noremap <leader><leader>z zR

" ............................................................... Folding levels
nmap <silent><leader>0 :set foldlevel=0<CR>
nmap <silent><leader>1 :set foldlevel=1<CR>
nmap <silent><leader>2 :set foldlevel=2<CR>
nmap <silent><leader>3 :set foldlevel=3<CR>
nmap <silent><leader>4 :set foldlevel=4<CR>
nmap <silent><leader>5 :set foldlevel=5<CR>
nmap <silent><leader>6 :set foldlevel=6<CR>
nmap <silent><leader>7 :set foldlevel=7<CR>
nmap <silent><leader>8 :set foldlevel=8<CR>
nmap <silent><leader>9 :set foldlevel=9<CR>

" Format _______________________________________________________________________

" ......................................................................... Line
set formatoptions=qrn1j  " coding options
" double spaces at the end of a wrapped line, becomes <br> by markdown
set nojoinspaces         " force single spacing after sentence punctuation!
set textwidth=80

" .................................................................... Line wrap
nmap <silent><leader><CR> :ToggleWrap<CR>

" ......................................................................... Tabs
set autoindent
set copyindent     " copy the previous indentation on autoindenting
set expandtab      " expand tabs into spaces, never use hard tabs!
set shiftround     " use multiple of shiftwidth when indenting with "<>"
set shiftwidth=2   " number of spaces for unindenting
set nosmartindent  " smartindent hash comments to beginning of line
set smarttab
set softtabstop=2
set tabstop=2      " global tab width

cabbrev spaces set expandtab
cabbrev tabs   set noexpandtab

" Search / completion __________________________________________________________

" ....................................................................... Search
set gdefault    " global by default
set hlsearch    " hilight searches by default
set ignorecase  " ignore case when searching
set magic       " regex magic
set showmatch   " set show matching parenthesis
set smartcase   " ignore case if search pattern is all lowercase

" tab to bracket pairs
nmap <Tab>     %
vmap <Tab>     %

" clear search highlight
nmap <silent>\ :noh<CR>

" ........................................................... Incremental search
set incsearch  " find the next match as we type the search

nmap <silent><F6> :ToggleWrapSearch<CR>

" ........................................................... Search and replace
" toggle magic and case sensitivity, \m to append magic tokens
cmap %%     \v
cmap ^^     \C

" replace current word!
nnoremap \\      :SearchReplace :%s/\C\<<C-r><C-w>\>/<CR>
" see magic settings
nnoremap //      :SearchReplace :%s/<CR>
vnoremap // :<C-u>SearchReplace :'<,'>s/<CR>

" ............................................................... Tab completion
set wildignore=.cache/**,cache/**  " stuff to ignore when tab completing
set wildignore+=*.class,*.bak,*.pyc,*.swp
set wildignore+=Desktop/**
set wildignore+=*.gem
set wildignore+=*.gif,*.jpg,*.png
set wildignore+=*.jar,*.tar.*,*.zip
set wildignore+=log/**
set wildignore+=*.o,*.obj,*~
set wildignore+=tmp/**
set wildmenu                       " enable ctrl-n and ctrl-p to scroll thru matches
set wildmode=list:longest,full     " command <Tab> completion order
set wildignore+=*vim/undo*

" default.vim
