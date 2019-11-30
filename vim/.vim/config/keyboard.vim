" sdothum - 2016 (c) wtfpl

" Keyboard
" ══════════════════════════════════════════════════════════════════════════════

" Keyboard (re)mappings ________________________________________________________

augroup kbd | autocmd! | augroup END

" ....................................................................... Leader
let mapleader      = "\<Space>"  " remap <leader> a la spacemacs
let s:modal_strict = 0           " modal vim cursor keys (0) allow (1) disable

" Cursor _______________________________________________________________________

" ........................................................... Backspace settings
set backspace=indent,eol,start  " allow backspace in insert mode
set whichwrap=b,s,h,l,<,>,[,]   " backspace and cursor keys wrap

" ............................................................. Cursor movements
" up/down by screen lines, not file lines
nnoremap k     gk
vnoremap k     gk
nnoremap j     gj
vnoremap j     gj

" up/down by paragraph sentence
nmap <leader>( {{)
nmap <leader>) })

" insert mode local region cursor movements
if s:modal_strict
  " <C-h> is overridden by auto-pairs delete <BS> when enabled
  imap <C-h>   <Left>
  imap <C-j>   <Down>
  imap <C-k>   <Up>
  imap <C-l>   <Right>
endif

" ................................................................. Disable keys
" affirm vim modal usage but these keys are remapped below anyway :)
if s:modal_strict
  imap <down>  <nop>
  imap <left>  <nop>
  imap <right> <nop>
  imap <up>    <nop>
  nmap <down>  <nop>
  nmap <left>  <nop>
  nmap <right> <nop>
  nmap <up>    <nop>
endif

" Keyboard shortcuts ___________________________________________________________

" ................................................................. Copy / paste
" prevent cascading paste insert
set pastetoggle=<F3>

" yank from the cursor to the end of the line, to be consistent with C and D.
" see yankring for plugin equivalent
nnoremap Y  y$
nnoremap vv V
nnoremap V  <C-v>$

" reselect/reyank text just pasted
nnoremap <leader>v gv
nnoremap <leader>V gvy
map      <leader>p pgvy

" highlight last inserted text
nnoremap <leader>i `[v`]

" disable paste mode when leaving Insert Mode
autocmd kbd InsertLeave * set nopaste

" .......................................................... Sentence operations
" use "as" suffix for outer sentence
" change sentence
nnoremap <leader>cc cis
" cut sentence
nnoremap <leader>dd dis
" yank sentence
nnoremap <leader>yy yis

" ........................................................ Clipboard cut / paste
" visual mode yank/cut clipboard actions
" "+Y yank to clipboard
vnoremap <C-F2> "+y
vnoremap <S-F2> "+Y
" "+D cut to clipboard
vnoremap <C-F3> "+d
vnoremap <S-F3> "+D

" " normal/insert mode paste actions
" " "+P pads space after insert
" " note: to enter visual block mode type v<C-v>
imap <F2> <ESC>"+pli
nmap <F2> h"+pl
" command mode insertion (paste) of current yank buffer
cmap <F2> <C-r>"

" Abbreviations ________________________________________________________________

" ..................................................................... Personal
" restore I capitalization convention
command! I unabbrev I | iabbrev i I

" i am
autocmd kbd Filetype markdown,mail,draft,note iabbrev I i

" keyboard.vim
