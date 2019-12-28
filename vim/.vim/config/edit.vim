" sdothum - 2016 (c) wtfpl

" Edit
" ══════════════════════════════════════════════════════════════════════════════

" Buffer _______________________________________________________________________

augroup edit | autocmd! | augroup END

" .............................................................. Command history
nmap <F1> q:
imap <F1> <C-o>q:
vmap <F1> <C-u>q:

" easier redo
nnoremap U <C-r>

" Line _________________________________________________________________________

" .................................................................. Insert line
command! SmartWrap silent! call edit#SmartWrap()

inoremap <silent><C-CR> <C-o>:SmartWrap<CR>

" insert blank line above/below
nnoremap <silent><leader><Up>   :silent set paste<CR>m`O<Esc>``:silent set nopaste<CR>
nnoremap <silent><leader><Down> :silent set paste<CR>m`o<Esc>``:silent set nopaste<CR>

" " duplicate line
" nnoremap <C-CR> :t.<CR>
" inoremap <C-CR> <C-o>:t.<CR>

" .................................................................. Delete line
" delete blank line above/below
nnoremap <silent><C-S-Up>   m`:silent -g/\m^\s*$/d<CR>``:silent nohlsearch<CR>
nnoremap <silent><C-S-Down> m`:silent +g/\m^\s*$/d<CR>``:silent nohlsearch<CR>

" ............................................................. Strip whitespace
command! StripTrailingWhitespaces silent! call edit#StripTrailingWhitespaces()

nmap <silent><F4>      :StripTrailingWhitespaces<CR>
vmap <silent><F4> :<C-u>StripTrailingWhitespaces<CR>

" " pre-write formatting
" autocmd edit BufWritePre * StripTrailingWhitespaces
" " focus oriented formatting
" autocmd edit BufLeave    * StripTrailingWhitespaces
" autocmd edit FocusLost   * StripTrailingWhitespaces

" Text manipulation ____________________________________________________________

" .......................................................... Reformat paragraghs
command! -range=% -nargs=1 Inject silent! execute '<line1>,<line2>call edit#Inject(<f-args>)'

" retain cursor position for insert mode reformatting
inoremap <silent><C-f> <Esc>lmZV:Inject {jv}kJvgq`Z:delmarks Z<CR>i
" reformat at cursor position
nnoremap <silent><C-f> mZV:Inject {jv}kJvgq`Z:delmarks Z<CR>

" ................................................................. Convert tabs
command! -range=% Space2Tab silent! execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'
command! -range=% Tab2Space silent! execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'

nmap <silent><leader><tab>        :retab<CR>
nmap <silent><leader><Space><tab> :Space2Tab<CR>
vmap <silent><leader><Space><tab> :Space2Tab<CR>
nmap <silent><leader><tab><Space> :Tab2Space<CR>
vmap <silent><leader><tab><Space> :Tab2Space<CR>

" ...................................................... Quote enclose selection
" extend enclosing %V 1 char right to enclose last character of block
vnoremap ' :s/\%V\(.*\%V.\)/'\1'/<CR>:noh<CR>`>l
vnoremap " :s/\%V\(.*\%V.\)/"\1"/<CR>:noh<CR>`>l

" .............................................................. Code block text
command! -range=% -nargs=0 CodeBlock silent! execute '<line1>,<line2>call edit#CodeBlock()'

" markup wiki code blocks
nnoremap <silent><leader>` V:CodeBlock<CR>
vmap     <silent><leader>`  :'<,'>CodeBlock<CR>

" Text shift ___________________________________________________________________

" .................................................................. Select text
" select all
nnoremap <C-a>    ggVG
" extend paragraph selection
vmap <C-PageUp>   {
vmap <C-PageDown> }

" select paragragh
command! ParagraphAbove silent! call edit#ParagraphAbove()
command! ParagraphBelow silent! call edit#ParagraphBelow()
  
nmap <silent><C-PageUp>   :ParagraphAbove<CR>
nmap <silent><C-PageDown> :ParagraphBelow<CR>

" ............................................................. Shift left right
" softwidth shifts, preserve selection when indenting
nnoremap <S-Left>  <<
inoremap <S-Left>  <C-d>
vnoremap <S-Left>  <gv
nnoremap <S-Right> >>
inoremap <S-Right> <C-t>
vnoremap <S-Right> >gv

" byte shift left / right
nnoremap <leader><Left>  :s/^ //<CR>:silent nohlsearch<CR>
nnoremap <leader><Right> :s/^/ /<CR>:silent nohlsearch<CR>
vnoremap <leader><Left>  :s/^ //<CR>:silent nohlsearch<CR>gv
vnoremap <leader><Right> :s/^/ /<CR>:silent nohlsearch<CR>gv

" move visual block with automatic alignment!
vnoremap L :m '<-2<CR>gv=gv
vnoremap N :m '>+1<CR>gv=gv

" ................................................................ Shift up down
command! MoveLineUp     silent! call edit#MoveLineUp()
command! MoveLineDown   silent! call edit#MoveLineDown()
command! MoveVisualUp   silent! call edit#MoveVisualUp()
command! MoveVisualDown silent! call edit#MoveVisualDown()
  
nmap <silent><S-Up>        :MoveLineUp<CR>
imap <silent><S-Up>   <ESC>:MoveLineUp<CR>a
vmap <silent><S-Up>   <ESC>:MoveVisualUp<CR>

nmap <silent><S-Down>      :MoveLineDown<CR>
imap <silent><S-Down> <ESC>:MoveLineDown<CR>a
vmap <silent><S-Down> <ESC>:MoveVisualDown<CR>

" Text shortcuts _______________________________________________________________

" ..................................................................... Filename
inoremap <C-w> <C-r>=expand("%:t:r")<CR>

" edit.vim
