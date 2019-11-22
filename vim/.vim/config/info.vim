" sdothum - 2016 (c) wtfpl

" Info
" ══════════════════════════════════════════════════════════════════════════════

" StatusLine ___________________________________________________________________

" ........................................................................ Setup
" window g:state [0] unmodified [1] unmodifiable [2] modified [3] inactive
if empty($DISPLAY)
  let g:state   = ['^', '-', '+', 'x']  " console
else
  if !empty($MONO)
    let g:state = ['', '', '', '']  " nerd-font utf-8 mono symbols, see ui.vim
  else
    let g:state = ['', '', '', '']  " nerd-font utf-8 symbols, see ui.vim
  endif
endif

let g:prose       = 0  " generic filetype, see theme.vim
let g:show_column = 0  " statusline current column, see theme.vim

augroup info | autocmd! | augroup END

" ......................................................... Statusline indicator
" trigger autocmd to flash column position (does not work for BOF)
nnoremap <silent><C-c> hl

autocmd info CursorHold  * let g:show_column = 0
autocmd info CursorMoved * let g:show_column = 1

" ................................................................. Syntax group
nnoremap <silent><F10> :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>

" Folding ______________________________________________________________________

" ................................................................. Fold methods
set foldenable            " fold by default
set foldlevelstart=10     " open most folds by default
" set foldlevelstart=1
set foldnestmax=10        " 10 nested fold max
" set foldmethod=indent   " fold based on indent (faster than syntax)
set foldmethod=syntax     " folding based on syntax

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

" info.vim
