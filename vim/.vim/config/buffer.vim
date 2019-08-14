" sdothum - 2016 (c) wtfpl

" Buffers
" ══════════════════════════════════════════════════════════════════════════════

  " File buffers _______________________________________________________________

    " .................................................................... Setup

      set autoread     " reload files changed outside vim
      " set autowrite  " automatically write a modified buffer on leaving
      set hidden       " allow hidden background buffers

      let s:repo = $HOME . '/stow/'  " directory to auto backup

      augroup buffer | autocmd! | augroup END

  " Diff buffer ________________________________________________________________

    " ................................................................ Open diff

      " go to left window in case a diff window is already open and close it
      nmap <silent><leader>dd :silent OpenDiff<CR>

  " File actions _______________________________________________________________

    " ...................................................... Buffer close / save

      " save buffers
      nmap <silent><leader>w  :silent write!<CR>
      nmap <leader>W          :silent write !sudo tee % >/dev/null<CR>
      nmap <silent><leader>ww :silent wqall!<CR>

      " close buffers
      nmap <silent><leader>d  :silent CloseUnique<CR>
      nmap <silent><leader>DD :CloseDiff<CR>:%bdelete!<CR>
      nmap <leader>D          :silent Singleton<CR>
      " discard quit
      nmap <silent><leader>qq :quitall!<CR>

    " .............................................................. Auto backup

      " auto backup
      autocmd buffer BufWrite  * QueueFile
      " save on losing focus, :wall on FocusLost does not trigger s:queueFile() (?)
      autocmd buffer FocusLost * QueueBuffers

  " Buffer handling ____________________________________________________________

    " ........................................................... Initialization

      " always switch to the current file directory, unless uri
      autocmd buffer BufEnter    * if bufname('') !~ '^[A-Za-z0-9]*://' | lcd %:p:h | echo | endif
      " return to last edit position when opening files (You want this!)
      autocmd buffer BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') | execute 'normal! g`"' | endif

    " ............................................................... Modifiable

      " toggle modifiable attribute
      nmap <silent><leader>- :let &modifiable = (&modifiable == 0 ? 1 : 0)<CR>

      " protected help
      autocmd buffer BufWinEnter *.txt,*.txt.gz if &filetype == 'help' | set nomodifiable | endif
      " check file sensitivity, even though may be sudoed
      " autocmd buffer BufRead   * if expand('%:p') !~ $HOME | set nomodifiable | endif
      " vim8 bug doesn't allow toggling &modifiable so set modifiable on globally
      " mode check for fzf terminal window
      autocmd buffer BufWinEnter * if ! Protected() | set modifiable | endif

    " ............................................................ Switch buffer

      " " goto buffer (just fingering convenience), see fzf settings.vim
      " nmap <leader>b :buffer<Space>
      nmap <leader>B   :echo '[' . bufnr('%') . '] ' . expand('%:p')<CR>

      " beakl si layout specific buffer navigation key assignments, note silent -> silent
      if $BEAKL > ''
        " don't wait for statusline refresh to set split colors, see ui.vim s:showInfo()
        nmap <silent><Delete> :CloseDiff<CR>:silent bprevious<CR>:SplitColors<CR>
        nmap <silent><Enter>  :Enter<CR>
      else
        nmap <silent>-        :CloseDiff<CR>:silent bprevious<CR>:SplitColors<CR>
        nmap <silent>+        :CloseDiff<CR>:silent bnext<CR>:SplitColors<CR>
      endif
      " switch to previously edited/viewed buffer
      nmap <silent><BS>       :CloseDiff<CR>:silent edit #<CR>:SplitColors<CR>

  " Window actions _____________________________________________________________

    " .......................................................... Window handling

      " kill (close) current window
      noremap <leader>q  <C-w>q
      " close all other windows
      noremap <leader>Q  <C-w>o

    " ............................................................ Split windows

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

    " ........................................................... Switch windows

      " switch to left / right split
      noremap <Left>     <C-w>h
      noremap <Right>    <C-w>l
      " switch to top / bottom split
      noremap <Up>       <C-w>k
      noremap <Down>     <C-w>j
      " switch windows
      " noremap <C-w>    <C-w><C-w>

  " Folding ____________________________________________________________________

    " ............................................................. Fold methods

      set foldenable            " fold by default
      set foldlevelstart=10     " open most folds by default
      " set foldlevelstart=1
      set foldnestmax=10        " 10 nested fold max
      " set foldmethod=indent   " fold based on indent
      set foldmethod=syntax     " folding based on syntax

      let javaScript_fold=1     " JavaScript
      let perl_fold=1           " Perl
      let php_folding=1         " PHP
      let r_syntax_folding=1    " R
      let ruby_fold=1           " Ruby
      let sh_fold_enabled=1     " sh
      let vimsyn_folding='af'   " Vim script
      let xml_syntax_folding=1  " XML

      " toggle fold tag / open all
      " noremap <leader>z         za
      " noremap <leader>Z         zA
      " noremap <leader><leader>z zR

    " ........................................................... Folding levels

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

" buffer.vim
