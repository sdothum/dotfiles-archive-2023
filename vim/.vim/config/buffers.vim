" sdothum - 2016 (c) wtfpl

" Buffers
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Buffer actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      set autoread                          " reload files changed outside vim
      " set autowrite                       " automatically write a modified buffer on leaving
      set hidden                            " allow hidden background buffers

      augroup buffer
        autocmd!
      augroup END

    " .............................................................. Buffer open

      " check file sensitivity, even though may be sudoed
      " autocmd buffer BufRead     * if expand('%:p') !~ $HOME | set nomodifiable | endif
      " vim8 bug doesn't allow toggling &modifiable so set modifiable on globally
      autocmd buffer BufWinEnter * if &filetype != 'help' | set modifiable | endif
      " always switch to the current file directory, unless uri
      autocmd buffer BufEnter    * if bufname('') !~ '^[A-Za-z0-9]*://' | lcd %:p:h | echo | endif
      " return to last edit position when opening files (You want this!)
      autocmd buffer BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') | execute 'normal! g`"' | endif

    " ...................................................... Buffer close / save

      " close buffer
      nmap <silent><leader>d    :silent bdelete!<CR>
      " Fast saving
      nmap <silent><leader>w    :silent write!<CR>
      " sudo save
      nmap <leader>W            :silent write !sudo tee % >/dev/null<CR>
      " (write and) close all buffers
      nmap <silent><leader>zz   :silent wqall!<CR>
      nmap <silent><leader>qq   :silent qall!<CR>

      " pre-write formatting
      autocmd buffer BufWritePre * call StripTrailingWhitespaces()
      autocmd buffer FocusLost   * call StripTrailingWhitespaces()
      " save on losing focus
      autocmd buffer FocusLost   * silent! :wall

    " ......................................................... Buffer switching

      " goto buffer (just fingering convenience)
      nmap <leader>b            :b<Space>
      " query current buffer
      nmap <leader>B            :echo expand('%:p')<CR>

      " silence vim's default (command line) file info message, note silent..silent
      vmap <silent><C-PageUp>   <ESC>:silent bprevious<CR>
      imap <silent><C-PageUp>   <ESC>:silent bprevious<CR>
      nmap <silent><C-PageUp>   :silent bprevious<CR>
      vmap <silent><C-PageDown> <ESC>:silent bnext<CR>
      imap <silent><C-PageDown> <ESC>:silent bnext<CR>
      nmap <silent><C-PageDown> :silent bnext<CR>
      " switch to previously edited/viewed buffer
      vmap <silent><C-BS>       <ESC>:silent e #<CR>
      imap <silent><C-BS>       <ESC>:silent e #<CR>
      nmap <silent><C-BS>       :silent e #<CR>

  " Window actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Window handling

      " kill (close) current window
      nmap <leader>q   <C-w>q
      " close all other windows
      nmap <leader>Q   <C-w>o

    " ............................................................ Split windows

      " horizontal / vertical split
      nmap <leader>s   <C-w>v<C-w>l
      nmap <leader>S   <C-w>s<C-w>l
      " maximize left:right / top:bottom
      nmap <leader>Z   <C-w><Bar>
      nmap <leader>z   <C-w>_
      " adjust viewports to the same size
      nmap <leader>=   <C-w>=

    " ........................................................... Switch windows

      " colemak shift-dh lmne cluster
      " switch to left / right split
      " nmap <C-m>     <C-w>h
      " nmap <C-e>     <C-w>l
      nmap <C-S-Left>  <C-w>h
      nmap <C-S-Right> <C-w>l
      " switch to top / bottom split
      " nmap <C-l>     <C-w>k
      " nmap <C-n>     <C-w>j
      nmap <C-S-Up>    <C-w>k
      nmap <C-S-Down>  <C-w>j

  " Folding ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Fold methods

      set foldenable                        " fold by default
      set foldlevelstart=10                 " open most folds by default
      set foldnestmax=10                    " 10 nested fold max
      set foldmethod=indent                 " fold based on indent
      " set foldmethod=syntax               " folding based on syntax
      set foldnestmax=3                     " deepest fold is 3 levels

      " toggle fold tag / open all
      noremap <leader>z         za
      noremap <leader>Z         zA
      noremap <leader><leader>z zR

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

  " System actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Print file

      " a la vimb
      command! Hardcopy execute "if &filetype == 'vimwiki' | execute '!hardcory wiki \"' . expand('%:t') . '\"'  | elseif expand('%:p') =~ 'Patricia' | execute '!hardcory wps' expand('%:t') | else | execute '!hardcory code' expand('%:t') | endif"

      nmap <silent><leader>ha :silent Hardcopy<CR>

    " ............................................................ Open terminal

      " open shell session in buffer directory
      nmap <silent><leader>te :silent call system('term "vimterm" STACK')<CR>

" buffers.vim
