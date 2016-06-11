" sdothum - 2016 (c) wtfpl

" Buffers
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Buffer actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Buffer opening

      set autoread                          " reload files changed outside vim
      " set autowrite                       " automatically write a modified buffer on leaving
      set hidden                            " allow hidden background buffers

      " query current buffer
      nmap <leader>bb     :echo @%<CR>

      " always switch to the current file directory, unless uri
      autocmd BufEnter    * if bufname('') !~ '^[A-Za-z0-9]*://' | lcd %:p:h | echo | endif
      " return to last edit position when opening files (You want this!)
      autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') | execute 'normal! g`"' | endif

    " .................................................. Buffer closing / saving

      " close buffer
      nmap <silent><leader>d    :silent bdelete!<CR>
      " Fast saving
      nmap <silent><leader>w    :silent write!<CR>
      " sudo save
      nmap <silent><leader>W    :w !sudo tee % >/dev/null
      " (write and) close all buffers
      nmap <silent><leader>zz   :silent wqall!<CR>
      nmap <silent><leader>qq   :silent qall!<CR>

      " save on losing focus
      autocmd FocusLost * silent! :wall

    " ......................................................... Buffer switching

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

      set nofoldenable                      " don't fold by default
      set foldmethod=indent                 " fold based on indent
      " set foldmethod=syntax               " folding based on syntax
      set foldnestmax=3                     " deepest fold is 3 levels

      " toggle fold / fold tags
      noremap <leader>f za
      noremap <leader>F Vatzf

    " ........................................................... Folding levels

      " 'f' key, not Function key!
      nmap <silent><leader>f0 :set foldlevel=0<CR>
      nmap <silent><leader>f1 :set foldlevel=1<CR>
      nmap <silent><leader>f2 :set foldlevel=2<CR>
      nmap <silent><leader>f3 :set foldlevel=3<CR>
      nmap <silent><leader>f4 :set foldlevel=4<CR>
      nmap <silent><leader>f5 :set foldlevel=5<CR>
      nmap <silent><leader>f6 :set foldlevel=6<CR>
      nmap <silent><leader>f7 :set foldlevel=7<CR>
      nmap <silent><leader>f8 :set foldlevel=8<CR>
      nmap <silent><leader>f9 :set foldlevel=9<CR>

  " Printing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Print file

      command! Hardcopy execute "if &filetype == 'vimwiki' | execute '!text=true prtex' EscapeFilename() | else | execute '!prtex' EscapeFilename() | endif"
      nmap <C-h> :silent Hardcopy<CR>

" buffers.vim
