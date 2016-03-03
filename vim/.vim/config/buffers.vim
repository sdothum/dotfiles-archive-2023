" sdothum - 2016 (c) wtfpl

" Buffers
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Buffer actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Buffer opening

      set autoread                          " reload files changed outside vim
      " set autowrite                       " automatically write a modified buffer on leaving
      set hidden                            " allow hidden background buffers

      " query current buffer
      nmap <leader>B      :echo @%<CR>

      " always switch to the current file directory, unless uri
      autocmd BufEnter    * if bufname('') !~ '^[A-Za-z0-9]*://' | lcd %:p:h | echo | endif
      " return to last edit position when opening files (You want this!)
      autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') | execute 'normal! g`"' | endif

    " .................................................. Buffer closing / saving

      " close buffer
      nmap <leader>d    :bdelete!<CR>
      " Fast saving
      nmap <leader>w    :write!<CR>
      " sudo save
      nmap <leader>W    :w !sudo tee % >/dev/null
      " (write and) close all buffers
      nmap <leader>zz   :wqall!<CR>
      nmap <leader>qq   :qall!<CR>

      " save on losing focus
      autocmd FocusLost * silent! :wall

    " ......................................................... Buffer switching

      vmap <C-PageUp>   <ESC>:bprevious<CR>
      imap <C-PageUp>   <ESC>:bprevious<CR>
      nmap <C-PageUp>   :bprevious<CR>
      vmap <C-PageDown> <ESC>:bnext<CR>
      imap <C-PageDown> <ESC>:bnext<CR>
      nmap <C-PageDown> :bnext<CR>
      " switch to prev buffer
      vmap <C-BS>       <ESC><C-^>
      imap <C-BS>       <ESC><C-^>
      nmap <C-BS>       <C-^>

  " Window actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Window handling

      " kill (close) current window
      nmap <leader>q    <C-w>q
      " close all other windows
      nmap <leader>Q    <C-w>o

    " ............................................................ Split windows

      " horizontal / vertical split
      nmap <leader>s    <C-w>v<C-w>l
      nmap <leader>S    <C-w>s<C-w>l
      " maximize left:right / top:bottom
      nmap <leader>z    <C-w><Bar>
      nmap <leader>Z    <C-w>_
      " adjust viewports to the same size
      nmap <leader>=    <C-w>=

    " ........................................................... Switch windows

      " colemak shift-dh lmne cluster
      " switch to left / right split
      " nmap <C-m>      <C-w>h
      " nmap <C-e>      <C-w>l
      nmap <C-S-Left>   <C-w>h
      nmap <C-S-Right>  <C-w>l
      " switch to top / bottom split
      " nmap <C-l>      <C-w>k
      " nmap <C-n>      <C-w>j
      nmap <C-S-Up>     <C-w>k
      nmap <C-S-Down>   <C-w>j

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
      nmap <leader>f0 :set foldlevel=0<CR>
      nmap <leader>f1 :set foldlevel=1<CR>
      nmap <leader>f2 :set foldlevel=2<CR>
      nmap <leader>f3 :set foldlevel=3<CR>
      nmap <leader>f4 :set foldlevel=4<CR>
      nmap <leader>f5 :set foldlevel=5<CR>
      nmap <leader>f6 :set foldlevel=6<CR>
      nmap <leader>f7 :set foldlevel=7<CR>
      nmap <leader>f8 :set foldlevel=8<CR>
      nmap <leader>f9 :set foldlevel=9<CR>

" buffers.vim
