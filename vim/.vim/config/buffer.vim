" sdothum - 2016 (c) wtfpl

" Buffers
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " File buffers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      set autoread                          " reload files changed outside vim
      " set autowrite                       " automatically write a modified buffer on leaving
      set hidden                            " allow hidden background buffers

      augroup buffer
        autocmd!
      augroup END

  " File handling ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      " nmap <leader>f :set filetype=

      autocmd buffer Filetype conf      setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype draft     setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72 syntax=mail
      autocmd buffer Filetype fish      setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype haskell   setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype lua       setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype mail      setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72 syntax=mail
      autocmd buffer Filetype markdown  setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      autocmd buffer Filetype note      setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      autocmd buffer Filetype python    setlocal nospell expandtab tabstop=4 shiftwidth=4 softtabstop=4
      autocmd buffer Filetype ruby      setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype shell     setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype sh        setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype slim      setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd buffer Filetype snippet   setlocal nospell noexpandtab tabstop=2 shiftwidth=2
      autocmd buffer Filetype vim       setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2

      autocmd buffer BufWinEnter *.vim  set filetype=vim
      autocmd buffer BufWinEnter *.wiki set filetype=markdown

    " ............................................................... Modifiable

      " toggle modifiable attribute
      nmap <silent><leader>-      :let &modifiable = (&modifiable == 0 ? 1 : 0)<CR>

      " protected help
      autocmd buffer BufWinEnter *.txt,*.txt.gz if &filetype == 'help' | set nomodifiable | endif

  " Buffer actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Config file

      " when updates won't break the current vim session!
      command! Vimrc call core#Vimrc()

      " autocmd! buffer BufWritePost * nested if expand('%:e') =~ 'vim' | call core#Vimrc() | endif

    " .............................................................. Buffer open

      " check file sensitivity, even though may be sudoed
      " autocmd buffer BufRead   * if expand('%:p') !~ $HOME | set nomodifiable | endif
      " vim8 bug doesn't allow toggling &modifiable so set modifiable on globally
      " mode check for fzf terminal window
      autocmd buffer BufWinEnter * if !core#Protected() | set modifiable | endif
      " always switch to the current file directory, unless uri
      autocmd buffer BufEnter    * if bufname('') !~ '^[A-Za-z0-9]*://' | lcd %:p:h | echo | endif
      " return to last edit position when opening files (You want this!)
      autocmd buffer BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') | execute 'normal! g`"' | endif

    " ...................................................... Buffer close / save

      " close all other buffers (and newly created no name buffer)
      command! Singleton %bd | e # | bd #

      " (sudo) save
      nmap <silent><leader>w      :silent write!<CR>
      nmap <leader>W              :silent write !sudo tee % >/dev/null<CR>
      " (write and) close buffers
      nmap <silent><leader>d      :silent bdelete!<CR>
      nmap <silent><leader>dd     :%bd!<CR>
      nmap <silent><leader>D      :silent Singleton<CR>
      nmap <silent><leader>ww     :silent wqall!<CR>
      " discard quit
      nmap <silent><leader>qq     :silent qall!<CR>

      " pre-write formatting
      autocmd buffer BufLeave    * call core#StripTrailingWhitespaces()
      autocmd buffer BufWritePre * call core#StripTrailingWhitespaces()
      autocmd buffer FocusLost   * call core#StripTrailingWhitespaces()
      " save on losing focus
      autocmd buffer FocusLost   * silent! :wall

    " ......................................................... Buffer switching

      " " goto buffer (just fingering convenience)
      " nmap <leader>b            :b<Space>
      " " query current buffer
      " nmap <leader>B            :echo expand('%:p')<CR>

      " " silence vim's default (command line) file info message, note silent..silent
      " vmap <silent><S-PageUp>   <ESC>:silent bprevious<CR>
      " imap <silent><S-PageUp>   <ESC>:silent bprevious<CR>
      " nmap <silent><S-PageUp>   :silent bprevious<CR>
      " vmap <silent><S-PageDown> <ESC>:silent bnext<CR>
      " imap <silent><S-PageDown> <ESC>:silent bnext<CR>
      " nmap <silent><S-PageDown> :silent bnext<CR>

      " planck keyboard specific buffer navigation key assignments
      " nmap <silent>-            :silent bprevious<CR>
      " nmap <silent>+            :silent bnext<CR>
      nmap <silent><Delete>       :silent bprevious<CR>
      nmap <silent>_              :silent bnext<CR>
      " switch to previously edited/viewed buffer
      nmap <silent><BS>           :silent e #<CR>

  " Window actions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Window handling

      " kill (close) current window
      noremap <leader>q           <C-w>q
      " close all other windows
      noremap <leader>Q           <C-w>o

    " ............................................................ Split windows

      " horizontal / vertical split
      noremap <leader>Z           <C-w>v<C-w>l
      noremap <leader>z           <C-w>s<C-w>l
      " maximize left:right / top:bottom
      noremap <leader>ZZ          <C-w><Bar>
      noremap <leader>zz          <C-w>_
      " adjust all splits to the same size
      noremap <leader>=           <C-w>=

      nnoremap <Up>               :resize +5<CR>
      nnoremap <Down>             :resize -5<CR>
      nnoremap <Left>             :vertical resize -5<CR>
      nnoremap <Right>            :vertical resize +5<CR>

    " ........................................................... Switch windows

      " colemak shift-dh lmne cluster
      " switch to left / right split
      " noremap <C-m>             <C-w>h
      " noremap <C-e>             <C-w>l
      noremap <C-Left>            <C-w>h
      noremap <C-Right>           <C-w>l
      " switch to top / bottom split
      " noremap <C-l>             <C-w>k
      " noremap <C-n>             <C-w>j
      noremap <C-Up>              <C-w>k
      noremap <C-Down>            <C-w>j
      " switch windows
      " noremap <C-w>             <C-w><C-w>

  " Folding ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Fold methods

      set foldenable                        " fold by default
      set foldlevelstart=10                 " open most folds by default
      set foldnestmax=10                    " 10 nested fold max
      set foldmethod=indent                 " fold based on indent
      " set foldmethod=syntax               " folding based on syntax
      set foldnestmax=3                     " deepest fold is 3 levels

      " toggle fold tag / open all
      " noremap         <leader>z za
      " noremap         <leader>Z zA
      " noremap <leader><leader>z zR

    " ........................................................... Folding levels

      nmap    <silent><leader>0   :set foldlevel=0<CR>
      nmap    <silent><leader>1   :set foldlevel=1<CR>
      nmap    <silent><leader>2   :set foldlevel=2<CR>
      nmap    <silent><leader>3   :set foldlevel=3<CR>
      nmap    <silent><leader>4   :set foldlevel=4<CR>
      nmap    <silent><leader>5   :set foldlevel=5<CR>
      nmap    <silent><leader>6   :set foldlevel=6<CR>
      nmap    <silent><leader>7   :set foldlevel=7<CR>
      nmap    <silent><leader>8   :set foldlevel=8<CR>
      nmap    <silent><leader>9   :set foldlevel=9<CR>

" buffer.vim
