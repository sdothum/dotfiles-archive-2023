" sdothum - 2016 (c) wtfpl

" Setup
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      set nocompatible                      " disable vi-compatibility

      augroup setup
        autocmd!
      augroup END

    " .......................................................... OS and hardware

      set lazyredraw                        " don't redraw while executing macros
      " set modelines=0                     " prevent modeline secrurity hole
      set modelines=1
      set mouse=a                           " enable mouse actions
      set shell=/bin/sh                     " required for plugin system call dependencies
      set title                             " change the terminal's title
      set ttyfast
      set timeout timeoutlen=1000 ttimeoutlen=100
      " set cryptmethod=blowfish            " encryption method

    " ................................................................... Leader

      let mapleader   = "\<Space>"          " remap <leader> a la spacemacs
      let g:mapleader = "\<Space>"
      " let mapleader   = "\<BS>"           " use right thumb on planck keyboard
      " let g:mapleader = "\<BS>"           " for better <space> responsiveness

      " non-latent space insertion (for lining up text, conflicting leader sequences, etc.)
      inoremap <C-Space> <Space>

    " .................................................................... Mark

      set viminfo='100,f1                   " save up to 100 marks, enable capital marks
      set viminfo^=%                        " remember info about open buffers on close
      " delete all marks in current buffer, see signature plugin
      " nmap <silent><leader>'' :delmarks!<CR>

    "  ............................................................ Undo history

      " keep persistent undo history across sessions, by storing in file
      silent !mkdir ~/.vim/backups 2>/dev/null
      set history=1000                      " store lots of :cmdline history
      set undodir=~/.vim/backups
      set undofile
      set undolevels=1000                   " maximum number of changes that can be undone
      set undoreload=10000                  " maximum number lines to save for undo
      " easier redo
      map U <C-r>

    " ................................................................. Messages

      " recover last error message
      nmap <leader>e :echo errmsg<CR>

      " clear messages after awhile to keep screen clean and distraction free!
      autocmd setup cursorhold * echo


  " Files ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Config file

      " quickly edit/reload the vimrc file
      nmap <silent><leader>vim         :edit $MYVIMRC<CR>
      " must switch to .vimrc first for unknown reason.. (bug?)
      nmap <silent><leader><leader>vim :buffer .vimrc<CR>:autocmd!<CR>:source $MYVIMRC<CR>

      " load .vimrc after save
      " autocmd setup BufWritePost $MYVIMRC      source $MVIMRC
      autocmd setup bufwritepost $MYVIMRC nested source $MYVIMRC
      autocmd setup BufWritePost ~/.vim/config/* buffer $MYVIMRC | source $MYVIMRC
      " PluginUpdate and config reload loses filetype, restore
      autocmd setup BufWinEnter *.vim            set filetype=vim

    " ..................................................................... Help

      " if $HOST == 'monad'                 " open help in maximum horizontal split
        autocmd setup BufWinEnter *.txt,*.txt.gz if &filetype == 'help' | wincmd _ | endif
      " endif

      imap <F1>           <C-o>:help<Space>
      nmap <F1>           :help<Space>
      vmap <F1>           <C-o>:help<Space>
      " list my function and leader key assignments
      imap <silent><S-F1> <C-o>:silent !term 'vmap' vmap<CR>
      nmap <silent><S-F1> :silent !term 'vmap' vmap<CR>
      vmap <silent><S-F1> <C-o>:silent !term 'vmap' vmap<CR>

    " ..................................................................... Swap

      set nobackup
      set directory=~/tmp,/tmp              " keep swap files in one location
      set noswapfile                        " turn off swap files
      set nowritebackup

  " Keyboard (re)mappings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ....................................................... Backspace settings

      set backspace=indent,eol,start        " allow backspace in insert mode
      set whichwrap=b,s,h,l,<,>,[,]         " backspace and cursor keys wrap

    " .......................................................... Modal switching

      " no more reaching for shift or esc keys
      nmap ;           :
      vmap ;           :
      " imap kk        <ESC>
      " command mode insertion (paste) of current yank buffer
      cmap <C-v>       <C-r>"

    " ......................................................... Cursor movements

      " up/down by screen lines, not file lines
      nnoremap j       gj
      nnoremap k       gk
      " up/down by paragraph sentence
      nmap <C-S-Left>  {{)
      nmap <C-S-Right> })

      " insert mode local region cursor movements
      " <C-h> is overridden by auto-pairs delete <BS> when enabled
      " imap <C-h>     <Left>
      " imap <C-j>     <Down>
      " imap <C-k>     <Up>
      " imap <C-l>     <Right>

    " ............................................................. Disable keys

      " affirm vim modal usage but these keys are remapped below anyway :-)
      " (re-enabled for colemak keyboard as qwerty key cluster no longer valid)
      " imap <down>    <nop>
      " imap <left>    <nop>
      " imap <right>   <nop>
      " imap <up>      <nop>
      " nmap <down>    <nop>
      " nmap <left>    <nop>
      " nmap <right>   <nop>
      " nmap <up>      <nop>

      " avoid accidental F1
      " imap <F1>      <ESC>
      " nmap <F1>      <ESC>
      " vmap <F1>      <ESC>

  " Search and completion ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Search

      set gdefault                          " global by default
      set hlsearch                          " hilight searches by default
      set ignorecase                        " ignore case when searching
      set incsearch                         " find the next match as we type the search
      set showmatch                         " set show matching parenthesis
      set smartcase                         " ignore case if search pattern is all lowercase

      " use extended regex statements for searches (unfortunately, must set
      " manually for search and replace)
      nmap /         /\v
      vmap /         /\v
      " clear search highlight
      nmap <silent>\ :noh<CR>

      " tab to bracket pairs
      nmap <Tab>     %
      vmap <Tab>     %

      " repeat latest f,t,F,T see modal searching remaps above
      nnoremap ,,    ;
      vnoremap ,,    ;

    " ........................................................... Tab completion

      set wildignore=.cache/**,cache/**     " stuff to ignore when tab completing
      set wildignore+=*.class,*.bak,*.pyc,*.swp
      set wildignore+=Desktop/**
      set wildignore+=*.gem
      set wildignore+=*.gif,*.jpg,*.png
      set wildignore+=*.jar,*.tar.*,*.zip
      set wildignore+=log/**
      set wildignore+=*.o,*.obj,*~
      set wildignore+=tmp/**
      set wildmenu                          " enable ctrl-n and ctrl-p to scroll thru matches
      set wildmode=list:longest,full        " command <Tab> completion order
      set wildignore+=*vim/backups*

" setup.vim
