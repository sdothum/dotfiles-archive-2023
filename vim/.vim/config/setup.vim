" sdothum - 2016 (c) wtfpl

" Setup
" ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................................... Vim

      let mapleader=','                     " remap <leader>
      let g:mapleader= ','

      " open help in maximum horizontal split
      if $HOST == 'monad'
        autocmd BufWinEnter *.txt,*.txt.gz if &filetype == 'help' | wincmd _ | endif
      endif

    " .......................................................... OS and hardware

      set lazyredraw                        " don't redraw while executing macros (good performance config)
      " set modelines=0                       " prevent modeline secrurity hole
      set modelines=1
      set mouse=a                           " use mouse for scrolling, jumping cursor, selecting by character/line!
      set shell=/bin/sh                     " required for plugin system call dependencies
      set title                             " change the terminal's title
      set ttyfast
      set timeout timeoutlen=1000 ttimeoutlen=100
      " set cryptmethod=blowfish              " encryption method

    " .............................................................. Config file

      " quickly edit/reload the vimrc file
      nmap <silent><leader>ev :edit $MYVIMRC<CR>
      " must switch to .vimrc first for unknown reason.. (bug?)
      nmap <silent><leader>sv :buffer .vimrc<CR>:autocmd!<CR>:source $MYVIMRC<CR>

      " load .vimrc after save
      augroup reload_vimrc
        autocmd!
        " autocmd BufWritePost $MYVIMRC        source $MVIMRC
        autocmd bufwritepost $MYVIMRC nested source $MYVIMRC 
        autocmd BufWritePost ~/.vim/config/* buffer $MYVIMRC | source $MYVIMRC
        " PluginUpdate and config reload loses filetype, restore
      augroup END
      autocmd BufWinEnter *.vim              set filetype=vim

    " ............................................................... Swap files

      set nobackup
      set directory=~/tmp,/tmp              " keep swap files in one location
      set noswapfile                        " turn off swap files
      set nowritebackup

    "  .......................................................... Undo / history

      " keep persistent undo history across sessions, by storing in file
      silent !mkdir ~/.vim/backups 2>/dev/null
      set history=1000                      " store lots of :cmdline history
      set undodir=~/.vim/backups
      set undofile
      set undolevels=1000                   " maximum number of changes that can be undone
      set undoreload=10000                  " maximum number lines to save for undo on a buffer reload
      " easier redo
      map U <C-r>

  " Keyboard (re)mappings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ....................................................... Backspace settings

      set backspace=indent,eol,start        " allow backspace in insert mode
      set whichwrap=b,s,h,l,<,>,[,]         " backspace and cursor keys wrap

    " .......................................................... Modal switching

      " no more reaching for shift or esc keys
      nmap ;               :
      vmap ;               :
      " imap kk            <ESC>
      " command mode insertion (paste) of current yank buffer
      cmap <C-v>           <C-r>"
      " clear search highlight and command line messages
      nmap <leader><Space> :nohlsearch<Bar>echo<CR>

    " ......................................................... Cursor movements

      " up/down by screen lines, not file lines
      nnoremap j           gj
      nnoremap k           gk
      " up/down by paragraph sentence
      nmap <C-S-Left>      {{)
      nmap <C-S-Right>     })

      " insert mode local region cursor movements
      " <C-h> is overridden by auto-pairs delete <BS> when enabled
      imap <C-h>           <Left>
      imap <C-j>           <Down>
      imap <C-k>           <Up>
      imap <C-l>           <Right>

    " ............................................................. Disable keys

      " affirm vim modal usage but these keys are remapped below anyway :-)
      " (re-enabled for colemak keyboard as qwerty key cluster no longer valid)
      " imap <down>  <nop>
      " imap <left>  <nop>
      " imap <right> <nop>
      " imap <up>    <nop>
      " nmap <down>  <nop>
      " nmap <left>  <nop>
      " nmap <right> <nop>
      " nmap <up>    <nop>

      " avoid accidental F1
      " imap <F1> <ESC>
      " nmap <F1> <ESC>
      " vmap <F1> <ESC>

  " Search and completion ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Search

      set gdefault                          " global by default
      set hlsearch                          " hilight searches by default
      set ignorecase                        " ignore case when searching
      set incsearch                         " find the next match as we type the search
      set showmatch                         " set show matching parenthesis
      set smartcase                         " ignore case if search pattern is all lowercase, case-sensitive otherwise

      " use extended regex statements for searches (unfortunately, must set
      " manually for search and replace)
      nmap /     /\v
      vmap /     /\v
      " tab to bracket pairs
      nmap <Tab> %
      vmap <Tab> %

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
      set wildmode=list:longest,full        " command <Tab> completion, list matches, then longest common part, then all
      set wildignore+=*vim/backups*

" setup.vim
