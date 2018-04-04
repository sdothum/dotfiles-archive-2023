" sdothum - 2016 (c) wtfpl

" Defaults
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Databases ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup default
        autocmd!
      augroup END

    " ..................................................................... Help

      autocmd default BufWinEnter *.txt,*.txt.gz if &filetype == 'help' | wincmd _ | endif

      nmap <F1>           :help<Space>
      imap <F1>           <C-o>:help<Space>
      vmap <F1>           <C-o>:help<Space>
      " list my function and leader key assignments
      nmap <silent><S-F1> :silent !term 'vmap' vmap<CR>
      imap <silent><S-F1> <C-o>:silent !term 'vmap' vmap<CR>
      vmap <silent><S-F1> <C-o>:silent !term 'vmap' vmap<CR>

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

    " .............................................................. Spell check

      set dictionary=/usr/share/dict/words
      set complete+=k                       " <C-p> to complete list word
      set keywordprg=dict
      set nospell                           " spell checking off by default for code
      " set thesaurus=/usr/share/dict/thesaurus
      " set complete+=s                     " disabled, selection list too long

      highlight SpellBad   guisp=red gui=undercurl,bold guifg=brown
      highlight SpellCap   guisp=red gui=undercurl,bold guifg=black
      highlight SpellRare  guisp=red gui=undercurl,bold guifg=blue
      highlight SpellLocal guisp=red gui=undercurl,bold guifg=green

  " Registers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Marks

      set viminfo='100,f1                   " save up to 100 marks, enable capital marks
      set viminfo^=%                        " remember info about open buffers on close
      " delete all marks in current buffer, see signature plugin
      " nmap <silent><leader>'' :delmarks!<CR>

      " ................................................................. Macros

      " quick q macro
      nnoremap <C-q>      @q
      " edit q macro
      nnoremap <leader>.. :<C-u><C-r><C-r>='let @q = '. string(getreg('q'))<CR><C-f><Left>
      " repeat last macro played @{0-9a-z":*}
      nnoremap Q          @@


  " Format ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ..................................................................... Line

      set formatoptions=qrn1j               " coding options
      let g:codeoptions = &formatoptions
      " double spaces at the end of a wrapped line, becomes <br> by markdown
      set nojoinspaces                      " force single spacing after sentence punctuation!
      set textwidth=80                      " normally 78-80, see autocmd for mail
      let g:linewidth = &textwidth          " see info#LongLines(), heading.vim

    " ..................................................................... Tabs

      set autoindent
      set copyindent                        " copy the previous indentation on autoindenting
      set expandtab                         " expand tabs into spaces, never use hard tabs!
      set shiftround                        " use multiple of shiftwidth when indenting with "<>"
      set shiftwidth=2                      " number of spaces for unindenting
      set nosmartindent                     " smartindent hash comments to beginning of line
      set smarttab
      set softtabstop=2
      set tabstop=2                         " global tab width

      cabbrev spaces set expandtab
      cabbrev tabs   set noexpandtab

  " Search / completion ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Search

      set gdefault                          " global by default
      set hlsearch                          " hilight searches by default
      set ignorecase                        " ignore case when searching
      set incsearch                         " find the next match as we type the search
      set magic                             " regex magic
      set showmatch                         " set show matching parenthesis
      set smartcase                         " ignore case if search pattern is all lowercase

      " tab to bracket pairs
      nmap     <Tab>      %
      vmap     <Tab>      %
      " disable magic and case sensitivity
      cmap     %%         \M
      cmap     ^^         \C

      " clear search highlight
      nmap     <silent>\  :noh<CR>

      " repeat latest f,t,F,T see modal searching remaps above
      nnoremap ,,         ;
      vnoremap ,,         ;

    " .................................................................. Replace

      nmap     %%         :%s
      vmap     %%         :s

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

" default.vim
