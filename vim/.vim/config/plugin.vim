" sdothum - 2016 (c) wtfpl

" Plugins
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugin manager ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      filetype off                          " safe startup (vundle requirement)

      " see https://github.com/junegunn/vim-plug
      " :PlugInstall/PlugUpdate/PlugClean
      " :PlugUpgrade (vim-plug)
      call plug#begin('~/.vim/plugged')

    " ................................................................... System

      let code   = { 'for' : ['c', 'cpp', 'haskell', 'lua', 'ruby', 'sh', 'snippets', 'vim'] }
      let prose  = { 'for' : ['draft', 'mail', 'markdown', 'note', 'vimwiki', 'wiki'] }

      Plug 'Shougo/vimproc.vim', { 'do' : 'make' }
      " Plug 'tpope/vim-scriptease'         " debugger
      " Plug 'tpope/vim-dispatch'           " launch async shell command
      Plug '~/.vim/custom/core'             " system wide primitives

  " Interface ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Layout

      " Plug 'junegunn/goyo.vim'            " distraction free mode
      " Plug 'altercation/vim-colors-solarized' breaks synIDattr with vim8
      Plug 'junegunn/limelight.vim'         " hyperfocus highlighting
      Plug 'bilalq/lite-dfm'                " distraction free mode
      Plug 'tyrannicaltoucan/vim-quantum'   " material design theme
      Plug 'lifepillar/vim-solarized8'      " solarized theme
      Plug '~/.vim/custom/ui'               " theme

    " ..................................................................... Info

      " Plug 'metakirby5/codi.vim'          " async evaluator
      " Plug 'machakann/vim-highlightedyank' " yank highlighter
      " Plug 'reedes/vim-wordy'             " word usage
      Plug 'itchyny/lightline.vim'          " statusline
      Plug 'majutsushi/tagbar', { 'on' : 'TagbarOpenAutoClose' } " ctags
      Plug 'bimlas/vim-eightheader'         " custom foldtext
      Plug '~/.vim/custom/info'             " statusline info

    " ................................................................... Keymap

      " Plug 'kana/vim-arpeggio'            " key chords
      Plug 'tpope/vim-rsi'                  " readline keybindings

    " ............................................................. Highlighting

      " Plug 'markonm/traces.vim'           " ex pattern/range highlghting
      " Plug 'romainl/vim-cool'             " auto clear search highlighting
      Plug 'nathanaelkane/vim-indent-guides', { 'on' : 'IndentGuidesToggle' } " colourized indent columns
      Plug 'sheerun/vim-polyglot'           " multilingual highlighting

  " Buffers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Management

      " Plug 'duff/vim-scratch'             " scratch buffer
      Plug 'chrisbra/NrrwRgn', { 'on' : 'NrrwrgnDo' } " visual block buffer

    " .......................................................... Version control

      " Plug 'tpope/vim-fugitive'           " github wrapper
      " Plug 'ludovicchabant/vim-lawrencium' " mercurial wrapper

    " ................................................................ Hypertext

      " Plug 'greyblake/vim-preview'        " markdown
      " Plug 'vimwiki/vimwiki'              " markdown wiki
      " Plug 'lervag/wiki'
      Plug 'junegunn/fzf.vim'               " fuzzy finder
      Plug 'Alok/notational-fzf-vim'        " fzf note search

    " ................................................................... Linter

      Plug 'w0rp/ale'                       " asynchronous lint engine

  " Editing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Navigation

      " Plug 'wellle/targets.vim'           " text objects
      Plug 'kshenoy/vim-signature'          " toggle marks
      Plug 'justinmk/vim-sneak'             " jump to location

    " .................................................................. History

      Plug 'sjl/gundo.vim', { 'on' : 'GundoToggle' } " undo
      Plug 'vim-scripts/YankRing.vim'       " paste (yank)

    " ............................................................... Formatting

      Plug 'scrooloose/nerdcommenter'       " toggle comment
      Plug 'junegunn/vim-easy-align'        " align text objects
      Plug 'reedes/vim-pencil'              " dynamic paragraph formatting
      Plug '~/.vim/custom/heading'          " heading formatter

    " ............................................................... Completion

      " Plug 'mattn/emmet-vim'              " html
      Plug 'jiangmiao/auto-pairs'           " insert/delete pairs
      Plug 'Shougo/neocomplete.vim'         " required by snippets
      Plug 'Shougo/neosnippet.vim'          " snippets
      Plug 'tpope/vim-endwise', code        " add 'end' statement
      Plug 'reedes/vim-litecorrect', prose  " spelling
      Plug 'tpope/vim-surround'             " pairwise c'hange, d'elete, y'ank

  " Plugins ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Configure

      call plug#end()
      unlet code prose

      filetype plugin on
      filetype indent on                    " required
      filetype on

" plugin.vim
