" sdothum - 2016 (c) wtfpl

" Bundles
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugin manager ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " bundles.vim must be sourced before plugins.vim

    " autocompletion plugin dependent on filetype
    let ext = expand('%:e')

    filetype off                                " safe startup (vundle requirement)

    " see https://github.com/junegunn/vim-plug
    " :PlugInstall/PlugUpdate/PlugClean
    " :PlugUpgrade (vim-plug)
    call plug#begin('~/.vim/plugged')

  " Interface ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Layout

      " Plug 'junegunn/goyo.vim'                " distraction free mode
      " Plug 'altercation/vim-colors-solarized' breaks synIDattr with vim8
      Plug 'junegunn/limelight.vim'             " hyperfocus highlighting
      Plug 'bilalq/lite-dfm'                    " distraction free mode
      Plug 'lifepillar/vim-solarized8'          " colour theme

    " ..................................................................... Info

      " Plug 'reedes/vim-wordy'                 " word usage
      Plug 'itchyny/lightline.vim'              " statusline
      Plug 'bimlas/vim-eightheader'             " custom foldtext
      Plug 'nathanaelkane/vim-indent-guides'    " colourized indent columns

    " ................................................................... Keymap

      " Plug 'kana/vim-arpeggio'                " key chords

    " ...................................................... Syntax highlighting

      Plug 'kien/rainbow_parentheses.vim'       " parentheses highlighting
      Plug 'dag/vim-fish'                       " shell highlighting
      Plug 'plasticboy/vim-markdown'            " markdown highlighting
      Plug 'slim-template/vim-slim'             " dsl highlighting

    " .......................................................... Syntax Checking

      " Plug 'metakirby5/codi.vim'              " async evaluator
      " Plug 'scrooloose/syntastic'             " lint
      Plug 'reedes/vim-litecorrect'             " auto correction

  " Buffers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Management

      " Plug 'kien/ctrlp.vim'                   " fuzzy finder
      " Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --key-bindings' } installed via pacman
      " Plug 'junegunn/fzf.vim' doesn't integrate well with gvim
      " Plug 'majutsushi/tagbar'                " tags buffer
      " Plug 'duff/vim-scratch'                 " scratch buffer
      Plug 'chrisbra/NrrwRgn'                   " visual block buffer

    " .......................................................... Version control

      " Plug 'tpope/vim-fugitive'               " github wrapper
      " Plug 'ludovicchabant/vim-lawrencium'    " mercurial wrapper

    " .................................................................... Files

      " Plug 'cwoac/nvim'                       " notational velocity
      " Plug 'greyblake/vim-preview'            " markdown
      Plug 'vimwiki/vimwiki'                    " markdown wiki

  " Editing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Navigation

      Plug 'kshenoy/vim-signature'              " toggle marks
      Plug 'justinmk/vim-sneak'                 " jump to location

    " .................................................................. History

      Plug 'sjl/gundo.vim'                      " undo
      Plug 'vim-scripts/YankRing.vim'           " paste (yank)

    " ............................................................... Formatting

      " Plug 'godlygeek/tabular'                " markdown tables
      Plug 'tomtom/tcomment_vim'                " toggle comment
      Plug 'tomtom/tlib_vim'                    " toggle comment library
      Plug 'junegunn/vim-easy-align'            " align text objects
      Plug 'reedes/vim-pencil'                  " dynamic paragraph formatting

    " ............................................................... Completion

      " Plug 'mattn/emmet-vim'                  " html
      Plug 'auto-pairs'                         " insert/delete pairs
      Plug 'tpope/vim-endwise'                  " add 'end' statement
      if ext == 'wiki'
        Plug 'lifepillar/vim-mucomplete'        " tab completion
      else
        Plug 'maxboisvert/vim-simple-complete'  " enter completion
      endif
      Plug 'garbas/vim-snipmate'                " snippets
      Plug 'MarcWeber/vim-addon-mw-utils'       " snippet library
      Plug 'tpope/vim-surround'                 " pairwise c'hange, d'elete, y'ank

    call plug#end()

    filetype plugin on
    filetype indent on                          " required
    filetype on

" bundles.vim
