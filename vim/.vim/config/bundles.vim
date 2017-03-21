" sdothum - 2016 (c) wtfpl

" Bundles
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugin manager ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " bundles.vim must be sourced before plugins.vim

    " autocompletion plugin dependent on filetype
    let ext = expand('%:e')

    filetype off                            " safe startup (vundle requirement)

    " see https://github.com/junegunn/vim-plug
    " :PlugInstall/PlugUpdate/PlugClean
    " :PlugUpgrade (vim-plug)
    call plug#begin('~/.vim/plugged')

  " User Interface ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................................... GUI

      " Plug 'junegunn/goyo.vim'
      Plug 'itchyny/lightline.vim'          " statusline
      Plug 'junegunn/limelight.vim'         " hyperfocus highlighting
      Plug 'bilalq/lite-dfm'                " distraction free mode
      " Plug 'altercation/vim-colors-solarized' breaks synIDattr with vim8
      Plug 'lifepillar/vim-solarized8'      " colour theme

    " ................................................................... Keymap

      " Plug 'kana/vim-arpeggio'

    " ...................................................... Syntax highlighting

      Plug 'kien/rainbow_parentheses.vim'   " parentheses highlighting
      Plug 'dag/vim-fish'                   " shell highlighting
      Plug 'plasticboy/vim-markdown'        " markdown highlighting
      Plug 'slim-template/vim-slim'         " dsl highlighting

  " Files ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Buffer management

      " Plug 'kien/ctrlp.vim'
      Plug 'chrisbra/NrrwRgn'               " edit selected text region
      " Plug 'duff/vim-scratch'

    " ..................................................................... Grep

      " Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --key-bindings' } installed via pacman
      " Plug 'junegunn/fzf.vim' doesn't integrate well with gvim

    " .......................................................... Version control

      " Plug 'tpope/vim-fugitive'
      " Plug 'ludovicchabant/vim-lawrencium'

  " Coding ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ..................................................................... Aids

      Plug 'bimlas/vim-eightheader'         " custom foldtext
      Plug 'nathanaelkane/vim-indent-guides' " colourized indent columns
      Plug 'tpope/vim-surround'             " pairwise c'hange, d'elete, y'ank

    " ............................................................... Commenting

      Plug 'tomtom/tcomment_vim'            " toggle comment
      Plug 'tomtom/tlib_vim'                " toggle comment library

    " ..................................................................... HTML

      " Plug 'mattn/emmet-vim'
      " Plug 'rstacruz/sparkup', {'rtp': 'vim/'}

    " ............................................................... Completion

      Plug 'auto-pairs'                     " insert/delete pairs
      Plug 'tpope/vim-endwise'              " add 'end' statement
      if ext == 'wiki'
        Plug 'lifepillar/vim-mucomplete'    " tab completion
      else
        Plug 'maxboisvert/vim-simple-complete' " enter completion
      endif

    " .......................................................... Syntax Checking

      " Plug 'w0rp/ale'
      " Plug 'metakirby5/codi.vim'
      " Plug 'scrooloose/syntastic'

  " Editing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................... Search / navigation

      " Plug 'majutsushi/tagbar'
      Plug 'kshenoy/vim-signature'          " toggle marks
      Plug 'justinmk/vim-sneak'             " jump to location

    " ............................................................... Formatting

      " Plug 'godlygeek/tabular'
      Plug 'junegunn/vim-easy-align'        " align text objects

    " ............................................................. Paste / undo

      Plug 'sjl/gundo.vim'                  " undo history
      " Plug 'tpope/vim-repeat'
      Plug 'vim-scripts/YankRing.vim'       " yank history

    " ................................................................. Snippets

      Plug 'garbas/vim-snipmate'            " snippets
      Plug 'MarcWeber/vim-addon-mw-utils'   " snippet library

  " Tools ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Notes / wiki

      " Plug 'cwoac/nvim'
      Plug 'vimwiki/vimwiki'                " wiki / markdown

    " .................................................................. Writing

      Plug 'reedes/vim-litecorrect'         " auto correction
      " Plug 'reedes/vim-pencil'
      " Plug 'reedes/vim-wordy'

    " .................................................................. Preview

      " Plug 'vim-scripts/openurl.vim'
      " Plug 'greyblake/vim-preview'

    call plug#end()

    filetype plugin on
    filetype indent on                      " required
    filetype on

" bundles.vim
