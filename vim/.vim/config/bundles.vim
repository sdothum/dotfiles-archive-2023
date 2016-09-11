" sdothum - 2016 (c) wtfpl

" Bundles
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugins ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " bundles.vim must be sourced before plugins.vim

    filetype off                            " safe startup (vundle requirement)

    " see https://github.com/junegunn/vim-plug
    " :PlugInstall/PlugUpdate/PlugClean
    " :PlugUpgrade (vim-plug)
    call plug#begin('~/.vim/plugged')

    " ...................................................................... GUI

      Plug 'junegunn/goyo.vim'
      Plug 'itchyny/lightline.vim'
      Plug 'junegunn/limelight.vim'
      Plug 'bilalq/lite-dfm'
      Plug 'altercation/vim-colors-solarized'

    " ........................................................ Buffer management

      " Plug 'mileszs/ack.vim'
      Plug 'kien/ctrlp.vim'
      " Plug 'chrisbra/NrrwRgn'
      " Plug 'duff/vim-scratch'

    " ................................................................... Coding

      Plug 'tpope/vim-endwise'
      Plug 'msanders/snipmate.vim'
      " Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
      " Plug 'scrooloose/syntastic'
      " Plug 'majutsushi/tagbar'
      Plug 'tomtom/tcomment_vim'
      " Plug 'mattn/emmet-vim'

    " .................................................. Completion / navigation

      " Plug 'SearchComplete'
      Plug 'ervandew/supertab'
      Plug 'Lokaltog/vim-easymotion'
      " Plug 'vim-scripts/matchit.zip'
      Plug 'kshenoy/vim-signature'

    " .................................................................. Editing

      Plug 'sjl/gundo.vim'
      " Plug 'tpope/vim-abolish'
      " Plug 'tpope/vim-repeat'
      Plug 'vim-scripts/YankRing.vim'

    " ............................................................... Formatting

      " Plug 'Townk/vim-autoclose'
      Plug 'auto-pairs'
      Plug 'junegunn/vim-easy-align'
      Plug 'tpope/vim-surround'

    " ............................................................. Productivity

      " Plug 'tpope/vim-fugitive'
      " Plug 'vim-scripts/calendar.vim--Matsumoto'
      " Plug 'ludovicchabant/vim-lawrencium'
      Plug 'vim-scripts/openurl.vim'

    " ...................................................... Syntax highlighting

      Plug 'kien/rainbow_parentheses.vim'
      Plug 'dag/vim-fish'
      " Plug 'tpope/vim-markdown'
      Plug 'plasticboy/vim-markdown'
      " Plug 'greyblake/vim-preview'
      Plug 'slim-template/vim-slim'

    " ................................................................... System

      " Plug 'kana/vim-arpeggio'
      " Plug 'Shougo/unite.vim'
      " Plug 'Shougo/vimfiler.vim'
      " Plug 'Shougo/vimshell.vim'

    " .................................................................. Writing
      Plug 'reedes/vim-litecorrect'
      " Plug 'reedes/vim-pencil'
      " Plug 'reedes/vim-wordy'
      Plug 'vimwiki/vimwiki'

    call plug#end()

    filetype plugin on
    filetype indent on                      " required
    filetype on

" bundles.vim
