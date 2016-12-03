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
      " Plug 'kien/ctrlp.vim'
      Plug 'chrisbra/NrrwRgn'
      " Plug 'duff/vim-scratch'

    " ................................................................... Coding

      Plug 'metakirby5/codi.vim'
      " Plug 'mattn/emmet-vim'
      " Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
      " Plug 'scrooloose/syntastic'
      " Plug 'majutsushi/tagbar'
      Plug 'tomtom/tcomment_vim'
      Plug 'tomtom/tlib_vim'
      Plug 'MarcWeber/vim-addon-mw-utils'
      Plug 'tpope/vim-endwise'
      Plug 'garbas/vim-snipmate'

    " .................................................. Completion / navigation

      " Plug 'vim-scripts/matchit.zip'
      " Plug 'SearchComplete'
      Plug 'justinmk/vim-sneak'
      Plug 'ervandew/supertab'
      " Plug 'Lokaltog/vim-easymotion'
      Plug 'kshenoy/vim-signature'

    " .................................................................. Editing

      Plug 'sjl/gundo.vim'
      " Plug 'tpope/vim-abolish'
      " Plug 'tpope/vim-repeat'
      Plug 'vim-scripts/YankRing.vim'

    " ............................................................... Formatting

      Plug 'auto-pairs'
      " Plug 'Townk/vim-autoclose'
      Plug 'junegunn/vim-easy-align'
      Plug 'bimbalaszlo/vim-eightheader'
      Plug 'tpope/vim-surround'

    " ............................................................. Productivity

      " Plug 'vim-scripts/calendar.vim--Matsumoto'
      " Plug 'cwoac/nvim'
      Plug 'vim-scripts/openurl.vim'
      " Plug 'tpope/vim-fugitive'
      " Plug 'ludovicchabant/vim-lawrencium'

    " ...................................................... Syntax highlighting

      Plug 'kien/rainbow_parentheses.vim'
      Plug 'dag/vim-fish'
      Plug 'plasticboy/vim-markdown'
      " Plug 'tpope/vim-markdown'
      " Plug 'greyblake/vim-preview'
      Plug 'slim-template/vim-slim'

    " ................................................................... System

      Plug 'ramele/agrep'
      " Plug 'Shougo/unite.vim'
      " Plug 'kana/vim-arpeggio'
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
