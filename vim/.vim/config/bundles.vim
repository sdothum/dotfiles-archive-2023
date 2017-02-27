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
      Plug 'itchyny/lightline.vim'
      Plug 'junegunn/limelight.vim'
      Plug 'bilalq/lite-dfm'
      Plug 'altercation/vim-colors-solarized'
      Plug 'bimbalaszlo/vim-eightheader'

    " ................................................................... Keymap

      " Plug 'kana/vim-arpeggio'

    " ...................................................... Syntax highlighting

      Plug 'kien/rainbow_parentheses.vim'
      Plug 'dag/vim-fish'
      Plug 'plasticboy/vim-markdown'
      Plug 'slim-template/vim-slim'

  " Files ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Buffer management

      " Plug 'kien/ctrlp.vim'
      Plug 'chrisbra/NrrwRgn'
      " Plug 'duff/vim-scratch'

    " ............................................................. File / shell

      " Plug 'mileszs/ack.vim'
      Plug 'ramele/agrep'
      " Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
      " Plug 'junegunn/fzf.vim'
      " Plug 'Shougo/unite.vim'
      " Plug 'Shougo/vimfiler.vim'
      " Plug 'Shougo/vimshell.vim'

    " .......................................................... Version control

      " Plug 'tpope/vim-fugitive'
      " Plug 'ludovicchabant/vim-lawrencium'

  " Coding ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ..................................................................... Atom

      Plug 'tomtom/tcomment_vim'
      Plug 'tomtom/tlib_vim'
      Plug 'tpope/vim-surround'

    " ..................................................................... HTML

      " Plug 'mattn/emmet-vim'
      " Plug 'rstacruz/sparkup', {'rtp': 'vim/'}

    " ............................................................... Completion

      Plug 'auto-pairs'
      Plug 'tpope/vim-endwise'
      if ext == 'wiki'
        Plug 'lifepillar/vim-mucomplete'
      else
        Plug 'maxboisvert/vim-simple-complete'
      endif

    " .......................................................... Syntax Checking

      " Plug 'w0rp/ale'
      " Plug 'metakirby5/codi.vim'
      " Plug 'scrooloose/syntastic'

  " Editing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................... Search / navigation

      " Plug 'vim-scripts/matchit.zip'
      " Plug 'SearchComplete'
      " Plug 'majutsushi/tagbar'
      Plug 'easymotion/vim-easymotion'
      Plug 'kshenoy/vim-signature'
      Plug 'justinmk/vim-sneak'

    " ............................................................... Formatting

      " Plug 'godlygeek/tabular'
      Plug 'junegunn/vim-easy-align'

    " ............................................................. Paste / undo

      Plug 'sjl/gundo.vim'
      " Plug 'tpope/vim-repeat'
      Plug 'vim-scripts/YankRing.vim'

    " ............................................................. Substitution

      Plug 'tpope/vim-abolish'
      Plug 'garbas/vim-snipmate'
      Plug 'MarcWeber/vim-addon-mw-utils'

  " Tools ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Notes / wiki

      " Plug 'cwoac/nvim'
      Plug 'vimwiki/vimwiki'

    " .................................................................. Writing

      Plug 'reedes/vim-litecorrect'
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
