" sdothum - 2016 (c) wtfpl

" Plugins
" ══════════════════════════════════════════════════════════════════════════════

" Plugin manager _______________________________________________________________

" ................................................................... Initialize
filetype off  " safe startup (vundle requirement)

" see https://github.com/junegunn/vim-plug
" :PlugInstall/PlugUpdate/PlugClean
" :PlugUpgrade (vim-plug)
call plug#begin('~/.vim/plugged')

" ....................................................................... System
let code  = { 'for' : ['c*', 'haskell', 'julia', 'lua', 'ruby', '*sh', 'snippets', 'vim'] }
let prose = { 'for' : ['draft', 'mail', 'markdown', 'note', 'wiki', 'html'] }

Plug 'Shougo/vimproc.vim', { 'do' : 'make' }
" Plug 'tpope/vim-scriptease'          " debugger
" Plug 'tpope/vim-dispatch'            " launch async shell command

" User Interface _______________________________________________________________

" ....................................................................... Layout
Plug 'junegunn/limelight.vim'          " hyperfocus highlighting
Plug 'bilalq/lite-dfm'                 " distraction free mode
Plug 'TaDaa/vimade'                    " hyperfocus highlghting for buffers

" ......................................................................... Info
" Plug 'metakirby5/codi.vim'           " async evaluator
" Plug 'reedes/vim-wordy'              " word usage
Plug 'majutsushi/tagbar'               " ctags
Plug 'lvht/tagbar-markdown', prose     " markdown for tagbar
Plug 'bimlas/vim-eightheader'          " custom foldtext

" ....................................................................... Keymap
" Plug 'kana/vim-arpeggio'             " key chords
" Plug 'Jorengarenar/pseudoClip'       " clipboard registers
" Plug 'tpope/vim-rsi'                 " readline keybindings
Plug 'tpope/vim-repeat'                " dot plugin

" ................................................................. Highlighting
" Plug 'JuliaEditorSupport/julia-vim'  " julia programming language
" Plug 'markonm/traces.vim'            " ex pattern/range highlghting
" Plug 'romainl/vim-cool'              " auto clear search highlighting
Plug 'itchyny/vim-cursorword', prose   " word highlighting
Plug 'nathanaelkane/vim-indent-guides', code  " colourized indent columns
Plug 'sheerun/vim-polyglot'            " multilingual highlighting
Plug 'machakann/vim-highlightedyank'   " yank highlghting

" Buffers ______________________________________________________________________

" ................................................................... Management
" Plug 'duff/vim-scratch'              " scratch buffer
" Plug 'simeji/winresizer'             " resize windows
Plug 'chrisbra/NrrwRgn'                " visual block buffer

" .............................................................. Version control
" Plug 'tpope/vim-fugitive'            " github wrapper
" Plug markwoodhall/vim-mercurial      " mercurial wrapper
Plug 'mhinz/vim-signify'               " vcs diff

" .................................................................... Hypertext
" Plug 'dyng/ctrlsf.vim'               " ctrl-shift-f clone
" Plug 'lervag/wiki'                   " vimwiki clone
Plug 'junegunn/fzf'                    " fuzzy finder (separate plug install to avoid update errors)
Plug 'junegunn/fzf.vim'                " fuzzy finder
Plug 'alok/notational-fzf-vim'         " fzf content search

" ...................................................................... History
Plug 'simnalamburt/vim-mundo', { 'on' : 'MundoToggle' }  " undo
Plug 'vim-scripts/YankRing.vim'        " paste (yank)

" ....................................................................... Linter
Plug 'w0rp/ale', code                  " asynchronous lint engine

" Editing ______________________________________________________________________

" ................................................................... Navigation
" Plug 'wellle/targets.vim'            " text objects
Plug 'Konfekt/FastFold'                " update folds
Plug 'masukomi/vim-markdown-folding', prose  " markdown foldexpr
Plug 'kshenoy/vim-signature'           " toggle marks
Plug 'justinmk/vim-sneak'              " jump to location

" ................................................................. Text objects
" Plug 'reedes/vim-textobj-quote'      " typographic characters
Plug 'tpope/vim-surround'              " pairwise c'hange, d'elete, y'ank
Plug 'glts/vim-textobj-comment', code  " 'c' select comment
Plug 'kana/vim-textobj-fold'           " 'z' select fold
Plug 'kana/vim-textobj-function', code " 'f' select function
Plug 'kana/vim-textobj-indent', code   " 'i' select with same indent level
Plug 'kana/vim-textobj-lastpat'        " '/' select last search pattern
Plug 'kana/vim-textobj-user'           " dependency for vim-textobj's

" ................................................................... Formatting
Plug 'scrooloose/nerdcommenter', code  " toggle comment
Plug 'junegunn/vim-easy-align'         " align text objects
Plug 'reedes/vim-pencil', prose        " dynamic paragraph formatting

" ................................................................... Completion
" Plug 'mattn/emmet-vim'               " html
Plug 'jiangmiao/auto-pairs'            " insert/delete pairs
Plug 'Shougo/neocomplete.vim'          " required by snippets
Plug 'Shougo/neosnippet.vim'           " snippets
Plug 'tpope/vim-endwise', code         " add 'end' statement
Plug 'reedes/vim-litecorrect', prose   " autocorrections

" Plugin completion ____________________________________________________________

" .................................................................... Configure
call plug#end()
unlet code prose

filetype plugin on
filetype indent on  " required
filetype on

" plugin.vim
