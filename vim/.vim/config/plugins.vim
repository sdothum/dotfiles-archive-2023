" sdothum - 2016 (c) wtfpl

" Plugins
" ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

  " Plugin settings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................................... Ack

      nmap <leader>A :Ack

    " ............................................................... Auto-pairs

      let g:AutoPairsMapBS = 1              " auto delete symbol pairs
      let g:AutoPairsMapSpace = 0           " disabled to make iabbrev work!

    " ................................................................... Ctrl-p

      let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
      let g:ctrlp_clear_cache_on_exit = 0   " enable cross-session caching
      let g:ctrlp_follow_symlinks = 1       " follow symbolic links
      let g:ctrlp_max_height = 30           " results window height
      let g:ctrlp_mruf_case_sensitive = 0   " avoid duplicate entries
      let g:ctrlp_mruf_max = 1000           " maximum mru entries to remember
      let g:ctrlp_open_multiple_files = 'i' " open multiple files in hidden buffers
      let g:ctrlp_open_new_file = 'v'       " <C-y> opens new file in vertical split
      let g:ctrlp_working_path_mode = 0     " default to current directory

      nmap <leader>b :CtrlPBuffer<CR>
      nmap <leader>M :CtrlPMixed<CR>
      nmap <leader>m :CtrlPMRU<CR>
      " nmap <leader>p     :CtrlP
      nmap <leader>p :CtrlP<CR>

    " ............................................................... Easy-align

      vmap <Enter>   <Plug>(EasyAlign)
      nmap <leader>a <Plug>(EasyAlign)

    " ............................................................... Easymotion

      let g:EasyMotion_do_mapping = 0       " disable default mappings
      let g:EasyMotion_smartcase = 1        " turn on case sensitive feature

      " bi-directional find motion
      " avoid conflict with regular vim search!
      nmap //                <Plug>(easymotion-sn)
      " one key binding
      " imap \               <C-o><Plug>(easymotion-s)
      " nmap \               <Plug>(easymotion-s)
      " 2 keystroke binding (backslash inhibits use in insert mode)
      imap <leader><leader>/ <C-o><Plug>(easymotion-s2)
      nmap <leader><leader>/ <Plug>(easymotion-s2)

      " line motions
      imap <leader><leader>j <C-o><Plug>(easymotion-j)
      nmap <leader><leader>j <Plug>(easymotion-j)
      imap <leader><leader>k <C-o><Plug>(easymotion-k)
      nmap <leader><leader>k <Plug>(easymotion-k)

      " word motions
      imap <leader><leader>E <C-o><Plug>(easymotion-gE)
      nmap <leader><leader>E <Plug>(easymotion-gE)
      imap <leader><leader>e <C-o><Plug>(easymotion-E)
      nmap <leader><leader>e <Plug>(easymotion-E)
      imap <leader><leader>W <C-o><Plug>(easymotion-B)
      nmap <leader><leader>W <Plug>(easymotion-B)
      imap <leader><leader>w <C-o><Plug>(easymotion-W)
      nmap <leader><leader>w <Plug>(easymotion-W)

    " .................................................................. Endwise

      " add fish shell syntax rule
      " see ~/.vim/plugged/vim-fish/syntax/fish.vim
      autocmd FileType fish
        \ let b:endwise_addition = 'end' |
        \ let b:endwise_words = 'function,begin,if,while,for,switch' |
        \ let b:endwise_syngroups = 'shFunctionKey'

    " ........................................................... Graphical undo

      let g:gundo_preview_bottom = 1
      " nmap <C-F7> :GundoToggle<CR>
      nmap <F3> :GundoToggle<CR>

    " ................................................................ Lightline

      " set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
      " set guifont=Inconsolata-dz\ for\ Powerline\ 9
      " set guifont=Liberation\ Mono\ for\ Powerline\ 9

      if has("gui_running")
        let g:lightline = {
          \  'colorscheme'  : 'solarized'
          \, 'separator'    : { 'left' : '⮀', 'right' : '' }
          \, 'subseparator' : { 'left' : '⮁', 'right' : '⮃' }
          \}
      else
        let g:lightline = {
          \  'colorscheme'  : 'solarized'
          \, 'separator'    : { 'left' : '', 'right' : '' }
          \, 'subseparator' : { 'left' : '/', 'right' : '/' }
          \}
      endif

      let g:lightline.active = {
        \  'left'  : [ [ 'mode', 'paste', ]
        \,             [ 'rootpath', 'basepath', 'filename' ]
        \,             [ 'readonly', 'wordcount', 'lineinfo', 'modified' ]
        \            ]
        \, 'right' : [ [ 'indent', 'spaces', 'filetype' ]
        \,             [ 'line', 'linecount' ]
        \,             [ 'atom', 'specialchar', 'column' ]
        \            ]
        \}

      let g:lightline.inactive = {
        \  'left'  : [ [ 'filename' ] ]
        \, 'right' : [ [ 'line', 'linecount' ] ]
        \}

      let g:lightline.tabline = {
        \  'left'  : [ [ 'tabs' ] ]
        \, 'right' : [ [ 'close' ] ]
        \}

      let g:lightline.component = {
        \  'mode'         : '%{lightline#mode()}'
        \, 'absolutepath' : '%F'
        \, 'relativepath' : '%f'
        \, 'rootpath'     : '%{expand("%:p") =~ ".*[/][^/]*[/][^/]*[/][^/]*" ? substitute(expand("%:p"), ".*[/]\\([^/]*\\)[/][^/]*[/][^/]*", "\\1", "") : ""}'
        \, 'basepath'     : '%{expand("%:p") =~ ".*[/][^/]*[/][^/]*" ? substitute(expand("%:p"), ".*[/]\\([^/]*\\)[/][^/]*", "\\1", "") : ""}'
        \, 'filename'     : '%t'
        \, 'modified'     : '%{&filetype == "help" ? "" : &modified ? "*" : &modifiable ? "" : "⎯"}'
        \, 'bufnum'       : '%n'
        \, 'paste'        : '%{&paste ? "PASTE" : ""}'
        \, 'readonly'     : '%{&filetype == "help" ? "" : &readonly ? "" : ""}'
        \, 'charvalue'    : '%b'
        \, 'charvaluehex' : '%B'
        \, 'fileencoding' : '%{strlen(&fenc) ? &fenc : &enc}'
        \, 'fileformat'   : '%{&fileformat}'
        \, 'filetype'     : '%{strlen(&filetype) ? &filetype : "no ft"}'
        \, 'percent'      : '%3p%%'
        \, 'percentwin'   : '%P'
        \, 'topbottom'    : '%{line("w0") == 1 ? (line("w$") == line("$") ? "ALL" : "Top") : line("w$") == line("$") ? "Bottom" : ""}'
        \, 'lineinfo'     : '%3l:%-2v'
        \, 'line'         : '%l'
        \, 'linecount'    : '%L'
        \, 'column'       : '%{getline(line(".")) == "" ? "" : virtcol(".")}'
        \, 'close'        : '%999X X '
        \, 'atom'         : '%{synIDattr(synID(line("."),col("."),1),"name")}'
        \}

      let g:lightline.component_visible_condition = {
        \  'rootpath'  : '(expand("%:p") =~ ".*[/][^/]*[/][^/]*[/][^/]*")'
        \, 'basepath'  : '(expand("%:p") =~ ".*[/][^/]*[/][^/]*")'
        \, 'modified'  : '(&filetype != "help" && (&modified || !&modifiable))'
        \, 'readonly'  : '(&filetype != "help" && &readonly)'
        \, 'paste'     : '&paste'
        \, 'topbottom' : '(line("w0") == 1 || line("w$") == line("$"))'
        \, 'column'    : '(getline(line(".")) != "")'
        \, 'atom'      : '(synIDattr(synID(line("."),col("."),1),"name") != "")'
        \}

      let g:lightline.component_function = {
        \  'indent'      : 'Indent'
        \, 'spaces'      : 'Spaces'
        \, 'wordcount'   : 'WordCount'
        \, 'lineinfo'    : 'LineInfo'
        \, 'specialchar' : 'SpecialChar'
        \}

      let g:lightline.mode_map = {
        \  'n'      : 'N'
        \, 'i'      : 'I'
        \, 'R'      : 'R'
        \, 'v'      : 'V'
        \, 'V'      : 'V-LINE'
        \, 'c'      : 'C'
        \, "\<C-v>" : 'V-BLOCK'
        \, 's'      : 'S'
        \, 'S'      : 'S-LINE'
        \, "\<C-s>" : 'S-BLOCK'
        \, '?'      : ' '
        \}

      " toggle lightline/default vim statusline
      imap <C-F10> <C-o>:call lightline#toggle()<CR>
      nmap <C-F10> :call lightline#toggle()<CR>

    " .................................................................. LiteDFM

      let g:lite_dfm_left_offset = 18
      " see LiteToggle functions.vim
      " nmap <silent><leader>z :LiteDFMToggle<CR>i<Esc>`^

    " ............................................................ Narrow region

      let g:nrrw_rgn_vert = 1               " open in vertical split buffer

    " ................................................................. Open URL

      nmap <leader>o :OpenUrl<CR>

    " ...................................................... Rainbow parentheses

      autocmd Syntax   * RainbowParenthesesLoadBraces
      autocmd Syntax   * RainbowParenthesesLoadChevrons
      autocmd Syntax   * RainbowParenthesesLoadRound
      autocmd Syntax   * RainbowParenthesesLoadSquare
      autocmd VimEnter * RainbowParenthesesToggle

    " ................................................................ Showmarks

      " let g:showmarks_enable = 0
      let g:showmarks_include = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY'

    " .......................................................... Solarized theme

      let g:solarized_contrast = 'high'     " preferred contrast level for light background
      let g:solarized_hitrail = 1
      let g:solarized_termcolors = 256
      let g:solarized_termtrans = 1         " terminal transparency (0) off (1) on

      colorscheme solarized
      if has("gui_running")
        set background=light
      else
        set background=dark
      endif
      syntax enable

    " ................................................................. Supertab

    set complete-=t

    " vimwiki uses <tab> for tables PLUS space leader benefits from use of tab
    let g:SuperTabMappingForward = '<C-Space>'
    let g:SuperTabMappingBackward = '<C-S-Space>'

    " ................................................................ Syntastic

      let g:syntastic_auto_jump = 0
      let g:syntastic_auto_loc_list = 1
      let g:syntastic_enable_signs = 1
      let g:syntastic_quiet_messages = {'level':'warnings'}

      " set statusline+=%#warningmsg#
      " set statusline+=%{SyntasticStatuslineFlag()}
      " set statusline+=%*

    " ................................................................... Tagbar

      " let g:tagbar_ctags_bin = 'ctags-exuberant'
      nmap <silent><leader>t :TagbarToggle<CR>

      " " see ctags.cnf
      " " from http://stackoverflow.com/questions/7037055/ctags-vimwiki-vim-and-tagbar-plugin
        " let g:tagbar_type_vimwiki = {
        "   \ 'ctagstype' : 'vimwiki',
        "   \ 'kinds'     : [
        "   \ 'h:header',
        "   \ ],
        "   \ 'sort'      : 0
        "   \ }

    " ................................................................ T-comment

      " see ToggleComment functions.vim
      nmap <leader>c <C-_><C-_>
      vmap <leader>c <C-_><C-_>

    " ............................................ Vim-litecorrect /  Vim-pencil

    augroup litecorrect
      autocmd!
      autocmd Filetype mail           call litecorrect#init()
      autocmd FileType markdown,mkd   call litecorrect#init()
      autocmd Filetype text           call litecorrect#init()
      autocmd Filetype vimwiki        call litecorrect#init()
    augroup END

    " let g:pencil#wrapModeDefault = 'hard' " default 'hard'
    " let g:pencil#textwidth = 72           " default 74
    " let g:pencil#joinspaces = 0           " 0=one_space (def), 1=two_spaces
    " let g:pencil#cursorwrap = 1           " 0=disable, 1=enable (def)
    " let g:pencil#autoformat = 1           " 0=manual, 1=auto (def)
    "
    " augroup pencil
    "   autocmd!
    "   autocmd Filetype mail         call pencil#init()
    "   autocmd FileType markdown,mkd call pencil#init()
    "   autocmd Filetype text         call pencil#init()
    "   autocmd Filetype vimwiki      call pencil#init()
    " augroup END

    " .................................................... Vim-shell / Vim-filer

      " requires make http://github.com/Shougo/vimproc
      let g:vimfiler_as_default_explorer = 1" explorer mode
      nmap <leader>!         :VimShell<CR>
      nmap <leader><leader>! :VimFiler<CR>

    " .................................................................. Vimwiki

      let g:vimwiki_list = [
        \  {
        \  'path'      : '~/vimwiki/thedarnedestthing/'
        \, 'path_html' : '~/vimwiki/thedarnedestthing/html/'
        \  }
        \, {
        \  'path'      : '~/vimwiki/thestory/'
        \, 'path_html' : '~/vimwiki/thestory/html/'
        \  }
        \, {
        \  'path'      : '~/vimwiki/truthordie/'
        \, 'path_html' : '~/vimwiki/truthordie/html/'
        \  }
        \, {
        \  'path'      : '~/vimwiki/shadowsandlight/'
        \, 'path_html' : '~/vimwiki/shadowsandlight/html/'
        \  }
        \, {
        \  'path'      : '~/vimwiki/healing/'
        \, 'path_html' : '~/vimwiki/healing/html/'
        \  }
        \, {
        \  'path'      : '~/vimwiki/colophon/'
        \, 'path_html' : '~/vimwiki/colophon/html/'
        \  }
        \, {
        \  'path'      : '~/vimwiki/notes/'
        \, 'path_html' : '~/vimwiki/notes/html/'
        \  }
        \]

      " header highlighting
      highlight VimwikiHeader1 guifg=#d70000
      highlight VimwikiHeader2 guifg=#af005f
      highlight VimwikiHeader3 guifg=#5f5faf
      highlight VimwikiHeader4 guifg=#0087ff
      highlight VimwikiHeader5 guifg=#00afaf
      highlight VimwikiHeader6 guifg=#5f8700

      " override highlight link
      autocmd Filetype vimwiki call VimWikiLink()

      " force <BS> mapping.. default assignment overwritten somewhere (auto-pairs ??)
      " nmap <BS> :call vimwiki#base#go_back_link()<CR>

      autocmd BufEnter *.wiki   set filetype=vimwiki
      autocmd Filetype markdown setlocal nocp spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      autocmd Filetype vimwiki  setlocal nocp spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      " cannot trap s:setup_buffer_leave() to avoid initialization error on 1st link
      " see arch install patch to initialize s:vimwiki_autowriteall

    " ................................................................. Yankring

      let g:yankring_default_menu_mode = 1  " menu on with no shortcut
      let g:yankring_dot_repeat_yank = 1    " allow repeating yankring action
      let g:yankring_enabled = 1            " disable yankring because of macro conflict
      let g:yankring_window_height = 30     " horizontal window height

      nmap Y              :<C-U>YRYankCount 'y$'<CR>
      " nmap <silent><F7> :YRShow<cr>
      " imap <silent><F7> <ESC>:YRShow<cr>
      nmap <silent><F2>   :YRShow<cr>
      imap <silent><F2>   <ESC>:YRShow<cr>

" plugins.vim
