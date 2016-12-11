" sdothum - 2016 (c) wtfpl

" Plugins
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugin settings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup plugin
        autocmd!
      augroup END

    " ...................................................................... Ack

      nmap <leader>A :Ack

    " ............................................................... Auto-pairs

      let g:AutoPairsMapBS    = 1           " auto delete symbol pairs
      let g:AutoPairsMapSpace = 0           " disabled to make iabbrev work!

    " ................................................................... Ctrl-p

      " let g:ctrlp_cache_dir           = $HOME . '/.cache/ctrlp'
      " let g:ctrlp_clear_cache_on_exit = 0    " enable cross-session caching
      " let g:ctrlp_follow_symlinks     = 1    " follow symbolic links
      " let g:ctrlp_max_height          = 30   " results window height
      " let g:ctrlp_mruf_case_sensitive = 0    " avoid duplicate entries
      " let g:ctrlp_mruf_max            = 1000 " maximum mru entries to remember
      " let g:ctrlp_open_multiple_files = 'i'  " open multiple files in hidden buffers
      " let g:ctrlp_open_new_file       = 'v'  " <C-y> opens new file in vertical split
      " let g:ctrlp_working_path_mode   = 0    " default to current directory
      "
      " " cannot silence vim's default file info message as with buffer keymaps
      " " see autocmd cursorhold in setup.vim for delayed clear
      " nmap <silent><leader>b :silent CtrlPBuffer<CR>
      " nmap <silent><leader>M :silent CtrlPMixed<CR>
      " nmap <silent><leader>m :silent CtrlPMRU<CR>
      " nmap <silent><leader>B :silent CtrlP<CR>

    " ............................................................... Easy-align

      let g:easy_align_delimiters =
        \{
        \  '>': { 'pattern': '>>\|=>\|>' }
        \, '^': { 'pattern': '=', 'left_margin': 0, 'right_margin': 0, 'align': 'right' }
        \, '(': { 'pattern': '(', 'left_margin': 1, 'right_margin': 0 }
        \, ')': { 'pattern': ')', 'left_margin': 0 }
        \}

      vmap <Enter>   <Plug>(EasyAlign)
      nmap <leader>a <Plug>(EasyAlign)

    " ............................................................... Easymotion

      " let g:EasyMotion_do_mapping = 0       " disable default mappings
      " let g:EasyMotion_smartcase = 1        " turn on case sensitive feature
      "
      " " bi-directional find motion
      " " avoid conflict with regular vim search!
      " nmap <leader>/         <Plug>(easymotion-sn)
      " " one key binding
      " " imap \               <C-o><Plug>(easymotion-s)
      " nmap <leader>\         <Plug>(easymotion-s)
      " " 2 keystroke binding (backslash inhibits use in insert mode)
      " imap <leader><leader>/ <C-o><Plug>(easymotion-s2)
      " nmap <leader><leader>/ <Plug>(easymotion-s2)
      "
      " " line motions
      " imap <leader><leader>n <C-o><Plug>(easymotion-j)
      " nmap <leader><leader>n <Plug>(easymotion-j)
      " imap <leader><leader>l <C-o><Plug>(easymotion-k)
      " nmap <leader><leader>l <Plug>(easymotion-k)
      "
      " " word motions
      " imap <leader><leader>E <C-o><Plug>(easymotion-gE)
      " nmap <leader><leader>E <Plug>(easymotion-gE)
      " imap <leader><leader>e <C-o><Plug>(easymotion-E)
      " nmap <leader><leader>e <Plug>(easymotion-E)
      " imap <leader><leader>W <C-o><Plug>(easymotion-B)
      " nmap <leader><leader>W <Plug>(easymotion-B)
      " imap <leader><leader>w <C-o><Plug>(easymotion-W)
      " nmap <leader><leader>w <Plug>(easymotion-W)

    " .............................................................. Eightheader

      " justified fold headers
      let &foldtext = "EightHeaderFolds(
        \ '\\=s:fullwidth',
        \ 'left',
        \ [ repeat( '  ', v:foldlevel - 1 ), ' ', '' ],
        \ '\\= s:foldlines . \" lines\"',
        \ '' )"

    " .................................................................. Endwise

      " add fish shell syntax rule
      " see ~/.vim/plugged/vim-fish/syntax/fish.vim
      autocmd plugin FileType fish
        \  let b:endwise_addition  = 'end'
        \| let b:endwise_words     = 'function,begin,if,while,for,switch'
        \| let b:endwise_syngroups = 'shFunctionKey'

    " ........................................................... Graphical undo

      let g:gundo_width          = 30
      let g:gundo_preview_bottom = 1
      let g:gundo_preview_height = 20

      " nmap <silent><C-F7>  :GundoToggle<CR>
      " nmap <silent><F3>    :GundoToggle<CR>
      nmap <silent><leader>u :GundoToggle<CR>

      autocmd plugin BufEnter __Gundo__ setlocal numberwidth=3 foldcolumn=0

    " ................................................................ Lightline


      let g:matchspace = ''                 " see ToggleSpaces coding.vim
      let s:powerline  = 0                  " powerline symbol slant (0) lower (1) upper

      if has("gui_running")
        " can't use GoyoFT as plugin is initialized before buffer read
        if argv (0) =~ 'vimwiki\|eml\|draft'
          let g:lightline =
            \{
            \  'colorscheme'  : 'solarized'
            \, 'separator'    : { 'left' : '', 'right' : '' }
            \, 'subseparator' : { 'left' : '', 'right' : '' }
            \}
        else
          if s:powerline == 0
            let g:lightline =
              \{
              \  'colorscheme'  : 'solarized'
              \, 'separator'    : { 'left' : '', 'right' : '' }
              \, 'subseparator' : { 'left' : '', 'right' : '' }
              \}
          else
            let g:lightline =
              \{
              \  'colorscheme'  : 'solarized'
              \, 'separator'    : { 'left' : '', 'right' : '' }
              \, 'subseparator' : { 'left' : '', 'right' : '' }
              \}
          endif
        endif
      else
        let g:lightline =
          \{
          \  'colorscheme'  : 'solarized'
          \, 'separator'    : { 'left' : '', 'right' : '' }
          \, 'subseparator' : { 'left' : '', 'right' : '' }
          \}
      endif

      let g:lightline.active =
        \{
        \  'left'  : [ [ 'mode', 'paste', 'matchspace' ]
        \,             [ 'rootpath', 'basepath', 'filename' ]
        \,             [ 'readonly', 'modified', 'wordcount', 'linesizes' ]
        \            ]
        \, 'right' : [ [ 'indent', 'spaces', 'filetype' ]
        \,             [ 'topbottom', 'bytepercent', 'linecount' ]
        \,             [ 'atom', 'specialchar', 'column' ]
        \            ]
        \}

      let g:lightline.inactive =
        \{
        \  'left'  : [ [ 'filename' ] ]
        \, 'right' : [ [ 'linecount' ] ]
        \}

      let g:lightline.tabline =
        \{
        \  'left'  : [ [ 'tabs' ] ]
        \, 'right' : [ [ 'close' ] ]
        \}

      let g:lightline.component =
        \{
        \  'absolutepath' : '%F'
        \, 'atom'         : '%{synIDattr(synID(line("."),col("."),1),"name")}'
        \, 'basepath'     : '%{expand("%:p") =~ ".*[/][^/]*[/][^/]*" ? substitute(expand("%:p"), ".*[/]\\([^/]*\\)[/][^/]*", "\\1", "") : ""}'
        \, 'bytepercent'  : '%{BytePercent()}%%'
        \, 'bufnum'       : '%n'
        \, 'charvalue'    : '%b'
        \, 'charvaluehex' : '%B'
        \, 'close'        : '%999X X '
        \, 'column'       : '%{getline(line(".")) == "" ? "" : virtcol(".")}'
        \, 'fileencoding' : '%{strlen(&fenc) ? &fenc : &enc}'
        \, 'fileformat'   : '%{&fileformat}'
        \, 'filename'     : '%t'
        \, 'filetype'     : '%{strlen(&filetype) ? &filetype : "no ft"}'
        \, 'linecount'    : '%L'
        \, 'lineinfo'     : '%3l:%-2v'
        \, 'line'         : '%l'
        \, 'matchspace'   : '%{g:matchspace}'
        \, 'mode'         : '%{lightline#mode()}'
        \, 'paste'        : '%{&paste ? "PASTE" : ""}'
        \, 'percent'      : '%-0p%%'
        \, 'percentwin'   : '%P'
        \, 'readonly'     : '%{&filetype == "help" ? "" : &readonly ? "" : ""}'
        \, 'relativepath' : '%f'
        \, 'rootpath'     : '%{expand("%:p") =~ ".*[/][^/]*[/][^/]*[/][^/]*" ? substitute(expand("%:p"), ".*[/]\\([^/]*\\)[/][^/]*[/][^/]*", "\\1", "") : ""}'
        \, 'topbottom'    : '%{line("w0") == 1 ? (line("w$") == line("$") ? "▯" : "▼") : line("w$") == line("$") ? "▲" : ""}'
        \}

      let g:lightline.component_visible_condition =
        \{
        \  'atom'        : '(synIDattr(synID(line("."),col("."),1),"name") != "")'
        \, 'basepath'    : '(expand("%:p") =~ ".*[/][^/]*[/][^/]*")'
        \, 'bytepercent' : '(line("w0") != 1 && line("w$") != line("$"))'
        \, 'column'      : '(getline(line(".")) != "")'
        \, 'matchspace'  : '(g:matchspace != "")'
        \, 'modified'    : '(&filetype != "help" && (&modified || !&modifiable))'
        \, 'paste'       : '&paste'
        \, 'readonly'    : '(&filetype != "help" && &readonly)'
        \, 'rootpath'    : '(expand("%:p") =~ ".*[/][^/]*[/][^/]*[/][^/]*")'
        \, 'topbottom'   : '(line("w0") == 1 || line("w$") == line("$"))'
        \}

      let g:lightline.component_function =
        \{
        \  'indent'      : 'Indent'
        \, 'linesizes'   : 'LineSizes'
        \, 'modified'    : 'Modified'
        \, 'spaces'      : 'Spaces'
        \, 'specialchar' : 'SpecialChar'
        \, 'wordcount'   : 'WordCount'
        \}

      let g:lightline.mode_map =
        \{
        \  '?'      : ' '
        \, 'c'      : 'C'
        \, "\<C-s>" : 'S-BLOCK'
        \, "\<C-v>" : 'V-BLOCK'
        \, 'i'      : 'I'
        \, 'n'      : 'N'
        \, 'R'      : 'R'
        \, 's'      : 'S'
        \, 'S'      : 'S-LINE'
        \, 'v'      : 'V'
        \, 'V'      : 'V-LINE'
        \}

      " toggle lightline/default vim statusline
      " imap <silent><C-F10> <C-o>:call lightline#toggle()<CR>
      " nmap <silent><C-F10> :call lightline#toggle()<CR>

    " ................................................................ Limelight

      let g:limelight_default_coefficient = 0.8
      let g:limelight_paragraph_span      = 0 " include preceding/following paragraphs
      let g:limelight_priority            = 1 " -1 to hlsearch highlight all paragraphs, 1 per paragraph

      " see views.vim
      " autocmd! User GoyoEnter Limelight
      " autocmd! User GoyoLeave Limelight!

    " .............................................................. Litecorrect

      autocmd plugin Filetype mail           call litecorrect#init()
      autocmd plugin FileType markdown,mkd   call litecorrect#init()
      autocmd plugin Filetype text           call litecorrect#init()
      autocmd plugin Filetype vimwiki        call litecorrect#init()

    " .................................................................. LiteDFM

      let g:lite_dfm_left_offset = 22       " see themes.vim

    " ............................................................ Narrow region

      let g:nrrw_rgn_vert = 1               " open in vertical split buffer

    " ................................................................. Open URL

      nmap <silent><leader>o :OpenUrl<CR>

    " ................................................................... Pencil

      " let g:pencil#wrapModeDefault = 'hard' " 'hard' (def), 'soft'
      " let g:pencil#textwidth       = 72     " 74 (def)
      " let g:pencil#joinspaces      = 0      " 0=one_space (def), 1=two_spaces
      " let g:pencil#cursorwrap      = 1      " 0=disable, 1=enable (def)
      " let g:pencil#autoformat      = 1      " 0=manual, 1=auto (def)
      "
      " autocmd plugin Filetype mail         call pencil#init()
      " autocmd plugin FileType markdown,mkd call pencil#init()
      " autocmd plugin Filetype text         call pencil#init()
      " autocmd plugin Filetype vimwiki      call pencil#init()

    " ...................................................... Rainbow parentheses

      autocmd plugin Syntax   * RainbowParenthesesLoadBraces
      autocmd plugin Syntax   * RainbowParenthesesLoadChevrons
      autocmd plugin Syntax   * RainbowParenthesesLoadRound
      autocmd plugin Syntax   * RainbowParenthesesLoadSquare
      autocmd plugin VimEnter * RainbowParenthesesToggle

    " ................................................................ Showmarks

      " let g:showmarks_enable = 0
      " let g:showmarks_include  = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY'

    " .......................................................... Solarized theme

      let g:solarized_contrast   = 'high'   " preferred contrast level for light background
      let g:solarized_hitrail    = 1
      let g:solarized_termcolors = 256
      let g:solarized_termtrans  = 1        " terminal transparency (0) off (1) on

      colorscheme solarized
      if has("gui_running")
        set background=light
      else
        set background=dark
      endif
      syntax enable

    " ................................................................ Signature

      " vim convention m'ark key conflics with my colemak-shift-dh layout
      " using apostrophe instead, preferable imo :-)
      let g:SignatureMap =
        \{
        \  'Leader'            : "'"
        \, 'PlaceNextMark'     : "',"
        \, 'ToggleMarkAtLine'  : "'."
        \, 'PurgeMarksAtLine'  : "'-"
        \, 'DeleteMark'        : "''"
        \, 'PurgeMarks'        : "'<Space>"
        \, 'PurgeMarkers'      : "'<BS>"
        \, 'GotoNextLineAlpha' : "']"
        \, 'GotoPrevLineAlpha' : "'["
        \, 'GotoNextSpotAlpha' : "`]"
        \, 'GotoPrevSpotAlpha' : "`["
        \, 'GotoNextLineByPos' : "]'"
        \, 'GotoPrevLineByPos' : "['"
        \, 'GotoNextSpotByPos' : "]`"
        \, 'GotoPrevSpotByPos' : "[`"
        \, 'GotoNextMarker'    : "[+"
        \, 'GotoPrevMarker'    : "[-"
        \, 'GotoNextMarkerAny' : "]="
        \, 'GotoPrevMarkerAny' : "[="
        \, 'ListBufferMarks'   : "'/"
        \, 'ListBufferMarkers' : "'?"
        \}

      nmap <leader>' '.

    " .................................................................... Sneak

      " by default, use cc, cl for s, S
      let g:sneak#streak       = 1          " streak mode
      let g:sneak#s_next       = 1          " clever next, use s S for ; .
      let g:sneak#absolute_dir = 0          " next follows direction of invocation
      let g:sneak#use_ic_scs   = 1          " use vim case setting
      let g:sneak#prompt       = '>'        " prompt

      " preserve s and remap to f
      autocmd plugin BufNewFile,BufRead * call Sneak_f()

    " ................................................................. Snipmate

      let g:snipMate = get(g:, 'snipMate', {})
      let g:snipMate.scope_aliases = {}

    " ................................................................. Supertab

      " vimwiki uses <tab> for tables PLUS space leader benefits from use of tab
      " let g:SuperTabMappingForward = '<C-Tab>'
      " let g:SuperTabMappingBackward = '<C-S-Tab>'

    " ................................................................ Syntastic

      let g:syntastic_auto_jump      = 0
      let g:syntastic_auto_loc_list  = 1
      let g:syntastic_enable_signs   = 1
      let g:syntastic_quiet_messages = {'level':'warnings'}

      " set statusline+=%#warningmsg#
      " set statusline+=%{SyntasticStatuslineFlag()}
      " set statusline+=%*

    " ................................................................... Tagbar

      " " let g:tagbar_ctags_bin = 'ctags-exuberant'
      " nmap <silent><leader>t :TagbarToggle<CR>

      " " see ctags.cnf
      " " from http://stackoverflow.com/questions/7037055/ctags-vimwiki-vim-and-tagbar-plugin
        " let g:tagbar_type_vimwiki =․
        "   \{
        "   \  'ctagstype' : 'vimwiki'
        "   \, 'kinds'     : [
        "   \  'h:header'
        "   \, ]
        "   \, 'sort'      : 0
        "   \}

    " ................................................................ T-comment

      " see ToggleComment functions.vim
      nmap <leader>c <C-_><C-_>
      vmap <leader>c <C-_><C-_>
      " imap ,c <C-_><C-_>

    " ................................................................. Vimfiler

      " requires http://github.com/Shougo/unite.vim
      " let g:vimfiler_as_default_explorer = 1" explorer mode
      " nmap <silent><leader><leader>! :VimFiler<CR>

    " ................................................................. Vimshell

      " nmap <silent><leader>!         :VimShell<CR>

    " .................................................................. Vimwiki

      let g:vimwiki_list =
        \[
        \  {
        \    'path'      : '~/vimwiki/thedarnedestthing/'
        \,   'path_html' : '~/vimwiki/thedarnedestthing/html/'
        \  }
        \, {
        \    'path'      : '~/vimwiki/thestory/'
        \,   'path_html' : '~/vimwiki/thestory/html/'
        \  }
        \, {
        \    'path'      : '~/vimwiki/truthordie/'
        \,   'path_html' : '~/vimwiki/truthordie/html/'
        \  }
        \, {
        \    'path'      : '~/vimwiki/shadowsandlight/'
        \,   'path_html' : '~/vimwiki/shadowsandlight/html/'
        \  }
        \, {
        \    'path'      : '~/vimwiki/healing/'
        \,   'path_html' : '~/vimwiki/healing/html/'
        \  }
        \, {
        \    'path'      : '~/vimwiki/colophon/'
        \,   'path_html' : '~/vimwiki/colophon/html/'
        \  }
        \, {
        \    'path'      : '~/vimwiki/notes/'
        \,   'path_html' : '~/vimwiki/notes/html/'
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
      autocmd plugin Filetype vimwiki  call VimWikiLink()
      autocmd plugin BufEnter *.wiki   set filetype=vimwiki
      autocmd plugin Filetype markdown setlocal nocp spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      autocmd plugin Filetype vimwiki  setlocal nocp spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      " cannot trap s:setup_buffer_leave() to avoid initialization error on 1st link
      " see arch install patch to initialize s:vimwiki_autowriteall

    " ................................................................. Yankring

      let g:yankring_default_menu_mode = 1  " menu on with no shortcut
      let g:yankring_dot_repeat_yank   = 1  " allow repeating yankring action
      let g:yankring_enabled           = 1  " disable yankring because of macro conflict
      let g:yankring_window_height     = 30 " horizontal window height

      nmap <silent>Y         :<C-U>YRYankCount 'y$'<CR>
      " nmap <silent><F7>    :YRShow<CR>
      " imap <silent><F7>    <ESC>:YRShow<CR>
      " nmap <silent><F2>    :YRShow<CR>
      " imap <silent><F2>    <ESC>:YRShow<CR>
      nmap <silent><leader>y :YRShow<CR>

" plugins.vim
