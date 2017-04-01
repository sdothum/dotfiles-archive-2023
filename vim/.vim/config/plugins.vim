" sdothum - 2016 (c) wtfpl

" Plugins
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugin settings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup plugin
        autocmd!
      augroup END

    " ............................................................... Auto-pairs

      let g:AutoPairsMapBS    = 1           " auto delete symbol pairs
      let g:AutoPairsMapSpace = 0           " disabled to make iabbrev work!

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

    " ...................................................................... Fzf

      " " fzf doesn't really work well within gvim
      " let g:fzf_launcher = 'term STACK WAIT SHELL %s'
      "
      " nmap <leader><leader>b :FZF<CR>

    " ........................................................... Graphical undo

      let g:gundo_width          = 30
      let g:gundo_preview_bottom = 1
      let g:gundo_preview_height = 20

      nmap <silent><leader>u :GundoToggle<CR>

      autocmd plugin BufEnter __Gundo__ setlocal numberwidth=3 foldcolumn=0

    " ............................................................ Indent guides

      let g:indent_guides_auto_colors = 0

      " subtle highlighting of even indents only, see gui.vim, theme.vim
      function! IndentTheme()
        if &background == 'light'
          execute 'highlight IndentGuidesOdd guibg='  . g:dfm_bg_light
          execute 'highlight IndentGuidesEven guibg=' . g:dfm_bg_line_light
          if g:ruler == 2
            execute 'highlight ColorColumn guibg='    . g:dfm_bg_column_light
          else
            execute 'highlight ColorColumn guibg='    . g:dfm_bg_line_light
          endif
        else
          execute 'highlight IndentGuidesOdd guibg='  . g:dfm_bg_dark
          execute 'highlight IndentGuidesEven guibg=' . g:dfm_bg_line_dark
          if g:ruler == 2
            execute 'highlight ColorColumn guibg='    . g:dfm_bg_column_dark
          else
            execute 'highlight ColorColumn guibg='    . g:dfm_bg_line_dark
          endif
        endif
      endfunction

    " ................................................................ Lightline

      let g:matchspace = ''                 " see ToggleSpaces() gui.vim
      let s:powerline  = 0                  " powerline symbol slant (0) lower (1) upper
      let g:lightline  =
        \{
        \  'colorscheme'  : 'solarized'
        \, 'separator'    : { 'left' : '', 'right' : '' }
        \, 'subseparator' : { 'left' : '', 'right' : '' }
        \}

      " defer lightline settings because plugin is initialized before buffers are read
      function! LightLine()
        if has("gui_running")
          if ProseFT()
            " prose line height makes for ugly powerline graphics
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
          \}

        let g:lightline.component_visible_condition =
          \{
          \  'basepath'    : '(expand("%:p") =~ ".*[/][^/]*[/][^/]*")'
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
          \, 'rootpath'    : 'RootPath'
          \, 'basepath'    : 'BasePath'
          \, 'topbottom'   : 'TopBottom'
          \, 'atom'        : 'Atom'
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

        " must disable lightline to allow new settings to be laaded
        call lightline#disable()
        call lightline#init()
        call lightline#enable()
      endfunction

      autocmd plugin BufEnter * call LightLine()

      function! Atom()
        return synIDattr(synID(line('.'), col('.'), 1), 'name')
      endfunction

      function! TopBottom()
        if line('w0') == 1
          return line('w$') == line('$') ? '▯' : '▼'
        else
          return line('w$') == line('$') ? '▲' : ''
        endif
      endfunction

      function! RootPath()
        if expand('%:p') =~ '.*[/][^/]*[/][^/]*[/][^/]*'
          " return substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '')
          let root = substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '')
          if root == ''
            return root
          else
            if root == substitute(expand('%:p'), '^[/]\([^/]*\)[/].*', '\1', '')
              return root
            else
              let root = substitute(expand('%:p'), '[/][^/]*[/][^/]*$', '', '')
              let root = substitute(root, $HOME, '~', '')
              let base = substitute(root, '.*[/]\([^/]*\)$', '\1', '')
              let root = substitute(root, '[^/]*$', '', '')
              let root = substitute(root, '\([/][.]*[^/]\)[^/]*', '\1', 'g')
              return root . base
            endif
          endif
        else
          return ''
        endif
      endfunction

      function! BasePath()
        if expand('%:p') =~ '.*[/][^/]*[/][^/]*'
          return substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*', '\1', '')
        else
          return ''
        endif
      endfunction

    " ................................................................ Limelight

      let g:limelight_default_coefficient = 0.8
      let g:limelight_paragraph_span      = 0 " include preceding/following paragraphs
      let g:limelight_priority            = 1 " -1 to hlsearch highlight all paragraphs, 1 per paragraph

      " see views.vim
      " autocmd! plugin User GoyoEnter Limelight
      " autocmd! plugin User GoyoLeave Limelight!

    " .............................................................. Litecorrect

      autocmd plugin Filetype mail         call litecorrect#init()
      autocmd plugin FileType markdown,mkd call litecorrect#init()
      autocmd plugin Filetype vimwiki      call litecorrect#init()

    " .................................................................. LiteDFM

      " let g:lite_dfm_left_offset = 22     " see themes.vim

      " see views.vim
      function! Margin()
        " let g:lite_dfm_left_offset = max([0, min([22, (&columns - &textwidth - &numberwidth) / 2])])
        let g:lite_dfm_left_offset = max([1, min([22, (&columns - &textwidth) / 2])])
      endfunction

      call Margin()

    " ............................................................... MUcomplete

      set completeopt+=menuone
      set shortmess+=c
      set completeopt+=noinsert,noselect

      " automatic completion
      let g:mucomplete#enable_auto_at_startup = 1

    " ............................................................ Narrow region

      let g:nrrw_rgn_vert          = 0       " open in horizontal split buffer
			let g:nrrw_topbot_leftright  = 'botright'
			let g:nrrw_rgn_nomap_nr      = 1      " disable nr mappings
			let g:nrrw_rgn_nomap_Nr      = 1
			let g:nrrw_rgn_resize_window = 'relative'
			let g:nrrw_rgn_rel_min       = 50     " relative window size

      function! CloseNR()
        if expand('%t') =~ 'NrrwRgn'
          execute ':wq'
        endif
      endfunction

      " apply refresh to narrow region buffer to apply layout defaults!
      vmap <leader>n <Plug>NrrwrgnDo:call Refresh()<CR>
      nmap <leader>n :call CloseNR()<CR>

    " ............................................................... Neosnippet

      let g:neosnippet#snippets_directory            = '~/.vim/snippets'
      let g:neosnippet#enable_snipmate_compatibility = 1
      " disable all runtime snippets
      let g:neosnippet#disable_runtime_snippets      = { '_' : 1 }
      " see CheckFiletype() files.vim
      let g:neosnippet#scope_aliases =
          \{
          \  'new'      : 'conf,fish,hs,ruby,sh,zsh'
          \, 'markdown' : 'vimwiki'
          \, 'text'     : 'mail'
          \}

      imap <C-e> <Plug>(neosnippet_expand_or_jump)
      smap <C-e> <Plug>(neosnippet_jump)

    " ............................................................ Nerdcommenter

      let g:NERDSpaceDelims            = 1  " space after comment delimiter
      let g:NERDCompactSexyComs        = 1  " prettify multi-line
      let g:NERDDefaultAlign           = 'left'
      let g:NERDCustomDelimiters       = { 'c': { 'left': '//','right': '' } }
      let g:NERDCommentEmptyLines      = 1  " comment blank lines
      let g:NERDTrimTrailingWhitespace = 1  " trim trailing whitespace

      map <leader>c <Plug>NERDCommenterToggle
      imap ,c       <C-o>:execute "normal \<Plug>NERDCommenterToggle"<CR>

    " ................................................................... Pencil

      let g:pencil#wrapModeDefault = 'hard' " 'hard' (def), 'soft'
      let g:pencil#textwidth       = 72     " 74 (def)
      let g:pencil#joinspaces      = 0      " 0=one_space (def), 1=two_spaces
      let g:pencil#cursorwrap      = 1      " 0=disable, 1=enable (def)
      let g:pencil#autoformat      = 1      " 0=manual, 1=auto (def)
      let g:pencil#mode_indicators =
          \{
          \  'hard' : 'Hard Pencil'
          \, 'auto' : 'Auto Pencil'
          \, 'soft' : 'Soft Pencil'
          \, 'off'  : 'No Pencil'
          \}

      imap <F6> <C-o>:silent TogglePencil<CR>:echo PencilMode()<CR>
      nmap <F6> :silent TogglePencil<CR>:echo PencilMode()<CR>

      autocmd plugin Filetype mail         call pencil#init()
      autocmd plugin FileType markdown,mkd call pencil#init()
      autocmd plugin Filetype vimwiki      call pencil#init()

    " ...................................................... Rainbow parentheses

      autocmd plugin Syntax   * RainbowParenthesesLoadBraces
      autocmd plugin Syntax   * RainbowParenthesesLoadChevrons
      autocmd plugin Syntax   * RainbowParenthesesLoadRound
      autocmd plugin Syntax   * RainbowParenthesesLoadSquare
      autocmd plugin VimEnter * RainbowParenthesesToggle

    " ................................................................ Signature

      " vim convention m'ark key conflicts with my colemak-shift-dh layout
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

    " .......................................................... Simple-complete

      set complete-=t
      set complete-=i

      " highlight PmenuSel term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=#839496 guibg=#002b36

    " .................................................................... Sneak

      " by default, use cc, cl for s, S
      let g:sneak#streak       = 1          " streak mode
      let g:sneak#s_next       = 1          " clever next, use s S for ; .
      let g:sneak#absolute_dir = 0          " next follows direction of invocation
      let g:sneak#use_ic_scs   = 1          " use vim case setting
      let g:sneak#prompt       = '>'        " prompt
      let g:sneak#label        = 1          " label mode

      " " remap sneak_s to preserve s
      " function! Sneak_f()
      "   if !exists("g:sneak_f")
      "     let g:sneak_f = 1
      "     unmap s
      "     unmap S
      "     call Colemak()
      "     nmap f <Plug>Sneak_s
      "     nmap F <Plug>Sneak_S
      "   endif
      " endfunction
      "
      " " preserve s and remap to f
      " autocmd plugin BufNewFile,BufRead * call Sneak_f()

    " ................................................................ Solarized

      let g:solarized_termtrans = 1         " terminal transparency (0) off (1) on

      if has("gui_running")
        set background=light
        colorscheme solarized8_light_high
      endif
      set termguicolors                     " for neovim

      syntax enable

    " ................................................................ Syntastic

      let g:syntastic_auto_jump      = 0
      let g:syntastic_auto_loc_list  = 1
      let g:syntastic_enable_signs   = 1
      let g:syntastic_quiet_messages = {'level':'warnings'}

      " set statusline+=%#warningmsg#
      " set statusline+=%{SyntasticStatuslineFlag()}
      " set statusline+=%*

    " ................................................................... Tagbar

      " let g:tagbar_ctags_bin = 'ctags-exuberant'
      nmap <silent><leader>t :TagbarToggle<CR>

      " see https://github.com/vimwiki/utils/blob/master/vwtags.py
      let g:tagbar_type_vimwiki =
          \{
          \  'ctagstype'  : 'vimwiki'
          \, 'kinds'      : ['h:header']
          \, 'sro'        : '&&&'
          \, 'kind2scope' : {'h':'header'}
          \, 'sort'       : 0
          \, 'ctagsbin'   : '~/.vim/vwtags.py'
          \, 'ctagsargs'  : 'default'
          \}

    " .................................................................. Vimwiki

      " disable tab for autocompletion
      let g:vimwiki_table_mappings = 0

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
      autocmd plugin Filetype vimwiki call VimWikiLink()
      autocmd plugin Filetype vimwiki setlocal nocp spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      " cannot trap s:setup_buffer_leave() to avoid initialization error on 1st link
      " see arch install patch to initialize s:vimwiki_autowriteall

    " ................................................................. Yankring

      let g:yankring_default_menu_mode = 1  " menu on with no shortcut
      let g:yankring_dot_repeat_yank   = 1  " allow repeating yankring action
      let g:yankring_enabled           = 1  " disable yankring because of macro conflict
      let g:yankring_window_height     = 30 " horizontal window height

      nmap <silent>Y         :<C-U>YRYankCount 'y$'<CR>
      nmap <silent><leader>y :YRShow<CR>

" plugins.vim
