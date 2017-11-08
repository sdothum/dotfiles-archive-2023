" sdothum - 2016 (c) wtfpl

" Plugins
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugin settings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup plugin
        autocmd!
      augroup END

    " ...................................................................... Ale

      let g:ale_sign_column_always = 1
      let g:ale_sign_error         = '⚑'
      let g:ale_sign_warning       = '⚑'
      let g:ale_linter_aliases     =
          \{
          \  'vimwiki' : 'markdown'
          \, 'mail'    : 'markdown'
          \}

      nmap <silent><C-k> <Plug>(ale_previous_wrap)
      nmap <silent><C-j> <Plug>(ale_next_wrap)
      nmap <silent><C-?> <Plug>(ale_detail)
      nmap <silent><C-!> :ALEToggle

    " ............................................................... Auto-pairs

      let g:AutoPairsMapBS    = 1           " auto delete symbol pairs
      let g:AutoPairsMapSpace = 0           " disabled to make iabbrev work!

    " ............................................................... Easy-align

      let g:easy_align_delimiters =
          \{
          \  '>' : { 'pattern' : '>>\|=>\|>' }
          \, '^' : { 'pattern' : '=',        'left_margin' : 0, 'right_margin' : 0, 'align' : 'right' }
          \, '(' : { 'pattern' : '(',        'left_margin' : 1, 'right_margin' : 0 }
          \, ')' : { 'pattern' : ')',        'left_margin' : 0 }
          \}

      vmap <Enter>   <Plug>(EasyAlign)
      nmap <leader>a <Plug>(EasyAlign)

    " .............................................................. Eightheader

      " justified fold headers
      let &foldtext =
          \"EightHeaderFolds(
          \  '\\=s:fullwidth'
          \, 'left'
          \, [ repeat( '  ', v:foldlevel - 1 ), ' ', '' ]
          \, '\\= s:foldlines . \" lines\"'
          \, ''
          \)"

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
      " powerline symbol slant (0) lower (1) upper
      let s:powerline  = ("$POWERLINE" > '' ? $POWERLINE : 0)
      let g:lightline  =
          \{
          \  'colorscheme'  : 'solarized'
          \, 'separator'    : { 'left' : '',  'right' : ''  }
          \, 'subseparator' : { 'left' : '', 'right' : '' }
          \}

      " defer lightline settings because plugin is initialized before buffers are read
      function! LightLine()
        if has("gui_running")
          if Prose()
            " prose line height makes for ugly powerline graphics
            let g:lightline =
                \{
                \  'colorscheme'  : 'solarized'
                \, 'separator'    : { 'left' : '',  'right' : ''  }
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
            \  'left'  : [
            \              [ 'mode',     'paste',    'matchspace', 'bufnum' ]
            \,             [ 'rootpath', 'basepath', 'filename'   ]
            \,             [ 'readonly', 'modified', 'wordcount', 'linesizes' ]
            \            ]
            \, 'right' : [
            \              [ 'indent',    'spaces',      'filetype'  ]
            \,             [ 'topbottom', 'linepercent', 'linecount' ]
            \,             [ 'atom',      'specialchar', 'column'    ]
            \            ]
            \}

        let g:lightline.inactive =
            \{
            \  'left'  : [
            \              [ 'indicator' ]
            \,             [ 'filename'  ]
            \,           ]
            \, 'right' : [ [ 'linecount' ] ]
            \}

        let g:lightline.tabline =
            \{
            \  'left'  : [ [ 'tabs'  ] ]
            \, 'right' : [ [ 'close' ] ]
            \}

        let g:lightline.component =
            \{
            \  'absolutepath' : '%F'
            \, 'bufnum'       : '%{BufCount() != "1‥1" ? BufCount() : ""}'
            \, 'charvalue'    : '%b'
            \, 'charvaluehex' : '%B'
            \, 'close'        : '%999X X '
            \, 'column'       : '%{getline(line(".")) == "" ? "" : virtcol(".")}'
            \, 'fileencoding' : '%{strlen(&fenc) ? &fenc : &enc}'
            \, 'fileformat'   : '%{&fileformat}'
            \, 'filename'     : '%t'
            \, 'filetype'     : '%{strlen(&filetype) ? &filetype : "no ft"}'
            \, 'indicator'    : '%{Modified(1)}'
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
            \, 'bufnum'      : '(BufCount() != "1‥1")'
            \, 'linepercent' : '(line(".") != 1 && line(".") != line("$"))'
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
            \, 'linepercent' : 'LinePercent'
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

    " ................................................................ Limelight

      let g:limelight_default_coefficient = 0.8
      let g:limelight_paragraph_span      = 0 " include preceding/following paragraphs
      let g:limelight_priority            = 1 " -1 to hlsearch highlight all paragraphs, 1 per paragraph

      " see views.vim
      " autocmd! plugin User GoyoEnter     Limelight
      " autocmd! plugin User GoyoLeave     Limelight!

    " .............................................................. Litecorrect

      nnoremap <C-s> [s1z=<c-o>
      inoremap <C-s> <c-g>u<Esc>[s1z=`]A<c-g>u

      autocmd plugin Filetype mail         call litecorrect#init()
      autocmd plugin FileType markdown,mkd call litecorrect#init()
      autocmd plugin Filetype vimwiki      call litecorrect#init()

    " .................................................................. LiteDFM

      " let g:lite_dfm_left_offset = 22     " see themes.vim

    " ............................................................ Narrow region

      let g:nrrw_rgn_vert          = 0      " open in horizontal split buffer
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

    " .............................................................. Neocomplete

      let g:neocomplete#enable_at_startup                 = 1
      let g:neocomplete#enable_smart_case                 = 1
      let g:neocomplete#sources#syntax#min_keyword_length = 3

      " inoremap <expr><Tab>  neocomplete#start_manual_complete()
      " inoremap <expr><TAB>  pumvisible() ? "\<Down>" :
    	"   \ neocomplete#start_manual_complete()

    	function! s:check_back_space() abort
        let col = col('.') - 1
        return !col || getline('.')[col - 1]  =~ '\s'
      endfunction

      inoremap <silent><expr><TAB>
	      \ pumvisible() ? "\<C-n>" :
	      \ <SID>check_back_space() ? "\<TAB>" :
	      \ neocomplete#start_manual_complete()

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

      imap <C-x> <Plug>(neosnippet_expand_or_jump)
      smap <C-x> <Plug>(neosnippet_jump)

    " ............................................................ Nerdcommenter

      let g:NERDSpaceDelims            = 1  " space after comment delimiter
      let g:NERDCompactSexyComs        = 1  " prettify multi-line
      let g:NERDDefaultAlign           = 'left'
      let g:NERDCustomDelimiters       = { 'c' : { 'left' : '//', 'right' : '' } }
      let g:NERDCommentEmptyLines      = 1  " comment blank lines
      let g:NERDTrimTrailingWhitespace = 1  " trim trailing whitespace

      map  <leader>c <Plug>NERDCommenterToggle
      imap ,c        <C-o>:execute "normal \<Plug>NERDCommenterToggle"<CR>

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

      "replace 'f' with 1-char Sneak
      nmap f <Plug>Sneak_f
      nmap F <Plug>Sneak_F
      xmap f <Plug>Sneak_f
      xmap F <Plug>Sneak_F
      omap f <Plug>Sneak_f
      omap F <Plug>Sneak_F
      "replace 't' with 1-char Sneak
      nmap t <Plug>Sneak_t
      nmap T <Plug>Sneak_T
      xmap t <Plug>Sneak_t
      xmap T <Plug>Sneak_T
      omap t <Plug>Sneak_t
      omap T <Plug>Sneak_T

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
      let g:syntastic_quiet_messages = { 'level' : 'warnings' }

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
          \, 'kinds'      : [ 'h:header' ]
          \, 'sro'        : '&&&'
          \, 'kind2scope' : { 'h' : 'header' }
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
          \,   'syntax'    : 'markdown'
          \  }
          \, {
          \    'path'      : '~/vimwiki/thestory/'
          \,   'path_html' : '~/vimwiki/thestory/html/'
          \,   'syntax'    : 'markdown'
          \  }
          \, {
          \    'path'      : '~/vimwiki/truthordie/'
          \,   'path_html' : '~/vimwiki/truthordie/html/'
          \,   'syntax'    : 'markdown'
          \  }
          \, {
          \    'path'      : '~/vimwiki/shadowsandlight/'
          \,   'path_html' : '~/vimwiki/shadowsandlight/html/'
          \,   'syntax'    : 'markdown'
          \  }
          \, {
          \    'path'      : '~/vimwiki/healing/'
          \,   'path_html' : '~/vimwiki/healing/html/'
          \,   'syntax'    : 'markdown'
          \  }
          \, {
          \    'path'      : '~/vimwiki/colophon/'
          \,   'path_html' : '~/vimwiki/colophon/html/'
          \,   'syntax'    : 'markdown'
          \  }
          \, {
          \    'path'      : '~/vimwiki/notes/'
          \,   'path_html' : '~/vimwiki/notes/html/'
          \,   'syntax'    : 'markdown'
          \  }
          \]

      " header highlighting
      highlight VimwikiHeader1 guifg=#d70000
      highlight VimwikiHeader2 guifg=#af005f
      highlight VimwikiHeader3 guifg=#5f5faf
      highlight VimwikiHeader4 guifg=#0087ff
      highlight VimwikiHeader5 guifg=#00afaf
      highlight VimwikiHeader6 guifg=#5f8700

      " restore vimwiki link
      function! VimwikiLink()
        highlight VimwikiLink guifg=#268bd2 gui=bold
      endfunction

      " resolve <CR>, <Tab> conflicts with autocompletion (simple-complete)
      function! VimwikiRemap()
        if !exists('b:vimwiki_remap')
          let b:vimwiki_remap = 1
          iunmap   <silent><buffer>           <CR>
          inoremap <expr><buffer><C-PageDown> vimwiki#tbl#kbd_tab()
          inoremap <expr><buffer><C-PageUp>   vimwiki#tbl#kbd_shift_tab()
          call RefreshGui()
        endif
      endfunction

      autocmd plugin Filetype vimwiki call VimwikiLink()
      autocmd plugin Filetype vimwiki setlocal nocp spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      autocmd plugin BufEnter *
            \ if &filetype == 'vimwiki' | call VimwikiRemap() | endif
      " cannot trap s:setup_buffer_leave() to avoid initialization error on 1st link
      " see arch install patch to initialize s:vimwiki_autowriteall

    " ................................................................. Yankring

      let g:yankring_default_menu_mode = 1  " menu on with no shortcut
      let g:yankring_dot_repeat_yank   = 1  " allow repeating yankring action
      let g:yankring_enabled           = 1  " disable yankring because of macro conflict
      let g:yankring_window_height     = 30 " horizontal window height
      let g:yankring_zap_keys          = '' " disable (conflicts with sneak)

      nmap <silent>Y         :<C-U>YRYankCount 'y$'<CR>
      nmap <silent><leader>y :YRShow<CR>

" plugins.vim
