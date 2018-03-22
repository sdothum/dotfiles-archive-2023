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
          \, 'wiki'    : 'markdown'
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
          \, [repeat( '  ', v:foldlevel - 1 ), ' ', '']
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

      let g:fzf_colors =
          \{
          \  'fg'      : ['fg', 'Normal']
          \, 'bg'      : ['bg', 'Normal']
          \, 'hl'      : ['fg', 'Special']
          \, 'fg+'     : ['fg', 'CursorLine']
          \, 'bg+'     : ['bg', 'CursorLine']
          \, 'hl+'     : ['fg', 'Special']
          \, 'info'    : ['fg', 'Special']
          \, 'border'  : ['fg', 'Ignore']
          \, 'prompt'  : ['fg', 'Directory']
          \, 'pointer' : ['fg', 'Special']
          \, 'marker'  : ['fg', 'Special']
          \, 'spinner' : ['fg', 'Special']
          \, 'header'  : ['fg', 'Directory']
          \}

      function! FzfColors()
        " g:fzf_colors initializes fzf only once, so override cursorline color (cannot seem to set other colors, such as hl+)
        let $FZF_DEFAULT_OPTS = '--reverse --color=fg+:' . ui#color('g:dfm_fg_' . &background)

        " hide bottom fzf window identifier
        execute 'highlight fzf1 guibg=' . g:dfm_bg . ' guifg=' . g:dfm_bg
        execute 'highlight fzf2 guibg=' . g:dfm_bg . ' guifg=' . g:dfm_bg
        execute 'highlight fzf3 guibg=' . g:dfm_bg . ' guifg=' . g:dfm_bg
      endfunction

      nmap <silent><leader>b :Buffers<CR>
      nmap <silent><leader>l :Lines<CR>
      nmap <silent><leader>m :Marks<CR>
      " nmap <leader>f       :FZF<CR>       " see notational-fzf for extended content search

    " ........................................................... Graphical undo

      let g:gundo_width          = 30
      let g:gundo_preview_bottom = 1
      let g:gundo_preview_height = 20

      nmap <silent><leader>u :GundoToggle<CR>

      autocmd plugin BufEnter __Gundo__ setlocal numberwidth=3 foldcolumn=0

    " ............................................................ Indent guides

      " subtle highlighting of even indents only, see core#ToggleColumn(), ui#IndentTheme()
      let g:indent_guides_auto_colors = 0

    " ................................................................ Lightline

      " powerline symbol slant (0) lower (1) upper
      let s:powerline  = ("$POWERLINE" > '' ? $POWERLINE : 0)
      let g:lightline  =
          \{
          \  'colorscheme'  : 'solarized'
          \, 'separator'    : { 'left' : '',  'right' : ''  }
          \, 'subseparator' : { 'left' : '', 'right' : '' }
          \}

      " defer lightline settings because plugin is initialized before buffers are read, see ui#LiteLine()
      function! LightLine()
        if has("gui_running")
          if s:powerline == 0
            let g:lightline =
                \{
                \  'colorscheme'  : g:colors_name
                \, 'separator'    : { 'left' : '', 'right' : '' }
                \, 'subseparator' : { 'left' : '', 'right' : '' }
                \}
          else
            let g:lightline =
                \{
                \  'colorscheme'  : g:colors_name
                \, 'separator'    : { 'left' : '', 'right' : '' }
                \, 'subseparator' : { 'left' : '', 'right' : '' }
                \}
          endif
        endif

        let g:lightline.active =
            \{
            \  'left'  : [['mode',        'paste',     'matchspace',  'bufnum'   ]
            \,            ['bufcount',    'rootpath',  'rootname',    'basename' ]
            \,            ['filename',    'indent',    'spaces'                  ]
            \,            ['readonly',    'modified'                             ]]
            \, 'right' : [['wordcount',   'atom',      'filetype'                ]
            \,            ['lineinfo',    'topbottom', 'linepercent', 'linecount']
            \,            ['specialchar', 'column',    'columninfo'              ]]
            \}

        let g:lightline.inactive =
            \{
            \  'left'  : [['indicator']
            \,            ['filename' ]]
            \, 'right' : [['linecount']]
            \}

        let g:lightline.tabline =
            \{
            \  'left'  : [['tabs' ]]
            \, 'right' : [['close']]
            \}

        let g:lightline.component =
            \{
            \  'absolutepath' : '%F'
            \, 'bufcount'     : '%{core#BufCount() > 1 ? core#BufCount() : ""}'
            \, 'bufnum'       : '%{core#BufCount() > 1 ? bufnr("%") : ""}'
            \, 'charvalue'    : '%b'
            \, 'charvaluehex' : '%B'
            \, 'close'        : '%999X X '
            \, 'column'       : '%{getline(line(".")) == "" ? "" : virtcol(".")}'
            \, 'fileencoding' : '%{strlen(&fenc) ? &fenc : &enc}'
            \, 'fileformat'   : '%{&fileformat}'
            \, 'filename'     : '%t'
            \, 'filetype'     : '%{strlen(&filetype) ? &filetype : "no ft"}'
            \, 'indicator'    : '%{info#Modified(1)}'
            \, 'linecount'    : '%L'
            \, 'lineinfo'     : '%3l:%-2v'
            \, 'line'         : '%l'
            \, 'mode'         : '%{lightline#mode()}'
            \, 'paste'        : '%{&paste ? "PASTE" : ""}'
            \, 'percent'      : '%-0p%%'
            \, 'percentwin'   : '%P'
            \, 'readonly'     : '%{core#Protected() ? "" : &readonly ? "  " : &modifiable ? "" : "  "}'
            \, 'relativepath' : '%f'
            \}

        " tagbar suppression doesn't work (for some reason) so disable lightline, see core#Tagbar()
        let g:lightline.component_visible_condition =
            \{
            \  'atom'        : '(!core#Tagbar())'
            \, 'basename'    : '(!core#Tagbar())'
            \, 'bufcount'    : '(core#BufCount() > 1)'
            \, 'bufnum'      : '(core#BufCount() > 1)'
            \, 'column'      : '(!core#Tagbar())'
            \, 'linepercent' : '(line(".") != 1 && line(".") != line("$") && !core#Tagbar())'
            \, 'mode'        : '(!core#Tagbar())'
            \, 'paste'       : '&paste'
            \, 'readonly'    : '(&filetype != "help" && (&readonly || !&modifiable))'
            \, 'topbottom'   : '((line("w0") == 1 || line("w$") == line("$")) && !core#Tagbar())'
            \}

        let g:lightline.component_function =
            \{
            \  'atom'        : 'info#Atom'
            \, 'basename'    : 'info#BaseName'
            \, 'columninfo'  : 'info#ColumnInfo'
            \, 'indent'      : 'info#Indent'
            \, 'lineinfo'    : 'info#LineInfo'
            \, 'linepercent' : 'info#LinePercent'
            \, 'matchspace'  : 'core#MatchSpace'
            \, 'modified'    : 'info#Modified'
            \, 'rootname'    : 'info#RootName'
            \, 'rootpath'    : 'info#RootPath'
            \, 'spaces'      : 'info#Spaces'
            \, 'specialchar' : 'info#SpecialChar'
            \, 'topbottom'   : 'info#TopBottom'
            \, 'wordcount'   : 'info#WordCount'
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
            \, 't'      : '🔍'
            \, 'v'      : 'V'
            \, 'V'      : 'V-LINE'
            \}
      endfunction

    " ................................................................ Limelight

      let g:limelight_default_coefficient = 0.8
      let g:limelight_paragraph_span      = 0 " include preceding/following paragraphs
      let g:limelight_priority            = 1 " -1 to hlsearch highlight all paragraphs, 1 per paragraph

      " see ui#ToggleProof(), ui#CodeView()
      " autocmd! plugin User GoyoEnter     Limelight
      " autocmd! plugin User GoyoLeave     Limelight!

    " .............................................................. Litecorrect

      nnoremap <C-s> [s1z=<c-o>
      inoremap <C-s> <c-g>u<Esc>[s1z=`]A<c-g>u

      " correction related, but really bound to Pencil
      nmap <silent><F6> :silent call core#ToggleSpell()<CR>
      imap <silent><F6> <C-o>:silent call core#ToggleSpell()<CR>
      vmap <silent><F6> <C-o>:silent call core#ToggleSpell()<CR>

      autocmd plugin Filetype draft        call litecorrect#init()
      autocmd plugin Filetype note         call litecorrect#init()
      autocmd plugin Filetype mail         call litecorrect#init()
      autocmd plugin FileType markdown,mkd call litecorrect#init()
      autocmd plugin Filetype vimwiki      call litecorrect#init()

    " .................................................................. LiteDFM

      let g:lite_dfm_left_offset = 22       " see ui#Margin()

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
      vmap <leader>n <Plug>NrrwrgnDo:call ui#Retheme()<CR>
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
      " see core#CheckFiletype()
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

      " <leader>cs to force commenting of first line comment
      map  <leader>c <Plug>NERDCommenterToggle
      imap ,c        <C-o>:execute "normal \<Plug>NERDCommenterToggle"<CR>

    " ........................................................... Notational-fzf

      " buffers load after plugins so parse command line for filename
      function! ArgFile()
        if argc() > 0
          return argv(argc() - 1)
        endif
        return ''
      endfunction

      let g:nv_search_paths          = ['./'] " default search from current directory
      let g:nv_default_extension     = ''
      let g:nv_main_directory        = './'   " create new notes in current directory
      let g:nv_use_short_pathnames   = 1
      let g:nv_create_note_window    = 'edit'
      let g:nv_preview_width         = 45

      " notational path rules: [regex, rootpath, ext]
      " note: regex magic is not enabled at this stage so force with '\v'
      let s:set_notational           = [['.wiki$',                           ['~/vimwiki', '~/drafts'],  'wiki' ]
          \,                            ['.draft$',                          ['~/drafts'],               'draft']
          \,                            ['.note$',                           ['~/notes'],                'note' ]
          \,                            ['\v([~]|' . $HOME . '|/stow)/bin/', ['~/bin'],                  ''     ]
          \,                            ['.vim/',                            ['~/.vim/config'],          'vim'  ]
          \,                            ['herbstluftwm/',                    ['~/.config/herbstluftwm'], ''     ]
          \,                            ['archlinux/',                       ['~/build/archlinux'],      ''     ]]

      " dynamically setup notational-fzf :)
      for i in s:set_notational
        if ArgFile() =~ i[0]
          let g:nv_search_paths      = i[1]
          let g:nv_default_extension = i[2]
          break
        endif
      endfor

      nnoremap <silent><leader>f :NV<CR>

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

      autocmd plugin Filetype draft        call pencil#init()
      autocmd plugin Filetype note         call pencil#init()
      autocmd plugin Filetype mail         call pencil#init()
      autocmd plugin FileType markdown,mkd call pencil#init()
      autocmd plugin Filetype vimwiki      call pencil#init()

    " .................................................................. Quantum

      let g:quantum_italics=1               " italicize comments

    " ................................................................ Signature

      " vim convention m'ark key conflicts with my colemak-shift-dh layout
      " using apostrophe instead, preferable imo :)
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
      "     call core#Colemak()
      "     nmap f <Plug>Sneak_s
      "     nmap F <Plug>Sneak_S
      "   endif
      " endfunction
      "
      " " preserve s and remap to f
      " autocmd plugin BufNewFile,BufRead * call Sneak_f()

      " sneak maps s, S == cc
      nmap <leader>s cl

      " replace 'f' with 1-char Sneak
      nmap f         <Plug>Sneak_f
      nmap F         <Plug>Sneak_F
      xmap f         <Plug>Sneak_f
      xmap F         <Plug>Sneak_F
      omap f         <Plug>Sneak_f
      omap F         <Plug>Sneak_F
      " replace 't' with 1-char Sneak
      nmap t         <Plug>Sneak_t
      nmap T         <Plug>Sneak_T
      xmap t         <Plug>Sneak_t
      xmap T         <Plug>Sneak_T
      omap t         <Plug>Sneak_t
      omap T         <Plug>Sneak_T

    " ................................................................ Solarized

      let g:solarized_termtrans = 1         " terminal transparency (0) off (1) on

      if has("gui_running")
        " follow the sun, see crontab
        if !empty(glob('~/.session/follow_the_sun'))
          colorscheme quantum
          set background=dark
        else
          colorscheme solarized8_high
          set background=light
        endif
      endif
      set termguicolors                     " for neovim

      syntax enable

    " ................................................................... Tagbar

      " let g:tagbar_ctags_bin    = 'ctags-exuberant'
      let g:tagbar_map_togglesort = 'r'     " preserve sneak s

      " markdown via .ctags
      let g:tagbar_type_markdown =
          \{
          \  'ctagstype'  : 'markdown'
          \, 'kinds'      : ['h:Heading_L1', 'i:Heading_L2', 'k:Heading_L3']
          \}

      " " see https://github.com/jszakmeister/markdown2ctags
      " let g:tagbar_type_markdown =
      "     \{
      "     \  'ctagstype'  : 'markdown'
      "     \, 'ctagsbin'   : '~/.vim/bin/markdown2ctags.py'
      "     \, 'ctagsargs'  : '-f - --sort=yes'
      "     \, 'kinds'      : ['s:sections', 'i:images']
      "     \, 'sro'        : '|'
      "     \, 'kind2scope' : { 's' : 'section' }
      "     \, 'sort'       : 0
      "     \}

      " " see https://github.com/vimwiki/utils/blob/master/vwtags.py
      " let g:tagbar_type_vimwiki =
      "     \{
      "     \  'ctagstype'  : 'vimwiki'
      "     \, 'kinds'      : ['h:header']
      "     \, 'sro'        : '&&&'
      "     \, 'kind2scope' : { 'h' : 'header' }
      "     \, 'sort'       : 0
      "     \, 'ctagsbin'   : '~/.vim/bin/vwtags.py'
      "     \, 'ctagsargs'  : 'default'
      "     \}

      " suppress unnecessary lightline processing
      nmap <silent><leader>t :TagbarOpenAutoClose<CR>:call lightline#disable()<CR>

    " .................................................................. Vimwiki

      " " disable tab for autocompletion
      " let g:vimwiki_table_mappings = 0
      " let g:vimwiki_table_auto_fmt = 0
      "
      " let g:vimwiki_list =
      "     \[
      "     \  {
      "     \    'path'      : '~/vimwiki/thedarnedestthing/'
      "     \,   'path_html' : '~/vimwiki/thedarnedestthing/html/'
      "     \,   'syntax'    : 'markdown'
      "     \  }
      "     \, {
      "     \    'path'      : '~/vimwiki/thestory/'
      "     \,   'path_html' : '~/vimwiki/thestory/html/'
      "     \,   'syntax'    : 'markdown'
      "     \  }
      "     \, {
      "     \    'path'      : '~/vimwiki/truthordie/'
      "     \,   'path_html' : '~/vimwiki/truthordie/html/'
      "     \,   'syntax'    : 'markdown'
      "     \  }
      "     \, {
      "     \    'path'      : '~/vimwiki/shadowsandlight/'
      "     \,   'path_html' : '~/vimwiki/shadowsandlight/html/'
      "     \,   'syntax'    : 'markdown'
      "     \  }
      "     \, {
      "     \    'path'      : '~/vimwiki/healing/'
      "     \,   'path_html' : '~/vimwiki/healing/html/'
      "     \,   'syntax'    : 'markdown'
      "     \  }
      "     \, {
      "     \    'path'      : '~/vimwiki/colophon/'
      "     \,   'path_html' : '~/vimwiki/colophon/html/'
      "     \,   'syntax'    : 'markdown'
      "     \  }
      "     \, {
      "     \    'path'      : '~/vimwiki/notes/'
      "     \,   'path_html' : '~/vimwiki/notes/html/'
      "     \,   'syntax'    : 'markdown'
      "     \  }
      "     \]
      "
      " " header highlighting
      " highlight VimwikiHeader1 guifg=#d70000
      " highlight VimwikiHeader2 guifg=#af005f
      " highlight VimwikiHeader3 guifg=#5f5faf
      " highlight VimwikiHeader4 guifg=#0087ff
      " highlight VimwikiHeader5 guifg=#00afaf
      " highlight VimwikiHeader6 guifg=#5f8700
      "
      " " restore vimwiki link
      " function! VimwikiLink()
      "   highlight VimwikiLink guifg=#268bd2 gui=bold
      " endfunction
      "
      " " resolve <CR>, <Tab> conflicts with autocompletion (simple-complete)
      " function! VimwikiRemap()
      "   if !exists('b:vimwiki_remap')
      "     let b:vimwiki_remap = 1
      "     iunmap   <silent><buffer>           <CR>
      "     inoremap <expr><buffer><C-PageDown> vimwiki#tbl#kbd_tab()
      "     inoremap <expr><buffer><C-PageUp>   vimwiki#tbl#kbd_shift_tab()
      "     call core#RefreshGui()
      "   endif
      " endfunction
      "
      " autocmd plugin Filetype vimwiki call VimwikiLink()
      " autocmd plugin Filetype vimwiki setlocal nocp spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      " autocmd plugin BufEnter *
      "       \ if &filetype == 'vimwiki' | call VimwikiRemap() | endif
      " " cannot trap s:setup_buffer_leave() to avoid initialization error on 1st link
      " " see arch install patch to initialize s:vimwiki_autowriteall

    " ................................................................. Yankring

      let g:yankring_default_menu_mode  = 1  " menu on with no shortcut
      let g:yankring_dot_repeat_yank    = 1  " allow repeating yankring action
      let g:yankring_enabled            = 1  " disable yankring because of macro conflict
      let g:yankring_min_element_length = 5  " minimum yankring size
      let g:yankring_window_height      = 30 " horizontal window height
      let g:yankring_zap_keys           = '' " disable (conflicts with sneak)

      nmap <silent>Y         :<C-U>YRYankCount 'y$'<CR>
      nmap <silent><leader>y :YRShow<CR>

" setting.vim
