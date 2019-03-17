" sdothum - 2016 (c) wtfpl

" Plugins
" ══════════════════════════════════════════════════════════════════════════════

  " Plugin settings ____________________________________________________________

    " .................................................................... Setup

      augroup plugin | autocmd! | augroup END

    " ...................................................................... Ale

      let g:ale_sign_column_always = 1
      let g:ale_sign_error         = '→'
      let g:ale_sign_warning       = '→'
      let g:ale_linter_aliases     =
          \{
          \  'wiki' : 'markdown'
          \, 'mail' : 'markdown'
          \}

      nmap <silent><leader>k <Plug>(ale_previous_wrap)
      nmap <silent><leader>j <Plug>(ale_next_wrap)
      nmap <silent><leader>? <Plug>(ale_detail)
      nmap <silent><leader>! :ALEToggle

    " ............................................................... Auto-pairs

      let g:AutoPairsMapBS                = 1       " auto delete symbol pairs
      " let g:AutoPairsMapSpace           = 1       " disable to make iabbrev work!
      " let g:AutoPairsFlyMode            = 1       " auto pair jumping
      let g:AutoPairsShortcutBackInsert = '<C-BS>'  " undo auto pair jump -> close pair

    " ............................................................... Easy-align

      let g:easy_align_delimiters =
          \{
          \  '>' : { 'pattern' : '>>\|=>\|>' }
          \, '^' : { 'pattern' : '=',        'left_margin' : 0, 'right_margin' : 0, 'align' : 'right' }
          \, '(' : { 'pattern' : '(',        'left_margin' : 0, 'right_margin' : 0 }
          \, ')' : { 'pattern' : ')',        'left_margin' : 0 }
          \, '[' : { 'pattern' : '[',        'left_margin' : 1, 'right_margin' : 0 }
          \, ']' : { 'pattern' : ']',        'left_margin' : 1 }
          \, '/' : { 'pattern' : '//',       'left_margin' : 2 }
          \, '-' : { 'pattern' : '--',       'left_margin' : 2 }
          \, '#' : { 'pattern' : ' #',       'left_margin' : 1 }
          \, '"' : { 'pattern' : ' "',       'left_margin' : 1 }
          \}

      let g:easy_align_ignore_groups = []  " process comments

      vmap <Enter>   <Plug>(EasyAlign)
      nmap <leader>a <Plug>(EasyAlign)
      " format markdown table
      vmap <Bar>     :EasyAlign! *<Bar><CR>

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

      " add fish shell syntax rule, see ~/.vim/plugged/vim-fish/syntax/fish.vim
      autocmd plugin FileType fish
           \  let b:endwise_addition  = 'end'
           \| let b:endwise_words     = 'function,begin,if,while,for,switch'
           \| let b:endwise_syngroups = 'shFunctionKey'

    " ...................................................................... Fzf

      " let g:fzf_colors =
      "     \{
      "     \  'fg'      : ['fg', 'Normal']
      "     \, 'bg'      : ['bg', 'Normal']
      "     \, 'hl'      : ['fg', 'Special']
      "     \, 'fg+'     : ['fg', 'CursorLine']
      "     \, 'bg+'     : ['bg', 'CursorLine']
      "     \, 'hl+'     : ['fg', 'Special']
      "     \, 'info'    : ['fg', 'Special']
      "     \, 'border'  : ['fg', 'Ignore']
      "     \, 'prompt'  : ['fg', 'Directory']
      "     \, 'pointer' : ['fg', 'Special']
      "     \, 'marker'  : ['fg', 'Special']
      "     \, 'spinner' : ['fg', 'Special']
      "     \, 'header'  : ['fg', 'Directory']
      "     \}

      let g:fzf_colors =
          \{
          \  'fg+'  : ['fg', 'CursorLine']
          \, 'bg+'  : ['bg', 'CursorLine']
          \, 'info' : ['fg', 'Special']
          \}

      " close any diff buffer before leaving buffer
      nmap <silent><leader>b :CloseDiff<CR>:Buffers<CR>
      nmap <silent><leader>l :Lines<CR>
      nmap <silent><leader>m :Marks<CR>
      " nmap <leader>f       :FZF<CR>  " see notational-fzf for extended content search

    " ........................................................... Graphical undo

      let g:gundo_width           = 30
      let g:gundo_preview_bottom  = 1
      let g:gundo_preview_height  = 20
      let g:gundo_close_on_revert = 1  " automatically close windows

      function! s:toggleUndo()
        let l:filetype = &filetype  " gundo alters markdown filetype to conf (autocmd buffer side-effect?)
        GundoToggle
        let &filetype = l:filetype
      endfunction

      " nmap <silent><leader>u :GundoToggle<CR>
      nmap <silent><leader>u :call <SID>toggleUndo()<CR>

      autocmd plugin BufEnter  __Gundo__         setlocal numberwidth=3 foldcolumn=0
      " for instance when gundo window is orphaned (trap timing conflict)
      autocmd plugin BufHidden __Gundo_Preview__ Quiet bdelete!\ __Gundo_Preview__

    " ............................................................ Indent guides

      let g:indent_guides_auto_colors = 0  " highlight even indents, see core#ToggleColumn(), theme#Indent()

      nmap <silent><leader><Bar> :IndentGuidesToggle<CR>

    " ................................................................ Limelight

      " let g:limelight_default_coefficient = 0.8
      let g:limelight_paragraph_span = 0  " include preceding/following paragraphs
      let g:limelight_priority       = 1  " -1 to hlsearch highlight all paragraphs, 1 per paragraph

    " .............................................................. Litecorrect

      nnoremap <C-s> [s1z=<c-o>
      inoremap <C-s> <c-g>u<Esc>[s1z=`]A<c-g>u

      function! s:toggleSpell()
        let &spell = &spell == 0 ? 1 : 0
        if PencilMode() != '' | execute &spell == 0 ? 'NoPencil' : 'Pencil' | endif
      endfunction

      " correction related, but really bound to Pencil
      nmap <silent><S-F6> :silent call <SID>toggleSpell()<CR>
      imap <silent><S-F6> <C-o>:silent call <SID>toggleSpell()<CR>
      vmap <silent><S-F6> :<C-u>silent call <SID>toggleSpell()<CR>

      autocmd plugin Filetype draft        call litecorrect#init()
      autocmd plugin Filetype note         call litecorrect#init()
      autocmd plugin Filetype mail         call litecorrect#init()
      autocmd plugin FileType markdown,mkd call litecorrect#init()

    " .................................................................. LiteDFM

      let g:lite_dfm_left_offset = 22    " see theme#Margin()

    " ............................................................ Narrow region

      let g:nrrw_rgn_vert          = 0   " open in horizontal split buffer
      let g:nrrw_topbot_leftright  = 'botright'
      let g:nrrw_rgn_nomap_nr      = 1   " disable nr mappings
      let g:nrrw_rgn_nomap_Nr      = 1
      let g:nrrw_rgn_resize_window = 'relative'
      let g:nrrw_rgn_rel_min       = 50  " relative window size

      function! s:closeNR()
        if expand('%t') =~ 'NrrwRgn' | execute ':wq' | endif
      endfunction

      " apply refresh to narrow region buffer to apply layout defaults!
      vmap <leader>n <Plug>NrrwrgnDo:call ui#Retheme()<CR>
      nmap <leader>n :call <SID>closeNR()<CR>

    " .............................................................. Neocomplete

      let g:neocomplete#enable_at_startup                 = 1
      let g:neocomplete#enable_smart_case                 = 1
      let g:neocomplete#sources#syntax#min_keyword_length = 3

      " inoremap <expr><Tab>  neocomplete#start_manual_complete()
      " inoremap <expr><TAB>  pumvisible() ?  "\<Down>" :
      "   \ neocomplete#start_manual_complete()

      function! s:check_back_space() abort
        let col = col('.') - 1
        return ! col || getline('.')[col - 1]  =~ '\s'
      endfunction

      inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" :
               \ <SID>check_back_space() ? "\<TAB>" :
               \ neocomplete#start_manual_complete()

    " ............................................................... Neosnippet

      let g:neosnippet#snippets_directory            = '~/.vim/snippets'
      let g:neosnippet#enable_snipmate_compatibility = 1
      let g:neosnippet#disable_runtime_snippets      = { '_' : 1 }  " disable all runtime snippets
      " see core#CheckFiletype()
      let g:neosnippet#scope_aliases =
          \{
          \  'new'  : 'conf,fish,hs,ruby,sh,zsh'
          \, 'text' : 'mail'
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
      function! s:argFile()
        if argc() > 0 | return argv(0) | endif
        return ''
      endfunction

      let g:nv_create_note_window  = 'edit'
      let g:nv_default_extension   = ''
      let g:nv_expect_keys         = []
      let g:nv_main_directory      = './'    " create new notes in current directory
      let g:nv_preview_direction   = 'right'
      let g:nv_preview_width       = 55
      let g:nv_search_paths        = ['./']  " default search from current directory
      let g:nv_show_preview        = 1       " alt-p to toggle preview
      let g:nv_use_short_pathnames = 1
      let g:nv_wrap_preview_text   = 1

      " notational path rules: [regex, rootpath, ext]
      " note: regex magic is not enabled at this stage so force with '\v'
      let s:set_notational = [['.wiki$',                           ['~/vimwiki', '~/drafts'],  'wiki' ]
          \,                  ['.draft$',                          ['~/drafts'],               'draft']
          \,                  ['.note$',                           ['~/notes'],                'note' ]
          \,                  ['\v([~]|' . $HOME . '|/stow)/bin/', ['~/bin'],                  ''     ]
          \,                  ['.vim/',                            ['~/.vim/config'],          'vim'  ]
          \,                  ['herbstluftwm/',                    ['~/.config/herbstluftwm'], ''     ]
          \,                  ['archlinux/',                       ['~/build/archlinux'],      ''     ]]

      " dynamically setup notational-fzf :)
      for i in s:set_notational
        if s:argFile() =~ i[0]
          let g:nv_search_paths      = i[1]
          let g:nv_default_extension = i[2]
          break
        endif
      endfor

      nnoremap <silent><leader>f :NV<CR>

    " ................................................................... Pencil

      let g:pencil#wrapModeDefault = 'hard'  " 'hard' (def), 'soft'
      let g:pencil#textwidth       = 72      " 74 (def)
      let g:pencil#joinspaces      = 0       " 0=one_space (def), 1=two_spaces
      let g:pencil#cursorwrap      = 1       " 0=disable, 1=enable (def)
      let g:pencil#autoformat      = 1       " 0=manual, 1=auto (def)
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

    " .................................................................. Quantum

      let g:quantum_italics=1  " italicize comments

    " ................................................................ Signature

      " vim convention m'ark key conflicts with my colemak-shift-dh layout
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

    " .................................................................. Signify

      let g:signify_vcs_list = ['hg']
      let g:signify_realtime = 1      " async updates

    " .................................................................... Sneak

      " by default, use cc, cl for s, S
      let g:sneak#streak       = 1    " streak mode
      let g:sneak#s_next       = 1    " clever next, use s S for ; .
      let g:sneak#absolute_dir = 0    " next follows direction of invocation
      let g:sneak#use_ic_scs   = 1    " use vim case setting
      let g:sneak#prompt       = '>'  " prompt
      let g:sneak#label        = 1    " label mode

      " " remap sneak_s to preserve s
      " function! s:sneak_f()
      "   if ! exists("g:sneak_f")
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
      " autocmd plugin BufNewFile,BufRead * call <SID>sneak_f()

      " sneak maps s, S == cc
      nmap <leader>s cl

      " replace 'f' with 1-char Sneak
      nmap f <Plug>Sneak_f
      nmap F <Plug>Sneak_F
      xmap f <Plug>Sneak_f
      xmap F <Plug>Sneak_F
      omap f <Plug>Sneak_f
      omap F <Plug>Sneak_F
      " replace 't' with 1-char Sneak
      nmap t <Plug>Sneak_t
      nmap T <Plug>Sneak_T
      xmap t <Plug>Sneak_t
      xmap T <Plug>Sneak_T
      omap t <Plug>Sneak_t
      omap T <Plug>Sneak_T

    " ................................................................ Solarized

      let g:solarized_termtrans = 1  " terminal transparency (0) off (1) on
      set termguicolors              " for neovim

      syntax enable

    " ................................................................... Tagbar

      " let g:tagbar_ctags_bin    = 'ctags-exuberant'
      let g:tagbar_map_togglesort = 'r'  " preserve sneak s

      nmap <silent><leader>t :TagbarOpenAutoClose<CR>

    " ............................................................ Textobj quote

      let s:educate = 0

      " typographical mode for prose (and html content editing)
      function! s:toggleEducate()
        let s:educate = s:educate ? 0 : 1
        if s:educate  " html <p> content shortcuts
          Educate
          " ToggleColumnWrap 0  " disable line wrap column highlight to show spelling errors
          imap ...      …<Space>
          imap --       <Space>&ndash;<Space>
          imap .<Space> .<Space><CR>
          imap ?<Space> ?<Space><CR>
          imap !<Space> !<Space><CR>
        else
          NoEducate
          " ToggleColumnWrap 1  " restore line wrap column highlight
          iunmap ...
          iunmap --
          iunmap .<Space>
          iunmap ?<Space>
          iunmap !<Space>
        endif
        call core#Status('Typography', s:educate)
      endfunction

      nmap <silent><F11> :call <SID>toggleEducate()<CR>
      imap <silent><F11> <C-o>:call <SID>toggleEducate()<CR>

      " with vim-surround: cs"q
      map  <silent><leader>qc <Plug>ReplaceWithCurly
      map  <silent><leader>qs <Plug>ReplaceWithStraight

      autocmd plugin FileType html     call textobj#quote#init()
      autocmd plugin FileType markdown call textobj#quote#init()

    " ................................................................. Yankring

      let g:yankring_default_menu_mode  = 1   " menu on with no shortcut
      let g:yankring_dot_repeat_yank    = 1   " allow repeating yankring action
      let g:yankring_enabled            = 1   " disable yankring because of macro conflict
      let g:yankring_min_element_length = 5   " minimum yankring size
      let g:yankring_window_height      = 30  " horizontal window height
      let g:yankring_zap_keys           = ''  " disable (conflicts with sneak)

      nmap <silent>Y         :<C-U>YRYankCount 'y$'<CR>
      nmap <silent><leader>y :YRShow<CR>

" setting.vim
