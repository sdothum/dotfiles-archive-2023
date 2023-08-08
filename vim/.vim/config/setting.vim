" sdothum - 2016 (c) wtfpl

" Plugins
" ══════════════════════════════════════════════════════════════════════════════

" Plugin settings ______________________________________________________________

augroup plugin | autocmd! | augroup END

" .......................................................................... Ale
let g:ale_sign_column_always = 1
let g:ale_sign_error         = '→'
let g:ale_sign_info          = g:ale_sign_error
let g:ale_sign_warning       = g:ale_sign_error
let g:ale_linter_aliases     =
  \{
  \  'wiki' : 'markdown'
  \, 'mail' : 'markdown'
  \}

nmap <silent><leader>k <Plug>(ale_previous_wrap)
nmap <silent><leader>j <Plug>(ale_next_wrap)
nmap <silent><leader>? <Plug>(ale_detail)
nmap <silent><leader>! :ALEToggle

" ................................................................... Auto-pairs
let g:AutoPairsMapBS                = 1       " auto delete symbol pairs
" let g:AutoPairsMapSpace           = 1       " disable to make iabbrev work!
" let g:AutoPairsFlyMode            = 1       " auto pair jumping
let g:AutoPairsShortcutBackInsert = '<C-BS>'  " undo auto pair jump -> close pair

" .......................................................................... coc
" May need for Vim (not Neovim) since coc.nvim calculates byte offset by count
" utf-8 byte sequence
set encoding=utf-8
" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
" delays and poor user experience
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s)
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying code actions to the selected code block
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying code actions at the cursor position
nmap <leader>ac  <Plug>(coc-codeaction-cursor)
" Remap keys for apply code actions affect whole buffer
nmap <leader>as  <Plug>(coc-codeaction-source)
" Apply the most preferred quickfix action to fix diagnostic on the current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Remap keys for applying refactor code actions
nmap <silent> <leader>re <Plug>(coc-codeaction-refactor)
xmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)
nmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)

" Run the Code Lens action on the current line
nmap <leader>cl  <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> to scroll float windows/popups
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges
" Requires 'textDocument/selectionRange' support of language server
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" ......................................................................... Cool
let g:CoolTotalMatches = 1  " command-line match statistics

" ................................................................... Cursorword
autocmd plugin BufRead * let b:cursorword = Prose() ? 1 : g:cursorword

" ................................................................... Easy-align
let g:easy_align_delimiters =
  \{
  \  '>' : { 'pattern' : '>>\|=>\|>' }
  \, '^' : { 'pattern' : '=',        'left_margin' : 0, 'right_margin' : 0, 'align' : 'right' }
  \, '(' : { 'pattern' : '(',        'left_margin' : 0, 'right_margin' : 0 }
  \, ')' : { 'pattern' : ')',        'left_margin' : 0 }
  \, '[' : { 'pattern' : '[',        'left_margin' : 1, 'right_margin' : 0 }
  \, ']' : { 'pattern' : ']',        'left_margin' : 1 }
  \, '/' : { 'pattern' : '//\|/\*',  'left_margin' : 2 }
  \, '\' : { 'pattern' : '\\ *$',    'left_margin' : 1 }
  \, '|' : { 'pattern' : '||',       'left_margin' : 1 }
  \, '-' : { 'pattern' : '--',       'left_margin' : 2 }
  \, '#' : { 'pattern' : ' #',       'left_margin' : 1 }
  \, '"' : { 'pattern' : ' "',       'left_margin' : 1 }
  \, ':' : { 'pattern' : ':',        'left_margin' : 0, 'right_margin' : 0 }
  \, ';' : { 'pattern' : ';;',       'left_margin' : 1, 'right_margin' : 2 }
  \, '.' : { 'pattern' : '\.',       'left_margin' : 1, 'right_margin' : 1 }
  \}

let g:easy_align_ignore_groups = []  " process comments

vmap <Enter>   <Plug>(EasyAlign)
nmap <leader>a <Plug>(EasyAlign)
" format markdown table
vmap <Bar>     :EasyAlign! *<Bar><CR>

" .................................................................. Eightheader
" justified fold headers
let &foldtext =
  \"EightHeaderFolds(
  \  '\\=s:fullwidth'
  \, 'left'
  \, [repeat( '  ', v:foldlevel - 1 ), ' ', '']
  \, '\\= s:foldlines . \" lines\"'
  \, ''
  \)"

" ...................................................................... Endwise
" add fish shell syntax rule, see ~/.vim/plugged/vim-fish/syntax/fish.vim
" autocmd plugin FileType fish
"     \  let b:endwise_addition  = 'end'
"     \| let b:endwise_words     = 'function,begin,if,while,for,switch'
"     \| let b:endwise_syngroups = 'shFunctionKey'

" .......................................................................... Fzf
" 'border' defines border around notational-fzf preview window
" fzf opens term window using ctermbg color, hence, slight tint mismatch with vim vindows
let g:fzf_colors =
  \{
  \  'fg'      : ['fg', 'Normal'    ]
  \, 'bg'      : ['bg', 'fzf_bg'    ]
  \, 'hl'      : ['fg', 'Error'     ]
  \, 'fg+'     : ['fg', 'Statement' ]
  \, 'bg+'     : ['bg', 'fzf_bg'    ]
  \, 'hl+'     : ['fg', 'Error'     ]
  \, 'border'  : ['fg', 'fzf_bg'    ]
  \, 'gutter'  : ['bg', 'fzf_bg'    ]
  \, 'header'  : ['fg', 'Normal'    ]
  \, 'info'    : ['fg', 'Error'     ]
  \, 'marker'  : ['fg', 'Error'     ]
  \, 'pointer' : ['fg', 'Error'     ]
  \, 'prompt'  : ['fg', 'Constant'  ]
  \, 'spinner' : ['fg', 'Error'     ]
  \}

" close any diff buffer before leaving buffer
nmap <silent><leader>b :CloseDiff<CR>:Buffers<CR>
" see notational-fzf
nmap <silent><leader>l :NV<CR>
nmap <silent><leader>m :Marks<CR>
nmap <silent><leader>f :FZF<CR>

" ............................................................... Graphical undo
let g:mundo_width           = 34
let g:mundo_preview_bottom  = 1
let g:mundo_preview_height  = 15
let g:mundo_close_on_revert = 1  " automatically close windows
let g:mundo_right           = 0  " side (right side produces wincmd error, see mundo.vim)
let g:mundo_verbose_graph   = 0  " condensed tree
let g:mundo_mirror_graph    = 0  " left align tree

function! s:toggleUndo()
  let l:filetype = &filetype     " mundo alters markdown filetype to conf (?)
  Quietly MundoToggle
  let &filetype = l:filetype
  if bufname('') == '__Mundo__'  " position cursor at current state (row) mundo bug
    normal ggf@jk:noh<CR>
  endif
endfunction

nmap <silent><leader>u :call <SID>toggleUndo()<CR>

" for instance when mundo window is orphaned (trap timing conflict)
autocmd plugin BufHidden __Mundo_Preview__ Quiet bdelete!\ __Mundo_Preview__

" ............................................................. Highlighted yank
let g:highlightedyank_highlight_duration = 250

" ................................................................ Indent guides
let g:indent_guides_auto_colors = 0  " highlight even indents, see gui:ToggleColumn, theme:Guides()

nmap <silent><leader><Bar> :IndentGuidesToggle<CR>

" .................................................................... Limelight
let g:limelight_default_coefficient = 0.8
let g:limelight_paragraph_span = 0  " include preceding/following paragraphs
let g:limelight_priority       = 1  " -1 to hlsearch highlight all paragraphs, 1 per paragraph

" .................................................................. Litecorrect
nnoremap <C-s> [s1z=<c-o>
inoremap <C-s> <c-g>u<Esc>[s1z=`]A<c-g>u

function! s:toggleSpell()
  let &spell = !&spell
  if !empty(PencilMode()) | execute &spell ? 'Pencil' : 'NoPencil' | endif
endfunction

" correction related, but really bound to Pencil
nmap <silent><S-F9>      :silent call <SID>toggleSpell()<CR>
imap <silent><S-F9> <C-o>:silent call <SID>toggleSpell()<CR>

autocmd plugin FileType mail,markdown call litecorrect#init()

" ...................................................................... Matchup

let g:matchup_matchparen_deferred           = 1
let g:matchup_matchparen_hi_surround_always = 1
let g:matchup_matchparen_offscreen          = {}  " suppress off screen status line notification

" ................................................................ Narrow region
let g:nrrw_rgn_vert          = 0   " open in horizontal split buffer
let g:nrrw_topbot_leftright  = 'botright'
let g:nrrw_rgn_nomap_nr      = 1   " disable nr mappings
let g:nrrw_rgn_nomap_Nr      = 1
let g:nrrw_rgn_resize_window = 'relative'
let g:nrrw_rgn_rel_min       = 50  " relative window size

" NrrwRgn buffer actions are not trapped by SplitColors autocmd (..?)
function! s:closeNR()
  if expand('%t') =~ 'NrrwRgn' | execute ':wq' | SplitColors | endif
endfunction

vmap <leader>n :NR<CR>:SplitColors<CR>
nmap <leader>n :call <SID>closeNR()<CR>

" ................................................................ Nerdcommenter
let g:NERDSpaceDelims            = 1  " space after comment delimiter
let g:NERDCompactSexyComs        = 1  " prettify multi-line
let g:NERDDefaultAlign           = 'left'
let g:NERDCustomDelimiters       = { 'c' : { 'left' : '//', 'right' : '' } }
let g:NERDCommentEmptyLines      = 1  " comment blank lines
let g:NERDTrimTrailingWhitespace = 1  " trim trailing whitespace

" <leader>cs to force commenting of first line comment
map  <leader>c <Plug>NERDCommenterToggle
imap ,c        <C-o>:execute "normal \<Plug>NERDCommenterToggle"<CR>

" ............................................................... Notational-fzf
let g:nv_create_note_window     = 'edit'
let g:nv_expect_keys            = []
let g:nv_main_directory         = './'    " create new notes in current directory
let g:nv_preview_direction      = 'right'
" let g:nv_preview_width        = 50      " does not calculate as expected, use default
let g:nv_show_preview           = 1       " alt-p to toggle preview
let g:nv_use_short_pathnames    = 1
" let g:nv_window_width         = '50%'   " window height
let g:nv_wrap_preview_text      = 0

" dynamically setup notational-fzf :)
runtime config/.notational-fzf.vim        " source $USER g:user_nv_paths: [regex, [path*], ext]
let g:nv_search_paths           = ['./']  " default search from current directory
let g:nv_default_extension      = ''

function! s:argFile()
  return argc() > 0 ? argv(0) : $PWD      " buffers load after plugins so parse command line for filename
endfunction

for i in g:user_nv_paths
  if s:argFile() =~ i[0]                  " use single path spec for file set
    let g:nv_search_paths       = i[1]
    let g:nv_default_extension  = i[2]
    break
  endif
endfor
unlet g:user_nv_paths

nnoremap <silent><leader>L :NV<CR>

" ..................................................................... Peakaboo
" peakaboo buffer bypass autocmd Layout plugin settings
let g:peekaboo_window = 'vert bo 40new | setlocal foldcolumn=2'  " flush left

" ....................................................................... Pencil
let g:pencil#wrapModeDefaultault = 'soft'  " 'hard' (default), 'soft'
let g:pencil#textwidth       = 72          " 74 (default)
let g:pencil#joinspaces      = 0           " 0=one_space (default), 1=two_spaces
let g:pencil#cursorwrap      = 1           " 0=disable, 1=enable (default)
let g:pencil#autoformat      = 1           " 0=manual, 1=auto (default)

" g:pencil#textwidth doesn't set (..?)
autocmd plugin FileType mail,markdown call pencil#init({'wrap': 'soft'}) | setlocal textwidth=72

" .................................................................... Signature
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

" ...................................................................... Signify
let g:signify_vcs_list = ['hg']
let g:signify_realtime = 1      " async updates

" ........................................................................ Sneak
" by default, use cc, cl for s, S
let g:sneak#streak       = 1    " streak mode
let g:sneak#s_next       = 1    " clever next, use s S for ; .
let g:sneak#absolute_dir = 0    " next follows direction of invocation
let g:sneak#use_ic_scs   = 1    " use vim case setting
let g:sneak#prompt       = '>'  " prompt
let g:sneak#label        = 1    " label mode

" " remap sneak_s to preserve s
" function! s:sneak_f()
"   if !exists("g:sneak_f") | let g:sneak_f = 1
"     unmap s
"     unmap S
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

" ....................................................................... Tagbar
" let g:tagbar_ctags_bin    = 'ctags-exuberant'
let g:tagbar_map_togglesort = 'r'  " preserve sneak s
let g:tagbar_width          = 40

nmap <silent><leader>t :TagbarOpenAutoClose<CR>

" ................................................................ Textobj quote
let g:typo = 0  " typography mode (0) off (1) on

" typography mode for prose (and html content editing)
function! s:toggleTypo()
  let g:typo = !g:typo
  if g:typo          " html <p> content shortcuts
    Typo
    " ToggleBreak 0  " disable line wrap column highlight to show spelling errors
    imap ...      …<Space>
    imap --       <Space>&ndash;<Space>
    imap .<Space> .<Space><CR>
    imap ?<Space> ?<Space><CR>
    imap !<Space> !<Space><CR>
  else
    NoTypo
    " ToggleBreak 1  " restore line wrap column highlight
    iunmap ...
    iunmap --
    iunmap .<Space>
    iunmap ?<Space>
    iunmap !<Space>
  endif
  Status Typography: g:typo
endfunction

nmap <silent><S-F5>      :call <SID>toggleTypo()<CR>
imap <silent><S-F5> <C-o>:call <SID>toggleTypo()<CR>

" " with vim-surround: cs"q
" map  <silent><leader>qc <Plug>ReplaceWithCurly
" map  <silent><leader>qs <Plug>ReplaceWithStraight
"
" autocmd plugin FileType html     call textobj#quote#init()
" autocmd plugin FileType markdown call textobj#quote#init()

" .................................................................... UltiSnips
let g:UltiSnipsExpandTrigger       = "<C-x>"
let g:UltiSnipsListSnippets        = "<C-z>"
let g:UltiSnipsJumpForwardTrigger  = "<C-j>"
let g:UltiSnipsJumpForwardTrigger  = "<C-l>"
let g:UltiSnipsJumpBackwardTrigger = "<C-k>"

" ....................................................................... Vimade
nmap <silent><S-F11>       :VimadeToggle<CR>

autocmd plugin BufWinEnter __Mundo_* VimadeBufDisable

" ................................................................. Vim-markdown
let g:vim_markdown_conceal             = 2
let g:vim_markdown_conceal_code_blocks = 0
let g:vim_markdown_math                = 1
let g:vim_markdown_toml_frontmatter    = 1
let g:vim_markdown_frontmatter         = 1
let g:vim_markdown_strikethrough       = 1
let g:vim_markdown_autowrite           = 1
let g:vim_markdown_edit_url_in         = 'tab'
let g:vim_markdown_follow_anchor       = 1


" .................................................................... Vim-yoink
let g:yoinkMaxItems                   = 10
let g:yoinkSyncNumberedRegisters      = 0
let g:yoinkIncludeDeleteOperations    = 1
let g:yoinkAutoFormatPaste            = 0
let g:yoinkMoveCursorToEndOfPaste     = 0
let g:yoinkSwapClampAtEnds            = 0
let g:yoinkIncludeNamedRegisters      = 1
let g:yoinkSyncSystemClipboardOnFocus = 0

nmap <c-n> <plug>(YoinkPostPasteSwapBack)
nmap <c-p> <plug>(YoinkPostPasteSwapForward)
nmap p <plug>(YoinkPaste_p)
nmap P <plug>(YoinkPaste_P)

" Also replace the default gp with yoink paste so we can toggle paste in this case too
nmap gp <plug>(YoinkPaste_gp)
nmap gP <plug>(YoinkPaste_gP)
nmap [y <plug>(YoinkRotateBack)
nmap ]y <plug>(YoinkRotateForward)

nmap <c-=> <plug>(YoinkPostPasteToggleFormat)
nmap y <plug>(YoinkYankPreserveCursorPosition)
xmap y <plug>(YoinkYankPreserveCursorPosition)

nmap <silent><leader>y :Yanks<CR>
nmap <silent><leader>Y :ClearYanks<CR>

" ..................................................................... Yankring
" let g:yankring_default_menu_mode  = 1   " menu on with no shortcut
" let g:yankring_dot_repeat_yank    = 1   " allow repeating yankring action
" let g:yankring_enabled            = 1   " disable yankring because of macro conflict
" let g:yankring_min_element_length = 5   " minimum yankring size
" let g:yankring_window_height      = 30  " horizontal window height
" let g:yankring_zap_keys           = ''  " disable (conflicts with sneak)
"
" nmap <silent>Y         :<C-U>YRYankCount 'y$'<CR>
" nmap <silent><leader>y :YRShow<CR>

" setting.vim
