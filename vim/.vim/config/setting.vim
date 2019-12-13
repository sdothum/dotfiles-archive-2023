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

" ................................................................... Cursorword
let g:cursorword = !empty(glob('~/.session/vim:cursorword'))

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
  \, '/' : { 'pattern' : '//',       'left_margin' : 2 }
  \, '\' : { 'pattern' : '\\ *$',    'left_margin' : 1 }
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
autocmd plugin FileType fish
    \  let b:endwise_addition  = 'end'
    \| let b:endwise_words     = 'function,begin,if,while,for,switch'
    \| let b:endwise_syngroups = 'shFunctionKey'

" .......................................................................... Fzf
" 'border' defines border around notational-fzf preview window
" { 'fg' : ['fg', 'Normal'], 'bg' : ['bg', 'Normal'] } cause a bg tint (..?)
let g:fzf_colors =
  \{
  \  'fg'      : ['bg', 'Ignore'    ]
  \, 'bg'      : ['bg', 'Ignore'    ]
  \, 'hl'      : ['fg', 'Error'     ]
  \, 'fg+'     : ['fg', 'Statement' ]
  \, 'bg+'     : ['bg', 'Normal'    ]
  \, 'hl+'     : ['fg', 'Error'     ]
  \, 'border'  : ['fg', 'Ignore'    ]
  \, 'gutter'  : ['bg', 'Normal'    ]
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
nmap <silent><leader>l :Lines<CR>
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
nmap <silent><S-F6>      :silent call <SID>toggleSpell()<CR>
imap <silent><S-F6> <C-o>:silent call <SID>toggleSpell()<CR>

autocmd plugin FileType mail,markdown call litecorrect#init()

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

" .................................................................. Neocomplete
let g:neocomplete#enable_at_startup                 = 1
let g:neocomplete#enable_smart_case                 = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3

" inoremap <expr><Tab>  neocomplete#start_manual_complete()
" inoremap <expr><TAB>  pumvisible() ?  "\<Down>" : neocomplete#start_manual_complete()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ neocomplete#start_manual_complete()

" ................................................................... Neosnippet
let g:neosnippet#snippets_directory            = '~/.vim/snippets'
let g:neosnippet#enable_snipmate_compatibility = 1
let g:neosnippet#disable_runtime_snippets      = { '_' : 1 }  " disable all runtime snippets
let g:neosnippet#scope_aliases =
  \{
  \  'new'  : 'conf,fish,hs,ruby,sh,zsh'
  \, 'text' : 'mail'
  \}

imap <C-x> <Plug>(neosnippet_expand_or_jump)
smap <C-x> <Plug>(neosnippet_jump)

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
let g:pencil#wrapModeDefaultault = 'hard'  " 'hard' (default), 'soft'
let g:pencil#textwidth       = 72          " 74 (default)
let g:pencil#joinspaces      = 0           " 0=one_space (default), 1=two_spaces
let g:pencil#cursorwrap      = 1           " 0=disable, 1=enable (default)
let g:pencil#autoformat      = 1           " 0=manual, 1=auto (default)

" g:pencil#textwidth doesn't set (..?)
autocmd plugin FileType mail,markdown call pencil#init() | setlocal textwidth=72

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
"   if !exists("g:sneak_f")
"     let g:sneak_f = 1
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

nmap <silent><S-F11>      :call <SID>toggleTypo()<CR>
imap <silent><S-F11> <C-o>:call <SID>toggleTypo()<CR>

" " with vim-surround: cs"q
" map  <silent><leader>qc <Plug>ReplaceWithCurly
" map  <silent><leader>qs <Plug>ReplaceWithStraight
"
" autocmd plugin FileType html     call textobj#quote#init()
" autocmd plugin FileType markdown call textobj#quote#init()

" ....................................................................... Vimade
autocmd plugin BufWinEnter __Mundo_* VimadeBufDisable

" ..................................................................... Yankring
let g:yankring_default_menu_mode  = 1   " menu on with no shortcut
let g:yankring_dot_repeat_yank    = 1   " allow repeating yankring action
let g:yankring_enabled            = 1   " disable yankring because of macro conflict
let g:yankring_min_element_length = 5   " minimum yankring size
let g:yankring_window_height      = 30  " horizontal window height
let g:yankring_zap_keys           = ''  " disable (conflicts with sneak)

nmap <silent>Y         :<C-U>YRYankCount 'y$'<CR>
nmap <silent><leader>y :YRShow<CR>

" setting.vim
