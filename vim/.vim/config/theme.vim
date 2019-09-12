" sdothum - 2016 (c) wtfpl

" Themes
" ══════════════════════════════════════════════════════════════════════════════

" The look _____________________________________________________________________

" ........................................................................ Setup
let g:colorscheme  = 'quantum'  " duochrome -> flatwhite -> quantum ->, see theme#NextScheme()

" .................................................................. Color codes
" duochrome monochromatic palette
let g:black           = '#222222'
let g:medium_gray     = '#767676'
let g:white           = '#f7f3ee'
let g:light_black     = '#424242'
let g:lighter_black   = '#545454'
let g:subtle_black    = '#303030'
let g:light_gray      = '#999999'
let g:lighter_gray    = '#CCCCCC'
let g:lightest_gray   = '#E5E5E5'
let g:dark_red        = '#C30771'
let g:light_red       = '#E32791'
let g:dark_blue       = '#008EC4'
let g:light_blue      = '#B6D6FD'
let g:dark_cyan       = '#20A5BA'
let g:light_cyan      = '#4FB8CC'
let g:dark_green      = '#10A778'
let g:light_green     = '#5FD7A7'
let g:dark_purple     = '#523C79'
let g:light_purple    = '#6855DE'
let g:light_yellow    = '#F3E430'
let g:dark_yellow     = '#A89C14'

" flatwhite colour palette (light)
let g:base1           = '#605a52'
let g:base2           = '#93836c'
let g:base3           = '#b9a992'
let g:base4           = '#dcd3c6'
let g:base5           = '#e4ddd2'
let g:base6           = '#f1ece4'
let g:base7           = '#f7f3ee'
let g:blue_bg         = '#dde4f2'
let g:blue_text       = '#4c5361'
let g:green_bg        = '#e2e9c1'
let g:green_text      = '#525643'
let g:orange_bg       = '#f7e0c3'
let g:orange_text     = '#5b5143'
let g:purple_bg       = '#f1ddf1'
let g:purple_text     = '#614c61'
let g:teal_bg         = '#d2ebe3'
let g:teal_text       = '#465953'

" one colour palette (light) for diff
let g:mono_1          = '#494b53'
let g:mono_2          = '#696c77'
let g:mono_3          = '#a0a1a7'
let g:mono_4          = '#c2c2c3'
let g:hue_1           = '#0184bc'      " cyan
let g:hue_2           = '#4078f2'      " blue
let g:hue_3           = '#a626a4'      " purple
let g:hue_4           = '#50a14f'      " green
let g:hue_5           = '#e45649'      " red 1
let g:hue_5_2         = '#ca1243'      " red 2
let g:hue_6           = '#986801'      " orange 1
let g:hue_6_2         = '#c18401'      " orange 2
let g:pmenu           = '#dfdfdf'
let g:special_grey    = '#d3d3d3'
let g:syntax_accent_2 = '#0083be'
let g:syntax_accent   = '#526fff'
let g:syntax_bg       = '#fafafa'
let g:syntax_cursor   = '#f0f0f0'
let g:syntax_gutter   = '#9e9e9e'
let g:vertsplit       = '#e7e9e1'
let g:visual_grey     = '#d0d0d0'

" quantum colour palette (dark)
let g:gray1           = '#263238'      " 023 (005f5f)
let g:gray2           = '#2c3a41'      " 023 (005f5f)
let g:gray3           = '#425762'      " 059 (5f5f5f)
let g:gray4           = '#658494'      " 066 (5f8787)
let g:gray5           = '#aebbc5'      " 146 (afafd7)
let g:blue            = '#70ace5'      " 074 (5fafd7)
let g:cyan            = '#69c5ce'      " 080 (5fd7d7)
let g:green           = '#87bb7c'      " 108 (87af87)
let g:indigo          = '#7681de'      " 104 (8787d7)
let g:orange          = '#d7956e'      " 173 (d7875f)
let g:purple          = '#a48add'      " 140 (af87d7)
let g:red             = '#dd7186'      " 168 (d75f87)
let g:yellow          = '#d5b875'      " 180 (d7af87)

" cursor, highlight
let g:cursor          = '#20fccf'      " analogous iA Writer '#20bbfc' cursor color
let g:black           = '#000000'      " cursor foreground
let g:blue            = '#0000e6'      " cursor foreground
let g:spell           = '#ffd1dc'      " light spelling/grammar error
let g:white           = g:base7        " cursor foreground

" hyperfocus
let g:light           = '#dddddd'      " light hyperfocus fade
let g:dark            = '#444444'      " dark hyperfocus fade
let g:light_fg        = g:light_black  " light cursorline (adjust to preferred highlight)
let g:insert_fg       = g:light_gray   " insert mode prose text (cursorline contrast)
let g:dark_fg         = g:gray5        " dark cursorline (adjust to preferred highlight)

augroup theme | autocmd! | augroup END

" Theme ________________________________________________________________________

" .......................................................... Default colorscheme
if has('gui_running')
  if ! empty(glob('~/.session/nighttime')) 
    let g:colorscheme = 'flatwhite'           " startup -> quantum
  endif
  if &diff                                    " diff highlights the SignColumn which can only be cleared afterwards..
    colorscheme one                           " diff mode doesn't work well with reverse (block) highlighting
    autocmd theme CursorHold * hi! link SignColumn NonText
  else 
    autocmd theme VimEnter * NextColorScheme  " startup (quantum) -> duochrome
  endif
endif

autocmd theme VimEnter * NoTilde              " startup timing fix
autocmd theme VimEnter * Theme                " startup timing fix

" ................................................................ Switch colour
nmap <silent><S-F8>      :LiteSwitch<CR>
imap <silent><S-F8> <C-o>:LiteSwitch<CR>

autocmd theme InsertEnter * LineNr
autocmd theme InsertLeave * LineNr

" theme.vim
