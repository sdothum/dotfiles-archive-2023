" sdothum - 2016 (c) wtfpl

" Plugins
" ══════════════════════════════════════════════════════════════════════════════

" Plugin settings ______________________________________________________________

" ............................................................... Notational-fzf
" notational velocity $USER path rules: [regex, [path*], ext]
" note: regex magic is not enabled at this stage so force with '\v'
let g:user_nv_paths = [['.wiki$',                           ['~/vimwiki', '~/drafts'],         'wiki' ]
  \,                   ['.draft$',                          ['~/drafts'],                      'draft']
  \,                   ['.note$',                           ['~/notes'],                       'note' ]
  \,                   ['\v([~]|' . $HOME . '|/stow)/bin/', ['~/bin'],                         ''     ]
  \,                   ['fish/',                            ['~/.config/fish'],                ''     ]
  \,                   ['.vim/',                            ['~/.vim/config', '~/.vim/after'], 'vim'  ]
  \,                   ['herbstluftwm/',                    ['~/.config/herbstluftwm'],        ''     ]
  \,                   ['arch/',                            ['~/build/arch'],                  ''     ]
  \,                   ['obarun/',                          ['~/build/obarun'],                ''     ]
  \,                   ['void/',                            ['~/build/void'],                  ''     ]]
