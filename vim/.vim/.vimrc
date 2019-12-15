" sdothum - 2016 (c) wtfpl

" Config
" ══════════════════════════════════════════════════════════════════════════════

" ...................................................................... Session
" dynamic settings, see after/plugin/*
let g:duochrome_cursorline = !empty(glob('~/.session/vim:cursorline'))   " highlight
let g:duochrome_cursorline = !empty(glob('~/.session/vim:underline')) ? 2 : g:duochrome_cursorline
let g:cursorword           = !empty(glob('~/.session/vim:cursorword'))   " highlighting
let g:dark                 = !empty(glob('~/.session/vim:dark'))         " background
let g:mono                 = !empty(glob('~/.session/vim:mono'))         " single width utf-8
let g:readability          = !empty(glob('~/.session/vim:readability'))  " fontsize
let g:trace                = !empty(glob('~/.session/vim:trace'))        " debug

if &diff | let g:duochrome_cursorline = g:duochrome_cursorline ? g:duochrome_cursorline : 2 | endif  " underline default

" ...................................................................... Startup
runtime config/startup.vim
runtime config/keyboard.vim

" ...................................................................... Plugins
runtime config/plugin.vim
runtime config/setting.vim

" ........................................................................... UI
runtime config/default.vim
runtime config/gui.vim

" ........................................................................ Theme
runtime config/theme.vim
runtime config/ui.vim
runtime config/statusline.vim

" ..................................................................... Editting
runtime config/buffer.vim
runtime config/edit.vim
runtime config/heading.vim

" vim: set ft=vim: .vimrc
