" sdothum - 2016 (c) wtfpl

" Config
" ══════════════════════════════════════════════════════════════════════════════

" ...................................................................... Session
" if file exists -> attribute on
" ~/.session/vim:cursorword : highlighting
" ~/.session/vim:dark       : background
" ~/.session/vim:fontsize   : bigger
" ~/.session/vim:mono       : single width utf-8
" ~/.session/vim:trace      : debug

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
