" sdothum - 2016 (c) wtfpl

" Keyboard
" ══════════════════════════════════════════════════════════════════════════════

  " Keyboard (re)mappings ______________________________________________________

    " .................................................................... Setup

      let mapleader   = "\<Space>"    " remap <leader> a la spacemacs
      let g:mapleader = "\<Space>"

      augroup kbd | autocmd! | augroup END

  " Cursor _____________________________________________________________________

    " ....................................................... Backspace settings

      set backspace=indent,eol,start  " allow backspace in insert mode
      set whichwrap=b,s,h,l,<,>,[,]   " backspace and cursor keys wrap

    " ......................................................... Cursor movements

      " up/down by screen lines, not file lines
      nnoremap k     gk
      vnoremap k     gk
      nnoremap j     gj
      vnoremap j     gj

      " up/down by paragraph sentence
      nmap <leader>( {{)
      nmap <leader>) })

      " insert mode local region cursor movements
      " <C-h> is overridden by auto-pairs delete <BS> when enabled
      " imap <C-h>   <Left>
      " imap <C-j>   <Down>
      " imap <C-k>   <Up>
      " imap <C-l>   <Right>

    " ............................................................. Disable keys

      " affirm vim modal usage but these keys are remapped below anyway :)
      " (re-enabled for colemak keyboard as qwerty key cluster no longer valid)
      " imap <down>  <nop>
      " imap <left>  <nop>
      " imap <right> <nop>
      " imap <up>    <nop>
      " nmap <down>  <nop>
      " nmap <left>  <nop>
      " nmap <right> <nop>
      " nmap <up>    <nop>

  " Keyboard shortcuts _________________________________________________________

    " ............................................................. Copy / paste

      " prevent cascading paste insert
      set pastetoggle=<F3>

      " yank from the cursor to the end of the line, to be consistent with C and D.
      " see yankring for plugin equivalent
      nnoremap Y  y$
      nnoremap vv V
      nnoremap V  <C-v>$

      " reselect/reyank text just pasted
      nnoremap <leader>v gv
      nnoremap <leader>V gvy
      map      <leader>p pgvy

      " highlight last inserted text
      nnoremap <leader>i `[v`]

      " disable paste mode when leaving Insert Mode
      autocmd kbd InsertLeave * set nopaste

    " ...................................................... Sentence operations

      " use "as" suffix for outer sentence
      " change sentence
      nnoremap <leader>cc cis
      " cut sentence
      nnoremap <leader>dd dis
      " yank sentence
      nnoremap <leader>yy yis

    " .................................................... Clipboard cut / paste

      " visual mode yank/cut clipboard actions
      " "+Y yank to clipboard
      vnoremap <C-F2> "+y
      vnoremap <S-F2> "+Y
      " "+D cut to clipboard
      vnoremap <C-F3> "+d
      vnoremap <S-F3> "+D

      " " normal/insert mode paste actions
      " " "+P pads space after insert
      " " note: to enter visual block mode type v<C-v>
      imap <F2> <ESC>"+pli
      nmap <F2> h"+pl
      " command mode insertion (paste) of current yank buffer
      cmap <F2> <C-r>"

  " Abbreviations ______________________________________________________________
  
    " ................................................................. Personal

      command! I call iabbrev I i

      nmap <C-F6> :unabbrev I<CR>
      imap <C-F6> <C-o>:unabbrev I<CR>
    
      autocmd kbd Filetype draft    iabbrev I i
      autocmd kbd Filetype mail     iabbrev I i
      autocmd kbd Filetype markdown iabbrev I i
      autocmd kbd Filetype note     iabbrev I i

" keyboard.vim
