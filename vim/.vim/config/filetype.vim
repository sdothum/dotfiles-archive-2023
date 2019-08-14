" sdothum - 2016 (c) wtfpl

" Filetypes
" ══════════════════════════════════════════════════════════════════════════════

    " .................................................................... Setup

      augroup ft | autocmd! | augroup END

      " check on open
      autocmd ft BufNewFile,BufRead * BufferSettings

    " ................................................................ Filetypes

      " nmap <leader>f :set filetype=

      autocmd ft Filetype conf      setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype fish      setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype haskell   setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype julia     setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype lua       setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype python    setlocal nospell expandtab   tabstop=4 shiftwidth=4 softtabstop=4
      autocmd ft Filetype ruby      setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype shell     setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype sh        setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype slim      setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2
      autocmd ft Filetype snippet   setlocal nospell noexpandtab tabstop=2 shiftwidth=2
      autocmd ft Filetype vim       setlocal nospell expandtab   tabstop=2 shiftwidth=2 softtabstop=2

      autocmd ft Filetype draft     setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72 syntax=mail
      autocmd ft Filetype mail      setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72 syntax=mail
      autocmd ft Filetype markdown  setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72
      autocmd ft Filetype note      setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72

      " call to LiteType() to correct startup timimg Prose() state
      autocmd ft BufWinEnter *.wiki set filetype=markdown | call LiteType()

    " ...................................................................... CSS

      " sort align, note <C-v><keycode> to embed command mode keycode
      nmap <silent><leader>css :g/{/normal! f{viB:sort<C-v><CR><CR>
      nmap <silent><leader>cSS :g/{/normal! f{viB:EasyAlign<C-v><CR><C-v><Space><CR>
      nmap <silent><leader>CSS :g/{/normal! f{viB:sort<C-v><CR><CR>:g/{/normal! f{viB:EasyAlign<C-v><CR><C-v><Space><CR>

      " block align
      nnoremap <silent><S-F4> :CssBlockAlign<CR>

    " ................................................................... E-mail

      " position cursor for email reply or new message, see .sup/config.yaml and bin/dcompose
      autocmd ft Filetype mail ComposeMail

" filetype.vim
