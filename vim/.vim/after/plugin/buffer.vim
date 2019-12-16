" sdothum - 2016 (c) wtfpl

" Buffers
" ══════════════════════════════════════════════════════════════════════════════

" File buffers _________________________________________________________________

" ................................................................ Close buffers
" delete any new diff buffer
function! s:closeDiff()
  if &diff     " caution: wincmd resets active window (affects :Buffer)
    wincmd h
    if empty(expand('%'))
      bdelete!
      diffoff  " restore pre-diff settings
      return 1
    endif
  endif
  return 0
endfunction

command! -bar CloseDiff silent! call <SID>closeDiff()
" close open diff or current buffer
command! CloseUnique silent! if !<SID>closeDiff() | silent bdelete | SplitColors | endif
" close all other buffers (and newly created no name buffer)
command! Singleton CloseDiff | %bdelete | edit # | bdelete # | SplitColors

" File actions _________________________________________________________________

" .................................................................. Auto backup
" queue files written for vhg (may contain repeated update entries)
let s:repo = empty($STOW) ? Home('.vim') : $STOW  " directory to auto backup

function! s:queueFile()
  if empty($QUEUE) | return | endif  " see v script (sets QUEUE and invokes vhg)
  let l:path = resolve(expand('%:p'))
  if l:path =~ s:repo | call system('echo ' . substitute(l:path, s:repo, '', '') . ' >>' . Home('.vim/job/') . $QUEUE) | endif
endfunction

command! QueueFile silent! call <SID>queueFile()

" :wall on FocusLost does not trigger autocmd BufWrite (?)
function! s:queueBuffers()
  if CommandWindow() | return | endif
  set lazyredraw
  let l:cur_buffer = bufnr('%')
  for l:buf in getbufinfo({'buflisted':1})
    if l:buf.changed
      execute 'buffer' . l:buf.bufnr
      update
      QueueFile
    endif
  endfor
  execute 'buffer' . l:cur_buffer
  set nolazyredraw
endfunction

command! QueueBuffers silent! call <SID>queueBuffers()

" Buffer handling ______________________________________________________________

" ................................................................ Switch buffer
" nmap <silent><Enter> :CloseDiff<CR>:silent bnext<CR>:call theme:SplitColors()<CR>
function! s:enter()
  if CommandWindow()  " on q: to enter command-line window
    execute "normal! \<CR>"
  else
    CloseDiff
    silent bnext
    SplitColors
  endif
endfunction

command! Enter silent! call <SID>enter()

" buffer.vim
