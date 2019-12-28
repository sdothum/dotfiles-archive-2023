" sdothum - 2016 (c) wtfpl

" Buffers
" ══════════════════════════════════════════════════════════════════════════════

" File buffers _________________________________________________________________

" ................................................................ Close buffers
" delete any new diff buffer
function! buffer#CloseDiff()
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

" File actions _________________________________________________________________

function! s:home(dir)
  return $HOME . '/' . a:dir
endfunction

" .................................................................. Auto backup
" queue files written for vhg (may contain repeated update entries)
let s:repo = empty($STOW) ? s:home('.vim') : $STOW  " directory to auto backup

function! buffer#QueueFile()
  if empty($QUEUE) | return | endif  " see v script (sets QUEUE and invokes vhg)
  let l:path = resolve(expand('%:p'))
  if l:path =~ s:repo | call system('echo ' . substitute(l:path, s:repo, '', '') . ' >>' . s:home('.vim/job/') . $QUEUE) | endif
endfunction

" :wall on FocusLost does not trigger autocmd BufWrite (?)
function! buffer#QueueBuffers()
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

" Buffer handling ______________________________________________________________

" ................................................................ Switch buffer
" nmap <silent><Enter> :CloseDiff<CR>:silent bnext<CR>:call theme:SplitColors()<CR>
function! buffer#Enter()
  if CommandWindow()  " on q: to enter command-line window
    execute "normal! \<CR>"
  else
    CloseDiff
    silent bnext
    SplitColors
  endif
endfunction

" buffer.vim
