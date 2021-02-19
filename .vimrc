packadd vim-c64jasm
au BufNewFile,BufRead *.asm set ft=c64jasm
au FileType asm set commentstring=;%s
noremap <F6> :wa<CR>:make %<.prg<bar> cwindow<CR>:redraw!<CR>
noremap <F7> :wa<CR>:make %<.debug<bar> cwindow<CR>:redraw!<CR>
noremap <F8> :wa<CR>:make %<.exe.prg<bar> cwindow<CR>:redraw!<CR>
" set errorformat=%EError:\ %m,%Cat\ line\ %l\\,\ column\ %c\ in\ %f,%Z
set errorformat=%f:%l:%c:\ %m
set autoindent
set shiftwidth=2
set tabstop=2
set softtabstop=2
set smartindent
set expandtab
set foldmethod=marker
set foldlevel=0
