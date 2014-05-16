if &cp | set nocp | endif
nnoremap h !!$HOME/.vim/ftplugin/mercuryhdr.sh %:set ft=mercury ff=unix ts=4 sw=4 et
nnoremap l o0%----------------------------------------------------------------------------%x
let s:cpo_save=&cpo
set cpo&vim
nnoremap ,m b:!mmake<Up>
nmap gx <Plug>NetrwBrowseX
nnoremap <F8> :s/% //ej
nnoremap <F7> 0i% j
nnoremap <F6> I{ %a }j
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
inoremap l ------------------------------------------------------------------------------80|C%
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autowrite
set background=dark
set backspace=2
set cscopeprg=/usr/bin/cscope
set cscopetag
set cscopeverbose
set expandtab
set fileencodings=ucs-bom,utf-8,latin1
set guicursor=n-v-c:block-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175,a:blinkon0
set guifont=Monospace\ 11
set helplang=en
set hidden
set history=50
set hlsearch
set incsearch
set laststatus=2
set mouse=a
set pastetoggle=<F2>
set ruler
set shiftwidth=4
set smartcase
set smarttab
set softtabstop=4
set suffixes=.bak,~,.swp,.o,.info.aux.log,.dvi,.bbl,.blg,.brf,.cb,.idx,.ilg,.inx,.out,.toc,.mh,.err,.init
set tabstop=4
set termencoding=utf-8
set titlestring=v:dim.m
set window=42
" vim: set ft=vim :
