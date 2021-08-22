call plug#begin()
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'mattn/emmet-vim'

" colorschemes
Plug 'nanotech/jellybeans.vim'
call plug#end()


set termguicolors

syntax enable 
"filetype on
syntax on
let mapleader = ","
set encoding=utf-8
set nospell

set number
set relativenumber
set autoindent
set shiftwidth=4
set tabstop=4 
set softtabstop=4 
set noexpandtab 
set smarttab

autocmd FileType python setlocal shiftwidth=4 tabstop=4 expandtab
autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 noexpandtab
autocmd FileType html setlocal shiftwidth=2 tabstop=2 noexpandtab
autocmd FileType jinja setlocal shiftwidth=2 tabstop=2 noexpandtab
autocmd FileType css setlocal shiftwidth=2 tabstop=2 noexpandtab

set background=dark
colorscheme jellybeans

let g:jellybeans_use_term_italics = 1


" key bindings -------------------------------------------------------

" toggle spellcheck
map <F5> :set spell!<CR>
" open terminal in insert mode on the bottom
map <leader>c :bo 15sp +te<CR>A
" go to normal mode in terminal with escape
tnoremap <Esc> <C-\><C-n>
" set Contorl-W with leader-w
nmap <leader>w <C-w>
" resize up window easy binding
nmap <leader>k :vertical resize +7<CR>
" resize down window easy binding
nmap <leader>j :vertical resize -7<CR>

" airline settings ----------------------------------------------------
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#tabline#enabled = 0
let g:airline_powerline_fonts = 1

" emmet settings ------------------------------------------------------
let g:user_emmet_leader_key='<leader>'
let g:user_emmet_mode='iv'
