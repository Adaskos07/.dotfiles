" plugins call
call plug#begin()
Plug 'morhetz/gruvbox'
Plug 'keitanakamura/neodark.vim'
Plug 'ayu-theme/ayu-vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'icymind/NeoSolarized'
Plug 'joshdick/onedark.vim'
Plug 'tomasiser/vim-code-dark'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'ryanoasis/vim-devicons'
Plug 'junegunn/fzf', { 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'jiangmiao/auto-pairs'
Plug 'mattn/emmet-vim'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'neovimhaskell/haskell-vim'
Plug 'alx741/vim-hindent'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jackguo380/vim-lsp-cxx-highlight'

"Plug 'dense-analysis/ale'
call plug#end()

set termguicolors
"syntax enable filetype on
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
autocmd FileType css setlocal shiftwidth=2 tabstop=2 noexpandtab

set updatetime=300


let g:spellfile_URL = 'http://ftp.vim.org/vim/runtime/spell'

" key bindings -------------------------------------------------------

" go to previous buffer
nmap <F1> :w<CR>:bp<CR>
" go to next buffer
nmap <F2> :w<CR>:bn<CR>
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
" run formatter
nmap <F6> :call CocAction('format')<CR>
" open nerdtree
map <leader>t :NERDTreeToggle<CR>

" color theme settings -----------------------------------------------
let g:gruvbox_termcolors = '256'
let g:gruvbox_contrast_dark ='high'
let g:gruvbox_contrast_light ='high'
let g:gruvbox_italic = 1
let g:gruvbox_improved_warnings = 1


let g:neosolarized_contrast = "high"
let g:neosolarized_bold = 1
let g:neosolarized_underline = 1
let g:neosolarized_italic = 1
let g:neosolarized_visibility = "high"

let g:onedark_termcolors=256


set background=dark
let ayucolor="mirage" "dark mirage light
colorscheme ayu "onedark NeoSolarized ayu palenight gruvbox


" enable transparency
"let t:is_transparent = 1
"hi Normal ctermbg=NONE guibg=NONE

"emmet key leader -----------------------------------------------------
let g:user_emmet_leader_key='<leader>'
let g:user_emmet_mode='iv'

" airline settings ----------------------------------------------------
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#tabline#enabled = 0
let g:airline_powerline_fonts = 1

" NERDTree settings ---------------------------------------------------

" enables folder icon highlighting using exact matchFullName = 1
let g:NERDTreeHighlightFolders = 1
" highlights the folder name
let g:NERDTreeHighlightFoldersFullName = 1

" coc settings --------------------------------------------------------
nmap <silent> g[ <Plug>(coc-diagnostic-prev)
nmap <silent> g] <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" rename symbol
nmap <leader>rn <Plug>(coc-rename)
" Use K to show the documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')
" -----------------------------------------------------------------------

"dictionary settings
set dictionary=/usr/share/dict/american-english
" shortcuts for the language switch
map <leader>pl :setlocal dictionary=/usr/share/dict/polish<bar>
            \:set spelllang=pl<CR>
map <leader>us :setlocal dictionary=/usr/share/dict/american-english<bar>
            \:set spelllang=en_us<CR>
map <leader>gb :setlocal dictionary=/usr/share/dict/british-english<bar>
            \:set spelllang=en_gb<CR>
map <leader>de :setlocal dictionary=/usr/share/dict/ngerman<bar>
            \:set spelllang=de<CR>
"
" ale linter settings
"let g:airline#extensions#ale#enabled = 1
""let g:ale_linters_explicit = 0
"let g:ale_linters = {
"    \ 'python' : ['flake8'],
"    \ 'haskell' : ['hlint'],
"    \ 'javascript' : ['eslint'],
"    \ }
"let g:ale_fixers = {
"    \ 'python' : ['autopep8', 'yapf'],
"    \ 'haskell' : ['hindent', 'stylish-haskell'],
"    \ }

"let g:ale_warn_about_trailing_whitespace = 1
"let g:ale_warn_about_trailing_blank_lines = 0
"" disable auto linting
"let g:ale_lint_on_text_changed = 'never'
"let g:ale_lint_on_insert_leave = 0
"let g:ale_lint_on_enter = 0
"let g:ale_completion_enabled = 0

" enable lua script
lua require'colorizer'.setup()

" haskell syntax coloring -------------------------------------------
let g:haskell_enable_quantification = 1   " enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " enable highlighting of `static`
let g:haskell_backpack = 1                " enable highlighting of backpack keywords
