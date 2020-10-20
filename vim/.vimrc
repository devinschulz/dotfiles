set nocompatible

packadd minpac
call minpac#init()

call minpac#add('airblade/vim-gitgutter')
call minpac#add('ajh17/VimCompletesMe')
call minpac#add('chrisbra/colorizer')
call minpac#add('chrisbra/unicode.vim')
call minpac#add('editorconfig/editorconfig-vim')
call minpac#add('frazrepo/vim-rainbow')
call minpac#add('itchyny/vim-gitbranch')
call minpac#add('jiangmiao/auto-pairs')
call minpac#add('junegunn/fzf', { 'do': { -> fzf#install() } })
call minpac#add('junegunn/fzf.vim')
call minpac#add('junegunn/vim-easy-align')
call minpac#add('k-takata/minpac', {'type': 'opt'})
call minpac#add('mattn/emmet-vim')
call minpac#add('mhinz/vim-grepper')
call minpac#add('prabirshrestha/asyncomplete.vim')
call minpac#add('prabirshrestha/vim-lsp')
call minpac#add('severin-lemaignan/vim-minimap')
call minpac#add('sheerun/vim-polyglot', {'type': 'opt'})
call minpac#add('terryma/vim-multiple-cursors')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-eunuch')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-vinegar')
call minpac#add('vim-scripts/matchit.zip')
call minpac#add('vim-test/vim-test')
call minpac#add('w0rp/ale')

" turn on syntax highlighting
syntax on

" make backspace behave properly in insert mode
set backspace=indent,eol,start

" enable file type detection and do language-dependent indenting
filetype plugin indent on

" incremental search
set incsearch

" highlight search
set hlsearch

" searches are case insensitive ...
set ignorecase

" ... unless they contain at least one uppercase letter
set smartcase

set lazyredraw
set ttyfast

" display a statusline
set laststatus=2

" custom status line
set statusline=
set statusline +=%1*\ %n\ %*              "buffer number
set statusline +=%5*%{&ff}%*              "file format
set statusline +=%3*%y%*                  "file type
set statusline +=%4*\ %<%F%*\ %*          "full path
set statusline +=%2*%m%*                  "modified flag
set statusline +=%1*%=%5l%*               "current line
set statusline +=%2*/%L%*                 "total lines
set statusline +=%1*%4v\ %*               "virtual column number
set statusline +=%2*0x%04B\ %*            "hex character under cursor

" turn on hybrid line numbers
set number relativenumber
set nu rnu

" use system clipboard
set clipboard=unnamed

" disable soft line wrapping
set nowrap

" display incomplete commands
set showcmd

" show 2 lines above and below the cursor
set scrolloff=2

" vertical splits will be at the bottom
set splitbelow

" horizontal splits will be to the right
set splitright

" disable auto comments on new lines
set formatoptions-=cro

" use two spaces for indentation
set tabstop=2 shiftwidth=2 softtabstop=2 expandtab

" enable 256 colors
set t_Co=256
set background=dark

" set the undo directory
set undodir=~/.vim/undodir
set undofile

" remove trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

" Set the leader key
let mapleader = ","

" Section: plugin configuration

" minpac
command! PackUpdate call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean call minpac#clean()
command! PackStatus call minpac#status()

" fzf
" shortcut to bring up the files search
map ; :Files<CR>

" asyncomplete
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"

" grepper
let g:grepper = {}
let g:grepper.tools = ['grep', 'git', 'rg']

" search for the current word
nnoremap <Leader>* :Grepper -cword -noprompt<CR>

" search for the current selection
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)

set grepprg=rg\ -H\ --no-heading\ --vimgrep
set grepformat=$f:$l:%c:%M

nnoremap <Leader>g :Grepper -tool git<CR>
nnoremap <Leader>G :Grepper -tool rg<CR>

