call plug#begin('~/.vim/plugged')
  Plug 'junegunn/vim-easy-align'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-commentary'
  Plug 'airblade/vim-gitgutter'
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
call plug#end()

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

" display a statusline
set laststatus=2
set statusline=%=%m\ %c\ %P\ %f

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

" highlight the current line
set cursorline

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

" remove trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e
