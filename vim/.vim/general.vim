set nocompatible                        " be iMproved, required
set encoding=utf-8 nobomb
filetype off                            " required

set ruler

" allow backspace in insert mode
set backspace=indent,eol,start

" lots of history
set history=1000

" show incomplete cmds in the bottom
set showcmd

" set the current mode in the bottom
set showmode

" reload files changed outside vim
set autoread

" no new line at end of file
set noeol

" don't reset cursor to start of line when moving
set nostartofline

" start scrolling 3 lines before the horiztonal window border
set scrolloff=3

" relative line numbers instead of absolute
set relativenumber

" don't create annoying backup files
set nobackup
set nowritebackup

" use one space instead of two after punctuation
set nojoinspaces

" Stop unnecessary rendering
set lazyredraw

" Automatically :write before running commands
set autowrite

if has('nvim')
  " Live updates for search and replace
  set inccommand=nosplit
endif

" set default encoding to UTF-8
set encoding=utf-8

" automatically reread changed files without asking
set autoread

" use `g` flag by default with :s/foo/bar
set gdefault

if has('clipboard')
  " when possible use + register for copy-paste
  if has('unnamedplus')
    set clipboard=unnamed,unnamedplus
  else
    set clipboard=unnamed
  endif
endif

" Automatically reload vimrc once saved
augroup myvimrc
  au!
  au BufWritePost .vimrc,vimrc so $MYVIMRC
augroup END

" Maintin indent of current line
set autoindent

" Always use spaces instead of tabs
set expandtab

" Spaces per tab (when shifting)
set shiftwidth=2

" Spaces per tab
set tabstop=2

" Show whitespace
set list
set listchars=tab:❘-,trail:·,extends:»,precedes:«,nbsp:×

" Folding
" Allow folding by indent level
set foldmethod=syntax

" Defines 1 col at window left, to indicate folding
set foldcolumn=1

" Activate folding by JS syntax
let javaScript_fold=1

" Start with all folds opened
set foldlevelstart=99

" Highlight text which extends beyond the column width
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

set notimeout
set ttimeout

" Remap the leader key
let mapleader = ","
let g:mapleader = ","

if has('nvim')
  let g:node_host_prog = expand('/usr/local/bin/neovim-node-host')
endif

