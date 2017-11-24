" ----------------------------------------------------------------------------
" Plugins
" ----------------------------------------------------------------------------

set nocompatible              " be iMproved, required
set encoding=utf-8 nobomb
filetype off                  " required

call plug#begin('~/.vim/plugged')

Plug 'sheerun/vim-polyglot'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'fatih/vim-go'
Plug 'Raimondi/delimitMate' " automatic closing of quotes, parenthesis, brackets, etc
Plug 'valloric/youcompleteme', { 'do': './install.py --all' }
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ernstvanderlinden/vim-coldfusion'
Plug 'editorconfig/editorconfig-vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'groenewege/vim-less'
Plug 'ntpeters/vim-better-whitespace'
Plug 'elzr/vim-json'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'gregsexton/MatchTag'  " highlight matching HTML tag
Plug 'mileszs/ack.vim'
Plug 'w0rp/ale'             " async lint engine
Plug 'andrewradev/splitjoin.vim'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-fuzzy.vim'
Plug 'othree/html5.vim'
Plug 'docunext/closetag.vim'
Plug 'chriskempson/base16-vim'
Plug 'junegunn/vim-easy-align'

call plug#end()

filetype plugin indent on    " required
syntax on

" ----------------------------------------------------------------------------
" General Config
" ----------------------------------------------------------------------------

set ruler                      " ruler
set backspace=indent,eol,start " allow backspace in insert mode
set history=1000               " lots of history
set showcmd                    " show incomplete cmds down the bottom
set showmode                   " set the current mode in the bottom
set autoread                   " reload files changed outside vim
set nocompatible
set noeol                      " no new line at end of file
set nostartofline              " don't reset cursor to start of line when moving
set scrolloff=3                " start scrolling 3 lines before the horiztonal window border
set relativenumber             " relative line numbers instead of absolute
set nobackup                   " don't create annoying backup files
set nowritebackup
set nojoinspaces               " Use one space instead of two after punctuation
set foldmethod=syntax

set encoding=utf-8             " set default encoding to UTF-8
set autoread                   " automatically reread changed files without asking
set gdefault                   " use `g` flag by default with :s/foo/bar

if has('clipboard')
  if has('unnamedplus')        " When possible use + register for copy-paste
    set clipboard=unnamed,unnamedplus
  else
    set clipboard=unnamed
  endif
endif

" ----------------------------------------------------------------------------
" Display
" ----------------------------------------------------------------------------

set title                      " show the file name i the window titlebar
set novisualbell               " no beeps or flashes
set noerrorbells               " no beeps
set belloff=all
set number                     " show line numbers
set numberwidth=5
set textwidth=80               " line length
set colorcolumn=+1             " set the column color after the text width
set nowrap                     " don't wrap long lines
set cursorline
set formatoptions=qrn1
set synmaxcol=512              " don't syntax long lines

set synmaxcol=300
set re=1

set nofoldenable               " disable folding
set foldlevelstart=99

set laststatus=2               " forcefully display the last status
set noshowmode                 " remove the duplicate -- INSERT -- below the status bar

" ----------------------------------------------------------------------------
" Input
" ----------------------------------------------------------------------------

set notimeout
set ttimeout

let mapleader = ","
let g:mapleader = ","

" ----------------------------------------------------------------------------
" Writing swaps
" ----------------------------------------------------------------------------

set noswapfile
" default is on, explicitly set on here.
" pros: prevent editing stale copy of same file in two vim instances
"set swapfile

" set the swap directory'
"set directory=$HOME/.vim/.tmp/swap/

" ----------------------------------------------------------------------------
" Window splitting and buffers
" ----------------------------------------------------------------------------

set splitbelow                 " split below for horizontal splits
set splitright                 " split right for vertical splits
set fillchars=vert:│           " vertical sep between windows (unicode)

" This makes vim act like all other editors, buffers can exist in the
" background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right
set hidden

" reveal already opened files from the quickfix window instead of opening new
" buffers
set switchbuf=useopen

set nostartofline               " don't jump to col1 on switch buffer

" open help vertically
command! -nargs=* -complete=help Help vertical belowright help <args>
autocmd FileType help wincmd L

" Do not show stupid q: window
map q: :q

" ----------------------------------------------------------------------------
" Match and search
" ----------------------------------------------------------------------------

set matchtime=1                       " tenths of a sec
set showmatch                         " briefly jump to matching paren?
set wrapscan                          " Searches wrap around end of the file.
set hlsearch                          " highlight search results
set ignorecase
set smartcase

" move the cursor to the next search result while typing
set incsearch

" Ignore searching these directories/files
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/node_modules/*

" ag search
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
endif

" ----------------------------------------------------------------------------
" ----------------------------------------------------------------------------





" ----------------------------------------------------------------------------
" Plugin: NerdTree
" ----------------------------------------------------------------------------

" Open sidebar by default
" autocmd vimenter * NERDTree

" Open automatically when no files are specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Automatically delete buffer once file has been deleted
let g:NERDTreeAutoDeleteBuffer = 1

let g:NERDTreeMinimalUI = 1
let g:NERDTreeDirArrows = 1
let g:NERDTreeWinPos = "left"
let g:NERDTreeShowHidden = 1

" Single-click to toggle directory nodes, double click to open non-directory
" nodes.
let g:NERDTreeMouseMode=2

" Default is 31 which is a tad too narrow
let g:NERDTreeWinSize=40

let NERDTreeIgnore = ['\.DS_Store$']

" ----------------------------------------------------------------------------
" Plugin: jsx
" ----------------------------------------------------------------------------

let g:jsx_ext_required = 0               " Allow JSX in normal JS files

" ----------------------------------------------------------------------------
" Plugin: go
" ----------------------------------------------------------------------------

let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1

" ----------------------------------------------------------------------------
" Plugin: Airline
" ----------------------------------------------------------------------------

let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='base16'

let g:airline#extensions#tmuxline#enabled = 1

" ----------------------------------------------------------------------------
" Plugin: CtrlP
" ----------------------------------------------------------------------------

" Open file menu
noremap <Leader>o :CtrlP<CR>
" Open buffer menu
noremap <Leader>b :CtrlPBuffer<CR>
" Open most recently used files
noremap <Leader>f :CtrlPMRUFiles<CR>A

let g:ctrlp_max_height = 20            " provide more space to display results
let g:ctrlp_mruf_max = 250             " track recently used files

" Use ripgrep https://github.com/BurntSushi/ripgrep
if executable('rg')
  " Use rg in CtrlP for listing files, automatically respects .gitignore
  let g:ctrlp_user_command = 'rg -i %s --files'
  " rg is fast enough that ctrlp doesn't need to cache
  let g:ctrlp_use_caching = 0

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
elseif executable('ag')
  " Use ag in CtrlP for listing files. automatically respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " ag is fast enough that ctrlp doesn't need to cache
  let g:ctrlp_use_caching = 0

else
  " Ignore files included within .gitignore
  let g:ctrlp_user_command = [
    \ '.git', 'cd %s && git ls-files . -co --exclude-standard',
    \ 'find %s -type f'
    \ ]
endif

" ----------------------------------------------------------------------------
" Plugin: Ale
" ----------------------------------------------------------------------------

let g:ale_sign_error = '❌'
let g:ale_sign_warning = '⚠️'

" ----------------------------------------------------------------------------
" Plugin invsearch.vim
" ----------------------------------------------------------------------------

map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" fuzzy search
map z/ <Plug>(incsearch-fuzzy-/)
map z? <Plug>(incsearch-fuzzy-?)
map zg/ <Plug>(incsearch-fuzzy-stay)

" ----------------------------------------------------------------------------
" Plugin: closetag
" ----------------------------------------------------------------------------

let g:closetag_html_style=1

" ----------------------------------------------------------------------------
" Color scheme
" ----------------------------------------------------------------------------

if has('termguicolors')
  set termguicolors
endif

colorscheme base16-onedark

" ----------------------------------------------------------------------------
" Indenting and white space
" ----------------------------------------------------------------------------

set autoindent                        " Maintin indent of current line
set expandtab                         " Always use spaces instead of tabs
set shiftwidth=2                      " Spaces per tab (when shifting)
set tabstop=2                         " Spaces per tab
set list                              " Show whitespace
set listchars=tab:❘-,trail:·,extends:»,precedes:«,nbsp:×

if has('folding')
  set foldmethod=indent
endif

" Highlight text which extends beyond the column width
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

" strip trailing spaces on sace
fun! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" ----------------------------------------------------------------------------
" Remaps
" ----------------------------------------------------------------------------

noremap <Left> :echoe "Use h"<CR>
noremap <Right> :echoe "Use l"<CR>
noremap <Up> :echoe "Use k"<CR>
noremap <Down> :echoe "Use j"<CR>
