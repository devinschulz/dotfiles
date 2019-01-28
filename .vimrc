" ----------------------------------------------------------------------------
" Plugins
" ----------------------------------------------------------------------------

set nocompatible                        " be iMproved, required
set encoding=utf-8 nobomb
filetype off                            " required

" Install dein if not already installed
if (!isdirectory(expand("$HOME/.config/nvim/repos/github.com/Shougo/dein.vim")))
  call system(expand("mkdir -p $HOME/.config/nvim/repos/github.com"))
  call system(expand("git clone https://github.com/Shougo/dein.vim $HOME/.config/nvim/repos/github.com/Shougo/dein.vim"))
endif

set runtimepath+=~/.config/nvim/repos/github.com/Shougo/dein.vim/

call dein#begin(expand('~/.config/nvim'))
call dein#add('haya14busa/dein-command.vim')

" General configuration
call dein#add('sheerun/vim-polyglot')          " A collection of language packs
call dein#add('tpope/vim-commentary')          " comment and uncomment things
call dein#add('tpope/vim-fugitive')
call dein#add('tpope/vim-repeat')
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-eunuch')              " Perform unix operations
call dein#add('tpope/vim-vinegar')             " netrw improvements
call dein#add('Raimondi/delimitMate')          " automatic closing of quotes, parenthesis, brackets, etc
call dein#add('airblade/vim-gitgutter')
call dein#add('editorconfig/editorconfig-vim')
call dein#add('ntpeters/vim-better-whitespace')
call dein#add('w0rp/ale')                      " async lint engine
call dein#add('andrewradev/splitjoin.vim')
call dein#add('junegunn/vim-easy-align')
call dein#add('mbbill/undotree')               " visualize history
call dein#add('embear/vim-localvimrc')         " Allow local vim overrides within a project
call dein#add('machakann/vim-highlightedyank') " Automatically highlight yanked text
call dein#add('terryma/vim-multiple-cursors')
call dein#add('itchyny/lightline.vim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('RRethy/vim-illuminate')         " Highlight same words
call dein#add('Shougo/deoplete.nvim')
call dein#add('Shougo/vimproc.vim', {'build' : 'make'})

" Search
call dein#add('haya14busa/incsearch.vim')
call dein#add('haya14busa/incsearch-fuzzy.vim')
call dein#add('mileszs/ack.vim')
call dein#add('junegunn/fzf.vim')

" HTML/XML
call dein#add('docunext/closetag.vim', {
      \ 'on_ft': ['html', 'xml', 'javascript', 'typescript']
      \})                                                 " Automatically close html tags
call dein#add('gregsexton/MatchTag', { 'on_ft': ['html', 'typescript', 'javascript'] }) " highlight matching HTML tag
call dein#add('mattn/emmet-vim')
call dein#add('othree/html5.vim', { 'on_ft': ['html', 'typescript', 'javascript'] })

" JavaScript
call dein#add('prettier/vim-prettier', { 'build': 'npm install' })
call dein#add('epilande/vim-es2015-snippets')
call dein#add('epilande/vim-react-snippets')
call dein#add('SirVer/ultisnips')
call dein#add('Quramy/vim-js-pretty-template')
call dein#add('flowtype/vim-flow', {
      \ 'on_ft': 'javascript',
      \ 'build': 'npm install -g flow-bin' })

" Typescript
call dein#add('Quramy/tsuquyomi')

" Go
call dein#add('fatih/vim-go', { 'on_ft': 'go' })

" Markdown
call dein#add('tpope/vim-markdown', { 'on_ft': 'markdown' })
call dein#add('nelstrom/vim-markdown-folding', { 'on_ft': 'markdown' })
call dein#add('dhruvasagar/vim-table-mode')
call dein#add('rhysd/vim-grammarous')
call dein#add('junegunn/limelight.vim')
call dein#add('junegunn/goyo.vim')
call dein#add('amix/vim-zenroom2')

" Shell
call dein#add('z0mbix/vim-shfmt', { 'on_ft': ['sh', 'zsh'] })

" Other obscure languages
call dein#add('ernstvanderlinden/vim-coldfusion', { 'on_ft': ['coldfusion'] })

" Colorschemes
call dein#add('mhartington/oceanic-next')

if dein#check_install()
  call dein#install()
  call dein#remote_plugins()
  let pluginsExist = 1
endif

call dein#end()

filetype plugin indent on    " required
syntax on

let g:node_host_prog = '/usr/local/bin/neovim-node-host'

" ----------------------------------------------------------------------------
" General Config
" ----------------------------------------------------------------------------

set ruler                      " ruler
set backspace=indent,eol,start " allow backspace in insert mode
set history=1000               " lots of history
set showcmd                    " show incomplete cmds in the bottom
set showmode                   " set the current mode in the bottom
set autoread                   " reload files changed outside vim
set noeol                      " no new line at end of file
set nostartofline              " don't reset cursor to start of line when moving
set scrolloff=3                " start scrolling 3 lines before the horiztonal window border
set relativenumber             " relative line numbers instead of absolute
set nobackup                   " don't create annoying backup files
set nowritebackup
set nojoinspaces               " use one space instead of two after punctuation
set lazyredraw
set autowrite                  " Automatically :write before running commands

if has('nvim')
  set inccommand=nosplit       " Live updates for search and replace
endif

set encoding=utf-8             " set default encoding to UTF-8
set autoread                   " automatically reread changed files without asking
set gdefault                   " use `g` flag by default with :s/foo/bar

if has('clipboard')
  if has('unnamedplus')        " when possible use + register for copy-paste
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

" ----------------------------------------------------------------------------
" Display
" ----------------------------------------------------------------------------

set title              " show the file name i the window titlebar
set novisualbell       " no beeps or flashes
set noerrorbells       " no beeps
set belloff=all
set number             " show line numbers
set numberwidth=5
set textwidth=80       " line length
set colorcolumn=+1     " set the column color after the text width
set nowrap             " don't wrap long lines
set cursorline
set formatoptions=qrn1
set re=1

set laststatus=2       " forcefully display the last status
set noshowmode         " remove the duplicate -- INSERT -- below the status bar

if has('termguicolors')
  set termguicolors
endif

colorscheme OceanicNext

" ----------------------------------------------------------------------------
" Input
" ----------------------------------------------------------------------------

set notimeout
set ttimeout
let mapleader = ","
let g:mapleader = ","

" ----------------------------------------------------------------------------
" Automatically fix typos
" https://sanctum.geek.nz/arabesque/vim-command-typos/
" ----------------------------------------------------------------------------

if has("user_commands")
  command! -bang -nargs=? -complete=file E e<bang> <args>
  command! -bang -nargs=? -complete=file W w<bang> <args>
  command! -bang -nargs=? -complete=file Wq wq<bang> <args>
  command! -bang -nargs=? -complete=file WQ wq<bang> <args>
  command! -bang Wa wa<bang>
  command! -bang WA wa<bang>
  command! -bang Q q<bang>
  command! -bang QA qa<bang>
  command! -bang Qa qa<bang>
endif

" ----------------------------------------------------------------------------
" Backup, swap and undofile
" ----------------------------------------------------------------------------

set noswapfile
set nobackup

set undofile
set undolevels=1000
set undoreload=10000
set undodir=$HOME/.vim/undofiles

if !isdirectory(&undodir)
  call mkdir(&undodir)
endif

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

set matchtime=1     " tenths of a sec
set showmatch       " show matching bracket
set wrapscan        " Searches wrap around end of the file.
set hlsearch        " highlight search results
set ignorecase
set smartcase
set matchpairs+=<:> " Add HTML brackets to pair matching

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
" Indenting and white space
" ----------------------------------------------------------------------------

set autoindent                        " Maintin indent of current line
set expandtab                         " Always use spaces instead of tabs
set shiftwidth=2                      " Spaces per tab (when shifting)
set tabstop=2                         " Spaces per tab
set list                              " Show whitespace
set listchars=tab:❘-,trail:·,extends:»,precedes:«,nbsp:×

" Folding
set foldmethod=syntax " Allow folding by indent level
set foldcolumn=1      " Defines 1 col at window left, to indicate folding
let javaScript_fold=1 " Activate folding by JS syntax
set foldlevelstart=99 " Start with all folds opened

" Highlight text which extends beyond the column width
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

" ----------------------------------------------------------------------------
" Remaps
" ----------------------------------------------------------------------------

" Enable tab completion for omnicomplete
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" ----------------------------------------------------------------------------
" Omnicomplete
" ----------------------------------------------------------------------------

autocmd FileType css,less,scss,sass setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" ----------------------------------------------------------------------------
" Plugin: Deoplete
" ----------------------------------------------------------------------------

let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
" set minimum syntax keyword length.
let g:deoplete#auto_completion_start_length = 1
let g:deoplete#auto_complete_delay = 50

" ----------------------------------------------------------------------------
" Plugin: fzf
" ----------------------------------------------------------------------------

set rtp+=/usr/local/opt/fzf
set rtp+=~/.fzf

nmap ; :Buffers<CR>
nmap <Leader>r :Tags<CR>
nmap <Leader>t :Files<CR>
nmap <Leader>a :Ag<CR>
nmap <Leader>c :Colors<CR>
nnoremap <C-p> :Files<CR>

let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow -g "!{.git,node_modules}/*" 2>/dev/null'

" Updates fzf to use the current colorscheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" Hide statusline when fzf buffer is open
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

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

" format with goimports instead of gofmt
let g:go_fmt_command = "goimports"

" ----------------------------------------------------------------------------
" Plugin: Ale
" ----------------------------------------------------------------------------

let g:ale_sign_error = '❌'
let g:ale_sign_warning = '⚠️'

let g:ale_fixers = {}
let g:ale_fixers['javascript'] = ['prettier']
let g:ale_fixers['javascript.jsx'] = ['prettier']
let g:ale_fixers['typescript'] = ['prettier']
let g:ale_fixers['typescript.jsx'] = ['prettier']
let g:ale_fixers['less'] = ['prettier']
let g:ale_fixers['scss'] = ['prettier']
let g:ale_fixers['css'] = ['prettier']
let g:ale_fixers['markdown'] = ['prettier']
let g:ale_javascript_prettier_options = '--trailing-comma es5 --no-semi --single-quote'

let g:ale_linters = {}
let g:ale_linters['javascript'] = ['']

let g:ale_fix_on_save =  1

" ----------------------------------------------------------------------------
" Plugin: neocomplete.vim
" ----------------------------------------------------------------------------

let g:neocomplete#enable_at_startup = 1

" ----------------------------------------------------------------------------
" Plugin: invsearch.vim
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
" Plugin: EasyAlign
" ----------------------------------------------------------------------------
"
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" ----------------------------------------------------------------------------
" Plugin: local vim
" ----------------------------------------------------------------------------

let g:localvimrc_ask = 0

" ----------------------------------------------------------------------------
" Plugin: Lightline
" ----------------------------------------------------------------------------

let g:lightline = {
  \ 'colorscheme': 'oceanicnext',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'readonly', 'path' ],
  \             [ 'git' ] ],
  \   'right': [ [ 'lineinfo' ],
  \              [ 'percent' ],
  \              [ 'filesize', 'fileformat', 'fileencoding', 'filetype' ] ],
  \ },
  \ 'component_function': {
  \   'git': 'GitInfo',
  \   'filesize': 'FileSize',
  \   'path': 'GetPath'
  \ },
  \ }

function! GitInfo()
  if exists('*fugitive#head')
    let branch = fugitive#head()
    return branch !=# '' ? ' '.branch : ''
  endif
  return ''
endfunction

function! FileSize()
  let size = getfsize(expand('%:p'))
  if size == 0 || size == -1 || size == -2
    return ''
  endif
  if size < 1024
    return size . ' bytes'
  elseif size < 1024 * 2024
    return printf('%.1f', size/1024.0) . 'k'
  elseif size < 1024 * 2024 * 2024
    return printf('%.1f', size/1024.0/1024.0) . 'm'
  else
    return printf('%.1f', size/1024.0/1024.0/1024.0) . 'g'
  endif
endfunction

function! GetPath()
  return split(expand('%:p:h'), '/')[-1] . '/' . expand('%:t')
endfunction

" ----------------------------------------------------------------------------
" Plugin: shfmt
" ----------------------------------------------------------------------------

let g:shfmt_extra_args = '-i 2 -ci' " Google style
let g:shfmt_fmt_on_save = 0

" ----------------------------------------------------------------------------
" Plugin: tsuquyomi
" ----------------------------------------------------------------------------

autocmd FileType typescript setlocal completeopt-=menu

au BufReadPost *.tsx set syntax=typescript

" ----------------------------------------------------------------------------
" Plugin: Flow
" ----------------------------------------------------------------------------

let g:flow#enable = 0

" ----------------------------------------------------------------------------
" Plugin: Better whitespace
" ----------------------------------------------------------------------------

let g:better_whitespace_enabled = 1
let g:strip_whitespace_on_save = 1
