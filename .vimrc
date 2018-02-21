" ----------------------------------------------------------------------------
" Plugins
" ----------------------------------------------------------------------------

set nocompatible              " be iMproved, required
set encoding=utf-8 nobomb
filetype off                  " required

call plug#begin('~/.vim/plugged')

" General configuration
Plug 'sheerun/vim-polyglot'             " A collection of language packs
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'Raimondi/delimitMate'             " automatic closing of quotes, parenthesis, brackets, etc
Plug 'airblade/vim-gitgutter'
Plug 'ernstvanderlinden/vim-coldfusion'
Plug 'editorconfig/editorconfig-vim'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'ntpeters/vim-better-whitespace'
Plug 'gregsexton/MatchTag'              " highlight matching HTML tag
Plug 'w0rp/ale'                         " async lint engine
Plug 'andrewradev/splitjoin.vim'
Plug 'godlygeek/tabular'
Plug 'docunext/closetag.vim'
Plug 'junegunn/vim-easy-align'
Plug 'mbbill/undotree'                  " visualize history
Plug 'embear/vim-localvimrc'            " Allow local vim overrides within a project
Plug 'machakann/vim-highlightedyank'    " Automatically highlight yanked text

" Search
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-fuzzy.vim'
Plug 'mileszs/ack.vim'

if has('nvim')
  Plug 'Shougo/denite.nvim'
else
  Plug 'ctrlpvim/ctrlp.vim'
endif

" JavaScript
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'epilande/vim-es2015-snippets'
Plug 'epilande/vim-react-snippets'
Plug 'SirVer/ultisnips'

" Go
Plug 'fatih/vim-go'

" Markdown
Plug 'tpope/vim-markdown'
Plug 'nelstrom/vim-markdown-folding'

" Colorschemes
Plug 'chriskempson/base16-vim'
Plug 'mhartington/oceanic-next'

" Autocomplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/neocomplete.vim'
endif

call plug#end()

filetype plugin indent on    " required
syntax on

" ----------------------------------------------------------------------------
" General Config
" ----------------------------------------------------------------------------

set ruler                             " ruler
set backspace=indent,eol,start        " allow backspace in insert mode
set history=1000                      " lots of history
set showcmd                           " show incomplete cmds down the bottom
set showmode                          " set the current mode in the bottom
set autoread                          " reload files changed outside vim
set noeol                             " no new line at end of file
set nostartofline                     " don't reset cursor to start of line when moving
set scrolloff=3                       " start scrolling 3 lines before the horiztonal window border
set relativenumber                    " relative line numbers instead of absolute
set nobackup                          " don't create annoying backup files
set nowritebackup
set nojoinspaces                      " use one space instead of two after punctuation
set lazyredraw

if has('nvim')
  set inccommand=nosplit              " Live updates for search and replace
endif

set encoding=utf-8                    " set default encoding to UTF-8
set autoread                          " automatically reread changed files without asking
set gdefault                          " use `g` flag by default with :s/foo/bar

if has('clipboard')
  if has('unnamedplus')               " when possible use + register for copy-paste
    set clipboard=unnamed,unnamedplus
  else
    set clipboard=unnamed
  endif
endif

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
" Statusline
" ----------------------------------------------------------------------------

let g:currentmode={
      \ 'n'  : 'Normal',
      \ 'no' : 'N·Operator Pending ',
      \ 'v'  : 'Visual',
      \ 'V'  : 'V·Line ',
      \ '' : 'V·Block ',
      \ 's'  : 'Select ',
      \ 'S'  : 'S·Line ',
      \ '' : 'S·Block ',
      \ 'i'  : 'Insert',
      \ 'R'  : 'R ',
      \ 'Rv' : 'V·Replace ',
      \ 'c'  : 'Command ',
      \ 'cv' : 'Vim Ex ',
      \ 'ce' : 'Ex ',
      \ 'r'  : 'Prompt ',
      \ 'rm' : 'More ',
      \ 'r?' : 'Confirm ',
      \ '!'  : 'Shell ',
      \ 't'  : 'Terminal '
      \}

function! GitInfo()
  if exists('*fugitive#head')
    let branch = fugitive#head()
    return branch !=# '' ? ' '.branch : ''
  endif
  return ''
endfunction

function! Readonly()
  return &readonly ? ' ' : ''
endfunction

function! FileSize()
  let bytes = getfsize(expand('%:p'))
  if (bytes >= 1024)
    let kbytes = bytes / 1024
  endif
  if (exists('kbytes') && kbytes >= 1000)
    let mbytes = kbytes / 1000
  endif

  if bytes <= 0
    return '0'
  endif

  if (exists('mbytes'))
    return mbytes . 'MB '
  elseif (exists('kbytes'))
    return kbytes . 'KB '
  else
    return bytes . 'B '
  endif
endfunction


" default the statusline to green when entering Vim
hi statusline ctermfg=8 ctermbg=15

set statusline=
set statusline+=%1*                                 " Color stop 1
set statusline+=\ %{toupper(g:currentmode[mode()])} " Current mode
set statusline+=\ %5*\ %{GitInfo()}                 " Display git branch
set statusline+=%2*\                               " Separator
set statusline+=%5*%{Readonly()}                    " File is readonly
set statusline+=\ %f                                " Path to the file
set statusline+=%=                                  " Switch to the right side
set statusline+=\ %y                                " File type
set statusline+=%5*\                               " Line number icon
set statusline+=\ %l                                " Current line
set statusline+=/                                   " Separator
set statusline+=%L                                  " Total lines
set statusline+=\ (%p%%)                            " Percentage through file
set statusline+=\ %c                                " Column number
set statusline+=\ %{FileSize()}                     " The file size

hi User0 ctermfg=255 ctermbg=39
hi User1 ctermfg=255
hi User2 ctermfg=12
hi User3 ctermfg=160
hi User4 ctermfg=70
hi User5 ctermfg=7
hi User6 ctermfg=3

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
" Indenting and white space
" ----------------------------------------------------------------------------

set autoindent                        " Maintin indent of current line
set expandtab                         " Always use spaces instead of tabs
set shiftwidth=2                      " Spaces per tab (when shifting)
set tabstop=2                         " Spaces per tab
set list                              " Show whitespace
set listchars=tab:❘-,trail:·,extends:»,precedes:«,nbsp:×

" Folding
if &foldmethod ==# ''
  set foldmethod=syntax               " Allow folding by indent level
endif
set foldlevel=0
set foldcolumn=0

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

if has('nvim')
  let g:deoplete#enable_at_startup = 1
  let g:deoplete#enable_smart_case = 1
  " set minimum syntax keyword length.
  let g:deoplete#auto_completion_start_length = 1
  let g:deoplete#auto_complete_delay = 50
endif

" ----------------------------------------------------------------------------
" Plugin: Denite
" ----------------------------------------------------------------------------

if has('nvim')
  " reset 50% winheight on window resize
  augroup deniteresize
    autocmd!
    autocmd VimResized,VimEnter * call denite#custom#option('default',
          \'winheight', winheight(0) / 2)
  augroup end

  call denite#custom#option('default', {
    \ 'prompt': '❯'
    \ })

  call denite#custom#var('file_rec', 'command',
    \ ['rg', '--files', '--glob', '!.git', ''])
  call denite#custom#var('grep', 'command', ['rg'])
  call denite#custom#var('grep', 'default_opts',
      \ ['--hidden', '--vimgrep', '--no-heading', '-S'])
  call denite#custom#var('grep', 'recursive_opts', [])
  call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
  call denite#custom#var('grep', 'separator', ['--'])
  call denite#custom#var('grep', 'final_opts', [])
  call denite#custom#map('insert', '<Esc>', '<denite:enter_mode:normal>',
    \'noremap')
  call denite#custom#map('normal', '<Esc>', '<NOP>',
    \'noremap')
  call denite#custom#map('insert', '<C-v>', '<denite:do_action:vsplit>',
    \'noremap')
  call denite#custom#map('normal', '<C-v>', '<denite:do_action:vsplit>',
    \'noremap')
  call denite#custom#map('normal', 'dw', '<denite:delete_word_after_caret>',
    \'noremap')

  nnoremap <C-p> :<C-u>Denite file_rec<CR>
  nnoremap <leader>s :<C-u>Denite buffer<CR>
  nnoremap <leader><Space>s :<C-u>DeniteBufferDir buffer<CR>
  nnoremap <leader>8 :<C-u>DeniteCursorWord grep:. -mode=normal<CR>
  nnoremap <leader>/ :<C-u>Denite grep:. -mode=normal<CR>
  nnoremap <leader><Space>/ :<C-u>DeniteBufferDir grep:. -mode=normal<CR>
  nnoremap <leader>d :<C-u>DeniteBufferDir file_rec<CR>
endif

" ----------------------------------------------------------------------------
" Plugin: NERDTree
" ----------------------------------------------------------------------------

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
" Plugin: CtrlP
" ----------------------------------------------------------------------------

if !has('nvim')
  " Open file menu
  noremap <Leader>o :CtrlP<CR>
  " Open buffer menu
  noremap <Leader>b :CtrlPBuffer<CR>
  " Open most recently used files
  noremap <Leader>f :CtrlPMRUFiles<CR>A

  let g:ctrlp_max_height = 20            " provide more space to display results
  let g:ctrlp_mruf_max = 250             " track recently used files
  let g:ctrlp_show_hidden = 1            " show hidden files in search results

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
endif

" ----------------------------------------------------------------------------
" Plugin: Ale
" ----------------------------------------------------------------------------

let g:ale_sign_error = '❌'
let g:ale_sign_warning = '⚠️'

let g:ale_fixers = {}
let g:ale_fixers['javascript'] = ['prettier']
let g:ale_fixers['javascript.jsx'] = ['prettier']
let g:ale_fixers['less'] = ['prettier']
let g:ale_fixers['scss'] = ['prettier']
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
