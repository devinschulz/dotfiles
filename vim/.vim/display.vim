syntax on
filetype plugin indent on

" show the file name i the window titlebar
set title

" no beeps or flashes
set novisualbell
" no beeps
set noerrorbells
set belloff=all

" show line numbers
set number
set numberwidth=5

" line length
set textwidth=80

" set the column color after the text width
set colorcolumn=+1

" don't wrap long lines
set nowrap

set cursorline
set formatoptions=qrn1
set re=1

" forcefully display the last status
set laststatus=2

" remove the duplicate -- INSERT -- below the status bar
set noshowmode

if has('termguicolors')
  set termguicolors
endif

colorscheme OceanicNext

" split below for horizontal splits
set splitbelow

" split right for vertical splits
set splitright

" vertical sep between windows (unicode)
set fillchars=vert:│

" This makes vim act like all other editors, buffers can exist in the
" background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right
set hidden

" reveal already opened files from the quickfix window instead of opening new
" buffers
set switchbuf=useopen

" don't jump to col1 on switch buffer
set nostartofline

" open help vertically
command! -nargs=* -complete=help Help vertical belowright help <args>
autocmd FileType help wincmd L

" Do not show stupid q: window
map q: :q

" if !has('gui_running')
"   set t_Co=256
" endif
