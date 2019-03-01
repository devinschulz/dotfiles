" ----------------------------------------------------------------------------
" Plugin: Deoplete
" ----------------------------------------------------------------------------

let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
" set minimum syntax keyword length.
let g:deoplete#auto_completion_start_length = 1
let g:deoplete#auto_complete_delay = 50

" Omnicomplete
augroup omnifuncs
  autocmd!
  autocmd FileType css,less,scss,sass setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType typescript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup end

" Enable tab completion for omnicomplete
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" ----------------------------------------------------------------------------
" Plugin: Ack.vim
" ----------------------------------------------------------------------------

" Use rg over ack
if executable('rg')
  let g:ackprg = 'rg --vimgrep --no-heading'
endif

" ----------------------------------------------------------------------------
" Plugin: SuperTab
" ----------------------------------------------------------------------------

" Fix tab cycling through list in reverse
let g:SuperTabDefaultCompletionType = "<c-n>"

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

let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
highlight ALEErrorSign ctermbg=NONE ctermfg=red
highlight ALEWarningSign ctermbg=NONE ctermfg=yellow

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
" Plugin: Airline
" ----------------------------------------------------------------------------

let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='oceanicnext'
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols = {}
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.maxlinenr = ''

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

