" ----------------------------------------------------------------------------
" Plugin: Coc.nvim
" ----------------------------------------------------------------------------

" https://kimpers.com/vim-intelligent-autocompletion/
let g:coc_global_extensions = [
  \ 'coc-emoji', 'coc-eslint', 'coc-prettier', 'coc-highlight', 'coc-snippets',
  \ 'coc-tsserver', 'coc-tslint', 'coc-tslint-plugin', 'coc-emmet',
  \ 'coc-css', 'coc-json', 'coc-pyls', 'coc-yaml', 'coc-yank', 'coc-html'
  \ ]

" Better display for messages
set cmdheight=2
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" Use `lp` and `ln` for navigate diagnostics
nmap <silent> <leader>lp <Plug>(coc-diagnostic-prev)
nmap <silent> <leader>ln <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> <leader>ld <Plug>(coc-definition)
nmap <silent> <leader>lt <Plug>(coc-type-definition)
nmap <silent> <leader>li <Plug>(coc-implementation)
nmap <silent> <leader>lf <Plug>(coc-references)

" Remap for rename current word
nmap <leader>lr <Plug>(coc-rename)

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" coc-prettier
"
" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Prettier with :Prettier for the current buffer
command! -nargs=0 Prettier :CocCommand prettier.formatFile

" Format range with Prettier
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" coc-snippets
"
" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'

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

let g:airline_theme='oceanicnext'
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#coc#enabled = 1
let g:airline_left_sep = ' ✱ '
let g:airline_right_sep = ' ✱ '
let g:airline_section_y = ''
let g:airline_section_error = '%{airline#util#wrap(airline#extensions#coc#get_error(),0)}'
let g:airline_section_warning = '%{airline#util#wrap(airline#extensions#coc#get_warning(),0)}'

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

" ----------------------------------------------------------------------------
" Plugin: Rust.vim
" ----------------------------------------------------------------------------

let g:rustfmt_autosave = 1
