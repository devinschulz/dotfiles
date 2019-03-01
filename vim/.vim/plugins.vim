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
call dein#add('Shougo/neosnippet-snippets')
call dein#add('RRethy/vim-illuminate')         " Highlight same words
call dein#add('Shougo/deoplete.nvim')
call dein#add('Shougo/vimproc.vim', {'build' : 'make'})
call dein#add('wellle/targets.vim')            " Provides additional text objects
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')

" Search
call dein#add('haya14busa/incsearch.vim')
call dein#add('haya14busa/incsearch-fuzzy.vim')
call dein#add('mileszs/ack.vim')
call dein#add('junegunn/fzf.vim')
call dein#add('ervandew/supertab')

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
call dein#add('carlitux/deoplete-ternjs', {
      \ 'build': 'npm install -g tern',
      \ 'on_ft': ['javascript', 'typescript', 'javascript.jsx'] })

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
