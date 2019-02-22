" tenths of a sec
set matchtime=1

" show matching bracket
set showmatch

" Searches wrap around end of the file.
set wrapscan

" highlight search results
set hlsearch

" case insensitive search
set ignorecase
set smartcase
set infercase

" Add HTML brackets to pair matching
set matchpairs+=<:>

" move the cursor to the next search result while typing
set incsearch

" lazy file name tab completion
set wildmode=longest,list,full
set wildmenu
set wildignorecase

" ignore files vim doesnt use
set wildignore+=.git,.hg,.svn
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.rbc,*.class
set wildignore+=*.ai,*.bmp,*.gif,*.ico,*.jpg,*.jpeg,*.png,*.psd,*.webp
set wildignore+=*.avi,*.divx,*.mp4,*.webm,*.mov,*.m2ts,*.mkv,*.vob,*.mpg,*.mpeg
set wildignore+=*.mp3,*.oga,*.ogg,*.wav,*.flac
set wildignore+=*.eot,*.otf,*.ttf,*.woff
set wildignore+=*.doc,*.pdf,*.cbr,*.cbz
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.kgb
set wildignore+=*.swp,.lock,.DS_Store,._*
set wildignore+=*/node_modules/*

" ripgrep search
if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading
  set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

