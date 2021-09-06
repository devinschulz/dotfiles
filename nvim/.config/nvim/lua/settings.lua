local M = {}

M.load_options = function()
	local global_options = {
		mapleader = ",",
		history = 500, -- Sets how many lines of history VIM has to remember
		-- no annoying sound on errors
		noerrorbells = true,
		novisualbell = true,
		tm = 500,
		t_vb = '',
	}

	for k, v in pairs(global_options) do 
		vim.g[k] = v
	end

	local options = {
		incsearch = true, -- incremental search
		ignorecase = true, -- searches are case insensitive
		smartcase = true, -- unless they contain at least one uppercase letter
		lazyredraw = true, -- don't redraw while executing macros (good performance config)
		magic = true,
		showmatch = true, -- show matching brackets when text indicator is over them
		mat = 2, -- show how many tenths of a second to blink when matching brackets
		number = true, -- line numbers
		relativenumber = true, -- relative line numbers
		wrap = false, -- disable soft line wrapping
		showcmd = true, -- display incomplete commands
		scrolloff = 8, -- show 8 lines above and below the cursor
		splitbelow = true, -- vertical splits will be at the bottom
		splitright = true, -- horizontal splits will be to the right 
		clipboard = 'unnamedplus', -- use system clipboard when yanking
		mouse = 'a', -- enable mouse for nvim
		completeopt = "menuone,noselect", -- completion global settings
		wildmenu = true, -- shows possible matches when using tab completion
		hid = true, -- buffer becomes hidden when it's abandoned 
		backspace = 'eol,start,indent',-- configure backspace so it acts as it should act
		whichwrap = '<,>,h,l,b,s',
		termguicolors = true,
		wildignore = '*.o,*~,*.pyc,*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store', -- ignore compiled files
		ruler = true, -- always show the cursor position
		shiftwidth = 2, -- the number of spaces inserted for each indentation
		tabstop = 2, -- insert 2 spaces for a tab
		expandtab = true, -- convert tabs to spaces
	}

	for k, v in pairs(options) do 
		vim.opt[k] = v
	end 


	-- good defaults for sessions
	vim.o.sessionoptions="blank,buffers,curdir,folds,help,options,tabpages,winsize,resize,winpos,terminal"

	-- colorscheme
	vim.g.tokyonight_style = "storm"
	vim.g.tokyonight_terminal_colors = true
	vim.g.tokyonight_italic_keywords = true
	vim.g.tokyonight_transparent = false
	vim.g.tokyonight_sidebars = { "terminal", "packer", "dapui_scopes", "dapui_breakpoints", "dapui_stacks", "dapui_watches", "dap-repl" }
	vim.cmd[[colorscheme tokyonight]]

end

return M
