local fn = vim.fn

-- The root dir to install all plugins. Plugins are under opt/ or start/ sub-directory.
vim.g.plugin_home = fn.stdpath("data") .. "/site/pack/packer"

local function packer_ensure_install()
    -- Where to install packer.nvim -- the package manager (we make it opt)
    local packer_dir = vim.g.plugin_home .. "/opt/packer.nvim"

    if fn.glob(packer_dir) ~= "" then return false end

    -- Auto-install packer in case it hasn't been installed.
    vim.api.nvim_echo({{"Installing packer.nvim", "Type"}}, true, {})

    local packer_repo = "https://github.com/wbthomason/packer.nvim"
    local install_cmd = string.format("!git clone --depth=1 %s %s", packer_repo,
                                      packer_dir)
    vim.cmd(install_cmd)

    return true
end

local fresh_install = packer_ensure_install()

vim.cmd("packadd packer.nvim")

local packer = require("packer")

--------------------------------------------------------------
-- Plugins
--------------------------------------------------------------

packer.startup(function(use)
    -- It's recommended to load impatient as early as possible
    use "lewis6991/impatient.nvim"

    use {"wbthomason/packer.nvim", opt = true} -- packer can manage itself

    -- Themes
    use {
        "phanviet/vim-monokai-pro",
        "projekt0n/github-nvim-theme",
        "EdenEast/nightfox.nvim",
        "folke/tokyonight.nvim",
    }

    -- language server configuration
    use {"onsails/lspkind-nvim", event = "VimEnter"}

    -- auto-completion engine
    use {
        "hrsh7th/nvim-cmp",
        after = "lspkind-nvim",
        config = [[require("config.nvim-cmp")]]
    }

    -- nvim-cmp completion sources
    use {"hrsh7th/cmp-nvim-lsp", after = "nvim-cmp"}
    use {"hrsh7th/cmp-path", after = "nvim-cmp"}
    use {"hrsh7th/cmp-buffer", after = "nvim-cmp"}
    use {"hrsh7th/cmp-omni", after = "nvim-cmp"}
    use {
        "quangnguyen30192/cmp-nvim-ultisnips",
        after = {"nvim-cmp", "ultisnips"}
    }

    -- nvim-lsp configuration (it relies on cmp-nvim-lsp, so it should be loaded after cmp-nvim-lsp).
    use {
        "neovim/nvim-lspconfig",
        after = "cmp-nvim-lsp",
        config = [[require("config.lsp")]]
    }

    use {"SirVer/ultisnips"}

    use {
        "nvim-treesitter/nvim-treesitter",
        "windwp/nvim-ts-autotag",
        config = function()
            require("nvim-treesitter.configs").setup {autotag = true}
        end,
        run = ":TSUpdate"
    }

    -- LSP UI
    use {
        "glepnir/lspsaga.nvim",
        branch = "main",
        config = function() require("lspsaga").init_lsp_saga() end
    }

    use {"ray-x/lsp_signature.nvim", after = "nvim-lspconfig"}

    use {"andymass/vim-matchup"}
    use "Pocco81/AutoSave.nvim"

    use {
        "nvim-lualine/lualine.nvim",
        requires = {"kyazdani42/nvim-web-devicons", opt = true},
        config = function()
            require("lualine").setup {theme = "tokyonight"}
        end
    }

    -- autopairs
    use {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        config = function() require("nvim-autopairs").setup() end
    }

    -- Editing
    use {
        "kylechui/nvim-surround",
        tag = "*",
        config = function() require("nvim-surround").setup() end
    }
    use "tpope/vim-commentary"
    use "jiangmiao/auto-pairs"

    -- Git
    use "airblade/vim-gitgutter"

    -- Git diff signs
    use {
        "lewis6991/gitsigns.nvim",
        event = "BufRead",
        config = function() require("gitsigns").setup() end
    }

    -- file managing , picker etc
    use {
        "nvim-tree/nvim-tree.lua",
        requires = "nvim-tree/nvim-web-devicons",
        config = function() require("nvim-tree").setup() end
    }

    -- Fancy tabs
    use {
        "akinsho/bufferline.nvim",
        tag = "v3.*",
        requires = "kyazdani42/nvim-web-devicons",
        config = function() require("bufferline").setup() end
    }

    use "kyazdani42/nvim-web-devicons"
    use "nvim-lua/plenary.nvim"
    use "nvim-lua/popup.nvim"
    use "nvim-telescope/telescope-media-files.nvim"
    use {"nvim-telescope/telescope.nvim", requires = "nvim-lua/plenary.nvim"}

    -- Vim version of magit
    use {"TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim"}

    -- Undo tree
    use {"simnalamburt/vim-mundo", cmd = {"MundoToggle", "MundoShow"}}

    -- misc
    use {
        "sbdchd/neoformat",
        config = function() require("config.neoformat").setup() end
    }

    use {
        "folke/trouble.nvim",
        requires = "kyazdani42/nvim-web-devicons",
        config = function() require("trouble").setup() end
    }

    -- Indent lines
    use "lukas-reineke/indent-blankline.nvim"

    -- Colorize hex, rgba, colours
    use {
        "norcalli/nvim-colorizer.lua",
        config = function() require("colorizer").setup() end
    }

    use "vim-test/vim-test"
    use "github/copilot.vim"

end)

if fresh_install then packer.sync() end
