
--------------------------------------------------------------
-- Plugins
--------------------------------------------------------------

require("packer").startup(function(use)
  use "wbthomason/packer.nvim" -- packer can manage itself
  use "phanviet/vim-monokai-pro"

  -- Themes
  use "folke/tokyonight.nvim"
  use "projekt0n/github-nvim-theme"

  -- language server configurations
  use "neovim/nvim-lspconfig"
  use "glepnir/lspsaga.nvim" -- LSP UI

  use {
    "kabouzeid/nvim-lspinstall",
    opt = true,
  }

  use {
    "ray-x/lsp_signature.nvim", 
    after = "nvim-lspconfig",
  }

  use {
    "andymass/vim-matchup",
  }

  use 'Pocco81/AutoSave.nvim'

  -- autocomplete and snippets
  use {
    "hrsh7th/nvim-compe",
    requires = "onsails/lspkind-nvim",
    -- event = "InsertEnter",
    config = function()
      require("config.compe").setup()
    end,
  }

  use "L3MON4D3/LuaSnip"

   -- autopairs
  use {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    -- after = "nvim-compe",
    config = function()
      require "config.autopairs"
    end,
  }

  use "liuchengxu/vista.vim"

 -- Editing
  use "tpope/vim-surround"
  use "jiangmiao/auto-pairs"
  use "machakann/vim-highlightedyank" -- Highlight yanking

  -- Comment
  use {
    "terrortylor/nvim-comment",
    config = function() 
      require("nvim_comment").setup()
    end,
  }

  -- Git
  use "airblade/vim-gitgutter"

  -- Git diff signs
  use {
    "lewis6991/gitsigns.nvim",
    event = "BufRead",
    config = function()
      require("config.gitsigns").setup()
    end,
  }

  -- file managing , picker etc
  use {
  	"kyazdani42/nvim-tree.lua",
    config = function() 
      require("config.nvimtree").setup()
    end,
  }

  use "kyazdani42/nvim-web-devicons"
  use "nvim-lua/plenary.nvim"
  use "nvim-lua/popup.nvim"
  use "nvim-telescope/telescope-media-files.nvim"
  use "nvim-telescope/telescope.nvim"

  use {
    "nvim-treesitter/nvim-treesitter",
    branch = "0.5-compat",
    run = ":TSUpdate",
    config = function()
      require("config.treesitter").setup()
    end,
  }

  use "nvim-treesitter/nvim-treesitter-refactor"

  use {
    'yamatsum/nvim-nonicons',
    requires = {'kyazdani42/nvim-web-devicons'}
  }

  -- misc
  use {'prettier/vim-prettier', run= 'yarn install'}

    -- nvim-bufferline
  use {
    'akinsho/nvim-bufferline.lua',
    config = function()
      require("config.bufferline").setup()
    end,
    event = "BufWinEnter",
  }

  use {
    'folke/trouble.nvim',
    requires = "kyazdani42/nvim-web-devicons"
  }

   -- Status line
  use {
    "famiu/feline.nvim",
    config = function()
      require("feline").setup()
    end
  }


  -- Indent lines
  use {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require('config.blankline').setup()
    end
  }

  -- Colorize hex, rgba, colours
  use {  
    "norcalli/nvim-colorizer.lua", 
    config = function() 
      require("colorizer").setup()
    end
  }

  use {
    "karb94/neoscroll.nvim",
    opt = true,
    config = function() 
      require("neoscroll").setup()
    end
  }

  -- Vim-test
  use "vim-test/vim-test"

end)
