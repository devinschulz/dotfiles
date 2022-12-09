local fn = vim.fn

-- The root dir to install all plugins. Plugins are under opt/ or start/ sub-directory.
vim.g.plugin_home = fn.stdpath("data") .. "/site/pack/packer"

local function packer_ensure_install()
  -- Where to install packer.nvim -- the package manager (we make it opt)
  local packer_dir = vim.g.plugin_home .. "/opt/packer.nvim"

  if fn.glob(packer_dir) ~= "" then
    return false
  end

  -- Auto-install packer in case it hasn't been installed.
  vim.api.nvim_echo({ { "Installing packer.nvim", "Type" } }, true, {})

  local packer_repo = "https://github.com/wbthomason/packer.nvim"
  local install_cmd = string.format("!git clone --depth=1 %s %s", packer_repo, packer_dir)
  vim.cmd(install_cmd)

  return true
end

local fresh_install = packer_ensure_install()

vim.cmd("packadd packer.nvim")

local packer = require("packer")

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "plugins.lua",
  command = "source <afile> | PackerCompile",
})

--------------------------------------------------------------
-- Plugins
--------------------------------------------------------------

packer.startup(function(use)
  -- It's recommended to load impatient as early as possible
  use("lewis6991/impatient.nvim")

  use({ "wbthomason/packer.nvim", opt = true }) -- packer can manage itself

  -- Themes
  use({
    "phanviet/vim-monokai-pro",
    "projekt0n/github-nvim-theme",
    "EdenEast/nightfox.nvim",
    "folke/tokyonight.nvim",
  })

  -- language server configuration
  use({ "onsails/lspkind-nvim", event = "VimEnter" })

  use({
    "VonHeikemen/lsp-zero.nvim",
    requires = {
      -- LSP Support
      { "neovim/nvim-lspconfig" },
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
      { "williamboman/mason.nvim" },
      { "jose-elias-alvarez/null-ls.nvim" },
      { "jayp0521/mason-null-ls.nvim" },
      { "WhoIsSethDaniel/mason-tool-installer.nvim" },

      -- Autocompletion
      { "hrsh7th/nvim-cmp" },
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-path" },
      { "saadparwaiz1/cmp_luasnip" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-nvim-lua" },
      { "lukas-reineke/cmp-rg" },
      { "quangnguyen30192/cmp-nvim-tags" },

      -- Snippets
      { "L3MON4D3/LuaSnip" },
      { "rafamadriz/friendly-snippets" },
    },
    config = [[require("config.lsp")]],
  })

  use({
    "nvim-treesitter/nvim-treesitter",
    "p00f/nvim-ts-rainbow",
    "windwp/nvim-ts-autotag",
    "David-Kunz/markid",
    "JoosepAlviste/nvim-ts-context-commentstring",
    "nvim-treesitter/nvim-treesitter-context",
    config = [[require("config.treesitter")]],
    run = ":TSUpdate",
  })

  -- LSP UI
  use({
    "glepnir/lspsaga.nvim",
    branch = "main",
    config = function()
      require("lspsaga").init_lsp_saga()
    end,
  })

  use({ "ray-x/lsp_signature.nvim", after = "nvim-lspconfig" })

  use({ "andymass/vim-matchup" })

  use("Pocco81/AutoSave.nvim")

  use({ "feline-nvim/feline.nvim", config = [[require('config.feline')]] })

  -- Editing
  use({
    "kylechui/nvim-surround",
    tag = "*",
    config = function()
      require("nvim-surround").setup()
    end,
  })
  use("tpope/vim-commentary")

  use("lukas-reineke/indent-blankline.nvim")

  use({
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = function()
      require("nvim-autopairs").setup()
    end,
  })

  use({
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
    event = "BufRead",
  })

  use({ "itchyny/vim-highlighturl", event = "BufRead" })

  use({
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
  })

  use("kazhala/close-buffers.nvim")

  use("liuchengxu/vista.vim")

  -- Git
  use({
    "lewis6991/gitsigns.nvim",
    event = "BufRead",
    config = function()
      require("gitsigns").setup()
    end,
  })
  use("rhysd/committia.vim")

  use({ "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim" })

  -- file managing , picker etc
  use({
    "nvim-tree/nvim-tree.lua",
    requires = "nvim-tree/nvim-web-devicons",
    config = [[require("config.nvim-tree")]],
  })

  -- Fancy tabs
  use({
    "akinsho/bufferline.nvim",
    tag = "v3.*",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("bufferline").setup()
    end,
  })

  use("kyazdani42/nvim-web-devicons")
  use("nvim-lua/plenary.nvim")
  use("nvim-lua/popup.nvim")
  use({
    "kdheepak/lazygit.nvim",
    "nvim-telescope/telescope-media-files.nvim",
    "nvim-telescope/telescope.nvim",
    "nvim-telescope/telescope-dap.nvim",
    requires = { { "nvim-lua/plenary.nvim" }, { "mfussenegger/nvim-dap" } },
    config = [[require('config.telescope')]],
  })

  use({ "ThePrimeagen/harpoon", config = [[require('config.harpoon')]], after = "telescope.nvim" })

  -- Undo tree
  use({ "simnalamburt/vim-mundo", cmd = { "MundoToggle", "MundoShow" } })

  use({
    "mhartington/formatter.nvim",
    config = [[require("config.formatter")]],
  })

  use({
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup()
    end,
  })

  -- Colorize hex, rgba, colours
  use({
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
  })

  use("github/copilot.vim")

  use({
    "rcarriga/nvim-dap-ui",
    requires = { "mfussenegger/nvim-dap" },
    config = function()
      require("dapui").setup()
    end,
  })

  use({
    "MarkEmmons/neotest-deno",
    "haydenmeade/neotest-jest",
    "rouge8/neotest-rust",
    "nvim-neotest/neotest",

    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "antoinemadec/FixCursorHold.nvim",
    },
    config = [[require("config.neotest")]],
  })

  use({
    "folke/which-key.nvim",
    config = [[require("config.which-key")]],
    event = "BufWinEnter",
  })
end)

if fresh_install then
  packer.sync()
end
