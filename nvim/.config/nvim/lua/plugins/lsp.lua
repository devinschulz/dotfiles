return {
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "prettierd",
        "stylua",
        "selene",
        "luacheck",
        "eslint_d",
        "shellcheck",
        "shfmt",
        "black",
        "isort",
        "flake8",
        "rustywind",
      },
    },
  },
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = {
      ensure_installed = {
        "bash-language-server",
        "css-lsp",
        "html-lsp",
        "write-good",
        "dockerfile-language-server",
        "cssmodules-language-server",
        "eslint-lsp",
        "lua-language-server",
        "prettierd",
        "rust-analyzer",
        "typescript-language-server",
        "yaml-language-server",
      },
      auto_update = true,
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      ---@type lspconfig.options
      servers = {
        bashls = {},
        cssls = {},
        dockerls = {},
        tsserver = {},
        svelte = {},
        eslint = {},
        html = {},
        gopls = {},
        marksman = {},
        pyright = {},
        rust_analyzer = {
          settings = {
            ["rust-analyzer"] = {
              cargo = { allFeatures = true },
              checkOnSave = true,
            },
          },
        },
        yamlls = {},
        tailwindcss = {},
      },
    },
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    -- enabled = false,
    config = function()
      local nls = require("null-ls")
      nls.setup({
        debounce = 150,
        save_after_format = false,
        sources = {
          nls.builtins.formatting.prettier,
          nls.builtins.formatting.stylua,
          nls.builtins.formatting.fish_indent,
          nls.builtins.formatting.shfmt,
          nls.builtins.diagnostics.markdownlint,
          nls.builtins.formatting.isort,
          nls.builtins.formatting.black,
          nls.builtins.diagnostics.flake8,
        },
        root_dir = require("null-ls.utils").root_pattern(".null-ls-root", ".neoconf.json", ".git"),
      })
    end,
  },
}
