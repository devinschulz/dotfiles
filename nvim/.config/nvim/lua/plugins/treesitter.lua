return {
  {
    "mfussenegger/nvim-treehopper",
    keys = { { "m", mode = { "o", "x" } } },
    config = function()
      vim.cmd([[
        omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>
        xnoremap <silent> m :lua require('tsht').nodes()<CR>
      ]])
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = "BufReadPre",
    config = true,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "bash",
        "css",
        "diff",
        "fish",
        "gitignore",
        "go",
        "graphql",
        "help",
        "html",
        "http",
        "javascript",
        "jsdoc",
        "json",
        "jsonc",
        "latex",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "regex",
        "rust",
        "scss",
        "sql",
        "svelte",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "yaml",
      },
    },
  },
}
