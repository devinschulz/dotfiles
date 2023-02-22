return {
  -- auto-resize windows
  {
    "anuvyklack/windows.nvim",
    event = "WinNew",
    dependencies = {
      { "anuvyklack/middleclass" },
      { "anuvyklack/animation.nvim", enabled = false },
    },
    keys = { { "<leader>Z", "<cmd>WindowsMaximize<cr>", desc = "Zoom" } },
    config = function()
      vim.o.winwidth = 5
      vim.o.equalalways = false
      require("windows").setup({
        animation = { enable = false, duration = 150 },
      })
    end,
  },
  -- scrollbar
  {
    "petertriho/nvim-scrollbar",
    event = "BufReadPost",
    config = function()
      local scrollbar = require("scrollbar")
      local colors = require("catppuccin.palettes").get_palette("macchiato")
      scrollbar.setup({
        handle = { color = colors.surface1 },
        excluded_filetypes = { "prompt", "TelescopePrompt", "noice", "notify" },
        marks = {
          Search = { color = colors.green },
          Error = { color = colors.red },
          Warn = { color = colors.yellow },
          Info = { color = colors.blue },
          Hint = { color = colors.flamingo },
          Misc = { color = colors.mauve },
        },
      })
    end,
  },
  {
    "folke/noice.nvim",
    opts = {
      messages = {
        view = "mini",
        view_error = "mini",
        view_warn = "mini",
        view_history = "messages",
      },
      notify = {
        view = "mini",
      },
      lsp = {
        -- Messages shown by lsp servers
        message = {
          view = "mini",
        },
      },
    },
  },
}
