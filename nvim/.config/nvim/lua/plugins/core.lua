return {
  {
    "rachartier/tiny-inline-diagnostic.nvim",
    event = "VeryLazy",
    priority = 1000,
    config = function()
      require("tiny-inline-diagnostic").setup()
    end,
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    dependencies = {
      "s1n7ax/nvim-window-picker",
      config = function()
        require("window-picker").setup()
      end,
    },
  },

  -- Remove once https://github.com/LazyVim/LazyVim/pull/5900 is released
  {
    "zbirenbaum/copilot.lua",
    opts = function()
      require("copilot.api").status = require("copilot.status")
    end,
  },
}
