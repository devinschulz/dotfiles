return {
  {
    "NvChad/nvim-colorizer.lua",
    opts = {
      user_default_options = {
        tailwind = true,
      },
    },
  },
  {
    "nvim-neorg/neorg",
    ft = "norg",
  },
  {
    "toppair/peek.nvim",
    build = "deno task --quiet build:fast",
    keys = {
      {
        "<leader>op",
        function()
          local peek = require("peek")
          if peek.is_open() then
            peek.close()
          else
            peek.open()
          end
        end,
        desc = "Peek (Markdown Preview)",
      },
    },
  },
  {
    "kdheepak/lazygit.nvim",
    event = "VeryLazy",
    keys = {
      { "<leader>gg", "<cmd>LazyGit<cr>" },
    },
  },
}
