local M = {}

M.setup = function()
  local actions = require('telescope.actions')
  local telescope = require "telescope"
  -- telescope.setup(telescope_config)
  telescope.setup {
    defaults = {
      prompt_prefix = "   ",
      selection_caret = " ",
      vimgrep_arguments = {
        'rg',
        '--color=never',
        '--no-heading',
        '--with-filename',
        '--line-number',
        '--column',
        '--smart-case',
      },
      color_devicons = true,
      file_ignore_patterns = {"Applications",
                              "Music",
                              "Pictures",
                              "Videos",
                              ".local",
                              "__pycache__",
                              "node_modules/*",
                              ".*%.pdf"},
      mappings = {
        i = {
          ["<C-j>"] = actions.move_selection_next,
          ["<C-k>"] = actions.move_selection_previous,
          ["<esc>"] = actions.close
      },
    },

  },
}
end

return M
