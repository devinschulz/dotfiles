local M = {}

M.setup = function()
  local bufferline = require "bufferline"
  bufferline.setup {
    options = {
      numbers = "ordinal",
      show_close_icon = false,
      -- mappings = true,
      -- number_style = "",
      tab_size = 20,
      separator_style = "thick",
      offsets = {
        {
        filetype = "NvimTree",
        text = "File Explorer",
        highlight = "Directory",
        text_align = "left",
        }
      },
    },
  }
end

return M
