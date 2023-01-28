local status_ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not status_ok then
  return
end

treesitter.setup({
  ensure_installed = "all",
  indent = {
    enable = true,
  },
  auto_install = true,
  sync_install = true,
  -- enabled with nvim-ts-context-commentstring
  context_commentstring = {
    enable = true,
  },
  -- enabled with nvim-ts-autotag plugin
  autotag = {
    enable = true,
  },
  highlight = {
    enable = true,
  },
  -- enabled with nvim-ts-rainbow plugin
  rainbow = {
    enable = true,
    extended_mode = true,
    max_file_lines = nil,
  },
  -- enabled with markid
  markid = { enable = true },
})

local ft_to_parser = require("nvim-treesitter.parsers").filetype_to_parsername
ft_to_parser.mjs = "typescript"
