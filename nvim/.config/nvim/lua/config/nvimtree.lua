local M = {}

nvimtree_config = {
  side = "left",
  width = 30,
  show_icons = {
    git = 1,
    folders = 1,
    files = 1,
    folder_arrows = 1,
  },
  ignore = { "__pycache__", ".git", "node_modules", ".cache" },
  auto_open = 1,
  auto_close = 1,
  quit_on_open = 0,
  follow = 1,
  hide_dotfiles = 0,
  git_hl = 1,
  root_folder_modifier = ":t",
  respect_buf_cwd = 1,
  tab_open = 0,
  group_empty = 1,
  -- respect_buf_cwd = 1,
  -- allow_resize = 1,
  lsp_diagnostics = 1,
  auto_ignore_ft = { "startify", "dashboard" },
  icons = {
    default = "",
    symlink = "",
    git = {
      unstaged = "",
      staged = "S",
      unmerged = "",
      renamed = "➜",
      deleted = "",
      untracked = "U",
      ignored = "◌",
    },
    folder = {
      default = "",
      open = "",
      empty = "",
      empty_open = "",
      symlink = "",
    },
},
}

M.setup = function()
  local g = vim.g

  for opt, val in pairs(nvimtree_config) do
    g["nvim_tree_" .. opt] = val
  end

  local nvim_tree_config = require "nvim-tree.config"
  local tree_cb = nvim_tree_config.nvim_tree_callback

  g.nvim_tree_bindings = {
    { key = { "l", "<CR>", "o" }, cb = tree_cb "edit" },
    { key = "h", cb = tree_cb "close_node" },
    { key = "v", cb = tree_cb "vsplit" },
  }
end

return M
