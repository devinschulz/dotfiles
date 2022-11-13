local status_ok, treesitter = pcall(require, 'nvim-treesitter.configs')
if not status_ok then return end

treesitter.setup {
  context_commentstring = {
    enable = true,
  },
  autotag = {
    enable = true,
  },
  highlight = {
    enable = true,
  },
  rainbow = {
    enable = true,
  }
}
