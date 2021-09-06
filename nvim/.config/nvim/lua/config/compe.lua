local M = {}

M.setup = function()
  require'compe'.setup {
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = "enable",
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = {
      border = "single",
      winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    };
    -- documentation = true,

    source = {
      path = true,
      buffer = true,
      nvim_lsp = true,
      nvim_lua = true,
      emoji = { kind = " ﲃ  (Emoji)", filetypes = { "markdown", "text" } },
      luasnip = true,
      -- for emoji press : (idk if that in compe tho)
    };
  }



  local keymap = vim.api.nvim_set_keymap
  local options = { noremap = true, silent = true, expr = true }
  keymap("i", "<C-Space>", [[compe#complete()]], options)
  -- keymap("i", "<CR>", [[compe#confirm(luaeval('require 'nvim-autopairs'.autopairs_cr()')]], options)
  keymap("i", "<C-e>", [[compe#close('<C-e>')]], options)
  keymap("i", "<C-f>", [[compe#scroll({ 'delta': +4 })]], options)
  keymap("i", "<C-d>", [[compe#scroll({ 'delta': -4 })]], options)

end

return M
