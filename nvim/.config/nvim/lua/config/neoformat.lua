local g = vim.g
local M = {}

M.setup = function()

    g.neoformat_run_all_formatters = 1

    vim.cmd([[
    augroup fmt
        autocmd!
        autocmd BufWritePre * undojoin | Neoformat
    augroup END
  ]])

    -- Python formatters
    g.neoformat_enabled_yaml = {"prettier"}
    g.neoformat_enabled_html = {"prettier"}
    g.neoformat_enabled_css = {"prettier"}
    g.neoformat_enabled_javascript = {"prettier"}
    g.neoformat_enabled_typescript = {"prettier"}
    g.neoformat_enabled_markdown = {"prettier"}
    g.neoformat_enabled_lua = {"luaformatter"}

end

return M
