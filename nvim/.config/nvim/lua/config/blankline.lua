local M = {}

M.setup = function()
  require("indent_blankline").setup {
    char = "┊",
    filetype = {"python", "lua", "html", "htmldjango", "yaml", "json", "css", "bash", "javascript", "typescript"}
}
end

return M

