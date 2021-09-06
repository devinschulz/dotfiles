local M = {}

M.setup = function()
  local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
  parser_configs.http = {
    install_info = {
      url = "https://github.com/NTBBloodbath/tree-sitter-http",
      files = { "src/parser.c" },
      branch = "main",
    },
  }

  local nvim_treesitter = require "nvim-treesitter.configs"
  nvim_treesitter.setup {
    autotag = {
      enable = true,
      filetypes = { "html", "htmldjango", "xml"}
    },
    ensure_installed = { "python",
                         "bash",
                         "css",
                         "comment",
                         "dockerfile",
                         "go",
                         "html",
                         "http",
                         "javascript",
                         "typescript",
                         "json",
                         "lua",
                         "regex",
                         "toml",
                         "yaml",
                       },
    highlight = {
      enable = true
    },
    indent = {
      enable = true
    },
    matchup = {
      enable = true
    },
  }
end

return M
