local status_ok, lsp = pcall(require, "lsp-zero")
if not status_ok then
  return
end

lsp.preset("recommended")
lsp.ensure_installed({
  "astro",
  "bashls",
  "cssls",
  "eslint",
  "gopls",
  "html",
  "jsonls",
  "pylsp",
  "rust_analyzer",
  "sumneko_lua",
  "svelte",
  "tailwindcss",
  "tsserver",
  "yamlls",
})

local status_ok, mason_tool_installer = pcall(require, "mason-tool-installer")
if status_ok then
  mason_tool_installer.setup({
    ensure_installed = {
      "prettier",
      "shellcheck",
      "fixjson",
      "stylua",
      "black",
    },
  })
end

local mason = require("mason")
local mason_null_ls = require("mason-null-ls")
local null_ls = require("null-ls")

mason.setup()
mason_null_ls.setup({
  ensure_installed = { "stylua", "prettier" },
  automatic_installation = true,
  automatic_setup = true,
})

null_ls.setup({
  sources = {
    null_ls.builtins.code_actions.cspell,
    null_ls.builtins.code_actions.proselint,
    null_ls.builtins.formatting.prettier,
    null_ls.builtins.formatting.black,
    null_ls.builtins.formatting.reorder_python_imports,
    null_ls.builtins.formatting.stylua,
  },
  on_attach = function(client, bufnr)
    if client.supports_method("textDocument/formatting") then
      vim.api.nvim_clear_autocmds({ buffer = bufnr })
      vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = bufnr,
        callback = function()
          if vim.bo.filetype == "typescriptreact" or vim.bo.filetype == "typescript" then
            vim.cmd("UpdateImports")
          end
          vim.lsp.buf.format({ bufnr = bufnr })
        end,
      })
    end
  end,
})

mason_null_ls.setup_handlers()

lsp.on_attach(function(client, bufnr)
  local opts = { buffer = bufnr, remap = false }
  local bind = vim.keymap.set

  bind("n", "<leader>rn", vim.lsp.buf.rename, opts)
  bind("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  bind("n", "<space>e", vim.diagnostic.open_float, opts)
  bind("n", "<leader>[", vim.diagnostic.goto_prev, opts)
  bind("n", "<leader>]", vim.diagnostic.goto_next, opts)
end)

local function update_imports()
  local params = {
    command = "_typescript.organizeImports",
    arguments = { vim.api.nvim_buf_get_name(0) },
    title = "",
  }
  vim.lsp.buf.execute_command(params)
end

lsp.configure("tsserver", {
  commands = {
    UpdateImports = {
      update_imports,
      description = "Update Imports",
    },
  },
})

lsp.nvim_workspace()
lsp.setup()

local status_ok, cmp = pcall(require, "cmp")
if not status_ok then
  return
end

local cmp_select = { behavior = cmp.SelectBehavior.Select }
local sources = lsp.defaults.cmp_sources()
table.insert(sources, { name = "nvim_lsp_signature_help" })
table.insert(sources, { name = "rg" })

local cmp_config = lsp.defaults.cmp_config({
  sources = sources,
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
    ["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
  },
})

cmp.setup(cmp_config)
