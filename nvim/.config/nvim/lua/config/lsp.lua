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
		},
	})
end

lsp.on_attach(function(client, bufnr)
	local opts = { buffer = bufnr, remap = false }
	local bind = vim.keymap.set

	-- bind("n", "<leader>f", vim.lsp.buf.format, opts)
	bind("n", "<leader>rn", vim.lsp.buf.rename, opts)
	bind("n", "<leader>ca", vim.lsp.buf.code_action, opts)
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
