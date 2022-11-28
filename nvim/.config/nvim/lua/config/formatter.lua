local status_ok, formatter = pcall(require, "formatter")
if not status_ok then
	return
end

local util = require("formatter.util")

local prettier = function()
	return {
		exe = "prettier",
		args = {
			"--config-precedence",
			"prefer-file",
			"--stdin-filepath",
			util.escape_path(util.get_current_buffer_file_path()),
		},
		stdin = true,
	}
end

formatter.setup({
	logging = true,
	log_level = vim.log.levels.WARN,
	filetype = {
		lua = {
			require("formatter.filetypes.lua").stylua,
		},
		css = prettier,
		graphql = prettier,
		html = prettier,
		javascript = prettier,
		javascriptreact = prettier,
		json = prettier,
		markdown = prettier,
		svelte = prettier,
		typescript = prettier,
		typescriptreact = prettier,
		yaml = prettier,
    svg = prettier,
		rust = {
			require("formatter.filetypes.rust").rustfmt,
		},
		sh = {
			require("formatter.filetypes.sh").shfmt,
		},
		go = {
			require("formatter.filetypes.go").goimports,
		},
		fish = {
			require("formatter.filetypes.fish").fishindent,
		},
		["*"] = {
			require("formatter.filetypes.any").remove_trailing_whitespace,
		},
	},
})

-- vim.api.nvim_exec(
-- 	[[
-- augroup FormatAutogroup
--   autocmd!
--   autocmd BufWritePost * FormatWrite
-- augroup END
-- ]],
-- 	true
-- )
