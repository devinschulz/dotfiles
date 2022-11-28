local augroup = vim.api.nvim_create_augroup
local command = vim.api.nvim_command
local autocmd = vim.api.nvim_create_autocmd

-- Automatically reload the file if it is changed outside of Nvim, see https://unix.stackexchange.com/a/383044/221410.
-- It seems that `checktime` does not work in command line. We need to check if we are in command
-- line before executing this command, see also https://vi.stackexchange.com/a/20397/15292 .
augroup("auto_read", { clear = true })

autocmd({ "FileChangedShellPost" }, {
	pattern = "*",
	group = "auto_read",
	callback = function()
		vim.notify("File changed on disk. Buffer reloaded!", vim.log.levels.WARN, { title = "nvim-config" })
	end,
})

-- Resize all windows when we resize the terminal
autocmd("VimResized", {
	group = augroup("win_autoresize", { clear = true }),
	desc = "autoresize windows on resizing operation",
	command = "wincmd =",
})

-- Highlight on yank
local yankGrp = augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", { command = "silent! lua vim.highlight.on_yank()", group = yankGrp })

-- go to last loc when opening a buffer
autocmd("BufReadPost", {
	command = [[if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif]],
})
