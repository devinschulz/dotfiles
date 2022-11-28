local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
	return
end

which_key.setup({
	plugins = {
		spelling = true,
	},
})

-- Telescope
which_key.register({
	["<C-p>"] = { "<cmd>Telescope find_files<cr>", "Find File" },
	["<leader>gg"] = { "<cmd>Telescope live_grep<cr>", "Search" },
	["<leader>rr"] = { "<cmd>Telescope oldfiles<cr>", "Open Recent Files" },
})

-- Bufferline
which_key.register({
	["<leader>"] = {
		["1"] = { "<cmd>BufferLineGoToBuffer 1<CR>", "Go to buffer 1" },
		["2"] = { "<cmd>BufferLineGoToBuffer 2<CR>", "Go to buffer 2" },
		["3"] = { "<cmd>BufferLineGoToBuffer 3<CR>", "Go to buffer 3" },
		["4"] = { "<cmd>BufferLineGoToBuffer 4<CR>", "Go to buffer 4" },
		["5"] = { "<cmd>BufferLineGoToBuffer 5<CR>", "Go to buffer 5" },
		["6"] = { "<cmd>BufferLineGoToBuffer 6<CR>", "Go to buffer 6" },
		["7"] = { "<cmd>BufferLineGoToBuffer 7<CR>", "Go to buffer 7" },
		["8"] = { "<cmd>BufferLineGoToBuffer 8<CR>", "Go to buffer 8" },
		["9"] = { "<cmd>BufferLineGoToBuffer 9<CR>", "Go to buffer 9" },
		bd = { "<cmd>BufferLinePickClose<CR>", "Close Buffer" },
		["<leader>"] = { "<cmd>BufferLineCycleNext<CR>", "Cycle open buffers" },
	},
})

which_key.register({
	["<leader>"] = {
		-- Nvim Tree
		t = { ":NvimTreeToggle<CR>", "Toggle Sidebar" },
		s = { ":NvimTreeFindFile<CR>", "Search in Sidebar" },

		-- Formatter
		f = { "<cmd>Format<CR>", "Format the document" },
		fw = { "<cmd>FormatWrite<CR>", "Format the document and save" },
		u = { "<cmd>UpdateImports<CR>", "Update imports" },
    
    -- harpoon
		ma = { "<cmd>lua require('harpoon.mark').add_file()<CR>", "Mark file" },
		mc = { "<cmd>Telescope harpoon marks<CR>", "Show marks" },
		mn = { "<cmd>lua require('harpoon.ui').nav_next()<CR>", "Next mark" },
		mp = { "<cmd>lua require('harpoon.ui').nav_prev()<CR>", "Previous mark" },
		mm = { "<cmd>lua require('harpoon.ui').toggle_quick_menu()<CR>", "Marks menu" },
		m1 = { "<cmd>lua require('harpoon.mark').nav_file(1)<CR>", "Go to mark 1" },
		m2 = { "<cmd>lua require('harpoon.mark').nav_file(2)<CR>", "Go to mark 2" },
		m3 = { "<cmd>lua require('harpoon.mark').nav_file(3)<CR>", "Go to mark 3" },
		m4 = { "<cmd>lua require('harpoon.mark').nav_file(4)<CR>", "Go to mark 4" },
		m5 = { "<cmd>lua require('harpoon.mark').nav_file(5)<CR>", "Go to mark 5" },
	},
})


