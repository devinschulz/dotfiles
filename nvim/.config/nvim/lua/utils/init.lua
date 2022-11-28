local utils = {}

function utils.executable(name)
	if vim.fn.executable(name) > 0 then
		return true
	end
	return false
end

function utils.add_keymap(mode, opts, keymaps)
	for _, keymap in ipairs(keymaps) do
		vim.api.nvim_set_keymap(mode, keymap[1], keymap[2], opts)
	end
end

function utils.add_keymap_normal_mode(opts, keymaps)
	utils.add_keymap("n", opts, keymaps)
end

function utils.add_keymap_visual_mode(opts, keymaps)
	utils.add_keymap("v", opts, keymaps)
end

function utils.add_keymap_visual_block_mode(opts, keymaps)
	utils.add_keymap("x", opts, keymaps)
end

function utils.add_keymap_insert_mode(opts, keymaps)
	utils.add_keymap("i", opts, keymaps)
end

function utils.add_keymap_term_mode(opts, keymaps)
	utils.add_keymap("t", opts, keymaps)
end

function utils.loaded_plugins()
	local count = 0

	for _, v in pairs(packer_plugins) do
		if v.loaded == true then
			count = count + 1
		end
	end

	return count
end

-- Merge two tables into a single table
function utils.merge(a, b)
	local c = {}
	for k, v in pairs(a) do
		c[k] = v
	end
	for k, v in pairs(b) do
		c[k] = v
	end
	return c
end

return utils
