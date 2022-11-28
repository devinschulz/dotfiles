local status_ok, harpoon = pcall(require, "harpoon")
if not status_ok then
	return
end

local status_ok, telescope = pcall(require, "telescope")
if status_ok then
	telescope.load_extension("harpoon")
end

harpoon.setup({})
