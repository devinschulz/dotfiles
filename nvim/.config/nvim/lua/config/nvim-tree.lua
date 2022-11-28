local status_ok, nvim_tree = pcall(require, "nvim-tree")
if not status_ok then
	return
end

nvim_tree.setup({
  auto_reload_on_write = true,
  disable_netrw = true,
  hijack_cursor = false,
  hijack_netrw = true,
  hijack_unnamed_buffer_when_opening = false,
	open_on_setup = false,
	open_on_setup_file = false,
	open_on_tab = false,
	ignore_buffer_on_setup = false,
  sort_by = "name",
  update_cwd = false,
  update_focused_file = {
    enable      = true,
    update_cwd  = true,
    ignore_list = {}
  },
	git = {
		ignore = false,
	},
	filters = {
		custom = { "^\\.DS_Store" },
	},
	renderer = {
		highlight_opened_files = "name",
		indent_markers = {
			enable = true,
		},
	},
  trash = {
    cmd = "trash",
    require_confirm = true
  }
})
