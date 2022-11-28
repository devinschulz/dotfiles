local status_ok, feline = pcall(require, "feline")
if not status_ok then
	return
end

local status_ok, theme_colors = pcall(require, "tokyonight.colors")
if not status_ok then
	return
end

local colors = {
	bg = theme_colors.bg_dark,
	fg = theme_colors.fg_dark,
	yellow = theme_colors.yellow,
	cyan = theme_colors.cyan,
	darkblue = theme_colors.blue7,
	green = theme_colors.green,
	orange = theme_colors.orange,
	violet = theme_colors.purple,
	magenta = theme_colors.magenta,
	blue = theme_colors.blue,
	red = theme_colors.red,
}

local vi_mode_colors = {
	NORMAL = colors.green,
	INSERT = colors.blue,
	VISUAL = colors.violet,
	OP = colors.green,
	BLOCK = colors.blue,
	REPLACE = colors.red,
	["V-REPLACE"] = colors.red,
	ENTER = colors.cyan,
	MORE = colors.cyan,
	SELECT = colors.orange,
	COMMAND = colors.magenta,
	SHELL = colors.green,
	TERM = colors.blue,
	NONE = colors.yellow,
}

local tokyonight = {
	fg = "#c8d3f5",
	bg = "#1e2030",
	bg_dark = "#222436",
	green = "#c3e88d",
	yellow = "#ffc777",
	purple = "#fca7ea",
	orange = "#d19a66",
	fg_dark = "#a9b1d6",
	red = "#ff757f",
	blue = "#82aaff",
	dark_red = "#c53b53",
}

feline.setup({
	theme = tokyonight,
})

feline.winbar.setup({})
