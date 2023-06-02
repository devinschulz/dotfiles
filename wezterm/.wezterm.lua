local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.color_scheme = 'Catppuccin Macchiato'

config.font = wezterm.font_with_fallback {
  'JetbrainsMono Nerd Font',
  'Hack Nerd Font Mono'
}
config.font_size = 13.0

config.hide_tab_bar_if_only_one_tab = true

return config