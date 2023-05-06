#!/bin/bash

stow alacritty
stow fish
stow git
stow tmux
stow nvim

npm i -g vscode-langservers-extracted \
  typescript

# Check if tmux package manager is installed, if not, install it
[ ! -d "$HOME/.tmux/plugins/tmp" ] &&
	git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

cargo install taplo-cli --locked --features lsp
