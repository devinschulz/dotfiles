#!/bin/bash

stow alacritty
stow fish
stow git
stow tmux
stow nvim
stow helix

npm i -g vscode-langservers-extracted \
  typescript \
	svelte-language-server \
	dockerfile-language-server-nodejs \
	bash-language-server

# Check if tmux package manager is installed, if not, install it
[ ! -d "$HOME/.tmux/plugins/tmp" ] &&
	git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

