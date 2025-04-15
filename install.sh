#!/bin/bash

stow ghostty
stow fish
stow git
stow tmux
stow nvim
stow helix
stow zellij

npm i -g vscode-langservers-extracted \
  typescript \
	svelte-language-server \
	dockerfile-language-server-nodejs \
	bash-language-server \
	@github/copilot-language-server \
	prettier

# Check if tmux package manager is installed, if not, install it
[ -d "$HOME/.tmux/plugins/tpm" ] || git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

