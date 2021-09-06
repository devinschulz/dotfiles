#!/bin/bash

stow alacritty
stow fish
stow git
stow kitty
stow tmux
stow nvim

# Check if tmux package manager is installed, if not, install it
[ ! -d "~/.tmux/plugins/tmp" ] &&
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

if [ ! -d ~/.local/share/nvim/site/pack/packer  ]; then
  echo "Installing packer"
  git clone https://github.com/wbthomason/packer.nvim \
    ~/.local/share/nvim/site/pack/packer/start/packer.nvim
  echo
  echo "packer installed!"
  echo
fi

# Packages required for language server protocol
npm i -g \
    bash-language-server \
    dockerfile-language-server-nodejs \
    eslint_d \
    javascript-typescript-langserver \
    typescript \
    typescript-language-server \
    pure-prompt \
    vim-language-server \
    vscode-css-languageserver-bin \
    vscode-html-languageserver-bin \
    vscode-json-languageserver \
    vue-language-server \
    yaml-language-server \
    import-js

# Golang language server
GO111MODULE=on go get golang.org/x/tools/gopls@latest
