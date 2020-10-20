#!/bin/bash

stow alacritty
stow fish
stow git
stow kitty
stow tmux
stow vim

# Check if tmux package manager is installed, if not, install it
[ ! -d "~/.tmux/plugins/tmp" ] &&
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# install minpac
git clone https://github.com/k-takata/minpac.git \
    ~/.vim/pack/minpac/opt/minpac

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
    yaml-language-server

# Golang language server
GO111MODULE=on go get golang.org/x/tools/gopls@latest
GO111MODULE=on go get github.com/jesseduffield/lazygit
GO111MODULE=on go get github.com/jesseduffield/lazydocker
