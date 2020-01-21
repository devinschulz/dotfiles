#!/bin/bash

stow config
stow emacs
stow git
stow kitty
stow tmux
stow vim

# Packages required for language server protocol
npm i -g \
    dockerfile-language-server-nodejs \
    eslint_d \
    javascript-typescript-langserver \
    typescript \
    typescript-language-server \
    vim-language-server \
    vscode-css-languageserver-bin \
    vscode-html-languageserver-bin

# Golang language server
GO111MODULE=on go get golang.org/x/tools/gopls@latest

# Install TPM (tmux plugin manager)
if [ ! -d ~/.tmux/plugins/tpm ]; then
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
