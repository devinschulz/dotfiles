#!/bin/bash

stow config
stow emacs
stow git
stow kitty
stow tmux
stow vim

# Packages required for language server protocol
npm i -g \
    vscode-html-languageserver-bin \
    dockerfile-language-server-nodejs \
    typescript \
    typescript-language-server \
    javascript-typescript-langserver \
    vscode-css-languageserver-bin \
    eslint_d

GO111MODULE=on go get golang.org/x/tools/gopls@latest
