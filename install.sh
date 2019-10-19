#!/bin/bash

stow config
stow emacs
stow fish
stow git
stow kitty
stow tmux
stow vim
stow starship

# Packages required for language server protocol
npm i -g \
    vscode-html-languageserver-bin \
    dockerfile-language-server-nodejs \
    typescript \
    typescript-language-server \
    javascript-typescript-langserver \
    vscode-css-languageserver-bin \
    @elm-tooling/elm-language-server \
    eslint_d

go get -u golang.org/x/tools/cmd/gopls
