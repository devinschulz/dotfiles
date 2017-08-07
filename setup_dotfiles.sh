#!/bin/bash

# get the dir of the current script
script_dir=`pwd`

if [ ! -f ~/.vimrc ]; then
  ln -s $script_dir/vim/vimrc.vim ~/.vimrc
fi

if [ ! -f ~/.tmux.conf ]; then
  ln -s $script_dir/tmux/tmux.conf ~/.tmux.conf
fi

