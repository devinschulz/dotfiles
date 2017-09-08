#!/bin/bash

# get the dir of the current script
script_dir=`pwd`

if [ ! -f ~/.vimrc ]; then
  ln -s $script_dir/.vimrc ~/.vimrc
  echo '.vim.rc linked'
else
	echo '.vim.rc already linked, skipping.'
fi

if [ ! -f ~/.tmux.conf ]; then
  ln -s $script_dir/.tmux.conf ~/.tmux.conf
  echo '.tmux.conf linked'
else
	echo '.tmux.conf already linked, skipping.'
fi

