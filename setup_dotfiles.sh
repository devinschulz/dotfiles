#!/bin/bash

# get the dir of the current script
script_dir=`pwd`

link_file() {
	if [ ! -f ~/${1} ]; then
		ln -s $script_dir/$1 ~/$1
		echo "$1 linked"
	else
		echo "$1 already linked, skipping."
	fi
}

files=( .gitconfig .gitignore_global .tmux.conf .vimrc .zshrc)

for file in "${files[@]}"; do
	link_file $file
done
