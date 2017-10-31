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

for file in .vimrc .tmux.conf .gitconfig .gitignore_global
do
	link_file $file
done

if [ ! -d ~/.brewfile ]; then
  mkdir ~/.brewfile
fi

if [ ! -f ~/.brewfile/Brewfile ]; then
	ln -s $script_dir/Brewfile ~/.brewfile/Brewfile
fi
