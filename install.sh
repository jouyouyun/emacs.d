#!/bin/bash

echo "Install emacs..."
rm -rf ~/.emacs.d
mkdir ~/.emacs.d

ln -sv $PWD/init.el ~/.emacs.d/init.el
ln -sv $PWD/core ~/.emacs.d/core
ln -sv $PWD/modules ~/.emacs.d/modules
ln -sv $PWD/bin ~/.emacs.d/bin
ln -sv $PWD/pyim ~/.emacs.d/pyim
ln -sv $PWD/core ~/.emacs.d/core
wget https://github.com/sindresorhus/github-markdown-css/raw/gh-pages/github-markdown.css -O ~/.emacs.d/github.css
