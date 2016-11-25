#!/bin/bash

echo "Install emacs..."
rm -rf ~/.emacs.d
mkdir ~/.emacs.d

ln -sv $PWD/init.el ~/.emacs.d/
ln -sv $PWD/core ~/.emacs.d/
ln -sv $PWD/modules ~/.emacs.d/
ln -sv $PWD/bin ~/.emacs.d/
ln -sv $PWD/pyim ~/.emacs.d/
