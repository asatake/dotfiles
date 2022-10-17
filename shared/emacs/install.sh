#!/usr/bin/env bash

set -eu

if [ ! -d "$HOME/repos" ]; then
    mkdir ~/repos
fi
cd "$HOME/repos"
if [ ! -d "$HOME/repos/emacs" ]; then
    git clone git://git.sv.gnu.org/emacs.git
fi
cd ./emacs
git pull
./autogen.sh
./configure --with-native-compilation \
            --without-x \
            --with-ns \
            --with-imagemagick \
            --with-json
make bootstrap
make install
