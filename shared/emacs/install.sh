#!/usr/bin/env bash

set -eu

mkdir ~/repos
cd ~/repos
git clone git://git.sv.gnu.org/emacs.git
cd ./emacs
./autogen.sh
./configure --with-native-compilation \
            --without-x \
            --with-ns \
            --with-imagemagick \
            --with-json
make bootstrap
make install
