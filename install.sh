#!/bin/bash

eval "$(curl -sL https://raw.githubusercontent.com/madurmanov/install/master/install.sh)"

install_start
install .emacs.d $HOME true
install_complete

exit 0
