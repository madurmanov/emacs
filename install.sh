#!/bin/bash

eval "$(curl -sL https://raw.githubusercontent.com/madurmanov/musical-install/master/musical-install.sh)"

mi_start
mi_install .emacs.d $HOME
mi_complete
