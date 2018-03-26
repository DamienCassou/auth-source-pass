#!/bin/sh

export PATH="/home/travis/.evm/bin:$PATH"

git clone https://github.com/rejeep/evm.git /home/travis/.evm
evm config path /tmp
evm install emacs-25.3-travis --use --skip
