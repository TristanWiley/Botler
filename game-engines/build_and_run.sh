#!/bin/sh
cd $(dirname "$0")
stack build --exec "$(find . -name 'game-engines-exe' | grep bin) $@"