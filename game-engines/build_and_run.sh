#!/bin/sh
stack build --exec $(find . -name 'game-engines-exe' | grep bin)
