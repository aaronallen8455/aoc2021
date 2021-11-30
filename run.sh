#!/bin/bash
cabal v2-build

cat ./input/$1 | cabal v2-exec aoc2021 -- $1 $2 --rts-opts +RTS -s
