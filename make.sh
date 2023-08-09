#!/bin/sh
filename="$(dirname $1)/$(basename -s .ft $1)"

ruby gbforth.rb $filename.ft -o "$filename".asm &&
  rgbasm -l "$filename".asm -o "$filename".o &&
  rgblink $filename.o -o $filename.gb -n $filename.sym &&
  rgbfix -f lhg $filename.gb

