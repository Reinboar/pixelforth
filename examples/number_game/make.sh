#!/bin/sh
filename="number"
gbforth_loc="../../gbforth.rb"

ruby $gbforth_loc $filename.ft -o "$filename".asm -I "../../" &&
  rgbasm -l "$filename".asm -o "$filename".o -I "../../" &&
  rgblink $filename.o -o $filename.gb -n $filename.sym &&
  rgbfix -f lhg $filename.gb

