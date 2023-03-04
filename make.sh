#!/bin/sh
ruby gbforth.rb test.ft -o test.asm &&
  rgbasm -l test.asm -o test.o &&
  rgblink test.o -o test.gb -n test.sym &&
  rgbfix -f lhg test.gb

