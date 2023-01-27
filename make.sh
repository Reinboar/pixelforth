#!/bin/sh
ruby gbforth.rb test.ft test.asm &&
  rgbasm test.asm -o test.o &&
  rgblink test.o -o test.gb -n test.sym &&
  rgbfix -f lhg test.gb

