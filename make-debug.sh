#!/bin/sh
filename="$(dirname $1)/$(basename -s .ft $1)"

ruby gbforth.rb $filename.ft -o "$filename".asm &&
  /opt/devkitpro/devkitARM/arm-none-eabi/bin/as -g -D -o "$filename".o "$filename".asm &&
  /opt/devkitpro/devkitARM/arm-none-eabi/bin/ld -Tgba_cart.ld -o "$filename".elf "$filename".o &&
  /opt/devkitpro/devkitARM/arm-none-eabi/bin/objcopy -O binary "$filename".elf "$filename".gba &&
  /opt/devkitpro/tools/bin/gbafix "$filename".gba -p -tTEST -mCK -r69 &&
  mgba-qt -g "$filename".gba &
  /opt/devkitpro/devkitARM/bin/arm-none-eabi-gdb -ex "set confirm off" -ex "target remote localhost:2345" -ex "add-symbol-file $filename.elf" -ex "layout regs" -ex "advance InitInterp"
  # rgbasm "$filename".asm -o "$filename".o &&
  # rgblink $filename.o -o $filename.gb -n $filename.sym &&
  # rgbfix -f lhg -p0 $filename.gb
