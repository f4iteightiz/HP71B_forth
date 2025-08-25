#!/bin/bash
#
awk -f hp71bccfilter H71B1.fth > temp.txt
awk -f forthbuild64column temp.txt > H71B1.SRC
awk -f hp71bccfilter DISPLAY.fth > temp.txt
awk -f forthbuild64column temp.txt > DISPLAY.SRC
#
# unix2dos -v /home/user/programming/hp71b/DISPLAY.SRC
# wine '/home/pascal/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe' DISPLAY.SRC DISPLAY.DAT /T
