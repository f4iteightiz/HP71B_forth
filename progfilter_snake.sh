#!/bin/bash
#
awk -f hp71bccfilter H71B1.fth > temp.txt
awk -f forthbuild64column temp.txt > H71B1.SRC
awk -f hp71bccfilter SNAKE.fth > temp.txt
awk -f forthbuild64column temp.txt > SNAKE.SRC
#

