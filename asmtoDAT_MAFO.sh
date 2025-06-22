#!/bin/bash
# awk -f hp71basfilter MAFO.TXT > MAFO.TXA
grep -v '^*' /home/user/programming/hp71b/ASM/MAFO.ASM | cut -c1-23 | sed 's/[[:space:]]\+$//' > /home/user/programming/hp71b/ASM/MAFO.TXT
unix2dos -v /home/user/programming/hp71b/ASM/MAFO.TXT
wine '/home/user/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe' MAFO.TXT MAFO.DAT /T
#

