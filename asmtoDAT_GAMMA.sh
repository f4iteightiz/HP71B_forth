#!/bin/bash
grep -v '^*' /home/pascal/programming/hp71b/ASM/GAMMA.ASM | cut -c1-31 | sed 's/[[:space:]]\+$//' > /home/pascal/programming/hp71b/ASM/GAMMA.TXT
unix2dos -v /home/pascal/programming/hp71b/ASM/GAMMA.TXT
wine '/home/pascal/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe' GAMMA.TXT GAMMA.DAT /T
#

