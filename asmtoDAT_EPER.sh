#!/bin/bash
grep -v '^*' /home/pascal/programming/hp71b/ASM/EPER.ASM | cut -c1-29 | sed 's/[[:space:]]\+$//' > /home/pascal/programming/hp71b/ASM/EPER.TXT
unix2dos -v /home/pascal/programming/hp71b/ASM/EPER.TXT
wine '/home/pascal/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe' EPER.TXT EPER.DAT /T
#

