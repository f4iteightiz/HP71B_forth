# HP71B_forth

Few programs written in forth for the HP71B. 

The target is to create a cross-development biotop HP71B on modern PC (especially due to editor convenience) with the help of Emacs, awk, and/or EMU71.

First upload the FTHUTILA and FTHUTILF into HP71B (See HP-71 Software Developers Handbook, Section 21). make sure the newest word TIME (in between FTUTILA and F) is uploaded into the HP71B for later use. See the assembler file in attachment.

The files can have different versions: one for cross-development on PC/Gforth, one adapted for PC/Gforth simulating the HP71B implementation and one for HP71B only (especially screen outputs, use of the BASICX commands, HP-IL ..).

71B2G.fth file only for use under Gforth (adapter and debugging words for simulating the HP71B words under Gforth)

H71Bx files, which target is to run on Gforth and/or HP71B, which contains different versions
> H71Bx.fth        Files running under Gforth
> H71Bx_HP.fth     If necessary, above files modified to work under GForth with few HP71B adaptation for debugging 80column screen outputs etc.
> HP71Bx.SRC       Files running under HP71B only
  
"ABCDxy" new programm for a specific task, with similar ending/naming like H71Bx, see above

progfilter_ABCDxy.sh  bash script for creating the ABCDxy.SRC files (or a version in between; a manual last rework has to be done for verifying the real number endings are ok or the blanks are ok since the script is not fully cleaning a ready to use file).

Example 1 PERE12 (calculation of ellipse perimeter)
1. Change/Edit on PC in a terminal with >> emacs 71B2G.fth H71B1.fth PERE12.fth --eval "(view-files-in-windows)" <<  ( the attached emacs.txt should be changed into .emacs and placed into the user area).
2. test/debug/use the file on the PC by starting it, in a terminal, with "gforth PERE12.fth" then "TEST" in the Gforth prompt.
3. change the format with "sh progfilter_pere12.sh" (it calls 2x awk scripts).
4. clean both created files HP71B1.SRC and PERE12.SRC , manually with a text editor in case the awk script had an issue (delete the test sequences in it or others).
5. clean the format prior the transfer to HP71B with "unix2dos -v H71B1.SRC" and "unix2dos -v PERE12.SRC"
6. change the format for uploading into EMU71, in a terminal with the command wine "/home/user/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe" H71B1.SRC H71B1.DAT /T
7. start ILPER; in the DosLink "In" edit field select H71B1.DAT or later PERE12.DAT,  switch off then on the EMU71  ,  in the basic prompt of the HP71B,  COPY :DOSLINK TO H71B1  ,  CAT ALL (file there?)  ,  in the FORTH prompt  ,  “ H71B1” LOADF 
8. test it in the forth prompt with  3.0 11.0 PERE12 Endline give the output below
> Param B: 3 
> Param A: 11 
> ELLIPSE PERIM = 47.6528567685  OK { 0 }

Example 2 GLOCK (show a terminal clock)
1. Change/Edit on PC in a terminal with >> emacs 71B2G.fth H71B1.fth GLOCK.fth --eval "(view-files-in-windows)" <<  ( the attached emacs.txt should be changed into .emacs and placed into the user area).
2. test/debug/use the file on the PC by starting it, in a terminal, with "gforth GLOCK.fth" then S" 24/06/04" STARTCLO in the Gforth prompt. See result in the terminal_pc file.
3. change the format with "sh progfilter_clock.sh" (it calls 2x awk scripts).
4. clean the created file CLOCK.SRC , manually with a text editor in case the awk script had an issue (delete the test sequences in it or others; rework the strings because it deletes the blanks in between).
5. clean the format prior the transfer to HP71B with "unix2dos -v CLOCK.SRC"
6. change the format for uploading into EMU71, in a terminal with the command wine "/home/user/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe" CLOCK.SRC CLOCK.DAT /T
7. start ILPER; in the DosLink "In" edit field select CLOCK.DAT,  switch off then on the EMU71  ,  in the basic prompt of the HP71B,  COPY :DOSLINK TO CLOCK  ,  CAT ALL (file there?)  ,  in the FORTH prompt  ,  “ CLOCK” LOADF 
8. setup date & time the HP71B in the BASIC prompt
>DISPLAY IS *
>SETDATE”23/11/29”
>SETTIME”14:41:00”
>FORTH
9. now in the forth prompt
>0 PRIMARY !
>D-D              ( screen will be the output )
>PAGE             ( clean the screen; not necessary on EMU71;  in case not in the CLOCK tested script on HW )
>STARTCLO         ( start the CLOCK; see the result in the TerminalEmulatorHP file )

Example 3: SNAKE  ( 2D game with a snake eating an apple then becoming longer )
1. Change/Edit on PC in a terminal with >> emacs 71B2G.fth H71B1.fth SNAKE.fth --eval "(view-files-in-windows)" << ( like above )
2. test/debug/use the file on the PC by starting it, in a terminal, with "gforth SNAKE.fth" then SSNAKE in the Gforth prompt. PC Keys: W/w up, A/a left S/s down D/d right. See SnakePC file.
3. change the format with "sh progfilter_snake.sh" (it calls 2x awk scripts).
4. clean the created file SNAKE.SRC , manually with a text editor in case the awk script had an issue (delete the test sequences in it or others; rework the strings because it deletes the blanks in between).
5. clean the format prior the transfer to HP71B with "unix2dos -v SNAKE.SRC"
6. change the format for uploading into EMU71, in a terminal with the command wine "/home/user/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe" SNAKE.SRC SNAKE.DAT /T
7. start ILPER; in the DosLink "In" edit field select SNAKE.DAT,  switch off then on the EMU71  ,  in the basic prompt of the HP71B,  COPY :DOSLINK TO SNAKE  ,  CAT ALL (file there?)  ,  in the FORTH prompt  ,  “ SNAKE” LOADF 
8. now in the forth prompt
>0 PRIMARY !
>D-D              ( screen will be the output )
>PAGE             ( clean the screen; not necessary on EMU71 )
>SSNAKE           ( start ; see the result in the SnakeHP file )
