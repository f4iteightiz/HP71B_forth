# HP71B_forth

Few programs written in forth for the HP71B. 

The target is to create a cross-development biotop HP71B on modern PC (especially due to editor convenience) with the help of Emacs, awk, and/or EMU71.

The files can have different versions: one for cross-development on PC/Gforth, one adapted for PC/Gforth simulating the HP71B implementation and one for HP71B only (especially screen outputs, use of the BASICX commands, HP-IL ..).

71B2G.fth file only for use under Gforth (adapter and debugging words for simulating the HP71B words under Gforth)

H71Bx files, which target is to run on Gforth and/or HP71B, which contains different versions
> H71Bx.fth        Files running under Gforth
> H71Bx_HP.fth     If necessary, above files modified to work under GForth with few HP71B adaptation for debugging 80column screen outputs etc.
> HP71Bx.SRC       Files running under HP71B only
  
"ABCDxy" new programm for a specific task, with similar ending/naming like H71Bx, see above

progfilter_ABCDxy.sh  bash script for creating the ABCDxy.SRC files (or a version in between; a manual last rework has to be done for verifying the real number endings are ok or the blanks are ok since the script is not fully cleaning a ready to use file).

Example PERE12.fth (calculation of ellipse perimeter)
1. Change/Edit on PC in a terminal with >> emacs 71B2G.fth H71B1.fth PERE12.fth --eval "(view-files-in-windows)" <<  ( the attached emacs.txt should be changed into .emacs and placed into the user area).
2. test/debug/use the file on the PC by starting it, in a terminal, with "gforth PERE12.fth" then "TEST" in the Gforth prompt.
3. change the format with "sh progfilter_pere12.sh" (it calls 2x awk scripts).
4. clean both created files HP71B1.SRC and PERE12.SRC , manually with a text editor in case the awk script had an issue (delete the test sequences in it or others).
5. clean the format prior the transfer to HP71B with "unix2dos -v H71B1.SRC" and "unix2dos -v PERE12.SRC"
6. change the format for uploading into EMU71, in a terminal with the command wine "/home/user/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe" H71B1.SRC H71B1.DAT /T
7. start ILPER; in the DosLink "In" edit field select H71B1.DAT or later PERE12.DAT,  switch off then on the EMU71  ,  in the basic prompt of the HP71B,  COPY :DOSLINK TO H71B1  ,  CAT ALL (file there?)  ,  in the FORTH prompt  ,  “ H71B1” LOADF 
8. test it in the forth prompt with  3.0 11.0 PERE12 Endline give 47.652857
