# HP71B_forth

Few programs written in forth for the HP71B. 

The target is to create a cross-development biotop HP71B on modern PC (especially due to editor convenience) with the help of Emacs, awk, and/or EMU71.

The files can have different versions: one for cross-development on PC/Gforth, one adapted for PC/Gforth simulating the HP71B implementation and one for HP71B only (especially screen outputs, use of the BASICX commands, HP-IL ..).

71B2G.fth file only for use under Gforth (adapter and debugging words for simulating the HP71B words under Gforth)

H71Bx files, which target is to run on Gforth and/or HP71B, which contains different versions
> H71Bx.fth        Files running under Gforth
> H71Bx_HP.fth     If necessary, above files modified to work under GForth with few HP71B adaptation for debugging 80column screen outputs etc.
> HP71Bx.SRC       Files running under HP71B only
  
"ABCD" directory of a new programm for a specific task, with similar ending/naming like H71Bx, see above

progfilter_ABCD.sh  bash script for creating the HP71B.SRC files (or a version in between; a manual last rework has to be done for verifying the real number endings are ok or the blanks are ok since the script is not fully cleaning a ready to use file

Example PERE12.fth (calculation of ellipse perimeter)
Edit on PC in a terminal with >> emacs 71B2G.fth H71B1.fth PERE12.fth --eval "(view-files-in-windows)" <<  ( the attached emacs.txt should be changed into .emacs and placed into the user area)
change the format with "sh progfilter_pere12.sh"
