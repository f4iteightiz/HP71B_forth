# HP71B_forth

Few programs written in forth for the HP71B. 

The target is to create a cross-development biotop HP71B on modern PC (especially due to editor convenience) with the help of Emacs, awk, and/or EMU71.

The files have different versions: one for cross-development on PC/Gforth, then one adapted for HP71B implementation

The structure of this main directory is following
> 71B2G.fth file only for use under Gforth (adapter and debugging words for simulating the HP71B words under Gforth)
> H71Bx directory, of working files which target is to run on Gforth and/or HP71B, which contains different sub-directories
>       GFORTH        Files running under Gforth
>       GFORTH_HP71B  If necessary, above files modified to work under GForth with few HP71B adaptation for debugging 80column screen outputs etc.
>       HP71B         If necessary, files running under HP71B only (if none there, then look in the other above directories)
> "ABCD" directory of a new programm for a specific task, with similar sub-directories like H71Bx, see above
