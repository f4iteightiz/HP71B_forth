\ ***************************** Basics *************************
\
( both lines below are only for gforth pc use                  )
( when it is used with the command gforth PERE12.fth           )
( leave gfoth with the word "bye" and return key               ) 
S" 71B2G.fth" INCLUDED
S" H71B1.fth" INCLUDED
\ 
\
\ a mantissa has maximum 12 numbers according HP71B
\ Forth manual page 20
\ "E" at then end of the number for gforth is a must
\
\ only xx.yyE dont work in HP71B
\ xx.yyE0 works on HP71B (E1 too etc.)
\ make sure this is manually corrected before you upload a
\ real number into the HP71B
\
\ D_SSET.. include this line in case of float calc
\   (in the word in the PC)
\
\ only words working on HP71B should be in
\   (word of HP71B or newly created words). 
\
\ For example F! should be not in because it works in gforth
\   (would work in a PC) but not later in HP71B.
\   and no need to be used since STO is doing it
\   if D_F! is in, will be deleted later since it has D_ in it
\   by the filtering script
\
\ Generic values..
\
\ PI number    which is PI in GFORTH
\              use eventually a BASICX command
3.14159265359E0  FCONSTANT  PINB         
\
\ 9.99999999999E499 FCONSTANT D_MAXREAL  \   in HP71B Mantissa 12 maxi
\ 0.00000000001E-499 FCONSTANT D_MINREAL
\
\ debugging recommendation:
\   see https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Debugging.html
\   a lot of stack outputs like below and
\   documenting the content of the stack on the right side of the code below
\
\ edit with
\   emacs HP71BCC.fth H71GFTH.fth PERE12.fth --eval "(view-files-in-windows)"
\ start with
\   gforth HP71BCC.fth H71GFTH.fth PERE12.fth
\
\ ****************** word PERE12 *********************
\
\ calculate ellipse perimeter GaussKummer-like with AGM exact function for 
\  "convergence boosting" = "HP71 Iterated function GAGM based"
\
\ Execution/Inputs
\ > (in Forth prompt) Ae Be PERE12. (EndLine)
\ Ae and Be in float: half parameter of the ellipse
\ in case Ae or Be = 0, it would be a flat ellipse.
\ The programm is considering this (result will be "perimeter = 4*(A+B)")
\
\ Outputs: perimeter
\
\ Modules used
\   FORTH/ASSEMBLER
\
\ under CC BY SA CreativeCommons 4.0
\
\ idea taken from here.. GAGM
\ https://semjonadlaj.com/SP/Computer+Algebra+in+Scientific+Computing_37-56.pdf
\ Coding steps like
\ https://www.hpmuseum.org/forum/thread-5820-post-171326.html#pid171326
\ P(a,b)=2*( P((a+b)/2,sqrt(a*b)) - pi*a*b/AGM(a,b) )
\
\ change log
\   date 2023 05 08 creation (based on PERE12 for HP41)
\   date 2024 06 02 ready upload to github
\
FVARIABLE B            \ b half ellipse perimeter
FVARIABLE A            \ a
FVARIABLE TV           \ temporary variable
FVARIABLE R            \ result 

DECIMAL
\                                                  
: PERE12 ( r1=a  r2=b  -- r=perimeter )
    B STO
    CR ." Param B:  " G_F. CR         \ optional Show X doesnt alter the content of X
    \     ." Line below optional which will be deleted later with awk script"
    \     ." because it content a word with D_"
\    B D_F.@ CR          \ optional show the content of the variable
\    ." line 90 in emacs of PERE12 " D_RPNS              \ typical optional line to show the content of the stacks
    X=0?                              \ assumption A & B in Y and X

    
    
    IF G_F+
	." Param A:  " G_F. CR
\	 ." line 96  " D_RPNS              \ optional show the content of the stacks
	4.0E0
\	 ." line 98  " D_RPNS              \ optional show the content of the stacks
	G_F*
	." ELLIPSE PERIM = " F. EXIT ELSE THEN
\     ." line 101  " D_RPNS              \ optional show the content of the stacks
    X<>Y
\     ." line 103 " D_RPNS              \ optional show the content of the stacks
    ." Param A:  " G_F. CR
    A STO
\    A D_F.@        \  optional show the content of the variable
\     ." line 107  " D_RPNS
    X=0?                                 \ A       B       
    IF G_F+
\         ." line 110  " D_RPNS              \ optional show the content of the stacks
	4.0E0
\         ." line 112  " D_RPNS              \ optional show the content of the stacks
	G_F*
\         ." line 114  " D_RPNS              \ optional show the content of the stacks
	." ELLIPSE PERIM = " G_F. EXIT ELSE THEN
\     ." line 116  " D_RPNS               \ optional show the content of the stacks
    FENTER G_F*                          \ A*A     B
\     ." line 118  " D_RPNS              \ optional show the content of the stacks
    X<>Y FENTER G_F*                     \ B*B     A*A
\     ." line 120  " D_RPNS              \ optional show the content of the stacks
    G_F+ TV STO                          \ A^2+B^2 (stored in TV) 
\     ." line 122  " D_RPNS              \ optional show the content of the stacks
    
    1.0E0 R STO
\
    BEGIN
	A RCL FENTER                     \ An      An
\	." line 128  " D_RPNS          \ optional show the content of the stacks
	B RCL FENTER                     \ Bn      Bn     An    An 
	RDN                              \ Bn      An     An    Bn 
\	." line 131  " D_RPNS          \ optional show the content of the stacks
	G_F*                             \ Bn*An   An     Bn     Bn
\	." line 133  " D_RPNS          \ optional show the content of the stacks
	SQRT B STO                       \ SQRT(An*Bn)=BN  An  Bn     Bn 
\        ." line 135  " D_RPNS          \ optional show the content of the stacks
	RDN                              \ An      Bn      Bn      BN
	G_F-                             \ Bn-An   Bn      BN      BN
\        ." line 138  " D_RPNS          \ optional show the content of the stacks
	CHS                              \ An-Bn   Bn      BN      BN
\	." line 140  " D_RPNS          \ optional show the content of the stacks
	R RCL                            \ Rn      An-Bn   Bn      BN
\	." line 142  " D_RPNS          \ optional show the content of the stacks
	2.0E0                            \ 2.0e    Rn      An-Bn   Bn
	G_F*                             \ 2*Rn    An-Bn   Bn      Bn
\	 ." line 145  " D_RPNS          \ optional show the content of the stacks
	R STO                            \ 2*Rn=RN   An-Bn   Bn     Bn
	G_FDROP                          \ An-Bn   Bn      Bn      Bn 
\         ." line 148  " D_RPNS          \ optional show the content of the stacks
	LASTX                            \ 2.0e    An-Bn   Bn      Bn
\	." line 150  " D_RPNS          \ optional show the content of the stacks
	G_F/                             \ (An-Bn)/2.  Bn  Bn      Bn
\         ." line 152  " D_RPNS          \ optional show the content of the stacks
	A RCL                            \ An  (An-Bn)/2   Bn    Bn
\         ." line 154  " D_RPNS          \ optional show the content of the stacks
	X<>Y                             \ (An-Bn)/2   An   Bn   Bn
\        ." line 156  " D_RPNS          \ optional show the content of the stacks
	G_F- A STO                       \ (An+Bn)/2=AN   Bn  Bn   Bn  L..(An-Bn)/2.
\         ." line 158  " D_RPNS          \ optional show the content of the stacks
	G_FDROP                          \ Bn  Bn  Bn  Bn
\         ." line 160  " D_RPNS          \ optional show the content of the stacks
	B RCL                            \ BN  Bn  Bn  Bn
	X<>Y                             \ Bn  BN  Bn  Bn
\	 ." line 163  " D_RPNS          \ optional show the content of the stacks
	LASTX X^2
\         ." line 165  " D_RPNS          \ optional show the content of the stacks
	R RCL                            \ RN ((An-Bn)/2.)^2  Bn   BN
	G_F* TV RCL G_F-                  \ -T+RN*((An-Bn)/2)^2 Bn   BN   BN
 \       ." line 168  " D_RPNS          \ optional show the content of the stacks
	CHS TV STO G_FDROP                \ Bn BN BN BN
\        ." line 170  " D_RPNS          \ optional show the content of the stacks
\	WTF??    \ this was included here for testing the awk script with tab in this file
\
    X=Y? UNTIL
\     ." line 174  " D_RPNS          \ optional show the content of the stacks
    1/X
\     ." line 176  " D_RPNS          \ optional show the content of the stacks
    TV RCL
\     ." line 178  " D_RPNS          \ optional show the content of the stacks
    G_F*
\     ." line 180  " D_RPNS          \ optional show the content of the stacks
    PINB
\     ." line 182  " D_RPNS          \ optional show the content of the stacks
    G_F*
\     ." line 184  " D_RPNS          \ optional show the content of the stacks
    \
    \ ***********      OUTPUT the result   *************
    ." ELLIPSE PERIM = " G_F. ;
\
\ --------------------------------------------------------------
\ for testing the program above
\
\ on the PC,
\   use a test command in a gforth prompt
\   TEST <cr>; see below
\   or
\   11.0E 3.0E PERE12 <cr>
\ on the HP71B,
\   "3.0 11.0 PERE12 <CR>"
\   or
\   "3.0E0 11.0E0 PERE12 <CR>"    Result 47.65..
\
\ TEST 
: TEST ( r1 r2 -- r3 )  ( vvv ) 
\ 2 blanks between the 2 brackets for the awk script to clean up later
\    D_CLEARSTACKS  \  optional
    3.0E0
\    D_RPNS       \  optional anywhere: put the stack content on the screen
    11.0E0
\    0.0E0
    \ ." Test function prior start of PERE12 which will be used later on HP71B" \ 3. = Comment for output to the screen anytime. Optional.
\    D_RPNS       \  optional see above
\    D_SSET       \  anywhere here or later
    PERE12 ;      \  call of the function which will be transfered later on an HP71B
                  \  Result must be 47.65..
\
\ *************************  NEXT  *********************
\
