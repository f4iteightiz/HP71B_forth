\ ***************************** Basics *************************
( )
( in this file are words for similar words in HP41             )
( or of modules available in HP41 with similar behaviour       )
( the target is to make the transfer of HP41 into Forth 71B    )
( as easiest as possible                                       )
( )
( both lines below are only for gforth pc use                  )
( when it is used with the command gforth HP41.fth             )
( leave gfoth with the word "bye" and return key               ) 
S" 71B2G.fth" INCLUDED
S" H71B1.fth" INCLUDED
( S" DISPLAY.fth" INCLUDED put new words into 71B2G            )
\ 
\ start with
\   gforth HP41.fth
\
\ ****************** word FACT ********************************
\ HP41C / CV /CX
\
\ calculate the factorial of a number
\ improve with warning when fractional part not zero
\
\ Execution/Inputs: X
\ Outputs: the value in X
\ use: FACT
\
\ Modules used
\   FORTH/ASSEMBLER
\
\ under CC BY SA CreativeCommons 4.0
\
\ change log
\   date 2024 07 28 creation
\
\ for integer
\ : FACI ( n -- n! ) 1 SWAP 1+ 1 ?DO I * LOOP ;  without ?DO
: FACI ( n -- n! ) 1 SWAP 1+ 1 2DUP = IF 2DROP ELSE DO I * 
	LOOP THEN ;
\
\ for real
\ take the integer of X and calculate factorial
\ besser than HP41 which give an error with 5.678
\ here 5.678 FACX will calculate "5.0 FACT"
: FACX ( X -- Int_X! )
    FABS FTOI FACI
    L STO
    G_FDROP
    ITOF ;
\
\
\ now similar to FACT, GAMMA(X) = FACT(X-1) for integer
\ https://rosettacode.org/wiki/Gamma_function#Forth
\
8 CONSTANT (GAMMA-SHIFT)
FVARIABLE FVAR
FVARIABLE FMORTV
\
: (MORTICI)                            ( f1 -- f2)
  -1 ITOF G_F+ 1 ITOF
  Y RCL 271828183E-8 G_F* 12 ITOF G_F* G_F/
  Y RCL 271828183E-8 G_F/ G_F+
  Y RCL Y^X X<>Y
  628318530E-8 G_F* SQRT G_F*             \ 2*pi
;

: GAMX  ( f1 -- f2)
  FVAR STO FENTER X<0? >R FENTER X=0? R> OR ABORT" GAMMA LESS OR EQUAL TO ZERO"
  FENTER (GAMMA-SHIFT) ITOF G_F+ (MORTICI) FMORTV STO FVAR RCL
  1 ITOF (GAMMA-SHIFT) 0 DO Y RCL I ITOF G_F+ G_F* LOOP FMORTV RCL X<>Y G_F/
;

: TESTG 5.3E0 GAMX FS. ;
\
\ ***************** CREATE REGISTER ARRAY  ********************
\ HP41C / CV /CX
\
\ create a list of float variables
\ similar to a contiguous store of
\ 00.. 01 .. 02.. 329.. registers for float storage in HP41
\
\ current length reserved is stored in the beginning of the list (array)
\
\ use case:
\
\ gforth
\ 150 SIZE REG41 ok  creates the list REG41 of 150 floats "registers" into the Forthram 16 nibbles @4 bits in HP71B
\ 12 SIZE REG41  ok
\ 33 SIZE REG42
\   LIMITATION TO SIZE 21  ok
\ 1.1E0 110 REG41 STO     store float value 1.1E0 into pos 110 of REG41
\ 110 REG41 RCL           retrieve floa value in pos 110 of the RG41 register list
\ 0 REG41
\   REGISTER AREA SIZE IS: 21  ok
\ 19 SIZE REG41  ok
\ 14 REG41 5.0E0 STO  ok
\ 20 REG41 5.0E0 STO OVER THE DEFINED SIZE 19 
\ :7: Stack underflow ..
\
\ gforth cross-developper on 64bits PC
\ 1 float . 8  ok
\ 1 cells . 8  ok
\
\ HP71B
\ 1 CELLS . 5 for integer 
\ 1 float is 16 
\
\ improve: PRREG REG41 issue on HP71B (due to N 5 nibbles, other 16 nibbles; when it is 8 bytes for all in gforth)
\
150 CONSTANT MRSIZE       \ maximum register size; registers from 0..149
: SIZE CREATE DUP MRSIZE > IF CR ." LIMITATION TO ADDRESS 0.." MRSIZE 1 - . DROP MRSIZE ELSE THEN DUP ,
    1 CELLS 8 = IF 1 CELLS ELSE 16 THEN \ this line can be shortened later to 16 on HP71B. it is for cross-development
    * NALLOT \ NALLOT is ALLOT N nibbles where N is in the integer stack
  DOES>                  \ n-target addr  --  addr-target
    DUP                  \ n-target addr addr
    @                    \ n-target addr N (=register 0..N-1)
    -ROT                  \ N n-target addr
    1 CELLS              \ N n-target addr cells( 8 on Debian ; 5 on HP71B )
    +
    -ROT       \ addr-reg-start  N   n-target    
    DUP        \ addr..          N   n-target n-target
    \ -8 ind
    \ -7 regswap
    \ -6 regmove
    \ -5 print reg according X PRREGX
    \ -4 print all reg content PRREG
    \ -3 clear all reg CLRG
    \ -2 clear reg according X CLRGX
    \ -1 size?
    \ 0...N-1 register access
    \

\ addr N -7 -7
\ REGSWAP
-7 =
IF
\ D_RPNS
DROP      \ addrcounter addr N 
ROT       \ addr N addrcounter 
    CRRV  \ addr N sss ddd nnn
    \
    \ test sss+nnn-1 and ddd+nnn-1 < N
    DUP   \ addr N sss ddd nnn nnn
    4 PICK
    +
    1-  
    5 PICK \ addr N sss ddd nnn nnn+sss-1 N
    <
    IF
    DUP   \ addr N sss ddd nnn nnn
    3 PICK
    +
    1-
    5 PICK  \ addr N sss ddd nnn ddd+nnn-1 N 
	<
	IF  \ addr N sss ddd nnn     tested register area fine
	    
    0 DO  \ addr N sss ddd

	2 PICK
	I
	+  \ addr N sss ddd sss+I
	1 CELLS 8 = IF 8 ELSE 16 THEN   \ addr N sss ddd S-POS cells
	*         \ addr N sss ddd (cells * S-POS )
	5 PICK +  \ addr N sss ddd addr-S
\
	2 PICK
	I
	+  \ addr N sss ddd addr-S ddd+I
	1 CELLS 8 = IF 8 ELSE 16 THEN
	*         \ addr N sss ddd addr-S (cells*D-POS)
	6 PICK +   \ addr N sss ddd addr-S addr-D
	<F>
    LOOP 2DROP 2DROP

ELSE   \ addr N sss ddd nnn 
  CR ." DEST REG OUTSIDE REGISTER 0.." 2DROP DROP 1- . DROP 	ABORT
THEN

ELSE   \ addr N sss ddd nnn
  CR ." START REG OUTSIDE REGISTER 0.." 2DROP DROP 1- . DROP 	ABORT
THEN 
\
ELSE DUP
\
\ INDN addr N -8 -8

    \ IND
-8 =
IF
\    D_RPNS
    DROP       \ INDN addr N    
    ROT        \ addr N INDN
    DUP
    3 PICK     \ addr N INDN INDN N
\
    < IF       \ addr N INDN 
\    
    3 PICK     \ addr N INDN addr
    SWAP       \ addr N addr INDN
    1 CELLS 8 = IF 8 ELSE 16 THEN \ 8 debian 64bits; 16 HP71B
    *
    +          \  addr N addr+(cells)
    FVIP       \  addr N Nnew
    SWAP       \  addr Nnew N
    DROP       \  addr Nnew
    1 CELLS 8 = IF 8 ELSE 16 THEN \ 8 debian 64bits; 16 HP71B
    *
    +          \  addr+(cells)
\	    D_RPNS
\
ELSE
  CR ." IND REG OUTSIDE REGISTER 0.." DROP 1- . DROP 	ABORT
    THEN

    
ELSE DUP
\
    
\ addr N -6 -6
\ REGMOVE
-6 =
IF
\ D_RPNS
DROP      \ addrcounter addr N 
ROT       \ addr N addrcounter 
    CRRV  \ addr N sss ddd nnn
    \
    \ test sss+nnn-1 and ddd+nnn-1 < N
    DUP   \ addr N sss ddd nnn nnn
    4 PICK
    +
    1-  
    5 PICK \ addr N sss ddd nnn nnn+sss-1 N
    <
    IF
    DUP   \ addr N sss ddd nnn nnn
    3 PICK
    +
    1-
    5 PICK  \ addr N sss ddd nnn ddd+nnn-1 N 
	<
	IF  \ addr N sss ddd nnn     tested register area fine
	    
    0 DO  \ addr N sss ddd

	2 PICK
	I
	+  \ addr N sss ddd sss+I
	1 CELLS 8 = IF 8 ELSE 16 THEN   \ addr N sss ddd S-POS cells
	*         \ addr N sss ddd (cells * S-POS )
	5 PICK +  \ addr N sss ddd addr-S
\
	2 PICK
	I
	+  \ addr N sss ddd addr-S ddd+I
	1 CELLS 8 = IF 8 ELSE 16 THEN
	*         \ addr N sss ddd addr-S (cells*D-POS)
	6 PICK +   \ addr N sss ddd addr-S addr-D
	DFV
    LOOP  2DROP 2DROP

ELSE   \ addr N sss ddd nnn 
  CR ." DEST REG OUTSIDE REGISTER 0.." 2DROP DROP 1- . DROP 	ABORT
THEN

ELSE   \ addr N sss ddd nnn
  CR ." START REG OUTSIDE REGISTER 0.." 2DROP DROP 1- . DROP 	ABORT
THEN 
\
ELSE DUP
\
    \ addr N -4 -4
\ PRREG
-4 =
IF
\    D_RPNS
    DROP       \ addr N
	    0 DO         \ addr
		CR ." R" I . ." =" DUP FV.
		1 CELLS 8 = IF 8 ELSE 16 THEN \ 8 debian 64bits; 16 HP71B
		+  \ addr+(cells)
	    LOOP
\	    D_RPNS
	    DROP
ELSE DUP

\ addr N -3 -3
\ CLRG
-3 =
IF
\    D_RPNS
    DROP       \ addr N
    0 DO       \ addr
	DUP    \ addr addr
	CLFV   \ addr 
	1 CELLS 8 = IF 8 ELSE 16 THEN \ 8 debian 64bits; 16 HP71B
	+      \ addr+(cells)
	    LOOP
\	    D_RPNS
	    DROP
ELSE DUP
\	  
\ addr N -2 -2	  
\ CLRGX
-2 =
IF
\ D_RPNS
DROP     \ addr N 
X CLRV   \ addr N sss eee ii

-ROT     \ addr N ii sss eee

    DUP      \ addr N ii sss eee eee 
    \
5 PICK   \ addr N ii sss eee eee N
\ CR ." [ addr N ii sss eee eee N ] " CR S. CR
    <        \ addr N ii sss eee flag
IF       \ addr N ii sss eee

SWAP     \ addr N ii eee sss

DUP      \ addr N ii eee sss sss
    5 PICK   \ addr N ii eee sss sss N
\     CR ." [ addr N ii eee sss sss N ] " CR S. CR
    <        \ addr N ii eee sss flag
IF       \ addr N ii eee sss

BEGIN    \ addr N ii eee POS(=sss)


DUP      \ addr N ii eee POS  POS

1 CELLS 8 = IF 8 ELSE 16 THEN   
         \ addr N ii eee POS  POS cells
\
*        \ addr N ii eee POS (cells * POS )
6 PICK   \ addr N ii eee POS (cells * POS ) addr
+        \ addr N ii eee POS addr+((POS)*(*16 hp71b or *8 PC 64bits))
\
\ CR ." float "
\ DUP
\ FV.
\
CLFV     \ addr N ii eee POS

3 PICK + \ addr N ii eee POS+ii
DUP      \ addr N ii eee POS+ii  POS+ii
3 PICK   \ addr N ii eee POS+ii  POS+ii eee
>

UNTIL    \  addr N ii eee POS+ii 

2DROP 2DROP DROP


ELSE  \ addr N ii eee sss
    CR ." START LOOP OUTSIDE REGISTER 0.." 2DROP DROP 1- . DROP
    	ABORT
THEN
ELSE   \ addr N ii sss eee
  CR ." END LOOP OUTSIDE REGISTER 0.." 2DROP DROP 1- . DROP 	ABORT
THEN 
\
ELSE DUP
\ addr N -1 -1
\ SIZE?
-1 =
IF DROP CR ." STORAGE SIZE IS: " DUP . ." , REGISTERs ID 0.." 1- . DROP
\
ELSE
\ n-target = Register 0..N-1
\ addr N n-target
\ D_RPNS
DUP ROT                  \ addr n-target n-target N
DUP                  \ addr n-target n-target N N
ROT                  \ addr n-target N N n-target 
	> IF
	    DROP                     \ addr n-target=POS
\	    D_RPNS
	    1 CELLS 8 = IF 8 ELSE 16 THEN   \  addr POS cells
\	    D_RPNS
	    * +     \ addr + ( POS * (*16 hp71b or *8 PC 64bits ) )
\	    D_RPNS
    ELSE
	CR ." OVER THE REACHABLE REGISTER 0.." 1- . CR
	\ DROP DROP
	ABORT
    THEN
\
\ ENDOF
\
THEN THEN THEN THEN THEN THEN THEN
\
\ D_RPNS
\   then use STO or  RCL after this word
;
\
\
\ SIZE? --------------------------------------------------------
\
\ ask for the register size 
\ ( -- )
: SIZE? -1 ;
\ use
\ 33 SIZE REG41     >> define REG41 of size 33 (register 0..32)
\ SIZE? REG41       >> 33 into integer stack
\ --------------------------------------------------------------
\
\
\ CLRGX --------------------------------------------------------
\
\ clear the registers according X ex from 0 to 14 .. 0.014
\ ( -- )
: CLRGX -2 ;
\ test
\ 5 REG41 10.0 STO  >> 10.0 into reg 5 of REG41
\ CLST              >> clear the stack
\ 5 REG41 RCL FS.   >> 10.0 is in X
\ 5.0 CLRGX REG41   >> clear the reg 5 of REG41
\ 5 REG41 RCL FS.   >> 0.0 into the stack at X
\ --------------------------------------------------------------
\
\
\ CLRG ---------------------------------------------------------
\
\ clear all registers
\ ( -- )
: CLRG -3 ;
\ test
\ 5 REG41 10.0 STO     >>   10.0 into reg 5 of REG41
\ CLRG REG41           >>   clear all registers of REG41
\ 5 REG41 RCL FS.      >>   0.0 into X (had 10.0 before)
\ --------------------------------------------------------------
\
\
\ PRREG --------------------------------------------------------
\
\ print all registers
\ ( -- )
: PRREG -4 ;
\ test
\ 5 REG41 10.0 STO      >>   10.0 into reg 5 of REG41
\ PRREG REG41
\ --------------------------------------------------------------
\
\
\ PRREGX --------------------------------------------------------
\
\ print the registers according X ex from 0 to 14 .. 0.014
\ ( -- )
: PRREGX -5 ;
\ 5 REG41 10.0 STO  >> 10.0 into reg 5 of REG41
\ CLST              >> clear the stack
\ 5 REG41 RCL FS.   >> 10.0 is in X
\ 5.01002 PRREGX REG41   >> print the reg content of 05 07 09 
\ --------------------------------------------------------------
\
\ REGMOVE ------------------------------------------------------
\
\ move the registers NNN registers from SSS to DDD
\ ( -- )
: REGMOVE -6 ;
\ --------------------------------------------------------------
\
\ REGSWAP ------------------------------------------------------
\
\ swap the NNN registers from SSS with DDD
\ ( -- )
: REGSWAP -7 ;
\ --------------------------------------------------------------
\
\
\ IND --------------------------------------------------------
\
\ indirect to the register according address in integer stack
\ ( -- )
: IND -8 ;
\ test
\ 10 REG41 0.0 STO      >> 0.0 into reg 10 of REG41
\ 5 REG41 10.0 STO
\ 5 IND REG41 11.0 STO  >> 11.0 into reg 10 of REG41
\ CLST                  >> clear the stack
\ 5 REG41 RCL FS.       >> 10.0 is in X
\ PRSTK
\ 5.0 CLRGX REG41       >> clear the reg 5 of REG41
\ 10 REG41 RCL FS.      >> 11.0 into the stack at X
\ --------------------------------------------------------------
\
\
15 SIZE REG41  \ registers 0..14
: TESTSIZE CR CR
    ." ************************************** " CR
    ."            START TEST                  " CR
    ." ************************************** " CR
    CR ." >>>>> 15 SIZE REG41 was done before = REG41 registers from 0 to 14" CR
    CR ." >>>>> 0 REG41 1000.0 D_SSET STO FS. CLST FS." CR
    0 REG41 1000.0E0 D_SSET STO
    FS.
\    D_RPNS
    CLST
    \
\    D_RPNS
    FS.
    \ -------------------------------------------------------
    CR ." >>>>> 14 REG41 1000.0 D_SSET STO FS. CLST FS. " CR
    14 REG41 1000.0E0 D_SSET STO
    FS.
\    D_RPNS
    CLST
    \
\    D_RPNS
    FS.
\ -------------------------------------------------------    
    CR ." >>>>> 5 REG41 5.0E0 STO FS. CLST FS. " CR
    5 REG41 5.0E0 D_SSET STO
\    D_RPNS
    FS.
    CLST
    FS.
\ -------------------------------------------------------  
    CR ." >>>>> 0 REG41 RCL FS. " CR
    0 REG41 RCL
    FS.
\    D_RPNS    
\ -------------------------------------------------------  
    CR ." >>>>> CLST FS. 5 REG41 RCL FS. " CR
    CLST
    FS.
    5 REG41 RCL
    FS.
\    D_RPNS    
    \
\ -------------------------------------------------------  
    CR ." >>>>> SIZE? REG41 " CR
    SIZE? REG41 CR
\ -------------------------------------------------------   
    CR ." >>>>> PRREG REG41 FS. CLST FS. " CR
    PRREG REG41
    FS.
\    D_RPNS
    CLST
    \
\    D_RPNS
    FS.
\ -------------------------------------------------------    
    \
    CR ." >>>>> 5.0 D_SSET CLRGX REG41 " CR
    5.0E0 D_SSET CLRGX REG41
    FS.
\    D_RPNS
    \
\ -------------------------------------------------------  
    CR ." >>>>> 5 REG41 RCL FS. " CR
    5 REG41 RCL
    FS.
\    D_RPNS
    \
\ -------------------------------------------------------  
    CR ." >>>>> 2 REG41 2.0E0 STO FS. CLST FS. " CR
    2 REG41 2.0E0 D_SSET STO
    FS.
    CLST
    FS.
\    D_RPNS    
    \
\ -------------------------------------------------------   
    CR ." >>>>> PRREG REG41 FS. CLST FS. " CR
    PRREG REG41
    FS.
\    D_RPNS
    CLST
    \
\    D_RPNS
    FS.
\ -------------------------------------------------------    
\ -------------------------------------------------------  
    CR ." >>>>> CLRG REG41 " CR
    CLRG REG41
\    D_RPNS
    \
\ -------------------------------------------------------   
    CR ." >>>>> PRREG REG41 " CR
    PRREG REG41
\    D_RPNS
    \
\    D_RPNS
\ -------------------------------------------------------    
\ -------------------------------------------------------  
    CR ." >>>>> 88.0 D_SSET 5 REG41 RCL FS. " CR
    88.0E0 D_SSET 5 REG41 RCL
    FS.
\    D_RPNS    
    \
\ -------------------------------------------------------  
    CR ." >>>>> 14 REG41 14.0 STO FS. CLST FS. " CR
    14 REG41 14.0E0 STO
    FS.
    \    D_RPNS
    CLST FS.
    \
\ -------------------------------------------------------  
    CR ." >>>>> REGMOVE REG41 " CR
    0 REG41 0.0E0 STO
    1 REG41 1.0E0 STO
    2 REG41 2.0E0 STO
    3 REG41 3.0E0 STO
    4 REG41 4.0E0 STO
    5 REG41 5.0E0 STO
    6 REG41 6.0E0 STO
    7 REG41 7.0E0 STO
    8 REG41 8.0E0 STO
    9 REG41 9.0E0 STO
    10 REG41 10.0E0 STO
    11 REG41 11.0E0 STO
    12 REG41 12.0E0 STO
    13 REG41 13.0E0 STO
        PRREG REG41
    0.006003E0 D_SSET X REGMOVE REG41
        PRREG REG41
\    D_RPNS
    \
\ -------------------------------------------------------  
    CR CR ." >>>>> REGSWAP REG41 " CR
    0 REG41 0.0E0 STO
    1 REG41 1.0E0 STO
    2 REG41 2.0E0 STO
    3 REG41 3.0E0 STO
    4 REG41 4.0E0 STO
    5 REG41 5.0E0 STO
    6 REG41 6.0E0 STO
    7 REG41 7.0E0 STO
    8 REG41 8.0E0 STO
    9 REG41 9.0E0 STO
    10 REG41 10.0E0 STO
    11 REG41 11.0E0 STO
    12 REG41 12.0E0 STO
    13 REG41 13.0E0 STO
        PRREG REG41 CR
    2.010004E0 D_SSET X REGSWAP REG41
        PRREG REG41
\    D_RPNS
    \
    \ -------------------------------------------------------
    \
    \
\ -------------------------------------------------------  
    CR ." >>>>> IND REG41 " CR
    0 REG41 0.0E0 STO
    1 REG41 1.0E0 STO
    2 REG41 2.0E0 STO
    3 REG41 7.7E0 STO
    4 REG41 4.0E0 STO
    5 REG41 5.0E0 STO
    6 REG41 6.0E0 STO
    7 REG41 7.0E0 STO
    8 REG41 8.0E0 STO
    9 REG41 9.0E0 STO
    10 REG41 10.0E0 STO
    11 REG41 11.0E0 STO
    12 REG41 12.0E0 STO
    13 REG41 13.0E0 STO
    3 IND REG41 22222.0E0 STO
    PRREG REG41
    CR ." >>>>> in register 7 should see 22222 " CR
\    D_RPNS
    \
\ -------------------------------------------------------  

    
\ -------------------------------------------------------  
\
    \    CR ." >>>>> 15 REG41 0.0 STO FS. " CR
\    15 REG41 0.0E0 D_SSET STO
\    FS.
\
    \    D_RPNS
    \
    \
    ." ************************************** " CR
    ."              END TEST                  " CR
    ." ************************************** " CR
;
\
VARIABLE 12OUT 
: CLK24 12OUT OFF ;
: CLK12 12OUT ON ;
CLK24
\ ************************* ITIME41 ****************************
\ CX Owner manual p240 (pdf 98)
\
\ HP41C / CV /CX with time module gives
\
\ PRSTK
\ 
\ T=  0.000000000
\ Z=  0.000000000
\ Y=  0.000000000
\ X=  0.000000000
\ TIME
\    11:09:04 PM   CLK12
\ or 23:09:04      CLK24
\ 23.09049900 ***
\ PRSTK
\
\ T=  0.000000000
\ Z=  0.000000000
\ Y=  0.000000000
\ X=  23.09049900
\
\ Execution/Inputs: nothing
\ Outputs: the value in X
\
\ Modules used
\   TIME from FTHUTILA ASM file
\   FORTH/ASSEMBLER
\
\ under CC BY SA CreativeCommons 4.0
\
\ change log
\   date 2025 04 27 creation
\
( Similar function in HP41 Time module                           )
( Executing TIME41 places a number representing the current      )
( time in the X-register.                                        )
( The number is formatted according to the                       )
( 24-hour/time format: HH.MMSShh                                 )
\
( and included into the integer stack SS MM HH                   )
( https://github.com/f4iteightiz/HP71B_forth/blob/main/CLOCK.SRC )
\
\ tested 02 May 2025
\ FS.
\ T=  0.000000 
\ Z=  0.000000 
\ Y=  0.000000 
\ X=  0.000000 
\ L=  0.000000 
\ OK { 0 } 
\ OTIME
\ 19:43:02 OK { 0 } 
\ OTIME41
\ 19:43:06 OK { 0 } 
\ FS.
\ T=  0.000000 
\ Z=  0.000000 
\ Y=  70982.320000 
\ X=  19.430536 
\ L=  0.000000 
\ OK { 0 } 
\ CLK12
\ OK { 0 } 
\ OTIME41
\ 07:43:53 PM OK { 0 } 
\ OTIME
\ 07:44:01 PM OK { 0 } 
\ CLK24
\ OK { 0 } 
\ OTIME
\ 19:44:12 OK { 0 } 
\ OTIME41
\ 19:44:16 OK { 0 } 
: 2DIGITS ( n -- )  \ prints a number with 2 digits, adding leading zero if needed
    DUP 10 < IF
	\        [CHAR] 0 EMIT  \ emit '0' if less than 10
	48 CHR$ TYPE
    THEN
    0 .R ;             \ print number right-aligned in 0 columns (prints all digits)

: SHOW-TIME ( hh mm ss -- )
\
12OUT @
IF
	DUP
	12 > IF
	    12 MOD
	    2DIGITS
	    \    [CHAR] : EMIT
	    ." :"
    2DIGITS
	    \    [CHAR] : EMIT
	    ." :"
	    2DIGITS
	    SPACE
           ." PM"
	ELSE
	     2DIGITS
	    \    [CHAR] : EMIT
	    ." :"
    2DIGITS
	    \    [CHAR] : EMIT
	    ." :"
    2DIGITS
	THEN
ELSE
    2DIGITS
    \    [CHAR] : EMIT
    ." :"
    2DIGITS
    \    [CHAR] : EMIT
    ." :"
    2DIGITS
\
THEN
\
 ;

VARIABLE TIMEHH
VARIABLE TIMEMM
VARIABLE TIMESS
: OTIME TIME FTOI         \ sssssss
    DUP                   \ sssssss   ssssssss
    3600 /                \ sssssss   HH
    DUP TIMEHH !          \ sssssss   HH
    SWAP                  \ HH        sssssss   
    3600 MOD DUP 60 / 
    DUP TIMEMM !          \ HH  MM  MM
    SWAP
    60 MOD DUP TIMESS !
    \    DUP 3 PICK 5 PICK
    SWAP ROT
    SHOW-TIME
; \ HH  MM  SS
: OTIME41 OTIME T71>41 ;  \  HH.MMSSss   show this with the word f.s

