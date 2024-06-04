( Clock program for HP71B                                      )
( Forth version for use in PC                                  )
( look at other variants for use on EMU71 or HP71B             )
( )
( license CC BY NC SA CreativeCommons 4.0                      )
( https://creativecommons.org/licenses/by-nc-sa/4.0/deed.de    )
( floppy @ https://www.hpmuseum.org/forum/                     )
( and                                                          )
( pascaldagornet at yahoo dot de                               )
( )
( Change log                                                   )
( 04 June 2024 Upload into Github                              )
( )
( Remarks )
( any lines with D_XYZ will be deleted later by the AWK script )
( which is used to transform this gforth script into a more    )
( efficient HP71B script                                       )
( )
( use on HP71B                                                 )
( in the BASIC prompt >                                        )
( DISPLAY IS *                                                 )
( SETDATE”24/06/04”                               04 June 2024 )
( SETTIME”14:41:00”                                            )
( FORTH                                                        )
( 0 PRIMARY !                                                  )
( D-D                          if the screen is not the output )
( PAGE                                        clean the screen )
( STARTCLO                                     start the CLOCK )
( key E will exit                                              )
( DATE$ display the date in BASIC                              )
(                                                              )
( on the PC, start it in a terminal with gforth GLOCK.fth      )
( start the clock with S"YY/MM/DD" STARTCLO in Gforth prompt   )
( where YY MM DD are the year month day                        )
(                                                              )
( more to be done?                                             )
( verify if SETDATE"23/11/14" can be done in FORTH or          )
( DATE asm can be programmed                                   )
( )
S" 71B2G.fth" INCLUDED
S" H71B1.fth" INCLUDED
\
CR CR S" STEP1: MEM ALLOC " TYPE CR D_PAUSE3S
( )
( setup string buffers MAXIMUM 255 )
( horiz/side lines and all ESC n% numbers for video control    )
76 STRING LINEBUF
180 STRING SIDEBUF
( number buffer string for video control )
84 STRING NBBUFSTR
32 STRING SEPBUFSTR
( )
( number fonts constructed according Sylvain Cote              )
( https://www.hpmuseum.org/forum/thread-20695-post-179305.html#pid179305 )
( )
(     12345678   12345678   12345678   12345678   12345678   12345678   12345678   12345678   12345678   12345678 )
(   +----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+ )
( 1 | ######## |    ##    | ######## | ######## | ##       | ######## | ######## | ######## | ######## | ######## | )
( 2 | ##    ## |    ##    |       ## |       ## | ##       | ##       | ##       |       ## | ##    ## | ##    ## | )
( 3 | ##    ## |    ##    |       ## |       ## | ##    ## | ##       | ##       |       ## | ##    ## | ##    ## | )
( 4 | ##    ## |    ##    | ######## |  ####### | ######## | ######## | ######## |       ## | ######## | ######## | )
( 5 | ##    ## |    ##    | ##       |       ## |       ## |       ## | ##    ## |       ## | ##    ## |       ## | )
( 6 | ##    ## |    ##    | ##       |       ## |       ## |       ## | ##    ## |       ## | ##    ## |       ## | )
( 7 | ######## |    ##    | ######## | ######## |       ## | ######## | ######## |       ## | ######## | ######## | )
(   +----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+ )
( )
S" STEP2: FONTS DEFINITION " TYPE CR D_PAUSE3S
( )
56 10 STRING-ARRAY NUMBCLO
( 0 creation )
S" ##########    ####    ####    ####    ####    ##########"
1 NUMBCLO S!
( 1 and others till 9 creation )
S"    ##      ##      ##      ##      ##      ##      ##   "
2 NUMBCLO S!
S" ########      ##      ############      ##      ########"
3 NUMBCLO S!
S" ########      ##      ## #######      ##      ##########"
4 NUMBCLO S!
S" ##      ##      ##    ##########      ##      ##      ##"
5 NUMBCLO S!
S" ##########      ##      ########      ##      ##########"
6 NUMBCLO S!
S" ##########      ##      ##########    ####    ##########"
7 NUMBCLO S!
S" ########      ##      ##      ##      ##      ##      ##"
8 NUMBCLO S!
S" ##########    ####    ############    ####    ##########"
9 NUMBCLO S!
S" ##########    ####    ##########      ##      ##########"
10 NUMBCLO S!
( )
( option for having more extented separated fonts below        )
( NOT IMPLEMENTED )
( Separation font )
(     123456   123456 )
(   +--------+--------+ )
( 1 |        |        | )
( 2 |        |        | )
( 3 |    #   |        | )
( 4 |        |   ##   | )
( 5 |    #   |        | )
( 6 |        |        | )
( 7 |        |        | )
(   +--------+--------+ )
\ 42 2 STRING-ARRAY SEPCLO
\ S"                #              #            " 1 SEPCLO S!
\ S"                     ##                     " 2 SEPCLO S!
( )
( Output example )
(    1         1         1         1         1         1         1         1  )
(  1 +----------------------------------------------------------------------+ )
(  2 |                                                                      | )
(  3 |  ########  ########         ##     ########      ########  ########  | )
(  4 |        ##        ##         ##     ##    ##            ##  ##    ##  | )
(  5 |        ##        ##         ##     ##    ##            ##  ##    ##  | )
(  6 |  ########   #######  ##     ##     ##    ##  ##  ########  ########  | )
(  7 |  ##              ##         ##     ##    ##      ##              ##  | )
(  8 |  ##              ##         ##     ##    ##      ##              ##  | )
(  9 |  ########  ########         ##     ########      ########  ########  | )
( 10 |                                                                      | )
( 11 +----------------------------------------------------------------------+ )
( 12 |                                                                      | )
( 13 |     ##     ########      ########  ########      ##        ########  | )
( 14 |     ##     ##                  ##        ##      ##        ##    ##  | )
( 15 |     ##     ##         #        ##        ##   #  ##        ##    ##  | )
( 16 |     ##     ########       #######  ########      ##    ##  ########  | )
( 17 |     ##           ##   #        ##  ##         #  ########  ##    ##  | )
( 18 |     ##           ##            ##  ##                  ##  ##    ##  | )
( 19 |     ##     ########      ########  ########            ##  ########  | )
( 20 |                                                                      | )
( 21 +----------------------------------------------------------------------+ )
( )
VARIABLE YY
VARIABLE MM
VARIABLE DD
( save date from input in gforth )
( string S" YY/MM/DD" on gforth  )
: DATESET
    \ S" DATE$" BASIC$ ( read the date from system on HP71B )
    \
D_PADSET
    \
2 LEFT$ VAL DROP YY !
( PAD is now counted string 2 charswith LEFT$ but 8 still there ) 
PAD 1 CHARS + COUNT DROP 8
4 5 SUB$ VAL DROP MM !
PAD 1 CHARS + COUNT DROP 8
2 RIGHT$ VAL DROP DD !
( )
\ test..
\ gforth
\   gforth 71B2G.fth H71B1.fth GLOCK.fth
\   S" 23/11/22" DATESET YY @ . MM @ . DD @ . 
;
( )
VARIABLE TIMEHH
VARIABLE TIMEMM
VARIABLE TIMESS
VARIABLE TIMEHH-N
VARIABLE TIMEMM-N
VARIABLE TIMESS-N
( )
( update the time variable )
( -- )
: TIMEINIT -1 TIMEHH ! -1 TIMEMM ! -1 TIMESS ! ;
: TIMEUPDT TIME FTOI DUP 3600 / TIMEHH ! 3600 MOD DUP 60 /
    TIMEMM ! 60 MOD TIMESS ! ;
: TIMEUPDT-N TIME FTOI DUP 3600 / TIMEHH-N ! 3600 MOD DUP 60 /
    TIMEMM-N ! 60 MOD TIMESS-N ! ;
( )
S" STEP3: LINE FRAME CREATION" TYPE CR D_PAUSE3S
( )
( draw the clock frame                                  )
( first string for the frame horizontal lines construct )
72 STRING LCLO
S" +----------------------------------------------------------------------+"
LCLO S!
\
\ S" -----------------------------------+"
\ LCLO AS+   append string? not necessary
\
: DRAW-LINES ( -- )
    ( 3 horizontal  lines )
    CELLV 5 =
    IF
( later for HP71B only )
    LINEBUF CLEARSTR
    1  1 AT-XY<BF LCLO S<&
    OUTPUT
( later for HP71B only )
    LINEBUF CLEARSTR
    1 11 AT-XY<BF LCLO S<&
    OUTPUT
( later for HP71B only )
    LINEBUF CLEARSTR
    1 21 AT-XY<BF LCLO S<&
    OUTPUT
ELSE
( for gforth only  )
    1 1 D_AT-XY LCLO TYPE
    1 11 D_AT-XY LCLO TYPE
    1 21 D_AT-XY LCLO TYPE
THEN ;
( )
: DRAW-SIDES
    ( )
    CELLV 5 =
    IF
( vertikale again by using the PAD )
( for HP71B branch only )
	SIDEBUF CLEARSTR
	11 2 DO
	    1 I AT-XY<BF S" |"
	D_PADSET
	S<& 72 I AT-XY<BF S" |"
	D_PADSET
	    S<&
	LOOP
	21 12 DO 1 I AT-XY<BF S" |"
	D_PADSET
	S<& 72 I AT-XY<BF S" |"
	D_PADSET
	S<& LOOP OUTPUT

ELSE
( vertikale gforth only since all words D_ will be deleted     )
( later with using the awk filtering script )    
    11 2 DO 1 I D_AT-XY ." |" 72 I D_AT-XY ." |" LOOP
    21 12 DO 1 I D_AT-XY ." |" 72 I D_AT-XY ." |" LOOP
( )
THEN ;
( )
( -- )
: DRAW-FRAMEC DRAW-LINES DRAW-SIDES ;
( ) 
( draw separation sign; see font above; using EMIT )
( OK in gforth; NOK in EMU71 )
( -- )
\ : DRAW-SEP  24 6 AT-XY ." ##" 48 6 AT-XY ." ##"
\    25 15 AT-XY ." #" 49 15 AT-XY ." #"
\    25 17 AT-XY ." #" 49 17 AT-XY ." #" ;
\
( draw separation sign; see font above; font buffer )
( use a generic word like below or adapt the script )
( to the used HW. above DRAW-SEP is only for GFORTH )
( -- )
: DRAW-SEP
    CELLV 5 =
    IF
	( HP71B branch )
	SEPBUFSTR CLEARSTR 24 6 AT-XY<BF S" ##"
	D_PADSET ( necessary if simulation on PC )
	S<& 48 6 AT-XY<BF S" ##"
	D_PADSET
	S<& 25 15 AT-XY<BF S" #"
	D_PADSET
	S<& 49 15 AT-XY<BF S" #"
	D_PADSET
	S<& 25 17 AT-XY<BF S" #"
	D_PADSET
	S<& 49 17 AT-XY<BF S" #"
	D_PADSET
	S<& OUTPUT
    ELSE 
	24 6 D_AT-XY ." ##" 48 6 D_AT-XY ." ##" 
	25 15 D_AT-XY ." #" 49 15 D_AT-XY ." #"
	25 17 D_AT-XY ." #" 49 17 D_AT-XY ." #"
    THEN ;
( )
( for debugging the font in gforth environment )
( could be used in // to OUTNB with            )
(    CELLV 5 = IF OUTNB ELSE OUTNBPC THEN      )
( X Y n -- )
: OUTNBPC
    1 + NUMBCLO DROP ( X Y AddrStr ) D_WARN
    8 1 DO D_WARN
	9 1 DO DUP ( X Y AddrStr AddrStr ) D_WARN
	    C@ ( X Y AddrStr Char ) D_WARN
	    SWAP 1 CHARS + SWAP ( X Y AddrStr+1 Char ) D_WARN
	    4 PICK I + 1 - ( X Y AddrStr+1 Char X-Char ) D_WARN
	    4 PICK J + 1 - ( X Y AddrStr+1 Char X-Char Y-Char ) D_WARN
	    AT-XY EMIT LOOP LOOP DROP DROP DROP D_WARN
;
( )
( for future HP71B font )
( dump the result, the string, with STRDUMP )
( for evaluating if it will work on HP71B )
: OUTNB  ( X Y n -- )
( clear the number buffer )
NBBUFSTR CLEARSTR ( X Y n BufStart )
( )
( get the correct string off the number array )
ROT 1 + NUMBCLO ( X Y BufStart StrStart )
2SWAP           ( X Y StrStart BufStart )
( )
( loop for all 8 * 7 number character )
8 1 DO
( X Y StrStart BufStart )
6 PICK ( X Y StrStart BufStart X-CharLine )
6 PICK I + 1 - ( X Y StrStart BufStart X-CharLine Y-CharLine )
AT-XY<BF       ( X Y StrStart BufStart+ )
2OVER          ( X Y StrStart BufStart+ StrStart )
I 1 - 8 * 1 + I 8 *  ( X Y StrStart BufStart+ StrStart _J-1_*8+1 J*8 )
SUB$ S<&             ( X Y StrStart BufStart++ )
LOOP 2NIP 2NIP OUTPUT ;
( )    
: DRAW-DATE
( draw all 6 single numbers ONE TIME ONLY                      )
( = no update during execution                                 )
    4  3 YY @ 10 /
( )
( keep OUTNB only in an HP71B and not the line below which     )
( should be used only in a gforth environment                  )
    CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
( )
    14 3 YY @ 10 MOD
    \ OUTNB or multiplatform PC & HP71B
     CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
    28 3 MM @ 10 /
    \ OUTNB or multiplatform PC & HP71B
     CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
    38 3 MM @ 10 MOD
    \ OUTNB or multiplatform PC & HP71B
     CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
    52 3 DD @ 10 /
    \ OUTNB or multiplatform PC & HP71B
     CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
    62 3 DD @ 10 MOD
    \ OUTNB or multiplatform PC & HP71B
     CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
;
( )
: UPDATE-SS
    TIMESS-N @ 10 / DUP TIMESS @ 10 / <> IF 52 13 ROT
	\ OUTNB
	CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
    ELSE DROP THEN 
    TIMESS-N @ 10 MOD DUP TIMESS @ 10 MOD <> IF 62 13 ROT
	\ OUTNB
	CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
ELSE DROP THEN ;
( )
: UPDATE-MM
    TIMEMM-N @ 10 / DUP TIMEMM @ 10 / <> IF 28 13 ROT
	\ OUTNB
	CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
    ELSE DROP THEN
TIMEMM-N @ 10 MOD DUP TIMEMM @ 10 MOD <> IF 38 13
    ROT
    \ OUTNB
    CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
ELSE DROP THEN ;
 ( )
: UPDATE-HH  TIMEHH-N @ 10 / DUP TIMEHH @ 10 / <> IF 4 13
	ROT
	\ OUTNB
	CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
    ELSE DROP THEN 
TIMEHH-N @ 10 MOD DUP TIMEHH @ 10 MOD <> IF 14 13
    ROT
    \ OUTNB
    CELLV 5 = IF OUTNB ELSE OUTNBPC THEN
ELSE DROP THEN ;
( )
( clock initialization )
: INITCLO DELAY00 PAGE
\    RES2CHR 2 OUTPUT (  so far not needed on HP71B  )
    DRAW-FRAMEC
    DRAW-SEP
    DATESET
    DRAW-DATE
    TIMEINIT TIMEUPDT-N UPDATE-SS UPDATE-MM UPDATE-HH ;
( )
( update time variables of the clock and letters in the screen )
( -- )
: UPDATE-CLOCK  TIMEUPDT-N TIMEHH @ TIMEHH-N @
    <> IF UPDATE-HH UPDATE-MM UPDATE-SS
	TIMEHH-N @ TIMEHH !
	TIMEMM-N @ TIMEMM !
	TIMESS-N @ TIMESS ! ELSE
	TIMEMM @ TIMEMM-N @
	<> IF UPDATE-MM UPDATE-SS
	    TIMEMM-N @ TIMEMM !
	    TIMESS-N @ TIMESS ! ELSE
	    TIMESS @ TIMESS-N @
	    <> IF UPDATE-SS TIMESS-N @ TIMESS !
	    ELSE THEN THEN THEN ;
( )
( refresh time loop or exit with e or E key )
( -- )
: RETIMELO  BEGIN UPDATE-CLOCK
( https://gforth.org/manual/Simple-Loops.html )
KEY? IF KEY
DUP 69 <> IF
101 <> IF FALSE ELSE TRUE THEN
ELSE DROP TRUE THEN
ELSE FALSE THEN UNTIL ;
( )
( start the clock )
( )
( str-gforth -- )
( str of type S" 23/11/24" on gforth )
( )
( -- ) ( nothing in HP71B ) 
: STARTCLO INITCLO RETIMELO ;
