( SNAKE.fth game under HP71B Forth                             )
( additional words in HP71B1.fth FTHUTILF.fth file             )
( )
( Try to stick to a gforth syntax for clarity, especially if   )
( the original was done in gforth                              )
( )
( Direct use of an HP71B syntax is possible and should be      )
( preferred in case it is coming from an HP71B version         )
( )
( original from https://github.com/robertpfeiffer/..
(      ..forthsnake/blob/master/snake.forth                    )
( )
( updates under CC BY SA CreativeCommons 4.0                   )
( https://creativecommons.org/licenses/by-nc-sa/4.0/deed.de    )
( pascaldagornet at yahoo dot de                               )
( )
( Use ..                                                       )
( editing  in a terminal with emacs for several files          )
(    all 3      71B2G H71B1 SNAKE                              )
( )
(   emacs 71B2G.fth H71B1.fth SNAKE.fth .. see 71B2G header    )
( )
( starting on PC with gforth SNAKE.fth and SSNAKE              )
( starting on HP71B with SSNAKE                                )
( )
( both lines below should be only on gforth pc use             )
S" 71B2G.fth" INCLUDED
S" H71B1.fth" INCLUDED
( )
( moving the snake with                                        )
( key 97  a left                                               )
(     119 w up                                                 )
(     100 d right                                              )
(     115 s down                                               )
(     or the Upsize of it                                      )
( )
( change log                                                   )
( 25 Sept 2023 creation                                        )
( 31 Oct  2023 upload https://www.hpmuseum.org/forum/          )
( 12 Dec  2023 update with no continuous render                )
(               but with only only necessary character updates )
( 07 Juni 2024 Github upload )
( )
76  STRING LINEBUF ( D_ from CLOCK )
128 STRING SIDEBUF ( D_ from CLOCK )
( )
52  STRING SL1
S" +--------------------------------------------------+" SL1 S!
( )
( side line buffer stored in string format                     )
( use like n STRV Str1                                         )
( which will give + - - .. + vertical of total height n        )
( when using OUTPUT                                            )
: STRV CREATE DUP 1 - 5 * 1 + DUP C, C, 43 C, 2 - 0
    DO 27 C, 66 C, 27 C, 68 C, 124 C, LOOP
    27 C, 66 C, 27 C, 68 C, 43 C,
  DOES>  1 CHARS + DUP 1 CHARS + SWAP C@ ;
( )
15 STRING SNAKEBUF
21 STRING LENGTHBUF
5  STRING APPLEBUF
( )
( original random generator                                    )
( : MYRAND OVER - UTIME + SWAP MOD + ;                         )
( a b -- r )
: MYRAND OVER - TIME 1000.00E0 F/ FP 100000.00E0 F* FTOI SWAP
    PMOD +  ;
( )
: SNAKE-SIZE 200 ;
( )
( not 2 mod = 0 numbers below; will impact the frame aspect    )
: XDIM 51 ; ( for screen position horiz: from 0 to 51 = 52 siz )
: YDIM 21 ; ( for screen position verti: from 0 to 21 = 22 siz )
( )
( construct side string of height n )
 YDIM 1 + STRV SV1
( )
CREATE SNAKE SNAKE-SIZE CELLS 2 * NALLOT ( coordinate of the snake body                                   )
CREATE APPLE 2 CELLS NALLOT              ( coordinate of the apple                                        )
CREATE APPLE_OLD 2 CELLS NALLOT          ( coordinate of the previous apple position                      )
                                         ( this position must not be overwritten by a body char sign      )
VARIABLE HEAD                            ( pos of the snake head in SNAKE variable, will be zero at start )
VARIABLE LENGTH                          ( current length of the snake                                    ) 
VARIABLE DIRECTION                       ( current direction to be used as a ticked variable              )
( )
( seg -- adr )
( return the address in the SNAKE memory alloc where the snake body start )
: SEGMENT
    HEAD @ +
    SNAKE-SIZE PMOD CELLS 2 * SNAKE + ;
( )
( x1 y1 x2 y2 -- x y )
( x1y1 origin ans x2y2 from direction )
: POS+ ROT + -ROT + SWAP ;
( )
: POINT= 2@ ROT 2@ ROT = -ROT = AND ;
( )
( -- x y )
( return the position of the head   )
: HEAD* 0 SEGMENT ;
( )
( -- x y )
( return the position of the tail   )
: TAIL*  LENGTH @ SEGMENT ;
( )
( update of the variable HEAD,      )
( pos of the head in SNAKE variable )
( move from 199 198 197 196.. down  )
( -- )
: MOVE-HEAD!
    HEAD @ 1 -
    SNAKE-SIZE PMOD
    HEAD ! ;
( )
( -- )
( add 1 into snake length )
: GROW!
    1 LENGTH +! ;
( )
( -- )
: EAT-APPLE!
    APPLE 2@ APPLE_OLD 2!  ( store the previous apple position )
    1 XDIM MYRAND 1 YDIM MYRAND APPLE 2! GROW! ;
( )
: STEP! ( xdiff ydiff -- )
    HEAD*
    2@
    MOVE-HEAD!
    POS+
    HEAD*
    2! ;
( )
( -- xadd yadd )
: LEFT  -1  0 ; ( will add x y to the position for direction )
: RIGHT  1  0 ;
: DOWN   0  1 ;
: UP     0 -1 ;
\
\ ( -- bool )
( check if the head is not within the frame dimension )
( which means it collided with the wall )
: WALL? HEAD* 2@ 1 YDIM WITHIN SWAP 1 XDIM WITHIN AND NOT  ;
\
( gforth previous with ?DO                                      )
( -- bool )
( : CROSSING?                                                   )
(    FALSE LENGTH @ 1 ?DO I SEGMENT HEAD* POINT= OR LOOP  ;     )
( new                                                           )
: CROSSING? FALSE LENGTH @ 1 2DUP = IF 2DROP ELSE DO I SEGMENT
	    HEAD* POINT= OR LOOP THEN  ;
( )
( -- bool )  \ put a blank in front of ( for awk filtering later
( check if head was at apple position )
: APPLE?
    HEAD*      ( return the address of the head )
    APPLE      ( return the address of the apple )
    POINT=  ;
( )
( -- bool )
: DEAD? WALL? CROSSING? OR  ;
( )
( )
( previous with ?DO                                             )
( -- )
( : DRAW-FRAME  0 0 AT-XY XDIM 0 ?DO ." +" LOOP                 )
( YDIM 0 ?DO XDIM I AT-XY ." +" CR ." +" LOOP XDIM 0 ?DO ." +"  )
( LOOP CR ;                                                     )
( -- )
( : DRAW-SNAKE LENGTH @ 0 ?DO I SEGMENT 2@ AT-XY ." #" LOOP ;   )
( )
( new tested fine on PC-gforth without ?DO                      )
( : DRAW-FRAME 0 0 AT-XY XDIM 0 2DUP = IF 2DROP ELSE DO ." +"   )
( LOOP THEN YDIM 0 2DUP = IF 2DROP ELSE DO XDIM I AT-XY ." +"   )
( CR ." +" LOOP THEN XDIM 0 2DUP = IF 2DROP ELSE DO ." +" LOOP  )
( THEN CR ;                                                     )
( )
( no big improvement seen if 43 emit instead of ." +"           )
( )
( only all fourth / third signed used for quicker 6s            )
( : DRAW-FRAME                                                  )
(    XDIM 1 DO I 0 AT-XY 45 EMIT I YDIM AT-XY 45 EMIT 4 +LOOP   )
(    YDIM 1 DO XDIM I AT-XY 124 EMIT CR 124 EMIT 3 +LOOP        )
(    0 0 AT-XY 43 EMIT                                          )
(    XDIM 0 AT-XY 43 EMIT                                       )
(    XDIM YDIM AT-XY 43 EMIT                                    )
(    0 YDIM AT-XY 43 EMIT                                       )
(    CR ;                                                       )
( )
( -- )
\ this had few weird appearance on HP71B Video80
\ : DRAW-FRAME 
\    0 0 AT-XY SL1 TYPE
\    YDIM 1 DO XDIM I AT-XY 124 EMIT CR 124 EMIT 3 +LOOP
\    0 YDIM AT-XY SL1 TYPE  CR ;
\
: DRAW-FRAME
( frame simplified for gforth only; the next code prior        )
    ( transfer to HP71B filtered by awk out due to D_ in the line  )
    ( that works fine under gforth - tested 12 Dec 2023 )
    0 0 D_AT-XY SL1 TYPE
    YDIM 1 DO XDIM I D_AT-XY 124 EMIT CR 124 EMIT 3 +LOOP
    0 YDIM D_AT-XY SL1 TYPE CR
    ( )
    ( below is for HP71B only else it screw up the gforth  )
    ( but however, can be used for debugging in terminal   )
    ( the block CELLV @ 5 = IF ELSE THEN is for HP71B only )
    ( take the lines "CELLV @ 5 = IF" and "ELSE THEN" away )
    ( if you want to debug all in // under gforth          )
    ( however, only the block below should stay in HP71B   )
    ( horizontal lines )
    ( )
    CELLV @ 5 = IF
    ( )
    LINEBUF CLEARSTR
    0  0 AT-XY<BF SL1 S<&
    OUTPUT
    LINEBUF CLEARSTR
    0 YDIM AT-XY<BF SL1 S<&
    OUTPUT
    ( )
    ( vertical lines )
    SIDEBUF CLEARSTR
    0 0 AT-XY<BF SV1 S<&
    OUTPUT
    ( )
    SIDEBUF CLEARSTR
    XDIM 0 AT-XY<BF SV1 S<&
	OUTPUT
	ELSE THEN
;
( -- )
( previous with ?DO )
( : draw-snake length @ 0 ?do i segment 2@ at-xy ." #" loop ;   )
( )
( )
( new tested fine on PC-gforth without ?DO                      )
: DRAW-SNAKE LENGTH @ 0 2DUP = IF 2DROP ELSE DO I SEGMENT 2@  
	    AT-XY ." #" LOOP THEN ;
( )
( only HP71B below                                   )
( then uncomment below for HP71B version             )
( and comment above which is the gforth version only )
\ : DRAW-SNAKE
\    SNAKEBUF CLEARSTR
\    LENGTH @   ( return the snake length n )
\    0 2DUP = IF 2DROP ELSE DO
	    ( from 0 to LENGTH )
\	    I SEGMENT 2@ AT-XY<BF 35 CHR$ S<& LOOP THEN OUTPUT ;
( )
( make the snake longer by 1 pos for the head  )
( and 1 pos shorter by the tail anyway         )
( .. ONLY if an apple was not there               )
( later .. )
: UPDATE-SNAKE
    ( gforth )
    ( must be taken out when use of HP71B )
    \    0 SEGMENT 2@ D_AT-XY 35 EMIT
    HEAD* 2@ D_AT-XY 35 EMIT
    TAIL* APPLE POINT= NOT IF TAIL* 2@ D_AT-XY BL EMIT ELSE THEN
    ( )
    ( HP71B )
\    SNAKEBUF CLEARSTR
\    0 SEGMENT 2@ AT-XY<BF S" #"
\    D_PADSET
\    S<& OUTPUT
\    SNAKEBUF CLEARSTR
    \    LENGTH @ SEGMENT 2@ AT-XY<BF 32 CHR$ S<& OUTPUT
    ( )
;
( -- )
( original word in gforth : DRAW-APPLE  APPLE 2@ AT-XY ." Q" ; )
: DRAW-APPLE
    ( )
    ( use only the gforth part below on the PC )
    ( use only the HP71B part below on HP71B )
    ( )
    ( gforth )
    APPLE 2@ D_AT-XY 81 EMIT  ( get the X Y coordinate of the apple and draw a Q  )   
    ( HP71 B )
    ( )
\    APPLEBUF CLEARSTR
\    APPLE 2@ AT-XY<BF S" Q"
\    D_PADSET
\    S<& OUTPUT
    ( )
( )
( can be changed later with a PAD string and use of OUTPUT     )
;
( )
( put a # on the last known position and draw the new apple    )
( ONLY if the new position is different from the old position  )
( position )
( -- )
: UPDATE-APPLE
    ( gforth )
\    APPLE_OLD APPLE POINT= NOT IF APPLE_OLD 2@ D_AT-XY 35 EMIT   ( get the old X Y coordinate of the apple and draw # at it  ) ELSE THEN
 APPLE_OLD 2@ D_AT-XY 35 EMIT
    ( )
    ( uncomment below and comment above when you go to HP71B )
    ( HP71B )
\    APPLEBUF CLEARSTR APPLE_OLD 2@ AT-XY<BF S" #"
\    D_PADSET
\    S<& OUTPUT
    ( )
    DRAW-APPLE
;
( )
( show the characters on the screen: constant like frame or    )
( variable like snake and apple                                )
( keep this only for gforth environment test                   )
( -- )
\ : RENDER
    ( )
\    RES2CHR OUTPUT  ( output a 2char allocated mem of reset )
\    PAGE
    (  remove that  PAGE  later )
    ( )
\    DRAW-SNAKE        ( draw snake.. change to only last white and head new ? )
\    DRAW-APPLE        ( draw last pos white and new with character ? )
\    DRAW-FRAME        ( draw again if reset was done )
\    CR LENGTH @ . ;   ( show the snake length on the screen )
( )
( show the snake length on the screen )
( eventually update for using OUTPUT )
( -- )
: DRAW-LENGTH
    ( )
    ( gforth.. line will be deleted later by the awk script due to D_ )
    0 22 D_AT-XY ." SNAKE LENGTH: " LENGTH @ .
    ( )
    ( uncomment below and comment above when you go to HP71B )
    ( or put the block CELLV @ 5 = IF ELSE THEN to take it automatically out )
    ( without this block setup , comment and/or uncomment above & below      )
    ( else it will screw up your terminal output                             )
    ( HP71B )
\    LENGTHBUF CLEARSTR
\    0 22 AT-XY<BF S" SNAKE LENGTH: "
\    D_PADSET
\    S<& LENGTH @ N>$ S<& OUTPUT
;
( )
( update the length number of the snake on the screen   )
( we update only the strict necessary characters on the )
( screen for avoiding time consuming activities         )
: UPDATE-LENGTH
    ( overtake gforth or HP71B below )
    ( not both                       )
    ( )
    ( gforth )
    14 22 D_AT-XY LENGTH @ .
    ( )
    ( HP71B )
\    LENGTHBUF CLEARSTR
\    14 22 AT-XY<BF LENGTH @ N>$ S<& OUTPUT
;
( -- )
( new data on the screen in case of new game ) 
( -- )
: NEWGAME!
    ( refresh from originally RENDER moved here )
    RES2CHR OUTPUT    ( output a 2char allocated mem of reset )
    ( )
    0 HEAD !                   ( store zero in head variable )
    XDIM 2 / YDIM 2 / SNAKE 2! ( initial position of the snake is middle )
    3 3 APPLE 2!               ( initial position of the apple is 3 3 )
    3 LENGTH !                 ( initial length is 3 for snake )
    ['] UP DIRECTION !         ( initial direction is up )
    LEFT STEP!                 ( one full turn. only for debugging? )
    LEFT STEP!
    LEFT STEP!
    LEFT STEP!
    ( originally DRAW-FRAME from RENDER which is working well )
    ( only under gforth )
    DRAW-FRAME        ( draw again if reset was done )
    DRAW-SNAKE        ( draw snake.. first time )
    DRAW-APPLE
    APPLE 2@
    APPLE_OLD 2!
    DRAW-LENGTH
;
( )
( time -- )
: GAMELOOP
    BEGIN
	( below was taken out from the original program because       )
	( drawing of the whole frame apple snake is too slow on HP71B )
	\ RENDER
	( preferably on HP71B slow HW, only the first and last char of the snake body will be updated )
	DUP MS   ( temporisation. reduction could accelerate )
( )
( KEY? will be changed to ?TERMINAL with awk script )
    KEY? IF KEY
( )
DUP 97  = IF ['] LEFT ELSE   ( a letter recognized )
DUP 65  = IF ['] LEFT ELSE   ( A )
DUP 119 = IF ['] UP ELSE     ( w )
DUP 87  = IF ['] UP ELSE     ( W )
DUP 100 = IF ['] RIGHT ELSE  ( d )
DUP 68  = IF ['] RIGHT ELSE  ( D )	
DUP 115 = IF ['] DOWN ELSE   ( s )
DUP 83 = IF ['] DOWN ELSE    ( S ) 
	DIRECTION @
   THEN THEN THEN THEN THEN THEN THEN THEN
DIRECTION !       ( new defined direction set )
DROP THEN
DIRECTION PERFORM ( direction activated: old or new defined )
STEP!             ( move head into the defined direction and the tail representation will be shortened ) 
APPLE?            ( apple detected at next step? )
IF EAT-APPLE!     ( eat apple = longer snake from the head )
    UPDATE-APPLE  ( old position become a # and new position will be defined )
    UPDATE-LENGTH ( increase the snake length number visible on the screen )
    ELSE UPDATE-SNAKE ( head side updated, tail side shortened )
THEN DEAD? UNTIL DROP ." *** GAME OVER ***" CR ;
( )
( start snake )
: SSNAKE
    DELAY00                ( speed up later in HP71B )
    0 PRIMARY !            ( make possible the screen output )
    RES2CHR OUTPUT         ( clear the screen on HP71B and GFORTH )
    CR CR ." Snake in Forth" CR CR
    3000 MS                ( pause 3s before it starts )
    NEWGAME!               ( reset the data for a new game )
( )
    200 GAMELOOP           ( will refresh all 0.2s in gforth )
    ( put above 0 in HP71B since the hardware is slow        )
    ( )    
    ( reduce the value before GAMELOOP if too slow on PC/gforth )
;
