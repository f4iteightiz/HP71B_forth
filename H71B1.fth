( ************************************************************ )
( new words for use on HP71B Forth                             )
( ideally words in gforth - 2012 version - for use on HP71B    )
( if the words exists in HP71B, overtake them instead of       )
( creating new words                                           )
(                                                              )
( File name H71B1.fth                                          )
(                                                              )
( File naming is H71Bx.fth if several used                     )
(                                                              )
( FOR USE ON HP71B and PC     )
( new secondary words         )
( Assembler in separate files )
(                                                              )
( created on a PC with gforth                                  )
( these words helps the implementation of programs written in  )
( gforth for making them working straight on HP71B w/ Forth    )
(                                                              )
( license CC BY NC SA CreativeCommons 4.0                      )
( https://creativecommons.org/licenses/by-nc-sa/4.0/deed.de    )
( pascaldagornet at yahoo dot de                               )
(            and                                               )
( floppy @ https://www.hpmuseum.org/forum/                     )
(                                                              )
( change log                                                   )
(   2023 07 14 creation                                        )
(   2023 11 01 upload internet                                 )
(   2023 12 12 STRDUMP CELLV RES2CHR added                     )
(                                                              )
( status of the words commented in this file                   )
(  1. word to be worked out                                    )
(  2. test it i.e. on pc with gforth, on EMU71, on HP71B       )
(  3. next i.e test it on EMU1 or HP71B                        )
(                                                              )
( important source of thoughts and solutions                   )
( https://www.reddit.com/r/Forth/                              )
( stackoverflow.com                                            )
( linuxquestions.org                                           )
( github example  https://github.com/bfox9900/CAMEL99-ITC/     )
(  blob/master/LIB.ITC/DOUBLE.FTH                              )
(                                                              )
( REMARKS                                                      )
(                                                              )
( situation: in FORTH71 means in ASM or in translator module   )  
(                                                              ) 
( word  FORTH71  GFORTH  Remark                                )
(                                                              )
( 1     YES      YES     dont do anything if same behaviour    )
(                        if different, create new word for     )
(                           gforth use on PC                   )
(                                                              )
( 2     NO       YES     new word HP_ in case HP HW specific   )
(                      which cannot be written for gforth, too )
(                                                              )
( 3     NO       NO      D_WORDX in HP71B2G.fth for debug use  )
(                        or here for new                       )
(                                                              )
( 4     YES      NO      put word in 71B2G.fth only            )
(                                                              )
(                                                              )
( Move here in the file all PINB 2EMIT RANDOMIZE etc. generic  )
( words for re-use                                             ) 
( )
( )
( So far no need identified for diverse dictionaries           )
( VOCABULARY HP71B1                                            )
( HP71B1 ALSO DEFINITIONS   \ ALSO will be filtered out by awk )
( )
( )
( CELLV ------------------------------------------------------ )
( Identify if the system is EMU71 / HP71B or a PC              )
( CELLV will be 5 on HP71B and 8 on PC 64 bits                 )
( eventually create later a CHARV , too                        )
VARIABLE CELLV
HERE 0 , HERE SWAP - CELLV !
( ------------------------------------------------------------ )
( )
( )
( RES2CHR ---------------------------------------------------- )
( tested 29 Nov 2023                                           )
( create a reset string of 2 chars for use with OUTPUT & Video )
( use..                                                        )
( RES2CHR OUTPUT  instead of PAGE on the HP71B if it is set    )
(   to DISPLAY IS * for use of OUTPUT                          )
( test on HP71B                                                )
( RES2CHR OUTPUT >>>  OK { 0 } screen is clean                 )
( work on gforth , too                                         )
( )
2 STRING RES2CHR 
RES2CHR 27 CHR$ S<& 69 CHR$ S<& 2DROP
( ------------------------------------------------------------ )
( )
( )
( TRUE ------------------------------------------------------- )
-1 CONSTANT TRUE
( ------------------------------------------------------------ )
( )
( )
( FALSE ------------------------------------------------------ )
0 CONSTANT FALSE  
( ------------------------------------------------------------ )
( )
( )
( FPI -------------------------------------------------------- )
( float PI if needed later. Its called PI in GFORTH            )
( 3.1415926535897932385E0 FCONSTANT FPI                        )
( ------------------------------------------------------------ )
( )
( )
( >= --------------------------------------------------------- )
( gforth-0.2 “greater-or-equal” n1 > n2 ?                      )
( n1 n2 – f )
: >= < 0= ;
( ------------------------------------------------------------ )
( )
( )
(                              test this                       )
( 0<> -------------------------------------------------------- )
( n -- flag )
: 0<> 0= 0= ;
( ------------------------------------------------------------ )
( )
( )
(                               test this                      )
( .R --------------------------------------------------------- )
( gforth core-ext “dot-r”                                      )
( in gforth, see .r show  : .r  >r s>d r> d.r ;                )
( Display n1 right-aligned in a field n2 characters wide.      )
( If more than n2 characters are needed to display the number, )
(   all digits are displayed.                                  )
( If appropriate, n2 must include a char for a leading “-”     )
( n1 n2 –  )
: .R  >R S->D R> D.R ;
( ------------------------------------------------------------ )
( )
( )
( D= --------------------------------------------------------- )
( tested on EMU71                                              )
( double “d-equals”                                            )
( reddit proposal                                              )
( d1 d2 – f )
: D0= OR 0= ;
: D= D- D0= ;
( ------------------------------------------------------------ )
( )
( )
( 2, --------------------------------------------------------- )
( tested on EMU71                                              )
( gforth-0.2 “2,”                                              )
( Reserve data space for two cells and store the double w1 w2  )
( there, w2 first in lower address                             )
( in gforth if we do see 2,                                    ) 
( : 2,  here 2 cells allot 2! ; in gforth                      )
( w1 w2 –  )
( = d --   )
: 2,  , ,  ;   
( ------------------------------------------------------------ )
( )
( )
( 2! --------------------------------------------------------- )
( tested on EMU71                                              )
( 5+ can be replaced by CELL+                                  )
( or..  HP_2!  SWAP OVER ! 5+ !                                )
( w1 w2 addr -- )
: 2!  SWAP OVER ! 5+ ! ;
( ------------------------------------------------------------ )
( )
( )
( 2@ --------------------------------------------------------- )
( tested with 2CONSTANT on emulator                            )
( a-addr – w1 w2 ) 
( a-addr -- lo hi )
(   w2 is the content of the cell stored at a-addr,            )
(   w1 is the content of the next cell.                        )
( or .. 2@  DUP 8+ @ SWAP @     on PC with gforth              )
( on the HP71B it will be .. 5+ can be replaced by CELL+       )
( or .. HP_2@  DUP 5+ @ SWAP @                                 )
: 2@  DUP 5+ @ SWAP @ ; 
( ------------------------------------------------------------ )
( )
( )
( 2CONSTANT -------------------------------------------------- )
( tested with 2@ on emu71B                                     )
( see Forth Application Techniques page 115                    )
( d <name> -- ; Create a 2constant with the name <name>        )
: 2CONSTANT  CREATE , , DOES> 2@ ;
( or   CREATE 2, DOES> 2@ ;                                    )
( ------------------------------------------------------------ )
( )
( )
(                   test this if needed another time           )
( 2VARIABLE -------------------------------------------------- )
( <name> -- ; Create a double variable in the dictionary       )
( does> -- a )
: 2VARIABLE  CREATE 0 , 0 , DOES> ;
( ------------------------------------------------------------ )
( )
( )
( 2NIP ------------------------------------------------------- )
( aa bb a b -- a b )
: 2NIP 2SWAP 2DROP ;
( ------------------------------------------------------------ )
( )
( )
( AT-XY ------------------------------------------------------ )
( tested standalone on emulator                                )
( Put the curser at position x y                               )
( The top left-hand corner of the display is at 0 0.           )
( on gforth see at-xy                                          )
( rewritten now for non ANSI, HP92198 HP-IL 80columns          )
( x y –  )
: HP_AT-XY 27 EMIT 37 EMIT SWAP EMIT EMIT ;
( addr1 n1 x y -- addr1 n5 )
( str1 x y -- str1+ )
: AT-XY<BF
    2SWAP   ( x y addr1 n1 ) 
    27 CHR$ S<&  ( x y addr1 n2 ) 
    37 CHR$ S<&  ( x y addr1 n3 ) 
    4 ROLL CHR$ S<&  ( y addr1 n4 ) 
    ROT ( addr1 n4 y ) 
    CHR$ S<& ( addr1 n5 ) 
;
( ------------------------------------------------------------ )
( )
( )
(                         test this                            )
( BLANK ------------------------------------------------------ )
( Store the space character into u chars starting at c-addr.   )
( gforth blank, string “blank”                                 )
( c-addr u –  ) 
: BLANK  20 FILL  ;
( ------------------------------------------------------------ )
( )
( )
( CELLS ------------------------------------------------------ )
( tested on emulator                                           )
( n1 – n2 )
( n2 is the number of address units of n1 cells.               )
( on gforth PC       1 cells . 8  ok                           )
(                    mean    : CELLS 8 * ;   on gforth PC      )
( on HP71B emulator HERE 0 , HERE SWAP - .   5 ok              )
( : HP_CELLS 5 * ;   previous version                          )
: CELLS CELLV @ * ;                                 
( ------------------------------------------------------------ )
( )
( )
( CELL+ ------------------------------------------------------ )
( test this on HP71B                                           ) 
( on PC w/ gforth    HERE 0 , HERE SWAP - .  8    ok           )
(                    1 cells . 8  ok                           )
(                    which mean on gforth PC  : CELL+  8 + ;   )
( on HP71B it was    HERE 0 , HERE SWAP - .  5    ok           )
(                    therefore CELL+ is 5+                     )
: CELL+ 5+ ;
: CELL- 5- ;
( ------------------------------------------------------------ )
( )
( )
( CHAR+ ------------------------------------------------------ )
( tested on EMU71                                              )
( same in gforth                                               )
( 1 chars +                                                    )
( 1 chars . 1 ok  on gforth PC = char+ is 1 + on PC            )
( HERE 0 c, HERE SWAP - . 1  ok  seen on PC                    )
( https://forth-standard.org/standard/core/CHARPlus            )
( HERE 0 c, HERE SWAP - .  2 ok on HP71B                       )
( : HP_CHAR+  2+  ;    previous version                        )
( : HP_CHAR-  2-  ;    previous version                        )
: CHAR+  CELLV @ 5 = IF 2+ ELSE 1+ THEN ;                 
: CHAR-  CELLV @ 5 = IF 2- ELSE 1- THEN ;                 
( ------------------------------------------------------------ )
( )
( )
( CHAR ------------------------------------------------------- )
( gforth ’<spaces>ccc’ – c                                     )
( Skip leading spaces. Parse the string ccc and return c,      )
(   the display code representing the first character of ccc.  )
( char J  put 74 on the top of the stack                       )
( https://github.com/bfox9900/CAMEL99-ITC/..                   )
(       .. blob/master/LIB.ITC/CHAR.FTH                        )
: CHAR  BL WORD CHAR+ C@ ;
( ------------------------------------------------------------ )
( )
(                         tested on EMU71B                     )
( CHARS ------------------------------------------------------ )
( like in gforth                                               )
( n2 is the number of address units of n1 chars.               )
( HERE 0 c, HERE SWAP - .  1  ok on PC                         )
( n1 is 1, means n2 is 1 on PC                                 )
( HERE 0 c, HERE SWAP - .  2 ok on HP71B                       )
( n1 n2 -- n3 )
: HP_CHARS 2* ; 
( ------------------------------------------------------------ )
( )
( )
( NIP -------------------------------------------------------- )
( tested on EMU71                                              )
( w1 w2 – w2 )
: NIP SWAP DROP ;
( test EMU71                                                   )
(   1 2 NIP                                                    )
(   . 2 OK { 0 }                                               )
( ------------------------------------------------------------ )
( )
( )
( N>$ -------------------------------------------------------- )
( )
( convert number in string                                     )
( n -- addr len )
: N>$  S->D STR$ ;
( )
( EMU71B test )
( [ ]  OK { 0 }                )
( 1479 N>$                     )
( OK { 2 }                     )
( S.                           ) 
( [ 210519 4 ]  OK { 2 }       )
( TYPE                         )
( 1479 OK { 0 }                )
( )
( gforth test                  )
( S. [ ]  ok                   )
( 1479 N>$   ok                )
( S. [ 139646129548854 4 ]  ok )
( TYPE 1479 ok                 )
( )
( ------------------------------------------------------------ )
( )
( )
(                                test this                     )
( ON --------------------------------------------------------- )
: ON  TRUE SWAP ! ;
: OFF FALSE SWAP ! ;
( ------------------------------------------------------------ )
( )
( )
( PAGE ------------------------------------------------------- )
( tested on EMU71                                              )
( Clear the screen                                             )
( same on gforth see page                                      )
( rewritten for HP92198 HP-IL 80columns together               )
: HP_PAGE  27 EMIT 69 EMIT
    \ RES2CHR 2 OUTPUT  perhaps add this here instead of
    \ into the main word
;
( ------------------------------------------------------------ )
( )
( )
( test this on HP71B if definition ok despite delta to gforth  )
( WITHIN ----------------------------------------------------- )
( for unsigned only numbers                                    )
( http://lars.nocrew.org/forth2012/core/WITHIN.html            )
( n low high -- flag )
: WITHIN OVER - >R - R> U< ;
( .. use gforth                                                )
( 5 5 5 within  ok                                             )
( . 0  ok                                                      )
( 5 5 6 within  ok                                             )
( . -1  ok                                                     )
( 6 5 6 within  ok                                             )
( . 0  ok                                                      )
( 12 7 6 within  ok                                            )
( . -1  ok                                                     )
( .. use HP71B                                                 )
( all like above except                                        )
( 12 7 6 within  ..  0 if the "delta" used                     )
( ------------------------------------------------------------ )
( )
( )
( RND -------------------------------------------------------- )
( )
( I:n R:Nb -- R:Nb-troncatenated to n decimal                  )
: RND
    DUP
    ITOF
    10^X
    G_F*
    IP
    ITOF
    10^X
    G_F/
    ;
( ------------------------------------------------------------ )
( )
( VALUE ------------------------------------------------------ )
( tested on emu71B                                             )
( rewrite VALUE of gforth for use on HP71B                     )
( Define name with the initial value w; this value can be      )
(  changed with to name                                        )
( use...   123 VALUE PARAM                                     )
: VALUE CREATE , DOES> @ ; 
( ------------------------------------------------------------ )
( )
( )
( )
( )
( COMPARE ---------------------------------------------------- )
( case-sensitive comparison                                    )
: COMPARE S= 0<>  ;
( ------------------------------------------------------------ )
( )
( )
( ------------ compile & loop words from here ---------------- )
( ------------ compile & loop words from here ---------------- )
( ------------ compile & loop words from here ---------------- )
( )
( )
(                         test this                            )
( POSTPONE --------------------------------------------------- )
\ put [UNDEFINED] etc. before
\ S" POSTPONE.FORTH83.fth" INCLUDED
(                                                              )
(                                                              )
( )
(                         test this                            )
( AGAIN ------------------------------------------------------ )
( https://www.reddit.com/r/Forth/comments/166khk2/..           )
(        .. howto_create_the_word_again_in_my_forth83_version/ )
( C: dest -- )
\ : AGAIN  POSTPONE FALSE POSTPONE UNTIL ; IMMEDIATE
( ------------------------------------------------------------ )
( )
( )
(                       work out if needed                     )
( [CHAR]------------------------------------------------------ )
\ [char] ( compilation ’<spaces>ccc’ – ; run-time – c  )
\     core,xchar-ext “bracket-char”
\ Compilation: skip leading spaces. Parse the string ccc.
\ Run-time: return c, the display code representing the
\ first character of ccc.
\   Interpretation semantics for this word are undefined.
\ You usually use char outside and [char] inside colon
\   definitions, or you just use 'C'.
\ https://gforth.org/
\ from see ?? .. [CHAR] CHAR  POSTPONE LITERAL 
\ ..
\ https://github.com/bfox9900/CAMEL99-ITC/blob/master/LIB.ITC/CHAR.FTH
\ https://forth-standard.org/standard/xchar/BracketCHAR
\ : [CHAR]  ( -- <c>)
\    ?COMP CHAR POSTPONE LITERAL ; IMMEDIATE
\ : [char] ( "name" -- c| )
\  char  state @ if [compile] literal [then] ; immediate  \ show a compiling issue with ;
( ------------------------------------------------------------ )
( )
( )
( CLEARSTR --------------------------------------------------- )
( clear a string = put it counted number to zero               )
( addr-c n -- addr-c 0 )
: CLEARSTR
    DROP DUP 1 CHARS - 0 SWAP C! 0 ;
( ------------------------------------------------------------ )
( )
( STRING comparison case sensitive --------------------------- )
( c2 c1 -- flag ) \ ASCII-case-insensitive
\ : CHARI= 
\  OVER XOR DUP 0= IF 2DROP TRUE EXIT THEN ( c1 x )
\  DUP 32 ( %100000 ) <> IF 2DROP FALSE EXIT THEN
\  OR  [CHAR] a  [ CHAR z 1+ ] LITERAL  WITHIN ;
( )
( c2 c1 -- flag ) \ ASCII-case-insensitive
\ : CHARI<>
\    CHARI= 0= ;
( )
( sd.txt2 sd.txt1 -- flag ) \ ASCII-case-insensitive
\ : EQUALSI
\  ROT OVER <> IF ( a2 a1 u1 ) DROP 2DROP FALSE EXIT THEN
\  OVER + ( a2 a1 a1z ) >R
\  BEGIN DUP R@ U< WHILE ( a2 a1 )
\    >R COUNT R> COUNT ROT ( a22 a12 c1 c2 )
\    CHARI<> IF 2DROP RDROP FALSE EXIT THEN
\  REPEAT 2DROP RDROP TRUE ;
( ------------------------------------------------------------ )
( )
( )
(                   work this out if necessary...              )
( ?DO -------------------------------------------------------- )
( according https://www.reddit.com/r/Forth/comments/16ruez4/.. )
( comment/k2w1oh3/?context=3                                   )
( ?do ... loop   equivalent to                                 )
( 2dup = if 2drop else  do ... loop  then                      )
( )
( C: -- orig do-sys ) ( -- xt.then )
\ : ?DO
\    COMPILE 2DUP COMPILE = [COMPILE] IF COMPILE 2DROP
   \    [COMPILE] ELSE [COMPILE] DO ['] THEN ; IMMEDIATE   \ worked on gforth
( )
( )
\ : LOOP ( do-sys | orig do-sys xt.then -- )
\   DUP ['] THEN <> IF  [COMPILE] LOOP EXIT THEN
\  >R [COMPILE] LOOP R> EXECUTE ; IMMEDIATE
( )
\ : +LOOP ( do-sys | orig do-sys xt.then -- )
\  DUP ['] THEN <> IF  [COMPILE] +LOOP EXIT THEN
\  >R [COMPILE] +LOOP R> EXECUTE ; IMMEDIATE
( ------------------------------------------------------------ )
( )
( )
( MS --------------------------------------------------------- )
( tested on EMU71                                              )
( from gforth chapter 6.31 Keeping track of Time               )
( n  “ms”                                                      )
( according HPforum minimum cycle is 0.001s = 10ms             )
( lets simulate the minimum of 10ms with loops                 )
: HP_MS 0 DO LOOP ;
( ------------------------------------------------------------ )
( )
( )
( KEY? ------------------------------------------------------- )
(                                                              )
( KEY? is gforth and equivalent to HP71B ?TERMINAL             )
( If ?TERMINAL is used in a new file, keep it                  )
( IF KEY? is used in a new file, change it to ?TERMINAL        )
(  by using the filtering with awk                             )
( ------------------------------------------------------------ )
( )
( )
( PERFORM ---------------------------------------------------- )
( https://www.forth.com/starting-forth/9-forth-execution/      )
( .. use?   ' GREET EXECUTE or                                 )
(           ' GREET pointer ! pointer @ EXECUTE                )
( https://www.complang.tuwien.ac.at/forth/gforth/..            )
(         ..Docs-html/Execution-token.html                     )
( a-addr – )
: PERFORM  @ EXECUTE  ;
( ------------------------------------------------------------ )
( )
( )
( - ROT ------------------------------------------------------ )
( tested on HP71B emulator                                     )
( n1 n2 n3 -- n3 n1 n2 )
: -ROT ROT ROT ;
( ------------------------------------------------------------ )
( )
( )
( PMOD ------------------------------------------------------- )
( like MOD of gforth, which has only positive result           )
: PMOD  DUP -ROT MOD DUP 0< IF + ELSE NIP THEN  ;
( ------------------------------------------------------------ )
( )
( )
(                        test this if necessary                )
( REFILL ----------------------------------------------------- )
\ ( – flag  ) core-ext,block-ext,file-ext “refill”
\ Attempt to fill the input buffer from the input source.
\ When the input source is the user input device, attempt to receive input into the terminal input device.
\ If successful, make the result the input buffer, set >IN to 0 and return true; otherwise return false.
\ When the input source is a block, add 1 to the value of BLK to make the next block the input source and current input buffer,
\ and set >IN to 0; return true if the new value of BLK is a valid block number, false otherwise.
\ When the input source is a text file, attempt to read the next line from the file.
\ If successful, make the result the current input buffer, set >IN to 0 and return true; otherwise, return false.
\ A successful result includes receipt of a line containing 0 characters.
\ holds len and address of input buffer
\   CREATE 'SOURCE  0 ,  0 ,
\
\   : REFILL ( -- ?) \ 0 means no input
    \ old
    \   TIB DUP 80 ACCEPT ( -- addr len)
    \ new
\    TIB DUP EXPECT96 ( -- addr )
\   DUP IF  
\        'SOURCE 2!
\        >IN OFF
\        TRUE
\   ELSE
\        NIP
\   THEN
\ ;
\ --------------------------------------------------------------
\
\
( STRDUMP ---------------------------------------------------- )
( )
( dump a string; output n char per line                        )
( will help to see if a buffer string constructed              )
( has the correct structure for being used later with          )
( the word OUTPUT                                              )
( use mostly only in gforth                                    )
( str n -- )
( addr1 n1  n -- )
: STRDUMP OVER 0 DO ( addr1 n1  n -- )
	DUP I SWAP MOD 0= IF CR ELSE THEN -ROT C@+ .
	ROT S"  " TYPE LOOP 2DROP DROP ; 
( )
( use.. )
( EMU71 )
( S4 10 STRDUMP .. S4 is a STRING )
( )
( 27  37  1  2  124  27  37  1  3  124    )
( 27  37  72  2  124  27  37  72  3  124  )
( 27  37  1  1  43  45  45  45  45  45   OK { x } )
( )
( gforth tested 23 Dec 12 )
( LENGTHBUF  ok                                                                  )
( s. [ 140446213901210 0 ]  ok                                                   )
( 0 22 AT-XY<BF S" SNAKE LENGTH: " S<& LENGTH @ N>$ S<&  ok                      )
( LENGTHBUF 20 strdump                                                           )
( 27  37  0  22  83  78  65  75  69  32  76  69  78  71  84  72  58  32  48   ok )
( lengthbuf maxlen s. [ 140446213901210 19 21 ]  ok                              )
( )
( ------------------------------------------------------------ )
( )
( )
\ test this if any need later
\ --------------------------------------------------------------
\ : TO  ( n -- )
\     '  >BODY STATE @ IF POSTPONE LITERAL  POSTPONE !  EXIT 
\     THEN  ! ; IMMEDIATE
\ --------------------------------------------------------------
\
\
\                   test this if any need later
\ +TO ----------------------------------------------------------
\ ( n -- )
\ : +TO ' >BODY STATE @ IF POSTPONE LITERAL POSTPONE +! EXIT THEN 
\ +! ; IMMEDIATE
\ --------------------------------------------------------------
\
\
\                                     test this
\ UNLOOP -------------------------------------------------------
\ unloop ( R:w1 R:w2 – ) core “unloop”
\ or this?   UNLOOP  2DUP >R 0 DO I R> LOOP DROP 
\ : UNLOOP   R> DROP  R> DROP ;
\ from https://www.reddit.com/r/Forth/comments/16ruez4/howto_create_the_word_do_with_forth83_words/
\ [UNDEFINED] RDROP [IF] : RDROP POSTPONE R> POSTPONE DROP ; IMMEDIATE [THEN]
\ : UNLOOP POSTPONE RDROP POSTPONE RDROP ; IMMEDIATE
\ --------------------------------------------------------------
\
\
(          test this if any need                               )
(          FTHUTILF has LIST which is enough                   )
( VLIST ------------------------------------------------------ )
( list the words on HP71B                                      )
( page 25 of the Forth/Assembler manual show                   )
(  E0000 address Forth ROM                                     )
(                                                              )
( HEX                                                          )
( : HP_VLIST E0005 D 1 DO DUP @ BEGIN DUP COUNT 1F AND 1- DUP  )
( >R 2* SWAP DUP >R + C@ 7F AND R> R> TYPE EMIT CR 5- @ ?DUPn  )
( 0= UNTIL 5+ LOOP DROP ;                                      )
( DECIMAL                                                      )
( ------------------------------------------------------------ )
DECIMAL
( )