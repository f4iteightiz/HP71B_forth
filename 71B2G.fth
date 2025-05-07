\ **************************************************************
\ *                                                            *
\ *                  file  71B2G.fth                           *
\ *                                                            *
\ **************************************************************
\
\ Layer file for debugging HP71B FORTH files under GFORTH
\
\      >>>>        FOR USE ON A PC ONLY         <<<<<
\      >>>>      for debugging on PC only       <<<<<
\      >>>>     for cross-developpment only     <<<<<
\      >>>>       NO NEED IN HP71B FORTH        <<<<<
\
\ The cross-development is on the PC therefore all words here
\   in this file
\   should NOT be transfered to HP71B but used in GFORTH
\ This file was created in GFORTH on emacs
\   it means some words works only on gforth on a PC
\ You can upload the file with others in a terminal with
\   (example)
\   emacs 71B2G.fth H71B1.fth PERE12.fth --eval "(view-files-in-windows)"
\
\ GFORTH is used for this because it has a compatibility with
\   emacs; see https://gforth.org/
\ Another Forth could be used: we recommend a Forth with an IDE
\   since it is the target to cross-developp HP71B Forth words
\   on a PC
\
(                                                               )
( license CC BY NC SA CreativeCommons 4.0                       )
( https://creativecommons.org/licenses/by-nc-sa/4.0/deed.de     )
( pascaldagornet at yahoo dot de                                )
(                                                               )
\
\ Before the files HP71B (not this one) are transfered to HP71B,
\  a) take the "e" out of the float point
\       or filter with awk -f hp71bccfilter XFORTH.fth 
\  b) take all lines from "\" on, off the file (with above awk script command)
\  c) replace all words G_XXXX by XXXX (with above awk script command).
\       these words were done in order to have a slight modified version of
\       XXXX during debugging because the words exists in HP71B forth and gforth
\       however sometime with different behaviour and its
\       better to have G_XXXX in order to distinguish gforth or adapted
\       words despite they could be overwritten by the forth system
\  d) take all the words D_YYYY out with the awk script
\     awk -f hp71bccfilter XFORTH.fth > NEWFILE.SRC
\  e) clean up the newfile manually (wide 64 character per line)
\      then transfer to HP71B
\
\ Use in a terminal:
\   .. editing:
\   emacs 71B2G.fth H71B1.fth PERE12.fth --eval "(view-files-in-windows)"
\   .. starting:
\   gforth 71B2G.fth H71B1.fth PERE12.fth" in a terminal
\  
\   .. in the gforth prompt (after starting with the command gforth in a terminal);
\   "see word" will show the word content in a gforth prompt
\   "A ?" will show the value of parameter A in a terminal
\   "~~" in a gforth prompt will show the code line and the stack (?)
\   .S will show the content of the data stack
\   F.S will show the content of the float stack
\   . will show the number at the top of the data stack and remove it from the data stack
\   >>>> it means, you can write in gforth all words and test them interactively
\
\ Tested under Debian 64bits Bullseye 
\
\ change log 
\   2022 06 01 creation / release
\   2022 07 28 case introduced
\   2024 01 03 comments updates, END$ done..
\   2024 06 17 several cleaning ups
\   202x xx xx --------------------------------
\   202x xx xx --------------------------------
\
\ Important information for future developments
\ 1. Angles in floating point operations in gforth are given in radians (a full circle has 2 pi radians).
\ 2. Forth83 is the base of the HP71B Forth https://forth.sourceforge.net/standard/fst83/fst83-0.htm
\    Word list https://forth.sourceforge.net/standard/fst83/fst83-12.htm
\
ONLY FORTH ALSO DEFINITIONS
\
VOCABULARY 71B2G 71B2G ALSO DEFINITIONS
\
DECIMAL
\
\ **************************************************************
\
\         GFORTH area for making HP71B commands compatible
\         This area will not be used later when the files
\         are transfered to HP71B
\
\         FORTH/ASSEMBLER Module words
\         FORTH/TRANSLATOR Module words are highlighted
\
\ **************************************************************
\
\ RPN stack like defined in the float HP71B mode
\ during initializing in gforth on PC, the default value is set to 0.0e = zero
\
\ User variables from Forth71B. All (if description done) are listed in the words below.

\ VARIABLE S0      \ same as @ SP0  
\ VARIABLE SP      \ Pointer to bottom of data stack, address 2FB11 on HP71B, exists in gforth
\ VARIABLE RP0     \ Pointer to bottom of return stack, address 2FB16 on HP71B, exists in gforth
\ VARIABLE USE     \ Next buffer.
\ VARIABLE PREV    \ Most recent mass storage buffer.
\ VARIABLE FIRST   \ First mass storage buffer.
\ VARIABLE LIMIT   \ End of FORTH RAM + 1
\ VARIABLE #TIB    \ Number of characters in TIB.
\
\ VARIABLE WARN    \ Warning mode.
\ VARIABLE OKFLG   \ Enable/disable OK in 0 U IT.
\ VARIABLE BLK     \ Line number in current LOA D F file. (Reset when load error occurs.)
\ VARIABLE >IN     \ Offset in TIB.
\ VARIABLE SPAN    \ contains the count of characters actually read by the last execution of EXPECT96
\ VARIABLE SCRFIB  \ FIB# of active LOA D F file.
\ VARIABLE CONTEXT \ Address of CONTEXT vocabulary. in gforth
\ VARIABLE CURRENT \ Address of CURRENT vocabulary. in gforth
\ VARIABLE STATE   \ Compilation flag.
\ VARIABLE BASE   Current base. in gforth
\ VARIABLE FENCE   \ F 0 R G E T boundary.
\ VARIABLE LINE#   \ Line number in current L 0 8 D F file. (Preserved after load error.)
\ VARIABLE SECONDARY \ Secondary HP-IL address.
\ VARIABLE PRIMARY   \ Primary HP-IL address.
\ VARIABLE ONERR     \ On-error execution address.

\ float point stack register listed here due to early use in this file
FVARIABLE L   \ L register, address 2FBC0 on hp71b
FVARIABLE X   \ X register, address 2FBD0 on hp71b
FVARIABLE Y   \ Y register, address 2FBE0 on hp71b
FVARIABLE Z   \ Z register, address 2FBF0 on hp71b
FVARIABLE T   \ T register, address 2FC00 on hp71b

\ ( PAUSELEN: Variable to hold pause length )
VARIABLE PAUSELEN 0 PAUSELEN !
\
\
\ user variables only for cross-development gforth with no need
\ later in an HP71B when the programm is tested/working
FVARIABLE TB           \ Temp BUFFER
FVARIABLE TBX          \ Temp BUFFER for stack param management
FVARIABLE TBY          \ Temp BUFFER for stack param management
FVARIABLE TBZ          \ Temp BUFFER for stack param management
FVARIABLE TBT          \ Temp BUFFER for stack param management
FVARIABLE TBL          \ Temp BUFFER for stack param management

VARIABLE DEPST         \ Temp BUFFER for stack param management

0   VALUE DEG          \ DEGREES mode
-1  VALUE RAD          \ RADIANS mode The default

0 VALUE EDM            \ ENG display mode deactivated
8 VALUE SDM            \ SCI display mode activated
8 SET-PRECISION
0 VALUE FDM            \ Fix display mode deactivated

\ not needed. see the word CELLV in H71B1
\ 8 result for 64bits PC  or 5 for HP71B
\ HERE 0 , HERE SWAP - VALUE CELDEV 
\ HERE 0 C, HERE SWAP - VALUE CHARDEV  \  think about this for 1 or 2
\
\ work something similar if needed
\ https://gforth.org/manual/Environmental-Queries.html
\ ." OS-CLASS" OS-CLASS TYPE
\ ." OS-TYPE " OS-TYPE  TYPE
\
\
\ PAD ----------------------------------------------------------
\ tested 27 Nov 2023                                           
\ temporary buffer from HP71B glossary below
CREATE PAD 80 C, 0 C, 80 ALLOT
\ --------------------------------------------------------------
\
\
\ ********************   FIRST PART    *************************
\
\ words to be used in main word file for debugging purpose only 
\ all starts with D_xxx in order to be deleted later by the awk script
\
\ **************************************************************
: D_WARN ; ( CR ." A line which contain D_ will be deleted with the awk filtering" CR )
\ --------------------------------------------------------------
\ clear the gforth float stack 
: FCLEARSTACK  ( r1 r2 .. rn -- )    
     FDEPTH 0 DO FDROP LOOP ;
\ however no use on HP71B because a fixed fstack of X Y Z T
\ which cannot be deleted like in gforth
\ set X etc. to zero?
\ --------------------------------------------------------------
\ save/move the r1..r4 of the GFORTH stack intro X Y Z T of the RPN
\ ( rx r1 r2 r3 r4 -- rx )
: D_STORPNTB   X F! Y F! Z F! T F!  ;
\ --------------------------------------------------------------
\ upload the gforth float stack r1..r4 from the RPN stack depth 4
\ ( rx -- rx r1 r2 r3 r4 ) 
: D_RCLRPNTB  T F@ Z F@ Y F@ X F@ ;
\ --------------------------------------------------------------
\ any use of this? if not, delete later
\ save/move the RPN into (temporary) buffer
: D_STORPNB  ( rx r1 r2 r3 r4 -- rx )  
    \ ." STORPNTB in  " D_RPNS
    X F@ TBX F!
    Y F@ TBY F! Z F@ TBZ F! T F@ TBT F! L F@ TBL F! ;
\ --------------------------------------------------------------
\ setup the gforth stack to maximum depth 4 like the RPN stack
\ fill the gforth stack with zero is depth <4
\ shorten the gforth stack to 4 if bigger than 4
( r1 r2 r3 r4 .. rn  --  rn-3 rn-2 rn-1 rn )
: D_SSET 
    FDEPTH  ( -- +n )
    CASE
	0 OF  0.0e FDUP FDUP FDUP  ENDOF
	1 OF  0.0e FSWAP 0.0e FSWAP 0.0e FSWAP  ENDOF
	2 OF  TBT F! TBZ F! 0.0e FDUP TBZ F@ TBT F@  ENDOF
	3 OF  TB F! 0.0e FROT FROT TB F@  ENDOF
	4 OF  ENDOF
	D_STORPNTB FCLEARSTACK D_RCLRPNTB
    ENDCASE
    D_STORPNTB D_RCLRPNTB
    4 DEPST !  ;
\ ------------------------------------------------------------------------------
\ check if stack was initialized = variable DEPST = 4. if not, do it
\ ( -- )
: D_?SINIT
    DEPST @ 4 =
    IF   \ 4 if D_SSET was done else 0
	FDEPTH 4 >   \ >4 if float placed before into the stack
	IF D_SSET ELSE THEN
    ELSE D_SSET THEN  ;
\ --------------------------------------------------------------
\  print out the Stacks for debugging purpose
\ ( -- )    
: D_RPNS  CR CR
    ." HP71B float stack content, RPN HP41 (& gforth stack):" CR
    ."    X     " X F@ 9 5 1 F.RDP ."      Top gforth    " FDEPTH DUP DEPST ! 0<> IF FDUP TBX F! 9 5 1 F.RDP ELSE THEN CR
    ."    Y     " Y F@ 9 5 1 F.RDP ."                    " FDEPTH 0<> IF FDUP TBY F! 9 5 1 F.RDP ELSE THEN CR
    ."    Z     " Z F@ 9 5 1 F.RDP ."                    " FDEPTH 0<> IF FDUP TBZ F! 9 5 1 F.RDP ELSE THEN CR
    ."    T     " T F@ 9 5 1 F.RDP ."      Bottom gforth " FDEPTH 0<> IF FDUP TBT F! 9 5 1 F.RDP ELSE THEN CR
    ."    LASTX " L F@ 9 5 1 F.RDP CR CR
    \
    DEPST @  \ is 4 if D_SSET done else ..
    CASE
	1 OF  TBX F@  ENDOF
	2 OF  TBY F@ TBX F@  ENDOF
	3 OF  TBZ F@ TBY F@ TBX F@  ENDOF
	4 OF  TBT F@ TBZ F@ TBY F@ TBX F@  ENDOF
	FCLEARSTACK  TBT F@ TBZ F@ TBY F@ TBX F@ 
    ENDCASE
    \
    ." float   stack gforth  " CR
    ."   <stack depth> bottom to top, of stack" CR
    ."   " F.S CR CR
    ." integer stack gforth  " CR
    ."   <stack depth> bottom to top, of stack" CR
    ."   " .S CR CR
    DEG IF ." DEGREE ON" ELSE ." RADIAN ON" THEN CR
    BASE @ ." BASE: " . CR CR  ;
\ --------------------------------------------------------------
\  show content of a variable at execution for debugging purpose
\ ( f-addr -- )
: D_F.@  CR F@ F. CR ;
\ .. use
\    PARAMX D_F.@
\ a) line will be deleted later when filtering with awk
\ b) show content of the variable PARAMX
\ --------------------------------------------------------------
\ reconstitute the gforth stack based on the RPN stack X Y Z T
\ typically after an operation of type * + - / which
\ had an effect on the stack becoming smaller
\ L not touched
\ ( r1 r2 r3 r4 r5 .. rn -- T Z Y X )
: D_RPNREC  FCLEARSTACK T F@ Z F@ Y F@ X F@  ;
\ --------------------------------------------------------------
\ clear all stacks. NOT the RPN Stack (stored in X Y Z T L )
\ use D_ for out filtering later with awk
\  ( r1 r2 rx -- none )
: D_CLEARSTACKS  CLEARSTACKS  ;
\ --------------------------------------------------------------
\ create more debugging words if necessary
\ --------------------------------------------------------------
\ put the usual gforth command for ansi terminal 
: D_AT-XY AT-XY ;
\ --------------------------------------------------------------
\ create a debug pause word
: D_PAUSE3S 3000 MS ;
\ --------------------------------------------------------------
\ create a word to put in line the PAD and the temporary string
\ created by a S" under gforth
\ Attention: D_PADSET to be used after any S"
\ or perhaps redefined S" according https://www.reddit.com/r/Forth/comments/180e1cm/comment/ka8rpwm/?utm_source=reddit&utm_medium=web2x&context=3
( addr-c n -- addrpad n )
( str -- pad-str ) 
: D_PADSET
    TUCK            ( n addr-c n )
    PAD 2 CHARS +   ( n addr-c n pad-1st-char )
    SWAP            ( n addr-c pad-1st-char n )
    CMOVE>          ( n )
    DUP             ( n n )
    PAD 1 CHARS +   ( n n pad-n-addr )
    C!              ( n )
    PAD 2 CHARS +
    SWAP            ( addrpad n )
;
\ test
\ EMU71
\ " abcd" >>> OK { 2 } 
\ PAD 1 CHARS + C@ S. >>> [ 214297 4 4 ]  OK { 3 } 
\ PAD 1 CHARS + COUNT TYPE >>> abcd OK { 3 } 
\ PAD 2 CHARS + 4 TYPE >>> abcd OK { 0 } 
\ PAD C@ . >>> 80  OK { 0 }
\ PAD 2 CHARS + 4 MAXLEN . >>> 80  OK { 0 } 
\
\ gforth
\ PAD 1 chars + C@ . 0  ok    when PAD not initialized, current length zero
\ PAD 2 chars + C@ . 126  ok  ..., trash in it
\ S" ABCD" D_PADSET  ok
\ PAD 1 chars + C@ . 4  ok
\ PAD 2 chars + C@ .  65  ok
\ PAD 3 chars + C@ .  66  ok
\ PAD 4 chars + C@ .  67  ok
\ PAD 2 chars + 4 type ABCD ok
\ PAD C@ .  80  ok
\ --------------------------------------------------------------
\ or ?
\ adaptor especially for PAD use and avoidance of D_PADSET
\ https://www.reddit.com/r/Forth/comments/180e1cm/comment/ka8rpwm/?context=3
\ : S" ( cccc" -- ) 
\      [CHAR] " PARSE
\      STATE @
\      IF  POSTPONE (S")  S,
\	  ELSE PAD DUP >R PLACE R> COUNT
\      THEN ; IMMEDIATE
\ --------------------------------------------------------------
\
\
( setup the stack in GFORTH
( both taken off on June 12 2024 because issue with gforth stack set to 3 )
( after taking FLITERAL out, reinserted and looks better )
D_SSET
\ D_?SINIT  ( must be this line in case of use of the float stack  )
D_RPNS
(  X       Y       Z       T       L    later on HP71B STACK   )
\
\
\ other debug words?
\
\ *********************   SECOND PART    ***********************
\
\                glossary HP71B  Forth/assembler    
\
\ **************************************************************
\
\ translation of all HP71B words to make them compatible for  
\   gforth https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Word-Index.html#Word-Index
\ for allowing an editing/debugging with emacs (gforth and emacs are a good mix)
\
\ 1. in case the word is the same in FORTH HP71B and GFORTH, nothing is done (just a comment in the right size)
\      a check of this must be done ("debugging of the emacs gforth debugger for HP71B FORTH")
\ 2. in case the same word has a different behaviour in GFORTH and HP71B, G_xxx is developped below for reflecting the
\      future behaviour in HP71B
\ 3. in case the word is not existing in GFORTH, a new word is made (however, emacs GFORTH would announce it)
\ 4... in case a word in GFORTH is existing, and not in HP71B but would be valuable, then a new Word should be
\      developped for HP71B. This word should NOT be in this file which contain ONLY words valuable
\      during a debugging phase
\      perhaps more to be done here for avoiding GFORTH words only to be uploaded into an HP71B later
\
\ >>>> 4x the sign ">" indicate an HP71B word which translation in gforth is so far not done
\
\ ***************************************************************************************************************
\ all words lister with VLIST..   "
(
?
:
]
;
*
#
.
[
/
,
3
2
1
0
@
!
>
<
=
-
+
J
I
'
L
T
Z
Y
X
S<
S=
S!
C,
U.
<>
M*
D<
<#
#>
#S
CR
*/
D.
M/
S0
.(
."
DO
IF
C!
C@
BL
D+
D-
U<
0>
0<
1-
2-
1+
2+
5-
5+
2/
2*
+!
0=
OR
R@
>R
R>
OF
F/
F*
F-
F+
N!
N@
F.
LN
H.
.S
IP
FP
ASC
VAL
POS
EOF
MAX
MIN
D.R
MOD
PAD
KEY
BYE
>IN
BLK
USE
TIB
RP0
SP0
ABS
NOT
XOR
AND
ROT
DUP
RP!
RP@
SP!
C@+
SP@
RDN
RUP
E^X
Y^X
1/X
X^2
TAN
COS
SIN
ENG
SCI
FIX
STD
RCL
STO
[']
UM*
CHS
LGT
HEX
4N@
S>&
S<&
END$
CHR$
STR$
SUB$
+BUF
TYPE
CASE
SIGN
?DUP
HOLD
/MOD
HERE
EMIT
WORD
QUIT
FIND
ROLL
LOOP
ELSE
THEN
BASE
PREV
WARN
S->D
DABS
FILL
2DUP
OVER
PICK
SWAP
DROP
GROW
10^X
X<>Y
ATAN
ACOS
ASIN
EXIT
SPAN
#TIB
FTOI
ITOF
SQRT
X#Y?
X=Y?
X>Y?
X=0?
X<Y?
CRLF
FABS
LEFT$
SMOVE
NULL$
OPENF
LOADF
BLOCK
ENDOF
M/MOD
*/MOD
DEPTH
SPACE
ABORT
?COMP
ALLOT
QUERY
CMOVE
+LOOP
WHILE
UNTIL
BEGIN
DOES>
LEAVE
LINE#
LIMIT
FIRST
STATE
ONERR
OKFLG
FENCE
WIDTH
COUNT
2SWAP
DIGIT
2DROP
NMOVE
2OVER
ENTER
LASTX
>BODY
FLUSH
EXPBF
CONBF
X>=Y?
X<=Y?
FINDF
FDROP
VARID
FSTR$
NFILL
RIGHT$
MAXLEN
STRING
CLOSEF
SMUDGE
SPACES
ABORT"
NUMBER
TOGGLE
LATEST
?STACK
FORGET
REPEAT
CREATE
SCRFIB
NMOVE>
CMOVE>
NEGATE
SHRINK
OUTPUT
BASICX
FENTER
UM/MOD
KILLBF
FINDBF
MAKEBF
BASIC$
BASICF
BASICI
NALLOT
ADJUSTF
SYNTAXF
CREATEF
ENDCASE
COMPILE
LITERAL
'STREAM
ENCLOSE
CONVERT
DECIMAL
CURRENT
CONTEXT
DNEGATE
DEGREES
RADIANS
EXECUTE
PRIMARY
LISTING
CLOSEALL
DLITERAL
TRAVERSE
EXPECT96
VARIABLE
CONSTANT
FLITERAL
PAGESIZE
ASSEMBLE
IMMEDIATE
-TRAILING
INTERPRET
[COMPILE]
SECONDARY
FVARIABLE
FCONSTANT
?TERMINAL
VOCABULARY
DEFINITIONS
STRING-ARRAY
)
\
\ ! ------------------------------------------------------------
\ in gforth, too
\
\ ( n addr -- )
\ Store n at addr
\ .. use
\ (HP71B) ..
\ VARIABLE VV1
\  OK { 0 } 
\ 1234 VV1 !
\ OK { 0 } 
\ VV1 @
\  OK { 1 } 
\ .
\ 1234  OK { 0 } 
\ --------------------------------------------------------------
\
\
\ " ------------------------------------------------------------
\ equivalent to the gforth S"
\
\ IMMEDIATE. In execute mode: Take the characters eee, terminated by the next ", from the input
\ stream, and store them in a temporary string variable at the PAD. The string variable's header shows a
\ maximum length of 80 characters or the current length, whichever is greater. Any other word that returns
\ another temporary string will wipe out the first string.
\ In compile mode: Compile into the dictionary the runtime address of ", two bytes for the length of the
\ string ccc (maximum length = current length), and the string itself. A string must be contained on a
\ single line of a source file.
\ ( -- str )
\
\ .. use
\
\ (HP71B)  Used in the form: " ccc"  
\ " ABCD" TYPE
\ ABCD OK { 0 }
\
\ (gforth)
\ S" ABCD" TYPE ABCD ok  
\ --------------------------------------------------------------
\
\
\ # --------------------------------------------------------------
\ in gforth too
\
\ https://gforth.org/manual/Formatted-numeric-output.html#index-_0023_0028--ud1-_002d_002d-ud2--_0029-core
\ ( ud1 – ud2  ) core “number-sign” Used between <<# and #>.
\ Prepend the least-significant digit (according to base) of ud1 to the pictured numeric output string.
\ ud2 is ud1/base, i.e., the number representing the remaining digits.
\ ( ud1 -- ud2 )
\ #
\ Divide ud1 by BASE, convert the remainder to an ASCII character, place this character in an output
\ string, and return the quotient ud2. Used in pictured output conversion; refer to <#.
\ .. use
\ Used in the form: <# # # # #>
\ --------------------------------------------------------------
\
\
\ #> -----------------------------------------------------------
\ in gforth too
\
\ ( xd – addr u  ) core “number-sign-greater” Complete the pictured numeric output string by discarding xd and returning addr u;
\ the address and length of the formatted string.
\ A Standard program may modify characters within the string. Does not release the hold area;
\ use #>> to release a hold area started with <<#, or <# to release all hold areas.
\ ( ud -- addr n )
\ #>
\ End pictured output conversion. #> drops ud and returns the text address and character count. (These
\ are suitable inputs for TYPE.)
\ --------------------------------------------------------------
\
\
\ #S -----------------------------------------------------------
\ in gforth too
\
\ ( ud – 0 0  ) core “number-sign-s”  Used between <<# and #>. Prepend all digits of ud to the pictured numeric output string.
\ #s will convert at least one digit. Therefore, if ud is 0, #s will prepend a “0” to the pictured numeric output string. 
\ ( ud -- 0 0 )
\ #S
\ Convert ud into digits (as by repeated execution of #), adding each digit to the pictured numeric-output
\ text until the remainder is zero. A single zero is added to the output if ud = O. Used between <# and #>.
\ --------------------------------------------------------------
\
\
\ #TIB ---------------------------------------------------------
\ ( -- addr) 
:  #TIB  CR CR ." not implemented in gforth" CR ;
\ Return the address of the variable #TIB, which contains the number of bytes in the terminal input buffer.
\ Set by QUERY.
\ --------------------------------------------------------------
\
\
\ ' ------------------------------------------------------------
\ in gforth, too
\
\ use in the form  ' name  ( -- addr ) return CFA of name
\ ( according "starting forth")
\ Attempts to find the execution token of name
\   (the word that follows in the input stream) in the dictionary.
\
\ see area of the COMPILE Word ( ' is included in a separate file)
\ --------------------------------------------------------------
\
\
\ 'STREAM ------------------------------------------------------
\ Return the address of the next character in the input stream.
\ ( -- addr ) 
: 'STREAM CR CR ." not implemented in gforth" CR ;
\ .. use ? 
\ --------------------------------------------------------------
\
\
\ ( ------------------------------------------------------------
\ in gforth, too
\
\ IMMEDIATE. Consider the characters ccc, delimited by ), as a comment to be ignored by the text interpreter.
\ The blank following ( is not part of ccc. ( may be freely used while interpreting or compiling.
\ A comment must be contained on a single line of a source file.
\ Used in the form: ( ccc)
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ *             like in gforth
\ */            like in gforth
\ */MOD         like in gforth
\ +             Like in GFORTH
\ +!            Like in GFORTH
\ --------------------------------------------------------------
\
\
\ +BUF ---------------------------------------------------------
\
\ Advance the mass-storage-buffer address (addr1) to the address
\ of the next buffer (addr2). +BUF returns a false flag if addr2
\ is the address of the buffer currently pointed to by PREV;
\ otherwise, +BUF returns a true flag.
\ ( addr1 -- addr2 flag )
: +BUF  CR CR ." not implemented in gforth" CR ;
\ returns the address of the next available buffer.
\ --------------------------------------------------------------
\
\
\ , ------------------------------------------------------------
\ in gforth , ( w –  ) core “comma”
\
\ Reserve data space for one cell and store w in the space.
\ in Forth71B: allot five nibbles and store n in the dictionnary
\ .. use
\ 1234 , 
\ --------------------------------------------------------------
\
\
\ - ------------------------------------------------------------
\ (minus) in gforth too
\
\ Subtract n2 from n1 and return the difference n3
\ ( n1 n2 -- n3 )
\ --------------------------------------------------------------
\
\
\ -TRAILING ----------------------------------------------------
\ like in gforth
\
\ Adjust the character count of the text beginning at addr to
\ exclude trailing blanks.
\ ( addr count1 -- addr count2 )
\ --------------------------------------------------------------
\
\
\ . ------------------------------------------------------------
\ (dot)   Like in GFORTH
\
\ Convert n according to BASE and display the result in a
\ free-field format with one trailing blank. Display
\ a minus sign if n is negative.
\ --------------------------------------------------------------
\
\
\ ." -----------------------------------------------------------
\ (Dot-quote) in gforth too
\
\ Used in the form " ccc"
\ COMPILE, IMMEDIATE. Compile the characters ccc, delimited by
\ ", so that later execution will transmit ccc to the current
\ display device. The blank following " is not part of ccc.
\ A string must be contained on a single line of a source file .
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ .(            like in gforth Compilation and interpretation semantics: Parse a string ccc delimited by a ) (right parenthesis).
\                 Display the string. This is often used to display progress information during compilation; see examples at gforth\
\ --------------------------------------------------------------
\
\
\ .S -----------------------------------------------------------
\ similar in gforth: output only the integer stack
\ 
\ update 2024 June 12: .S = D_RPNS taken out because the gforth float stack was reduced by 1 when use. reason ?
\                      just keep the .S as it is
\
\ however rewritten https://www.reddit.com/r/Forth/comments/tt3ll7/how_do_you_replace_the_ok_prompt_in_gforth/
\ use .S on a terminal instead of  D_RPNS?
\ : .S  ." ( " TYPE depth 0 max maxdepth-.s @ min dup 0 ?DO dup i - pick . LOOP ." )" TYPE drop ;
\
\ test HP71B
\ 1 2 Endline
\ .S Endline >> 2 1  OK { 2 }
\
\ test PC Gforth
\ 1 2 .S incase ": .S D_RPNS ;"
\
\ HP71B float stack content, RPN HP41 (& gforth stack):
\   X       0.00000     Top gforth      0.00000
\   Y       0.00000                     0.00000
\   Z       0.00000                     0.00000
\   T       0.00000     Bottom gforth   0.00000
\   LASTX   0.00000
\
\ float   stack gforth  
\  <stack depth> bottom to top, of stack
\  <4> 0.000000000000E0 0.000000000000E0 0.000000000000E0 0.000000000000E0 
\
\ integer stack gforth  
\   <stack depth> bottom to top, of stack
\   <2> 1 2 
\
\ RADIAN ON
\ BASE: 10 
\
\  ok
\
\ --------------------------------------------------------------
\
\ /             ( n1 n2 – n  ) core “slash”  n=n1/n2      Like in GFORTH
\ /MOD          ( n1 n2 – n3 n4  ) core “slash-mod”  n1=n2*n4+n3; n3 is the modulus, n4 the quotient. 
\ 0             Zero
\ 0<            ( n – f ) core “zero-less-than” gforth
\ 0=            ( n – f ) core “zero-equals” gforth
\ 0>            ( n – f ) core-ext “zero-greater-than” gforth
\ 1             One  in gforth core
\ 1+            in gforth core
\ 1-            in gforth core
\ --------------------------------------------------------------
\
\
\ 1/X ----------------------------------------------------------
\ tested on 19 Sept 2023
\ ( r1 -- r2 )
\    X    Y     Z      T      L 
\    1/X  Y     Z      T      X
: 1/X  D_?SINIT   FDUP L F!  1/F  FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ 10^X ---------------------------------------------------------
\ tested 19 Sept 2023 gforth
\ ( r1 -- r2 )
\    X     Y     Z      T      L
\    10^X  Y     Z      T      X
: 10^X  D_?SINIT  FDUP L F!  FALOG  FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ 2            same in gforth
\ 2*           same in gforth
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
 : 2+ 2 + ;  
 : 2- 2 - ;
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ 2/           same in gforth
\ 2DROP        same in gforth
\ 2DUP         same in gforth
\ 2OVER        same in gforth
\ 2SWAP        same in gforth
\ 3            same in gforth
\ --------------------------------------------------------------
\
\
\ 4N@ ----------------------------------------------------------
\ tested on 19 Nov 2023
\ return the four-nibble (two-byte) quantity located at addr.
: 4N@ ( addr -- n ) 
  BASE @ >R   \ Save the current number base
  DECIMAL @ 256 DUP * 1 - AND
  R> BASE !   \ Restore the original number base
;
\ .. test on emulator 71B 
\ " ABCDEFGH" DROP 4N@ .
\ 16961  OK { 5 } 
\ . ret 16961 which is 4241 in HEX = BA
\
\ .. test on gforth prompt
\ S" ABCDEFGH" DROP 4N@ . 16961  ok
\ --------------------------------------------------------------
\
\
\ 5+ & 5- ------------------------------------------------------
\ usage of 5+ in Forth71B is mostly for memory adding
\ therefore the layer is defined according this
: 5+ 1 CELLS + ;   
: 5- 1 CELLS - ;
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ :            Like in GFORTH
\ ;            Like in GFORTH
\ <            in gforth, too
\                < ( n1 n2 – f ) core “less-than”
\ <#           in gforth, too
\                <# ( –  ) core “less-number-sign”
\                Initialise/clear the pictured numeric output string. 
\ <>           in gforth, too
\                <> ( n1 n2 – f ) core-ext “not-equals”
\ =            Like in GFORTH
\ >            Like in GFORTH
\ --------------------------------------------------------------
\
\ --------------------------------------------------------------
\ >BODY        like in gforth
\                >body ( xt – a_addr  ) core “to-body”
\                Get the address of the body of the word represented by xt (the address of the word’s data field).
\              Return the PFA (addr2) of the word whose CFA is addr1. (addr2 = addr1 + 5.)
( addr1 -- addr2 )
\ --------------------------------------------------------------
\
\
\ >IN ----------------------------------------------------------
\ in gforth, too
\
\ Return the address of the variable >IN, which contains the current offset within the input stream. The
\ offset is expressed in nibbles and points to the first position past the first blank.
 ( -- addr1 )
\ --------------------------------------------------------------
\
\
\ >R -----------------------------------------------------------
\ in gforth, too
\ COMPILE. Transfer n to the return stack.
\ ( n -- )
\ --------------------------------------------------------------
\
\
\ ? ------------------------------------------------------------
\ in gforth, too
\
\ ( a-addr –  ) tools “question” Display the contents of address a-addr in the current number base. 
\ Display the contents of address a-addr in the current number base.
\ use the . dot format on HP71
\ Display the number at addr using the current BASE and the . (dot) format.
\ used in the form:   HEX 2FCC5 ?
\ --------------------------------------------------------------
\
\
\                work this out
\ ?COMP --------------------------------------------------------
\ COMPILE. Issue a *FTH ERR: compile only* message if not in
\  compile mode.
\ In the INCLUDED file, see word "COMPILE" here, 
\ You will find something similar like below
\ : ?COMP    ( -- ) STATE @ 0= ABORT" Compile only" ;
: ?COMP CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ ?DUP ---------------------------------------------------------
\ like in gforth
\ ?DUP       ( w -- 0 | w w ) It performs a dup if w is nonzero.
\ --------------------------------------------------------------
\
\
\ ?STACK -------------------------------------------------------
\ Issue a FTH ERR: empty stack message if the stack pointer is
\ above the bottom of the stack; or issue a FTH ERR: full stack
\ message if the stack pointer has grown into the pad.
: ?STACK  CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ ?TERMINAL ----------------------------------------------------
\ tested 11 Oct 2023
\ Return a true flag if a key has been pressed and placed in the
\ key buffer; otherwise, return a false flag.
\ ( -- flag )
\ : ?TERMINAL  POSTPONE KEY? ; IMMEDIATE   seems ok
: ?TERMINAL  KEY?  ;
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ @            already in gforth
\ ABORT        already in gforth
\ ABORT"       already in gforth
\ ABS          already in gforth
\ --------------------------------------------------------------
\
\
\ ACOS ---------------------------------------------------------
\
\ tested 19 sept 2023
( r1 -- acos"r1" )
\    X       Y      Z      T      L
\    ACOS(X) Y      Z      T      X 
: ACOS  D_?SINIT FDUP L F! FACOS DEG IF 360.0E0 F* 2.0E0 F/ PI F/ ELSE THEN FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ ADJUSTF ------------------------------------------------------
\ Adjust a file by n nibbles, starting at addr and moving toward
\ greater addresses, and return a true flag if successful or a
\ false flag if not.
\ ( addr n -- flag )
\ ADJUSTF enlarges the file for positive n or shrinks the file for negative n.
\ ? https://gforth.org/manual/General-files.html
: ADJUSTF  CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ ALLOT        already in gforth
\ AND          see gforth, too
\ --------------------------------------------------------------
\
\
\ ASC ----------------------------------------------------------
\
\ tested 15Sept 2023 
\ return asci number of the first character of a string
( addr u -- n)
: ASC  DROP C@ ;
\ test
\ HP71B
\  " J" ASC
\ gforth
\   vgl CHAR J S. [ 74 ]  ok
\   S" J" ASC S. [ 74 ]  ok
\ --------------------------------------------------------------
\
\
\ ASIN ---------------------------------------------------------
\
\ tested 19 sept 2023 gforth
( r1 -- asin"r1" )
\    X       Y      Z      T      L
\    ASIN(X) Y      Z      T      X
: ASIN  D_?SINIT FDUP L F! FASIN  DEG IF 360.0E0 F* 2.0E0 F/ PI F/ ELSE THEN FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ ASSEMBLE -----------------------------------------------------
\ ( str -- )
\ Assemble the file whose name is specified  by str.
\ use BASICX, so you cant call assemble from BASIC
: ASSEMBLE  CR CR ." not implemented in gforth" CR ;
\ in gforth, the CODE assembler passing words could be used
\ however, the PC ASSEMBLER code is very different from
\ the HP71B assembler which makes low sense to use such
\ words on a PC (64bits AMD assembler)
\ https://www.complang.tuwien.ac.at/ubvl/amd64/amd64h.html
\ --------------------------------------------------------------
\
\
\ ATAN ---------------------------------------------------------
\
\ tested 20 Sept 2023
( r1 -- atan"r1" )
\    X       Y      Z      T      L
\    ATAN(X) Y      Z      T      X
: ATAN D_?SINIT  FDUP L F! FATAN   DEG IF 360.0E0 F* 2.0E0 F/ PI F/  ELSE THEN  FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ BASE ---------------------------------------------------------
\ in gforth, too    base ( – a-addr  ) core “base”
\ By default, the number base used for integer number conversion
\ is given by the contents of the variable base.
\ .. use HP71B
\ BASE @
\ OK { 1 }
\ .
\ 10 OK { 0 }
\ .. use gforth
\ BASE @  ok
\ . 10  ok
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ BASIC -- no development so far since BASIC commands ----------
\ below in page 17 of the forth manual
\
( str1 str2 -- str1 )
: BASIC$ CR  ." BASIC$ not in gforth" CR 2DROP ;
\  passes a string containing a string expression to the BASIC
\   system for evaluation.
\  It returns the resulting string to the PAD area and the
\   address and character count to the data stack.
\  The resulting string is truncated to 255 characters if it
\   exceeds this length.
\  example " A$" BASIC$
\
: BASICF  CR CR ." BASICF not implemented in gforth" CR ;
\  passes a string containing a numeric expression to the BASIC system for evaluation.
\  It returns the value of the numeric expression to the X-register in the FORTH floating-point stack.
\  example " TIME" BASICF
\
: BASICI  CR CR ." BASICI not implemented in gforth" CR ;
\  passes a string containing a numeric expression to the BASIC system for evaluation.
\  It returns the value of the numeric expression to the FORTH data stack.
\  example " A(1)" BASICI
\  " STATUS" BASICI  returns to the integer data stack a value describing the loop status.
\
: BASICX CR CR ." BASICX not implemented in gforth" CR ;
\  passes a string containing BASIC statements to the BASIC system for parsing and execution.
\  It returns no value to the FORTH environment. E: A ~:; I C ::< can alter the value of BASIC variables.
\  If the string begins with a line number, it will be added to the current BASIC edit file.
\  The string can also call BASIC programs. When the BASIC interpreter finishes, it issues a poll that allows the FORTH system to regain control.
\  If an error occurs, the BASIC system reports the error to the user, and FORTH runs the system equivalent of the A E: 0 P T word.
\  " A6=T(4)*PI" BASICX
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ BEGIN ... UNTIL                Like in GFORTH
\ BEGIN ... WHILE ... REPEAT     Like in GFORTH
\ BL       ( -- 32(10) )         Like gforth;
\                            return the ASCII blank value
\ --------------------------------------------------------------
\
\
\ BLK ----------------------------------------------------------
\
\ Return the address of the variable BLK, which contains the
\ number of the line being interpreted from the active file.
\ The value of BLK is an unsigned number; if it is zero, the
\ input stream is taken from the keyboard device.
\ BLK is a user variable, see above, containing either the line
\ number of the file being interpreted by LOADF or else 0
\ (input from keyboard).
\ ( -- addr)
: BLK  CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ BLOCK --------------------------------------------------------
\
\ Return the address of the first byte in the mass-storage-buffer copy of line n in the active file. If line n
\ hasn't already been copied from the file (in RAM or on mass storage) into a mass storage buffer, BLOCK
\ does so.
\ BLOCK  ( n -- addr ) like in gforth Return the address of the first byte in the mass-storage-buffer
\                      copy of line n in the active file. If line n hasn't already been copied from the file
\                      (in RAM or on mass storage) into a mass storage buffer, BLOCK does so.
\ https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Blocks.html
: BLOCK  CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ BYE           get out of FORTH intergreter; like gforth
\
\ C!            Like in GFORTH       c! ( c c-addr – ) core “c-store”
\                                    Store c into the char at c-addr.
\               see ASCII codes https://www.asciitable.com/
\
\ C,            like in gforth
\               c, ( c –  ) core “c-comma”
\               Reserve data space for one char and store c in the space.
\
\ C@            Like in GFORTH
\ --------------------------------------------------------------
\
\
\ C@+ ----------------------------------------------------------
\
\ tested 20 Sept 2023
\ Return c, the first character in the string specified by str1 and str2, where addr2 = addr1 + 2 and count2 = count1 - 1 .
\  If count1 = 0, c = 0 and str2 = str1.
\ ( str1 -- str2 c )
: C@+  DUP 0= IF 48 ELSE 1 - SWAP DUP C@ SWAP 1 CHARS + -ROT THEN ;
\ use ..
\ HP71B 
\   " ABCD" C@+ .
\   65   OK { 2 }
\   TYPE
\   BCD OK { 0 }
\   " ABCDE" C@+ S. DROP C@+ S. DROP C@+ S. [ 214299 4 65 ] [ 214301 3 66 ] [ 214303 2 67 ]  OK { 3 } 
\ gforth
\   S" ABCDE" C@+ S. DROP C@+ S. DROP C@+ S.  [ 94325704834433 4 65 ] [ 94325704834434 3 66 ] [ 94325704834435 2 67 ]  ok
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ CASE .. OF ... ENDOF ...   in gforth, too
\ ENDCASE                    in gforth, too
\ --------------------------------------------------------------
\
\
\ CHR$ ---------------------------------------------------------
\
\ tested 21 Sept 2023
\ Convert the two low-order nibbles of n into an ASCII character and place it in a string specified by str.
\ The string is a temporary string of length 1, located on the pad.
\ in fact, inverse of ASC
\ ( n -- str )
\ : CHR$  1 ALLOCATE THROW TUCK C! 1 ;
\
\ .. instead put the char into the PAD as per HP71B behaviour
: CHR$ PAD 2 CHARS + DUP ROT SWAP C! 1 PAD 1 CHARS + C! 1 ;
\
\ test
\ HP71B emulator
\   125 CHR$ >>> OK { 2 }
\   TYPE >>> } OK { 0 } .. show the } character which is ASCII DECIMAL 125
\              ..  n from 0 to 255 https://www.asciitable.com/
\   125 CHR$ S. PAD S. DROP TYPE >>> [ 214297 1 ] [ 214297 1 214293 ] } OK { 0 } 
\ gforth
\   125 CHR$  ok                                    
\   .S <2> 94315339501536 1  ok
\   TYPE >>> } ok
\   75 CHR$ TYPE >>> K ok 
\   48 CHR$ TYPE >>> 0 ok 
\   125 CHR$ S. PAD S. DROP TYPE [ 139989000575234 1 ] [ 139989000575234 1 139989000575232 ] } ok
\ --------------------------------------------------------------
\
\
\ CHS ----------------------------------------------------------
\
\ tested on 20 Sept 2023
\ ( r1 -- "-"r1 )
\    X    Y     Z      T      L
\    -X   Y     Z      T      X
: CHS D_?SINIT FDUP L F! FNEGATE FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ CLFV ---------------------------------------------------------
\
\ initialize float variable to zero
\ ( addr -- )
: CLFV  0.0E0 F! ;
\ test
\ FVARIABLE FVAR1  ok
\ 1.0E0 FVAR1 F!
\ 1.0E0 2.0E0 3.0E0 fs. 3.00000000000000E0  ok
\ fs. 2.00000000000000E0  ok
\ fs. 1.00000000000000E0  ok
\ 1.0E0 2.0E0 3.0E0 FVAR1 F!  ok
\ fs. 2.00000000000000E0  ok
\ --------------------------------------------------------------
\
\
\ CLOSEALL -----------------------------------------------------
\
\ Close all open files (that is, files with an open FIB entry).
\ ( -- )
: CLOSEALL  CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ CLOSEF -------------------------------------------------------
\
\ Close the specified file whose FIB# is n.
\ ( n -- )
: CLOSEF  CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ CLRV ---------------------------------------------------------
\
\ read the float variable sss,eeeii and put sss eee ii into the
\ integer stack
\ ( addr -- sss eee ii )
: CLRV
    f@ fdup f>s            \ Kopiere und hole den Ganzzahlteil: f sss
    >r                     \ sss nach Rückgabestack
    fdup f>s               \ Kopie von f, erneut sss holen
    s>f f-                 \ f - sss = nur Nachkommastellen
    100000e f*             \ Nachkommastellen * 100000 (verschiebe 5 Stellen)
    f>s                    \ in Integer konvertieren
    dup 100 /              \ eee extrahieren
    swap 100 mod           \ ii extrahieren
    \
    dup                    \ ii should be never 0 (minimum 1)
    0= if
	1 +
	else then
    \
    r>                     \ sss vom Rückgabestack holen
    rot rot
;
\ test
\ FVARIABLE FVAR1  ok
\ 123.6780201E0 FVAR1 F!
\ FVAR1 CLRV S.  [ 123 678 2 ]  ok
\ . 2  ok
\ . 678  ok
\ . 123  ok
\ 
\ --------------------------------------------------------------
\
\
\ CLST ---------------------------------------------------------
\
\ Clear float stack
: CLST  X 0.0E0 F! Y 0.0E0 F! Z 0.0E0 F! T 0.0E0 F! D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ CLX ----------------------------------------------------------
\
\ clear the register X
\ ( -- )
: CLX  0.0E0 X F! ;
\ --------------------------------------------------------------
\
\ CMOVE --------------------------------------------------------
\ like in gforth
\ cmove ( c-from c-to u – ) string “c-move”
\ Copy the contents of ucount characters from data space at c-from to c-to.
\ The copy proceeds char-by-char from low address to high address; i.e., for overlapping areas it is safe if c-to<=c-from.
\ Move un bytes, first moving the byte at addr1 to addr2 and finally moving the byte at addr1 + 2(un - 1) to addr2 + 2(un -1). If un = 0, nothing is moved.
\ ( addr1 addr2 un -- )
\ CMOVE
\ --------------------------------------------------------------
\
\
\ CMOVE> ------------------------------------------------------- 
\ like in gforth Copy the contents of ucount characters from data space at c-from to c-to.
\ The copy proceeds char-by-char from high address to low address; i.e., for overlapping areas it is safe if c-to>=c-from.
\ Move un bytes, first moving the byte at addr1 + 2(un - 1) to addr2 + 2(un-1) and finally moving the
\ byte at addr1 to addr2. If un = 0, nothing is moved.
\ ( addr1 addr2 un -- )
\ CMOVE>
\ --------------------------------------------------------------
\
\
\                               test this
\ COMPILE ------------------------------------------------------
\ 12 Juni 2024 line below taken out during searching for reason when gforth stack reduced by 1
\
\ use in the form : name1 ... COMPILE name2 ...
\ COMPILE. Compile the CFA of name2 when namel is executed.
\ Typically name1 is an immediate word and name2 is not;
\ COMPILE ensures that name2 is compiled, not executed,
\ when namel is encountered in a new definition.
\ https://gist.github.com/ruv/7c0b6fae5d5f388dd54062b59b732118
\ S" COMPILE.FORTH83.GFORTH.fth" INCLUDED 
\ --------------------------------------------------------------
\
\
\ CONBF --------------------------------------------------------
\ ( n1 n2 -- flag)
\ Contract by n1 nibbles the general-purpose buffer whose ID# is
\ n2, and return a true flag; or return a false flag if such a
\ buffer doesn't exist.
\ If the specified buffer contains fewer than n1 nibbles, CONBF
\ contracts it to 0 nibbles. n1 must not exceed FFF.
: CONBF CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ CONSTANT -----------------------------------------------------
\ see Gforth, too
\ --------------------------------------------------------------
\
\
\ CONTEXT ------------------------------------------------------
\ see gforth and variable above: context @ is the wid of the word list at the top of the search order. 
\ --------------------------------------------------------------
\
\
\                       test this when needed
\ CONVERT ------------------------------------------------------
\ ( d1 addr1 -- d2 addr2 )
\ Accumulate the string of digits beginning at addr1 + 2 into the double number d1 and return the result d2 and the address addr2 of the next non-digit character.
\ For each character that is a valid digit in BASE, CONVERT converts the digit into a number, multiplies the current double number (initially d1 ) by
\ BASE and adds the converted digit to the current double number. When CONVERT encounters a non-digit character, it returns the current double number and the non-digit character's address.
: CONVERT COUNT >NUMBER DROP 2- CR CR ." CONVERT not tested in gforth" CR ;
\
\ further explanation gforth..
\
\ convert ( ud1 c-addr1 – ud2 c-addr2  ) core-ext-obsolescent “convert”  Obsolescent: superseded by >number. 
\
\ >number ( ud1 c-addr1 u1 – ud2 c-addr2 u2  ) core “to-number”
\ Attempt to convert the character string c-addr1 u1 to an unsigned number in the current number base.
\ The double ud1 accumulates the result of the conversion to form ud2.
\ Conversion continues, left-to-right, until the whole string is converted or a character that is not convertable in the current number base is encountered (including + or -).
\ For each convertable character, ud1 is first multiplied by the value in BASE and then incremented by the value represented by the character.
\ c-addr2 is the location of the first unconverted character (past the end of the string if the whole string was converted).
\ u2 is the number of unconverted characters in the string. Overflow is not detected. 
\ --------------------------------------------------------------
\
\
\ COS ----------------------------------------------------------

\ tested 19 Sept 2023
\ default is radian. However DEG perhaps ON
\ if DEGREES not explicitly activated, it will
\  be default value DEG 0 which is "NOT DEGREE" or RAD is the default
\ ( r1 -- cos"r1" )
\    X      Y      Z      T      L
\    COS(X) Y      Z      T      X
: COS  D_?SINIT FDUP L F!   \ save the X to L
    DEG IF 360.0E0 F/ 2.0E0 F* PI F* ELSE THEN FCOS FDUP X F! ;
\ .. use
\ EMU71
\   60.0 COS >>> OK { 0 }
\   F. >>> 5.000E-1  OK { 0 }
\   RADIANS >>> OK { 0 }
\   1.04719755 COS >>> OK { 0 }
\   F. >>> 5.000E-1  OK { 0 }
\   DEGREES >>> OK { 0 }
\ gforth
\   60.0E0 COS F. RADIANS 1.04719755 COS F. DEGREES -0.95241298 1.  ok
\   DEGREES 60.0E0 COS F. RADIANS 1.04719755 COS F. DEGREES 0.5 1.  ok
\ --------------------------------------------------------------
\
\
\ COUNT --------------------------------------------------------
\
\ in gforth, too
\ (addr1 -- addr2 n) Return the address (addr2) of the first character, and the character count (n), of the counted string beginning at addr1
\ The first byte at addrl must contain the character count n.
\ --------------------------------------------------------------
\
\
\ CR -----------------------------------------------------------
\
\ carriage return      like in gforth
\ --------------------------------------------------------------
\
\
\ CREATE -------------------------------------------------------
\
\ in gforth, too
\ --------------------------------------------------------------
\
\
\                      test this
\ CREATEF ------------------------------------------------------
\
\ Create a text file in RAM whose name is specified by str and that contains n nibbles.
\ If successful, CREATEF returns the address of the beginning of the file header (which contains the file name);
\ otherwise, it returns a false flag. If the specified string exceeds eight characters, the file name will be the first eight characters.
\ ( str n -- addr ) or ( str n -- false )
\ https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Blocks.html
\ https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/General-files.html#General-files
: CREATEF  ROT OPEN-BLOCKS GET-BLOCK-FID  CR CR ." not tested" CR ;
\ --------------------------------------------------------------
\
\
\ CRLF ---------------------------------------------------------
\
\ tested 29 sept 2023
\ Return str specifying the two-character string constant containing the ASCII characters carriage-return and line-feed.
\ This string can be concatenated with other strings for use with words such as 0UTPUT.
\ ( -- str )
: CRLF  S\" \r\n"  ;
\ --------------------------------------------------------------
\
\
\ CURRENT ------------------------------------------------------
\
\ ( -- addr ) like gforth
\ Return the address of the variable CURRENT, which specifies the vocabulary to receive new word definitions.
\ See variable declaration above
\ CURRENT
\ --------------------------------------------------------------
\
\
\ D+ -----------------------------------------------------------
\
\ see gforth, too
\ --------------------------------------------------------------
\
\
\ D- -----------------------------------------------------------
\
\ see gforth, too
\ --------------------------------------------------------------
\
\
\ D. -----------------------------------------------------------
\
\ ( d -- )
\ ( HP71B ) Display d according to BASE in a free-field format, with a leading minus sign if d is negative.
\ ( gforth ) d. ( d –  ) double “d-dot”
\            Display (the signed double number) d in free-format. followed by a space. 
\ .. use in HP71B
\    1 2 D.
\    2097153  OK { 0 } 
\    1 0 D.
\    1  OK { 0 } 
\ --------------------------------------------------------------
\
\
\ D.R ----------------------------------------------------------
\
\ tested on 19 Sept 2023
\ ( d n -- )
\ Display d (according to BASE) right-justified in a field n characters wide.
\ https://github.com/bfox9900/CAMEL99-ITC/blob/master/LIB.ITC/DOUBLE.FTH
\ where RJUST replaced by OVER - SPACES TYPE
: D.R  ( d n --) >R TUCK DABS <# #S ROT SIGN #> R> OVER - SPACES TYPE ;
\ Example usage:
\ 42 0 10 D.R  \ Right-justify the number 42 in a 10-character field
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ D<             see gforth, too
\ DABS           ( d -- abs(d) ), see gforth, too
\ DECIMAL        see gforth, too
\ DEFINITIONS    same as gforth? Set the CURRENT vocabulary to match the CONTEXT vocabulary.
\ --------------------------------------------------------------
\
\
\ DEGREES ------------------------------------------------------
\
\ tested 15 sept 2023
\ set degrees angular mode 
: DEGREES -1 TO DEG 0 TO RAD ;
\ --------------------------------------------------------------
\
\
\ DEPTH --------------------------------------------------------
\
\ see gforth, too
\ ( -- n)
\ Return n, the number of items on the data stack (not counting
\ n itself).
\ --------------------------------------------------------------
\
\
\ DIGIT --------------------------------------------------------
\
\ ( c n1 -- n2 true ) or ( c n1 -- false )
\ If c is a valid digit in base n1, return that digit's binary value (n2) and a true flag; otherwise, return a false flag.
: DIGIT  CR CR ." not implemented in gforth" CR ;
\ .. use ?
\ --------------------------------------------------------------
\
\
\                 test this
\ DLITERAL -----------------------------------------------------
\
\ COMPILE, IMMEDIATE. Compile d into the word being defined,
\ such that d will be returned when the word is executed.
\ ( d -- )
: DLITERAL CREATE 2, 2, DOES> 2@ 2@  CR CR ." not tested" CR ;
\ .. use ?
\    within a word definition (like LITERAL)
\ : WORDX .. [ n1 n2 ] DLITERAL .. ; ?
\ nx could come out of a word like "2 CELLS"
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ DNEGATE        ( d -- -d ) in gforth, too
\ DO ... +LOOP   in gforth, too
\ DO ... LOOP    in gforth, too
\ DOES>          in gforth, too
\ DROP           ( n1 n2 -- n1 ) in gforth, too
\ DUP            in gforth, too      dup ( w – w w ) core “dupe”
\ EMIT           in gforth, too      emit ( c –  ) core “emit”. use ->  32 EMIT
\                                    Send the byte c to the current output; for ASCII characters, emit is equivalent to xemit.
\ --------------------------------------------------------------
\
\
\ ENCLOSE ------------------------------------------------------
\
\ ( addr c -- addr n1 n2 n3 )
\ Examine the string that begins at addr, and return:
\ • nl, the nibble offset from addr to the first character that doesn't match the delimiter character c.
\ • n2, the nibble offset from addr to the first delimiter character c that follows non-delimiter characters in the string.
\ • n3, the nibble offset from addr to the first unexamined character.
\ An ASCII null is treated as an unconditional delimiter.
: ENCLOSE CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\      
\ END$ ---------------------------------------------------------
\
\ tested 03 Jan 2023
\ creates a temporary substring from the last part of a string.
\ INTO THE PAD
\
\ Create a temporary string (specified by str2) consisting of the nth character and all subsequent characters in the string specified by strl.
\ (RIGHT$ is similar but takes substring length, not character position, for a parameter.)
\ ( str1 n -- str2 )
: END$              ( addr-c norg npos )
    DUP
    0=
    IF 1 +
    ELSE THEN
    2DUP            ( addr-c norg npos norg npos )
    < IF ." warning.. END$ outside string length = garbage gathering" CR
    ELSE THEN       ( addr-c norg npos )
    2DUP
    - 
    1 +             ( addr-c norg npos newlength )
    ROT DROP        ( addr-c npos newlength )
    -ROT            ( newlength addr-c npos )
    1 -
    CHARS +         ( newlength addr-c-new )
    OVER            ( newlength addr-c-new newlength )
    PAD 2 CHARS +   ( newlength addr-c-new newlength pad-1st-char )
    SWAP            ( newlength addr-c-new pad-1st-char newlength )
    CMOVE>          ( newlength )
    DUP             ( n n )
    PAD 1 CHARS +   ( n n pad-n-addr )
    C!              ( n )
    PAD 2 CHARS +
    SWAP            ( addrpad n )
;
\
\ test
\
\ HP71B
\ 22 STRING S1
\ " abcdefghijklmnop" S1 S!
\ S1 0 END$ TYPE
\ abcdefghijklmnop OK { 0 }
\ S1 1 END$ TYPE
\ abcdefghijklmnop OK { 0 }
\ S1 5 END$ TYPE
\ efghijklmnop OK { 0 }
\
\ gforth
\ 22 STRING S1  ok
\ S" abcdefghijklmnop" S1 S!   ok
\ S1 0 END$ type abcdefghijklmnop ok
\ S1 1 END$ type abcdefghijklmnop ok
\ S1 5 END$ type efghijklmnop ok
\ S1 18 END$ type warning.. END$ outside string length = garbage gathering
\ --------------------------------------------------------------
\
\
\                test this. check behaviour on HP71B
\ ENG ----------------------------------------------------------
\
\ set the display format.
\ Select engineering display mode with n + 1 significant digits
\  displayed, for 0 <= n <= 11.
\ (n --)
: ENG DUP EDM ! SET-PRECISION  0 SDM ! 0 FDM ! CR CR ." ENG not tested" CR ;
\ --------------------------------------------------------------
\
\
\                (not HP IL so far planned)
\ ENTER --------------------------------------------------------
\
\ HP-IL specific  page 18 of the Forth manual
\ instructs the HP 82401A HP-IL Interface to receive data from an HP-IL device.
\ The HP-IL module puts the bytes received into a temporary location (the HP-71 math stack).
\ The FORTH system then moves the bytes into an address specified by the user when executing ENTER.
\ The byte count and the address of the data are always returned to the user.
: ENTER  CR CR ." ENTER not implemented in gforth" CR ;        
\ --------------------------------------------------------------
\
\
\ EOF ----------------------------------------------------------
\
\ ( -- flag )
\ Return a true flag if there are no more records in the active file; otherwise, return a false flag.
\ EOF examines the record length of the next record in the file specified by the FIB# in SCRFIB. It assumes
\ that the current pointer into the file is pointing at the next record length and that the file is a text file.
: EOF  CR CR ." EOF not implemented in gforth" CR ;
\ returns a true flag if the end of the active file has been reached, a false flag if not.
\ look at gforth file-eof? ( wfileid – flag ) gforth-0.6 “file-eof-query” or others
\ Flag is true if the end-of-file indicator for wfileid is set. 
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ EXECUTE        in gforth, too
\ EXIT           in gforth, too
\ --------------------------------------------------------------
\
\
\ EXPBF --------------------------------------------------------
\
\ expands a specified general purpose buffer by a specified number of nibbles.
\ Expand by n1 nibbles the general-purpose buffer whose ID# is n2, and return a true flag; or return a false
\ flag if such a buffer doesn't exist, if the resulting size would exceed 2K bytes, if there is insufficient
\ memory, or if n1 is negative. n1 must not exceed FFF.
\ ( n1 n2 -- flag ) 
: EXPBF CR CR ." EXPBF not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                               to be tested
\ EXPECT96 -----------------------------------------------------
\
: EXPECT96   ( addr – )
\ accept 96 characters from the keyboard (or less if ENDLINE), 
\ append 2 nulls and store the result at addr and above (greater address)
    \ Copies the text into the command stack.
    BASE @ >R   \ Save the current number base
    DECIMAL     \ Set the number base to decimal
    96 ACCEPT   \   in gforth   accept ( c-addr +n1 – +n2  ) core “accept”
                \               Get a string of up to n1 characters from the user input device and store it at c-addr.
                \               n2 is the length of the received string. The user indicates the end by pressing RET. 
                \               Gforth supports all the editing functions available on the Forth command line
                \               (including history and word completion) in accept. 
    SPAN !
    R> BASE !   \ Restore the original number base
CR CR ." EXPECT96 not tested" CR ;
\ --------------------------------------------------------------
\
\
\ E^X ----------------------------------------------------------
\ tested 19 Sept 2023
\ ( r1 -- exp"r1" )
\    X      Y      Z      T      L 
\    EXP(X) Y      Z      T      X
: E^X    
    \    ." exp(x) stacks in " D_RPNS
    D_?SINIT  FDUP L F! FEXP FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ F- -----------------------------------------------------------
\
\ tested 15 Sept 2023
\ must stay at the end for avoiding mix between new F- and
\ gforth F-  However could have conflict. Use G_ again
\ = F- . Extended for compatibility to fixed stack level of 4 & debug purpose on PC
\ ( r1 r2 -- r1-r2 )
\    X      Y      Z      T      L
\    Y-X    Z      T      T      X
: G_F- D_?SINIT FDUP L F!   F-   X F!   Z F@ Y F!   T F@ Z F!   D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ F* -----------------------------------------------------------
\
\ tested 15 Sept 2023
\ ( r1 r2 -- r1*r2 )
\    X      Y      Z      T      L
\    X*Y    Z      T      T      X
\ = F* later on HP71 filtered by awk
: G_F* D_?SINIT
\    ." f* stacks in 1 " D_RPNS
    FDUP L F! F* X F! Z F@ Y F! T F@ Z F!
    D_RPNREC \ reconstitute the gforth stack based on the RPN stack X Y Z T
\    ." f* stacks out " D_RPNS
;
\ --------------------------------------------------------------
\
\
\ F+ -----------------------------------------------------------
\
\ = gforth F+ . Extended for compatibility to fixed stack level of 4 & debug purpose on PC
\ ( r1 r2 -- r1+r2 )
\    X      Y      Z      T      L 
\    X+Y    Z      T      T      X
: G_F+
    D_?SINIT FDUP L F! F+ X F! Z F@ Y F! T F@ Z F!
    D_RPNREC \ reconstitute the gforth stack based on the RPN stack X Y Z T
;
\ --------------------------------------------------------------
\
\
\ F/ -----------------------------------------------------------
\
\ similar to F/ .
\ Extended for compatibility to fixed stack level of 4 & debug purpose on PC
\ ( r1 r2 -- r1/r2 )
\    X      Y      Z      T      L
\    Y/X    Z      T      T      X
: G_F/
\    ." f/ stacks in " D_RPNS
    D_?SINIT  FDUP L F! F/ X F! Z F@ Y F! T F@ Z F!
    D_RPNREC \ reconstitute the gforth stack based on the RPN stack X Y Z T
;
\ --------------------------------------------------------------
\
\
\ F. -----------------------------------------------------------
\
\ tested 15 Sept 2023
\ update 18 June 2024 F. instead of G_F. for overwritting the gforth F.
\
\ original F. in gforth will throw out the top of the float stack
\ display content of X and dont alter the stack; different from standard forth F.
\ update required for formatting the outputs with SCI ENG
\ see https://gforth.org/manual/Floating_002dpoint-output.html
\ ( -- )
: F. D_?SINIT X F@ F. ;
\ --------------------------------------------------------------
\
\
\ FABS ---------------------------------------------------------
\
\ tested 15 Sept 2023
\ Updated 18 Juni 2024 change from G_FABS to FABS naming
\ overwritting the gforth FABS for updating the simulated
\ RPN stack
\
\ original FABS
\ ( r1 -- abs"r1" )
\    X      Y      Z      T      L 
\    abs(X) Y      Z      T      X
: FABS   
\    ." fabs stacks in " D_RPNS
    D_?SINIT FDUP L F!
    FABS       \ the gforth word is called
    FDUP X F!
\    ." fabs stacks out " D_RPNS
;
\ --------------------------------------------------------------
\
\
\ FCONSTANT ----------------------------------------------------
\ in gforth     fconstant ( r "name" –  ) floating “f-constant”
\ .. use 1.234E0 FCONSTANT PARAMF
\ --------------------------------------------------------------
\
\
\ FDROP --------------------------------------------------------
\
\ tested on 19 Sept 2023
\
\ https://www.forth.com/starting-forth/4-conditional-if-then-statements/
\ original FDROP in HP71B but with another behaviour in gforth
\ due to RPN stack of fixed depth 4
\ ( r -- )
\   X      Y      Z      T      L 
\   Y      Z      T      T      L
: G_FDROP
    \    ." G_FROP in  " D_RPNS
    D_?SINIT
    Y F@ X F! 
    Z F@ Y F!
    T F@ Z F!
    D_RPNREC   \ reconstitute the gforth stack based on the RPN stack X Y Z T
\    ." G_FROP out " D_RPNS
;
\ --------------------------------------------------------------
\
\
\ FENCE --------------------------------------------------------
\ return the address of the variable FENCE which contains
\ the address below which the dictionnary is protected from FORGET
\ .. not a concept in gforth
:  FENCE  CR CR ." FENCE not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ FENTER -------------------------------------------------------
\
\ in EMU71, > UN: FENTER < in forth prompt
\   Word: FENTER
\   LFA: E4BFC      Link: E4B8A 
\   NFA: E4C01      686454E445542D
\   CFA: E4C0F      Primitive
\   OK { 0 } 
\
\ tested 19 Sept 2023
\ similar to gforth FDUP w/o stack move
\ ( t z y r -- z y r r )
\    X      Y      Z      T      L 
\    X      X      Y      Z      L
: FENTER     
    D_?SINIT
    X F@
    Z F@ T F! 
    Y F@ Z F!
    X F@ Y F!
    D_RPNREC  \ reconstitute the gforth stack based on the RPN stack X Y Z T
    ;
\ --------------------------------------------------------------
\
\
\ FILL ---------------------------------------------------------
\ like in gforth  fill ( c-addr u c – ) core “fill”
\                 Store c in u chars starting at c-addr.
\ Fill memory from addr through addr + (2un - 1) with un copies of byte.
\ FILL has no effect if un=O.
\ ( addr un byte -- )
\ --------------------------------------------------------------
\
\
\             what to do with the CFA ?
\ FIND ---------------------------------------------------------
\
\ test EMU71
\ " SEGMENT" . 2- S. COUNT TYPE
\ 7 [ 218255 ] SEGMENT OK { 0 } 
\
\ " SEGMENT" . 2- FIND S.
\ 7 [ 215038 1048575 ]  OK { 2 } 
\
\ " SEGMENT" S. . 2- FIND S.
\ [ 218257 7 ] 7 [ 215038 1048575 ]  OK { 2 } 
\ .
\ -1  OK { 1 }    >> SEGMENT non immediate words.
\
\ LIST .. SEGMENT ..
\
\ " FENTER" S. . 2- FIND S. . HEX .
\ [ 35491 6 ] 6 [ E4C0F FFFFF ] -1 -1B3F1  OK { 0 } 
\
\ " R>" . 2- FIND S.
\ 2 [ E09F0 FFFFF ]  OK { 2 } 
\      
\ ( addr1 -- addr2 n )
\ Search the dictionary (in the currently active search order) for the word contained in the counted string at
\ addr1 If the word is found, FIND returns the word's CFA (= addr2) and either n = 1 (if the word is
\ immediate) or n = -1 (if the word isn't immediate). If the word isn't found, FIND returns addr2 = addr1
\ and n = O.
: FIND find ;
\ --------------------------------------------------------------
\
\
\ FINDBF -------------------------------------------------------
\ ( n -- addr) or (n -- false)
\ Return the start-of-data address in the general-purpose buffer whose ID# is n, or return a false flag if
\ such a buffer doesn't exist.
: FINDBF CR CR ." FINDBF not implemented in gforth" CR ;
\ finds the current address of a specified general purpose buffer.
\ --------------------------------------------------------------
\
\
\ FINDF --------------------------------------------------------
\ ( str -- addr) or (str -- false)
\ Search main RAM for the file whose name is specified by str, and return either the address
\ of the beginning of the file header (if successful) or a false flag (if not).
\ If the specified string exceeds eight characters, FINDF considers only the first eight characters.
: FINDF CR CR ." FINDF not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ FIRST --------------------------------------------------------
\ is a user variable containing the address of the first mass memory buffer in memory.
\ Return the address of the variable FIRST, which contains the address of the first (lowest addressed) mass
\  storage buffer in the FORTHRAM file.
: FIRST CR CR ." FIRST not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ FIX ----------------------------------------------------------
\ tested 2024 June 17
\
\ EMU71
\ 12 FIX
\ OK { 0 } 
\ 1.0 3.0 PERE12
\
\ Param B: 3.00000000000 
\ Param A: 1.00000000000 
\ ELLIPSE PERIM = 13.3648932206  OK { 0 } 
\
\ set the display format.
\ https://gforth.org/manual/Floating_002dpoint-output.html
: FIX ( n - )
    DUP TO FDM SET-PRECISION 0 TO SDM 0 TO EDM ;
\
\ test gforth
\ 12 FIX  ok
\ 0.0E 1.0E 9.0E MAGM  ok
\ F. 3.96407842431  ok
\ S. [ ]  ok
\ .S <0>  ok
\
\ --------------------------------------------------------------
\
\
\                 test this
\ FLITERAL -----------------------------------------------------
\ like in gforth
\ IMMEDIATE, COMPILE. Compile the value x (the contents of the X-register) into the dictionary. When
\ the colon definition is later executed, x will be placed in the X-register, lifting the floating-point stack.
\ : FLITERAL  FLiteral
\    CR CR ." not tested for gforth" CR ;
\ --------------------------------------------------------------
\
\
\ FLUSH --------------------------------------------------------
\ see gforth
\ Unassign all mass storage buffers.
\ --------------------------------------------------------------
\
\
\ FORGET -------------------------------------------------------
\ nothing in gforth.. workaround would be use of MARKER. use tbd
\ https://gforth.org/manual/Forgetting-words.html#index-marker_0028--_0022_003cspaces_003e-name_0022-_002d_002d--_0029-core_002dext
: FORGET  CR CR ." FORGET not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ FORTH --------------------------------------------------------
\ nothing and no need in gforth
\ --------------------------------------------------------------
\
\
\ FP -----------------------------------------------------------
\
( r1 r2 r3 r4,xxx -- r1 r2 r3 0,xxx )  \ https://en.wikipedia.org/wiki/Fractional_part
\ Take fractional part of X into X (and the original X into the L)
\ POSITIVE NUMBER
\ https://en.wikipedia.org/wiki/Floor_and_ceiling_functions
\    X,xxx      Y      Z      T      L 
\    0,xxx      Y      Z      T      X
: FP  D_?SINIT  FDUP L F!
    FDUP          \  rrr.rrr  rrr.rrr
    FTRUNC        \  rrr.rrr  int(rrr.)
    F-            \  0.rrr
    FDUP X F!
;
\ test gforth
\ 1.23456E0 FP F. 0.23456  ok
\ -1.23456E0 FP F. -0.23456  ok
\
\ HP71B
\ 1.23456 FENTER
\ OK { 0 } 
\ FP
\ OK { 0 } 
\ F.
\ .23456  OK { 0 }
\ -2.3456 FENTER FP F.
\ -.3456  OK { 0 } 
\ --------------------------------------------------------------
\
\
\                   test this
\ FSTR$ --------------------------------------------------------
\
\ https://gforth.org/manual/Floating_002dpoint-output.html
\ converts the number in the X-register to a string.
( X -- str )
: FSTR$ 12 5 0 F>STR-RDP CR CR ." FSTR$ not tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ FTOI ---------------------------------------------------------
\ Update 28July2024: INIT & FDUP included
\
\ ( r – n )  \ floating-ext “f-to-s”  gforth like
\ Convert x (the contents of the X-register) to an integer and return it to the data stack. If Ix| > FFFFF, an
\ overflow error occurs. FTOI takes the absolute value of x, rounds it to the nearest integer, and converts it
\ to a five-nibble value. If x was positive, FTOI returns this result; if x was negative, FTOI returns the
\ twos complement of this result.
: FTOI   D_?SINIT FDUP F>S ;
\
\ test gforth
\ D_RPNS 
\
\ HP71B float stack content, RPN HP41 (& gforth stack):
\    X       2.00000     Top gforth      2.00000
\    Y       2.00000                     2.00000
\    Z       5.67800                     5.67800
\    T       0.00000     Bottom gforth   0.00000
\    LASTX   0.00000
\
\  float   stack gforth  
\   <stack depth> bottom to top, of stack
\   <4> 0.000000000000E0 5.678000000000E0 2.000000000000E0 2.000000000000E0 
\
\ integer stack gforth  
\   <stack depth> bottom to top, of stack
\   <1> 5 
\
\ RADIAN ON
\ BASE: 10 
\
\  ok
\ 3.456789E0 FTOI D_RPNS 
\
\ HP71B float stack content, RPN HP41 (& gforth stack):
\    X       3.45679     Top gforth      3.45679
\    Y       2.00000                     2.00000
\    Z       2.00000                     2.00000
\    T       5.67800     Bottom gforth   5.67800
\    LASTX   0.00000
\
\ float   stack gforth  
\   <stack depth> bottom to top, of stack
\   <4> 5.678000000000E0 2.000000000000E0 2.000000000000E0 3.456789000000E0 
\
\ integer stack gforth  
\   <stack depth> bottom to top, of stack
\   <2> 5 3 
\
\ RADIAN ON
\ BASE: 10 
\
\ ok
\
\ test EMU41
\ FS.
\ T: 7 
\ Z: -3 
\ Y: 3 
\ X: 2.3456 
\ L: 1 
\  OK { 0 } 
\ FTOI .       >> integer appear in integer stack
\ 2  OK { 0 } 
\ FS.
\ T: 7 
\ Z: -3 
\ Y: 3 
\ X: 2.3456   >> X not touched
\ L: 1 
\ OK { 0 } 
\ 
\ --------------------------------------------------------------
\
\
\ FVARIABLE ----------------------------------------------------
\ ( "name" –  ) floating “f-variable”     same in gforth
\ --------------------------------------------------------------
\
\
\ GROW ---------------------------------------------------------
\ 
\ ( n -- flag )
\ Enlarge the user dictionary by n nibbles and return a true flag; or if there is insufficient memory, return a
\ false flag (without enlarging the dictionary).
: GROW  CR CR ." GROW not implemented in gforth since not necessary. invoke initialization options at gforth start instead" CR ;
\ --------------------------------------------------------------
\
\
\ H. -----------------------------------------------------------
\ released 17June2024
\
\ ( un - )
\ Display un in base 16 as an unsigned number with one trailing blank.
: H.  hex. ;
\
\ test EMU71
\ 15 H.
\ F  OK { 0 } 
\ 16 H.
\ 10  OK { 0 } 
\
\ test gforth
\ 16 hex. $10  ok
\ 15 hex. $F  ok

\
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ HERE           see gforth, too
\ HEX            see gforth, too      hex ( –  ) core-ext “hex”
\                                           Set base to &16 (hexadecimal).
\ --------------------------------------------------------------
\
\
\ HOLD ---------------------------------------------------------
\ Insert character c into a pictured numeric output string.
\ Used between <# and #>.
\ see gforth, too
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ I                    see gforth, too
\ IF .. THEN           see gforth, too
\ IF .. THEN .. ELSE   see gforth, too
\ IMMEDIATE            see gforth, too
\ --------------------------------------------------------------
\
\
\ INTERPRET ----------------------------------------------------
\ Interpret the input stream to its end, beginning at the offset contained in >IN. The input stream comes
\ from the TIB (if BLK contains 0) or from the mass storage buffer containing the nth line of the active file
\ (if BLKcontains n.)
\ >>>> INTERPRET
: INTERPRET  CR CR ." INTERPRET not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ IP -----------------------------------------------------------
\
\ Take the integer part of the contents of the X-register.
\ IP places the result in the X-register and the
\ original value of x in the LAST X register.
 ( r1 r2 r3 r4,xxx -- r1 r2 r3 0,xxx ) 
\ Take integer part of X into X (and the original X into the L)
\    X,xxx      Y      Z      T      L 
\    X          Y      Z      T      X,xxx
: IP D_?SINIT X F@ FDUP L F! FTRUNC FDUP X F! 
D_RPNREC \ reconstitute the gforth stack based on the RPN stack X Y Z T
;
\ test
\ HP71B
\ 1.2345 IP F.
\ 1  OK { 0 } 
\ -3.4567 IP F.
\ -3  OK { 0 } 
\ --------------------------------------------------------------
\
\
\ ITOF ---------------------------------------------------------
\
\ tested 18June2024
\
\ test EMU71
\ F. X<>Y F. X<>Y 2 ITOF F. X<>Y F.
\ 3.00000000000 8.00000000000 2.00000000000 3.00000000000  OK { 2 } 
\
\ place an integer into the float stack and move it up
( n -- r )  
\    X          Y      Z      T      L 
\    float(n)   X      Y      Z      L
: ITOF D_?SINIT S>F Z F@ T F! Y F@ Z F! X F@ Y F! FDUP X F! 
D_RPNREC \ reconstitute the gforth fstack based on the RPN stack X Y Z T
;
\ test gforth
\ 8.0E 3.0E F. X<>Y F. X<>Y 2 ITOF F. X<>Y F. 3. 8. 
\ 2. 3.  ok
\ D_RPNS 
\ HP71B float stack content, RPN HP41 (& gforth stack):
\   X       3.00000     Top gforth      3.00000
\   Y       2.00000                     2.00000
\   Z       8.00000                     8.00000
\   T       0.00000     Bottom gforth   0.00000
\   LASTX   0.00000
\
\ float   stack gforth  
\  <stack depth> bottom to top, of stack
\  <4> 0.000000000000E0 8.000000000000E0 2.000000000000E0 3.000000000000E0 
\
\ integer stack gforth  
\  <stack depth> bottom to top, of stack
\  <0> 
\
\ RADIAN ON
\ BASE: 10 
\
\ ok
\
\ --------------------------------------------------------------
\
\
\ J ------------------------------------------------------------
\
\ see gforth, too
\ --------------------------------------------------------------
\
\
\ KEY ----------------------------------------------------------
\
\ in gforth key ( – char  ) core “key”
\ Receive (but do not display) one character, char. 
\ --------------------------------------------------------------
\
\
\ KILLBF -------------------------------------------------------
\
\ ( n -- flag )
\ Delete the general-purpose buffer whose ID# is n, and return a true flag; or return a false flag if no such
\ buffer exists.
\ KILLBF       deletes a specified general purpose buffer.
: KILLBF  CR CR ." KILLBF not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ L ------------------------------------------------------------
\
\ Return the address of the floating-point LAST X register.
( -- addr )
\ see fvariable definition above
\ --------------------------------------------------------------
\
\
\ LASTX --------------------------------------------------------
\
\ Lift the floating-point stack and copy the contents of the LAST X register into the X-register.
( r1 -- r1 L )
( RPN: X Y Z T L --   L X Y Z L )
: LASTX ( ." LASTX In  " D_RPNS ) D_?SINIT Z F@ T F! Y F@ Z F! X F@ Y F! L F@ X F! 
D_RPNREC \ reconstitute the gforth fstack based on the RPN stack X Y Z T
;
\ --------------------------------------------------------------
\
\
\                   test on HP71B and gforth
\ LATEST -------------------------------------------------------
\ see gforth
\ https://gforth.org/manual/Name-token.html#index-latest-_0028-_002d_002d-nt-_0029-gforth_002d0_002e6
\ latest ( – nt  ) gforth-0.6 “latest”
\ nt is the name token of the last word defined; it is 0 if the last word has no name. 
\
\ ( -- addr )
\ Return the NFA of the most recent word in the CURRENT vocabulary.
\ >>>> LATEST
\ --------------------------------------------------------------
\
\
\              look at behaviour in HP71B and gforth
\ LEAVE --------------------------------------------------------
\ in gforth
\ https://gforth.org/manual/Counted-Loops.html#index-LEAVE-_0028-compilation-_002d_002d-_003b-run_002dtime-loop_002dsys-_002d_002d-_0029-core
\
\ COMPILE, IMMEDIATE. Skip to the word after the next L00P or +LOOP. LEAVE terminates the loop
\ and discards the control parameters. Used only within a DO ... LOOP or +LOOP construct.
\ >>>> LEAVE
\ --------------------------------------------------------------
\
\
\ LEFT$ --------------------------------------------------------
\
\ tested 2023 11 22
\        2024 01 03  D_PADSET added
\ ( str1 n -- str2 )
\ creates a temporary substring from the first part of a string.
\ Create a temporary string (specified by str2) consisting of the first n characters in the string specified by str1·
: LEFT$  ( addr len n -- addr len') NIP D_PADSET ;
\
\ use ..
\
\ HP71B
\ " DATE$" BASIC$  >>> 23/11/21 into the PAD
\  OK { 2 } 
\ S. >>> [ 213727 8 ]  OK { 2 } 
\ 2 LEFT$ VAL S.
\ [ 23 0 ]  OK { 2 } 
\ DROP S.
\ [ 23 ]  OK { 1 } 
\ --------------------------------------------------------------
\
\
\ LIMIT --------------------------------------------------------
\
\ ( -- addr )
\ is a user variable containing the address of the first byte beyond the mass-memory-buffer  area.
\ Return the address of the variable LIMIT, which contains the first address beyond the mass-storage-buffer area.
\ >>>> LIMIT
: LIMIT CR CR ." LIMIT not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ LINE# ---------------------------------------------------------
\ ( -- addr )
\   is a user variable containing the line number being loaded from the file specified by SCRFIB.
\ Return the address of the variable LINE#, which contains the number of the line being loaded from the
\   active file (specified by SCRFIB).
\ >>>> LINE#   
: LINE#  CR CR ." LINE# not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ LISTING ------------------------------------------------------
\ ( -- str )
\ Return str specifying the contents of the string variable LISTING, which identifies the file or device to
\ which the assembler will direct its output. LISTING can contain up to 20 characters.
\ >>>> LISTING
: LISTING  CR CR ." LISTING not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ LITERAL ------------------------------------------------------
\
\ in gforth, too
\ COMPILE, IMMEDIATE. Compile n into the word being defined,
\ such that n will be returned when the word is executed.
\ ( n -- )
\ LITERAL              
\ .. use
\ (Forth83) https://forth.sourceforge.net/standard/fst83/fst83-12.htm#literal
\ [ 16b ] LITERAL
\ Compiles a system dependent operation so that when later executed, 16b will be left on the stack.
\
\ (gforth)
\ https://gforth.org/manual/Literals.html#index-Literal_0028--compilation-n-_002d_002d-_003b-run_002dtime-_002d_002d-n--_0029-core
\ between [ and ] it could be the number 2 CELLS
\ --------------------------------------------------------------
\
\
\ LN -----------------------------------------------------------
\
( r1 -- ln"r1" )   
\   natural log
\   X      Y      Z      T      L
\   LN(X)  Y      Z      T      X
: LN    D_?SINIT    FDUP L F!   FLN FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ LOADF --------------------------------------------------------
\ Interpret the entire file specified by str. If the file cannot be opened for any reason (doesn't exist, wrong
\ type, already opened, etc.), LOADF will give the error message FTH ERR: filename cannot loadd.
\ ( str -- )
\ >>>> LOADF
: LOADF  CR CR ." LOADF not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ LGT ----------------------------------------------------------
\
( r1 -- log"r1" )  
\   base 10 log
\   X      Y      Z      T      L
\   LN(X)  Y      Z      T      X
: LGT  D_?SINIT FDUP L F! FLOG FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ LOOP ---------------------------------------------------------
\ tested with SNAKE.fth on 11 Oct 2023
\
\ : loop ( do-sys | orig do-sys xt.then -- )
\  dup ['] then <> if  [compile] loop exit then
\  >r recurse r> execute
\ ; immediate
\
\ : +loop ( do-sys | orig do-sys xt.then -- )
\  dup ['] then <> if  [compile] +loop exit then
\  >r recurse r> execute
\ ; immediate
\ --------------------------------------------------------------
\
\
\ --------------------------------------------------------------
\ DO ... LOOP    in gforth, too
\ M*             in gforth
\ --------------------------------------------------------------
\
\
\                           test this
\ M/ -----------------------------------------------------------
\ Divide the double number d by the single number n1, and return the single-number remainder n2 and the
\ single-number quotient n3. All numbers are signed.
\ ( d n1 -- n2 n3 )
: M/ FM/MOD CR CR ." M/ not tested in gforth" CR  ;
\ --------------------------------------------------------------
\
\
\                            test this
\ M/MOD --------------------------------------------------------
\ Divide the double number ud1 by the single number un1, and return the single-number remainder un2 and
\ the double-number quotient ud2. All numbers are unsigned.
\ ( ud1 un1 -- un2 ud2 )
: M/MOD UD/MOD CR CR ." M/MOD not tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ MAKEBF -------------------------------------------------------
\ creates a general purpose buffer of a specified size.
\ Create a buffer n nibbles long and return a true flag, the buffer ID#, and the address of the beginning of
\ data area in the buffer; or if unsuccessful (not enough memory, no free buffer ID#s), return a false flag.
\ n cannot exceed 4095(10).
\ ( n -- addr ID# true )
\ ( n -- false )
\ >>>> MAKEBF
: MAKEBF  CR CR ." MAKEBF not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ MAX ----------------------------------------------------------
\
\ in gforth
\ Return the greater of n1 and n2.
\ ( n1 n2 -- n3 )
\ --------------------------------------------------------------
\
\
\ MAXLEN -------------------------------------------------------
\
\ tested 14 Oct 2023
\   modif 22 Nov 23: CHAR instead of CELL
\ returns the maximum allocated length of a string. MAX 255
\ Return the maximum length (that is, bytes of memory allotted in the dictionary)
\ for the string specified by str.
\ ( str -- n )
\ : MAXLEN DROP 2 CELLS - @ ;
 : MAXLEN DROP 2 CHARS - C@ ;
\
\ use
\
\ HP71B
\   " ABCD" MAXLEN . >>> 80  OK { 0 }
\ 50 STRING S2 " ABC" S2 S!
\ S2 MAXLEN . >>> 50  OK { 0 }
\ 300 STRING S3 >>> OK { 0 } 
\ " ABCD" S3 S! >>> OK { 0 } 
\ S3 MAXLEN . >>> 255  OK { 0 } 
\
\ gforth
\ 50 STRING S2 S" ABC" S2 S!   ok
\ s2 maxlen . 50  ok
\ s" abcd" maxlen . 0  ok  .. SHOULD BE 80 ..  if not in dictionnary  s" abcd" D_PADSET will solve this
 \ S" XYZTUVWXOOO" D_PADSET MAXLEN S. [ 80 ]  ok
\ 300 STRING S3 >>> length reduced to maximum 255  ok
\ s3 maxlen s. [ 255 ]  ok 
\ --------------------------------------------------------------
\
\
\ MIN ----------------------------------------------------------
\ in gforth too
\ Return the smaller of n1 and n2
\ ( n1 n2 -- n3 )
\ --------------------------------------------------------------
\
\   
 \ MOD ----------------------------------------------------------
 \
\ Divide n1 by n2 and return the remainder n3 with the same sign as n1
\ ( n1 n2 -- n3 )
\ HP71B
\ -10  3 MOD .  \ Output will be -1
\ 10 -3 MOD .   \ Output will be 1
\ 10  3 MOD .   \ Output will be 1
\ -10 -3 MOD .  \ Output will be -1
: MOD 2DUP 0< IF 0< IF /MOD DROP
	            ELSE
	              2DUP /MOD DROP - NEGATE SWAP DROP
		    THEN
              ELSE
		  0< IF 2DUP /MOD DROP - NEGATE SWAP DROP
		  ELSE
		      /MOD DROP
		      THEN
              THEN ;
\ different in gforth, which is positive -> see PMOD new word
\   copying the gforth MOD word behaviour
\
\   gforth MOD
\    -10 3 mod . 2  ok
\    10 -3 mod . -2  ok
\    10 3 mod . 1  ok
\    -10 -3 mod . -1  ok
\ --------------------------------------------------------------
\
\
\                      test on HP71B
\ N@ -----------------------------------------------------------
\
: N@ ( addr -- n )  \ return the contents of the nibble at addr. The four high-order nibbles of n are zero
  BASE @ >R   \ Save the current number base
  HEX     \ Set the number base to HEX
  C@       \ Read the byte at the address  
  0xF AND  \ Mask out the four high-order nibbles
  R> BASE !   \ Restore the original number base
CR CR ." N@ not tested widely in gforth" CR ;
\ test on emulator 71B gives..n@
\ " ABCDEFGH" ret OK { 2 }
\ . ret 8  (length 8)
\ . ret 202957  addr where the first char is
\ 202957 N@ ret OK { 1 }
\ . ret  1  which is the low byte of A which is "41" in HEX
\
\ test on gforth prompt
\ S" ABCDEFGH"  ok
\ drop  ok
\ N@  ok
\ . 1  ok
\ --------------------------------------------------------------
\
\
\                             test this
\ N! -----------------------------------------------------------
\ Store at addr the low-order nibble of n.
\ ( n addr -- )
: N!
    BASE @ >R   \ Save the current number base
    DECIMAL     \ Set the number base to decimal
    15 AND SWAP C!
    R> BASE !   \ Restore the original number base
CR CR ." N! not tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ NALLOT -------------------------------------------------------
\ tested 2024 June 7 in SNAKE
\ Add n nibbles to the parameter field of the most recently defined word
\ (regardless of the CURRENT and CONTEXT vocabularies).
\ ( n -- )
: NALLOT ALLOT ;
\ this is the equality as long CHARS and CELLS are used before
\ --------------------------------------------------------------
\
\
\ NEGATE -------------------------------------------------------
\
\ ( n -- -n )
\ see gforth, too
\ --------------------------------------------------------------
\
\
\                        test this
\ NFILL --------------------------------------------------------
\
\ Fill memory from addr through addr + (un - 1) with un copies
\ of the low-order nibble in n. NFILL has no effect if un = O.
\ ( addr un n -- )
: NFILL  FILL CR CR ." NFILL not tested in gforth" CR ;
\ .. use ?
\ --------------------------------------------------------------
\
\
\ NMOVE --------------------------------------------------------
\ Move un nibbles, first moving the nibble at addrl to addr2 and
\ finally moving the nibble at addr1 + (un - 1) to addr2 + (un - 1).
\ NMOVE has no effect if un = o.
\ ( addr1 addr2 un -- )
\ >>>> NMOVE
: NMOVE  CR CR ." NMOVE not implemented in gforth" CR ;
\ .. use ?
\ --------------------------------------------------------------
\
\
\ NMOVE> -------------------------------------------------------
\ Move un nibbles, first moving the nibble at addrl + (un -1)
\ to addr2 + (un -1) and finally moving the nibble at addr1 to addr2.
\ NMOVE> has no effect if un = o.
\ ( addr1 addr2 un -- )
\ >>>> NMOVE>
: NMOVE>  CR CR ." NMOVE> not implemented in gforth" CR ;
\ .. use ?
\ --------------------------------------------------------------
\
\ 
\ NOT ----------------------------------------------------------
\ tested 2024 June 7 on gforth and HP71B
\ Return the ones complement (true Boolean NOT) of n1.
\ ( n1 -- n2 )
( b -- b )
: NOT  TRUE XOR ;
\ --------------------------------------------------------------
\
\
\                         test this
\ NULL$ --------------------------------------------------------
\ create a temporary string (specified by str) in the pad,
\ with maximum length 80 and current length 0
\ ( -- str)
: NULL$ S" " D_PADSET
CR CR ." NULL$ not tested in gforth" CR ;
\
\ use ..
\ ENU71
\   NULL$ .S >>> 0 213599  OK { 2 } 
\   MAXLEN . >>> 80  OK { 0 }
\
\ gforth
\   null$ maxlen s. [ 80 ]  ok
\ --------------------------------------------------------------
\
\
\                         to be worked out
\ NUMBER -------------------------------------------------------
\
 ( addr -- d )
 ( addr -- )
: NUMBER  
\ examine the counted string at addr and convert it into a double number d
    \ REMARK: counted string address is where the n is
    \         NOT the first character address
    \ low mem                high mem
    \ max-length n-addr addr-first-char..
\           - if the string contain a ".", NUMBER tries to convert to float and put it in X and move the float stack
\             if the string contain a "." but is not a valid float , it will show a data type error
\           - if no "." it will try to convert into an integer. 
\             if it is not a valid integer , it will show a NUMBER not recognized error
    \
    \ input will be  in gforth, S" ( compilation ’ccc"’ – ; run-time – c-addr u  ) core,file “s-quote”
    \ tested in gforth
    \ .. integer path
    \ S" 123456" DECIMAL .S <2> 94769981895760 6  ok
    \ 2DUP  ok
    \ S>NUMBER?  ok
    \ . -1  ok
    \ 2NIP  ok
    \ .S <2> 123456 0  ok
    \ .. float path
    \ S" 1.23456E0" DECIMAL .S <2> 94769981895728 9  ok
    \ >FLOAT  ok
    \ . -1  ok
    \ F.S <1> 1.234560000000E0  ok
    \ .S <0>  ok
\
    BASE @ >R   \ Save the current number base
    \
    DECIMAL     \ Set the number base to decimal
    \
    DUP        \ duplication for a second try with float   ( addr addr )
    1 CHAR    
    S>NUMBER?   \ Convert the string to a number integer    ( addr u d f )
    \ gforth  s>number? ( addr u – d f  ) 
    \ converts string addr u into d, flag indicates success 
    0=          \ no success
    IF
	." issue reading the integer number in the string" 2DROP   ( addr u )
	>FLOAT 
	\ >float ( c-addr u – f:... flag ) floating “to-float”
	\ Actual stack effect: ( c_addr u – r t | f ). Attempt to convert the character string c-addr u to internal floating-point representation.
	\ If the string represents a valid floating-point number, r is placed on the floating-point stack and flag is true.
	\ Otherwise, flag is false. A string of blanks is a special case and represents the floating-point number 0.
	\ test gforth
	\ S" 1.234567E0"  ok
	\ >FLOAT  ok
	\ . -1  ok
	\ F.S <1> 1.234567000000E0  ok
	0=
	IF
	    ." issue reading the float number in the string" EXIT
	ELSE
	    D_SSET    \ update the 4 level stack with number in X
	THEN
    ELSE
      2NIP
    THEN   
R> BASE !   \ Restore the original number base
CR CR ." NUMBER to be worked out in gforth" CR ;
\ use..
\
\ HP71B
\
\ wrong try with wrong address
\ " 1234" NUMBER . >>> FTH ERR:NUMBER not recognized
\ " 1.2345" NUMBER . >>> FTH ERR:NUMBER not recognized
\ PAD 1 CHARS + COUNT TYPE >>> 1.2345 OK { 0 } 
\ PAD 1 CHARS + COUNT S. >>> [ 213727 6 ]  OK { 2 }
\
\ " 1.2345" S. >>> [ 213727 6 ]  OK { 2 } 
\ PAD 1 CHARS + NUMBER F. >>> 1.2345  OK { 1 } 
\
\ .. the address for COUNT is the correct
\ PAD 1 CHARS + . >>> 213725  OK { 0 }  ..and NOT  213727
\
\ " 1.234R"
\ PAD 1 CHARS + NUMBER  >>> ERR:Data Type
\ S. >>> [ ]  OK { 0 } 
\
\ " 12345" DROP 1 CHARS - NUMBER S. >>> [ 12345 0 ]  OK { 2 } 
\ DROP S. >>> [ 12345 ]  OK { 1 } 
\
\ " 12345R" DROP 1 CHARS - NUMBER S. >>> FTH ERR:NUMBER not recognized
\
\ gforth
\ address testing so far..
\ S" 1234" drop 1 cells - count type 1234�q ok
\ S" 1234" DROP 1 CELLS - DUP @ .  33  ok       
\ S" 1234" DROP 1 CHARS - DUP @ .  224197226752  ok
\ S" 1234" MAXLEN . 0  ok
\ S" 1234" s. [ 94345548754592 4 ]  ok
\ S" 1234" drop 1 cells - count s. [ 94345548754617 33 ]  ok
\
\ --------------------------------------------------------------
\
\
\ OKFLG --------------------------------------------------------
\ Return the address of the variable OKFLG. If the value of OKFLG is 0, the 0K (n) message is shown
\ when the FORTH system is ready for input; otherwise, the message is suppressed.
\ ( -- addr )
\ >>>> OKFLG
: OKFLG  CR CR ." OKFLG not implemented in gforth" CR ;
\ .. use ?
\ 1 OKFLG !  deactivate the OK { n } message and the screen
\ dont change after a CR
\ -1 deactivate, too
\ 0 OKFLG !  activate it
\ --------------------------------------------------------------
\
\
\ ONERR --------------------------------------------------------
\ Return the address of the variable ONERR, which contains the CFA of the user's error routine.
\ The value of ONERR is checked when a FORTH-system error occurs.
\ If the value of ONERR is zero, the error is processed by the system's error routine.
\ If the value of ONERR is not zero, control is transferred instead to the user's error routine.
\ The stacks are not reset. The BASIC keywords F0RTH and FORTHX set the value of ONERR to zero.
\ ( -- addr )
\ >>>> ONERR         The user variable ONERR contains the CFA of a word to execute when an error occurs
: ONERR  CR CR ." ONERR not implemented in gforth" CR ;
\               Page 71 of the manual show the error nb and messages
\ .. use ?
\ --------------------------------------------------------------
\
\
\ OPENF --------------------------------------------------------
\ opens a FIB entry for a specified file.
\ Open an FIB for the file whose name is specified by str, and store the FIB# into SCRFIB. If successful,
\ OPENF returns a true flag. If the file was empty or there was a problem in opening the file, 0PENF
\ returns str and a false flag.
\ ( str -- f )
\ ( str -- str f )
\ >>>> OPENF
: OPENF  CR CR ." OPENF not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ OR -----------------------------------------------------------
\ Like in GFORTH
\ --------------------------------------------------------------
\
\
\             partially implemented
\ OUTPUT -------------------------------------------------------
\ HP-IL Page 18 of Forth manual.
\ instructs the HP 82401A HP-IL Interface to send data to an
\ HP-IL device. The user supplies a byte count and the address
\ of the data to be output.
\ ( addr n -- )
( str -- )
: OUTPUT
    DUP ( addr n n -- )
    2 = IF   ( addr n -- ) ( if 2 check if this is perhaps a reset, then use PAGE, if not, use perhaps TYPE )
	2DUP ( addr n addr n )
	C@+  ( addr n addr+ n char )
	27 = IF  ( addr n addr+ n )
	    C@+  ( addr n addr++ n char )
	    69 = IF  ( addr n addr++ n )
		2DROP 2DROP PAGE ELSE 2DROP
		( )
		( take TYPE below out during debugging on gforth      )
		( or developp a decoding word for making TYPE working )
				TYPE
		( )
	    THEN THEN
    ELSE
		( )
		( take TYPE below out during debugging on gforth      )
		( or developp a decoding word for making TYPE working )
				TYPE
		( )
    THEN ;
\ --------------------------------------------------------------
\
\
\ OVER ---------------------------------------------------------
\ in gforth, too
\ --------------------------------------------------------------
\
\                             
\ PAD ----------------------------------------------------------
\ tested 22 nov 2023
\ Return the address of the pad, which is a scratch area used
\ to hold >> character strings << for intermediate processing.
\ see page 30 of the Forth71 manual: Changes when words are compiled or executed.
\ ( -- addr )
\
\ D_PADSET necessary on gforth because
\
\ on HP71B, all " ABCD" strings are included into the PAD
\   because they are temporary strings
\   LEFT$ RIGHT$ SUB$ are other creators of temporary strings
\ on gforth, S" ABCD" dont put on the PAD, therefore all
\   S" strings created for further use with PAD help
\   (not only to be showned on the screen
\   or stored immediatly after)
\ see above
\
\ slight different in gforth     pad ( – c-addr  ) core-ext “pad”
\               c-addr is the address of a transient region that can be used as temporary data storage.
\                                At least 84 characters of space is available. 
\ use .. 
\
\ (HP71B)
\ PAD .
\ 2FD0B (subject to change; see forth manual page 30)
\
\ " ABCDEFGH"
\ OK { 2 }
\ PAD 10 TYPE
\ PABCDEFGH OK { 0 }
\
\ PAD 1 CHARS + COUNT           
\ OK { 2 } 
\ S.
\ [ 213599 8 ]  OK { 2 } 
\ TYPE
\ ABCDEFGH OK { 0 }
\
\ PAD 1 CHARS + COUNT TYPE
\ ABCDEFGH OK { 0 }
\
\ PAD 1 CHARS + COUNT MAXLEN .
\ 80  OK { 0 }
\
\ PAD 1 CHARS + COUNT TYPE
\ EFGHIJ OK { 0 }
\ .. 88 is ascii X
\ PAD 2 CHARS + 88 SWAP C!
\ OK { 0 } 
\ PAD 1 CHARS + COUNT TYPE
\ XFGHIJ OK { 0 } 
\ 
\ " ABCD" S. PAD .
\ [ 213727 4 ] 213723  OK { 2 } 
\
\ (gforth)
\ .. tbd
\
\ --------------------------------------------------------------
\
\
\ PAGESIZE -----------------------------------------------------
\ Return the address of the variable PAGESIZE, which contains the number
\ of printed lines per page for the assembler listing.
\ The default value is 56; the minimum value is 8.
\ ( -- addr )
: PAGESIZE  CR CR ." PAGESIZE not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
: PI PI D_SSET ;
\
\ PICK ---------------------------------------------------------
\ tested 8 Nov 2023
\ different version in gforth
\ Return a copy of the nrth entry on the data stack (not counting n1 itself). For example, 1 PICK is
\ equivalent to DUP, and 2 PICK is equivalent to 0VER.
\ ( n1 -- n2 )
\ use from gforth    pick ( S:... u – S:... w ) core-ext “pick”
\                    Actually the stack effect is x0 ... xu u -- x0 ... xu x0 .
\ tested ..
\
\ HP71B
\ 1 2 3 4 5 4 PICK .S
\ 2 5 4 3 2 1  OK { 6 } 
\ S.
\ [ 1 2 3 4 5 2 ]  OK { 6 }
\
\ below in gforth
\ 1 2 3 4 5 4 PICK .S <6> 1 2 3 4 5 2  ok
\ S. [ 1 2 3 4 5 2 ]  ok
\
: PICK 1 - PICK ;
\ -------------------------------------------------------------
\
\                          
\ POS ----------------------------------------------------------
\ tested 20 nov 2023
\
\ Search the string specified by str2 for a substring that matches
\ the string specified by str1, and return the position of the
\ first character in the matching substring (or a false flag if there is no matching substring).
\ ( str1 str2 -- n )  or  ( str1 str2 -- false )
: POS  2DUP 2ROT    ( str2 str2 str1 )
    SEARCH     ( addr2 n2 addr3 n3 flag )
    IF DROP    ( addr2 n2 addr3 )
	SWAP   ( addr2 addr3 n2 )
	DROP   ( addr2 addr3 )
	SWAP - ( addr3-addr2 ) 
	1 CHARS / 1 + ( nCharsPos )
	ELSE 2DROP 2DROP FALSE
	THEN ;
\ returns the position within a string of a substring.
\
\ use the gforth
\   search ( c-addr1 u1 c-addr2 u2 – c-addr3 u3 flag  ) string “search”
\   Search the string specified by c-addr1, u1 for the string specified by c-addr2, u2.
\   If flag is true: match was found at c-addr3 with u3 characters remaining.
\   If flag is false: no match was found; c-addr3, u3 are equal to c-addr1, u1.
\
\ .. test for use
\    
\ HP71B
\ 22 STRING S1 " ABCDEFGHIJ" S1 S! " GH" S1 POS S.
\ [ 7 ]  OK { 1 } 
\
\ gforth
\ 22 STRING S1 S" ABCDEFGHIJ" S1 S! S" GH" S1 POS S. [ 7 ]  ok
\ 22 STRING S1 S" ABCDEFGHIJ" S1 S! S" XX" S1 POS S. [ 0 ]  ok
\
\ --------------------------------------------------------------
\
\
\ PREV ---------------------------------------------------------
\ is a user variable containing the address of the mass memory buffer last used.
\ Return the address of the variable PREY, which contains the
\ address of the most recently referenced mass storage buffer.
: PREV  CR CR ." PREV not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\               no use because no HP-IL on PC w/ gforth
\ PRIMARY ------------------------------------------------------
\ PRIMARY and SECONDARY, specify the intended device for 0UTPUT and ENTER.
\ Default contents of the variables are 1 for PRIMARY and 0 for SECONDARY.
\ The user must ensure that these variables are properly set up before executing ENTER or OUTPUT.
\ Return the address of the variable PRIMARY, which specifies an HP-IL address. The valid range for
\ PRIMARY is 0 through 31, and the default value is 1. (The contents of PRIMARY and SECONDARY
\ specify which HP- IL device to use with ENTER and 0UTPUT. If system flag - 22 is clear, the contents of
\ PRIMARY alone specify a simple address; if system flag -22 is set, the contents of PRIMARY and
\ SECONDARY specify an extended address.)
VARIABLE PRIMARY
\ .. use
\ --------------------------------------------------------------
\
\
\         test this query  (expect) vs gforth refill / accept )
\ QUERY --------------------------------------------------------
\ Accept characters from the current keyboard until 96 characters are received or an (END LINE )
\ character is encountered, and store them in the TIB. 0UERY sets #TIB to the value of SPAN
\
\ gforth REFILL
\ refill ( – flag  ) core-ext,block-ext,file-ext “refill”
\ Attempt to fill the input buffer from the input source.
\ When the input source is the user input device, attempt to receive input into the terminal input device.
\ If successful, make the result the input buffer, set >IN to 0 and return true; otherwise return false.
\ When the input source is a block, add 1 to the value of BLK to make the next block the input source and current input buffer,
\ and set >IN to 0; return true if the new value of BLK is a valid block number, false otherwise.
\ When the input source is a text file, attempt to read the next line from the file.
\ If successful, make the result the current input buffer, set >IN to 0 and return true; otherwise, return false.
\ A successful result includes receipt of a line containing 0 characters.
\ holds len and address of input buffer
\
\ ( -- )
: QUERY REFILL DROP ;
\ --------------------------------------------------------------
\
\
\                                test this
\ QUIT ---------------------------------------------------------
\ Clear the return stack, set execution mode, and return control to the keyboard.
\ No message is displayed.
\ ( -- )
\ QUIT        see gforth
\ --------------------------------------------------------------
\
\
\ R> -----------------------------------------------------------
\ in gforth, too
\ COMPILE. Remove n from the top of the return stack and return a copy to the data stack.
\ ( -- n )
\ --------------------------------------------------------------
\
\
\                             test this
\ R@ -----------------------------------------------------------
\ in gforth, too
\ COMPILE. Return a copy of the number on the top of the return stack.
\ ( -- n )
\ gforth   r@ ( – w ; R: w – w  ) core “r-fetch”
: R@
    R@
    CR CR ." R@ to be tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ RADIANS ------------------------------------------------------
\
\ tested 21 Sept 2023
\ Select RADIANS angular mode.
: RADIANS  0 TO DEG -1 TO RAD  ; 
\ --------------------------------------------------------------
\
\
\ RCL ----------------------------------------------------------
\
\ tested with PERE12
\ lift the float stack and place in the X register the number found at addr
\ ( f-addr -- r )
\    X              Y            Z      T      L   = GFORTH STACK w/o L
\    (f-addr)->X    Xbefore      Y      Z      L   = updated GFORTH Stack 
: RCL
    \    ." RCL stacks in " D_RPNS
    D_?SINIT
    F@
    D_?SINIT
\    ." RCL stacks out " D_RPNS
;
\ --------------------------------------------------------------
\
\
\ RCLIND -------------------------------------------------------
\
\ lift the float stack and place in the X register the number found at ind addr
\ ( f-addr -- r )
\    X              Y            Z      T      L   = GFORTH STACK w/o L
\    (f-addr)->X    Xbefore      Y      Z      L   = updated GFORTH Stack 
: RCLIND
    \    ." RCL stacks in " D_RPNS
    D_?SINIT
    DUP   ( addr addr  -- )
    DUP   ( addr addr addr -- )
    F@ F>S  ( addr addr N -- )
    SWAP      ( addr N addr -- )
    CELLS 8 = IF 1 CELLS ELSE 16 THEN
    -
    @    ( addr N POS -- )
    -   ( addr POS-N -- )
    CELLS 8 = IF 1 CELLS ELSE 16 THEN
    2 *
    *
    +
    F@
    D_?SINIT
\    ." RCL stacks out " D_RPNS
;
\ --------------------------------------------------------------
\
\
\ RDN ----------------------------------------------------------
\
\ tested 19 Sept 2023
\ ( r1 r2 r3 r4 -- r4 r1 r2 r3 )
\    X      Y      Z      T      L 
\    Y      Z      T      X      L
: RDN
\    ." RDN In  " D_RPNS
    D_?SINIT
    X F@ TB F!
    Y F@ X F!
    Z F@ Y F!
    T F@ Z F!
    TB F@ T F!
    D_RPNREC \ reconstitute the gforth fstack based on the RPN stack X Y Z T
\    ." RDN Out  " D_RPNS
;
\ --------------------------------------------------------------
\
\
\ RIGHT$ -------------------------------------------------------
\ creates a temporary substring of specified length from the last part of a string.
\ Create a temporary string (specified by str2) consisting of the last (rightmost) n characters in the string
\ specified by str1 (END$ is similar but takes character position, not substring length, for a parameter.)
\ ( str1 n -- str2 )
\ from here https://github.com/bfox9900/CAMEL99-ITC/blob/master/LIB.ITC/STRING%2B.FTH
\
\ /string ( c-addr1 u1 n – c-addr2 u2 ) string “slash-string”
\ Adjust the string specified by c-addr1, u1 to remove n characters from the start of the string. 
\
: RIGHT$ ( addr len n -- addr len')
    OVER       ( addr len n len )
    SWAP       ( addr len len n )
    -          ( addr len len-n )
    /STRING    ( addr2 n )
    D_PADSET ;
\ use..
\ HP71B
\ 213727 8 2 RIGHT$ VAL DROP S.
\ [ 21 ]  OK { 1 } 
\ PAD S. >>> show the addr of the counted string with n in it
\ [ 21 213723 ]  OK { 2 } 
\ --------------------------------------------------------------
\
\                         
\ ROLL ---------------------------------------------------------
\
\ already in gforth but different by 1 position
\   see gforth ( x0 x1 .. xn n – x1 .. xn x0  ) core-ext “roll”
\ in HP71B
\ Move the nth entry on the data stack (not counting n itself) to the top of the stack. For example,
\ 2 ROLL is equivalent to SWAP, and 3 ROLL is equivalent to ROT.
\ ( n -- )
: ROLL 1 - ROLL  ;
\ this roll ihere is the 
\ .. use
\ test HP71B
\ 1 2 3 4 3 ROLL
\  OK { 4 } 
\ 2 4 3 1  OK { 4 } 
\
\ test gforth
\ 1 2 3 4 2 ROLL  ok
\ .S <4> 1 3 4 2  ok
\
\ --------------------------------------------------------------
\
\
\ ROT ----------------------------------------------------------
\
\ in gforth
\ Rotate the top three entries on the data stack, bringing the deepest to the top of the stack.
\ ( n1 n2 n3 -- n2 n3 n1 )
\ ROT like in gforth
\ --------------------------------------------------------------
\
\
\ RP! ----------------------------------------------------------
\
\ in gforth too
\ Reset the return stack to 0 addresses.
\ ( -- )
\ RP!
\ gforth  rp! ( a-addr – ) gforth-0.2 “rp-store”
\ .. use ?
\ --------------------------------------------------------------
\
\
\ RP@ ----------------------------------------------------------
\
\ in gforth too
\ Return the current value of the return-stack pointer.
\ ( -- addr )
\ RP@
\ gforth   rp@ ( – a-addr ) gforth-0.2 “rp-fetch”
\ .. use ?
\ --------------------------------------------------------------
\
\
\ RP0 ----------------------------------------------------------
\
\ Return the address of the system variable RPO, which contains the address of the bottom of the return
\ stack. (The bottom of the return stack has a greater address than the top.)
\ RP0    same in gforth
\ --------------------------------------------------------------
\
\
\ RUP ----------------------------------------------------------
\
\ tested 19 sept 2023
\ Roll up the floating-point stack. RUP copies from the X-register into the Y -register, from the Y -register
\ into the Z-register, from the Z-register into the T-register, and from the T-register into the X-register.
\ ( r1 r2 r3 r4 -- r4 r1 r2 r3 )
\    X      Y      Z      T      L 
\    T      X      Y      Z      L
: RUP ( ." RUP In  " D_RPNS ) D_?SINIT  T F@ TB F!  Z F@ T F!  Y F@ Z F!  X F@ Y F!  TB F@ X F!  
D_RPNREC \ reconstitute the gforth fstack based on the RPN stack X Y Z T
( ." RUP Out  " D_RPNS ) ;
\ --------------------------------------------------------------
\
\
\ S! -----------------------------------------------------------
\
\ tested on gforth and HP71B
\   modif 22 nov 2023 chars instead of cells
\ store the content of string specified by str1
\ into the string specified by str2 (str2 length become the length of str1)
\ str is addr n  where n number of characters and addr the address of the first character
( addr1 n1 addr2 n2 -- )
: S!
    2DUP    ( addr1 n1 addr2 n2 addr2 n2 )
    MAXLEN  ( addr1 n1 addr2 n2 n2max )
    4 PICK  ( addr1 n1 addr2 n2 n2max n1 )
    < IF ." FTH ERR:S! string won't fit" CR DROP DROP DROP DROP      
	ELSE
	    \
    DROP           \ addr1 n1 addr2 
    \  take the length away from str2: ASSUMED for now n1 <= n2max
    \ NO CHECK for memory leaks
    DUP            \ addr1 n1 addr2 addr2
    1 CHARS -      \ addr1 n1 addr2 addr2-1
    ROT            \ addr1 addr2 adr2-1 n1
    2DUP           \ addr1 addr2 adr2-1 n1 adr2-1 n1
    SWAP           \ addr1 addr2 adr2-1 n1 n1 adr2-1
    !              \ addr1 addr2 adr2-1 n1
    SWAP DROP      \ addr1 addr2 n1
    CMOVE> THEN ;
    \  in gforth    cmove ( c-from c-to u – ) string “c-move”
    \               Copy the contents of ucount characters from data space at c-from to c-to.
    \               The copy proceeds char-by-char from low address to high address;
    \               i.e., for overlapping areas it is safe if c-to<=c-from. 
\ .. use/test gforth
\ DECIMAL 10 STRING STR1  ok
\ STR1  ok
\ . 0  ok
\ . 140528396303280  ok
\ S" ABCD" STR1 S!  ok
\ .S <0>  ok
\ STR1  ok
\ .S <2> 140528396303280 4  ok
\ TYPE ABCD ok
\
\ .. use HP71B
\ DECIMAL 10 STRING S1
\ " 1234567" S1 S!
\ S1 TYPE (-> output 1234567)
\ DECIMAL 22 STRING S2
\ " ABCDEFGHIJKLMN" S2 S!
\ S2 TYPE (-> output ABCDEFGHIJKLMN )
\ S1 TYPE (-> output 1234567)
\
\ 10 STRING STR5
\ OK { 0 } 
\ " ABCDE" STR5 S!
\ OK { 0 } 
\ STR5 TYPE
\ ABCDE OK { 0 } 
\ " 12" STR5 S!
\ OK { 0 } 
\ STR5 TYPE
\ 12 OK { 0 } 
\ --------------------------------------------------------------
\
\
\ S->D ---------------------------------------------------------
\
\ tested 19 Sept 2023
\ ( n -- d )
\ Return a signed double number d with the same value and sign as the signed single number n.
: S->D S>D ;
\ --------------------------------------------------------------
\
\
\ S0 -----------------------------------------------------------
\
\ tested on 21 sept 2023
\ ( HP71B manual ) Return the address of the bottom of the data stack.
\ ( -- addr) 
: S0  SP0 @  ;  \  see Brodie P215, not in gforth (see SP0 for this)
\ .. use
\ page 30 of the manual: changes when GROW or SHRINK is executed.
\ --------------------------------------------------------------
\
\
\                          test this
\ S< -----------------------------------------------------------
\
\ Returns a true flag if string1 < string2, a false flag if not.
\ Return a true flag if the string specified by strl is "less than" the string specified by str2,
\ or a false flag if not. S< first compares the ASCII values of the first characters; if they are equal, it then compares the
\ second characters, and so on. ABC is defined to be less than ABCD.
\ ( str1 str2 -- flag )
: S< STR<
    CR CR ." S< to be tested in gforth" CR ;
\ gforth  str< ( c-addr1 u1 c-addr2 u2 – f  ) gforth-0.6 “str-less-than”
\ .. use ?
\ --------------------------------------------------------------
\
\
\         
\ S<& ----------------------------------------------------------
\
\ tested 17 Nov 2023
\ ( str1 str2 -- str3 )   str3 = str1(&str2)
\ adds a copy of one string to the end of another string.
\ Append the contents of the string specified by str2 to the end of the string specified by str1, and return
\ str3, the address and length of the resulting string. The address of str3 is the address of strl; the length of
\ str3 is the combined length of str1 and str2. If the concatenation would exceed str1's maximum length, no
\ concatenation occurs and str3 = str1. Either str1 or str2 can specify a temporary string in the pad. The <
\ sign indicates that the left string will contain the result of the concatenation.
: S<&  2OVER  ( addr1 n1 addr2 n2 addr1 n1 )
    MAXLEN ( addr1 n1 addr2 n2 maxlen1 ) 
    OVER
    5 PICK
    +      ( addr1 n1 addr2 n2 maxlen1 n1+n2 ) 
    < IF
	2DROP
    ELSE
	( addr1 n1 addr2 n2 )
	2OVER 	( addr1 n1 addr2 n2 addr1 n1 )
	3 PICK 	( addr1 n1 addr2 n2 addr1 n1 n2 )
	+ 	( addr1 n1 addr2 n2 addr1 n2+n1 )
	SWAP 	( addr1 n1 addr2 n2 n2+n1 addr1 )
	1 CHARS ( addr1 n1 addr2 n2 n2+n1 addr1 1Ch )
	- 	( addr1 n1 addr2 n2  n2+n1 addr1-1Ch )
	C! 	( addr1 n1 addr2 n2 )
	2OVER   ( addr1 n1 addr2 n2 addr1 n1 )
	CHARS
	+	( addr1 n1 addr2 n2 addr1-end )
	-ROT  ( addr1 n1 addr1-end addr2 n2 )
	DUP
	0 DO    ( addr1 n1  addr1-end addr2 n2 )
	    C@+  ( addr1 n1  addr1-end addr2+ n2-1 C )
	    4 PICK  ( addr1 n1  addr1-end addr2+ n2-1 C addr1-end )
	    I CHARS
	    +    ( addr1 n1  addr1-end addr2+ n2-1 C addr1-end+ )
	    C!  ( addr1 n1  addr1-end addr2+ n2-1 ) 
	LOOP
	2DROP 2DROP DUP 1 CHARS - C@
	THEN ; 

\ gforth  append ( c-addr1 u1 c-addr2 u2 – c-addr u  ) gforth-0.7 “append”
\ C-addr u is the concatenation of c-addr1 u1 (first) and c-addr2 u2 (second). c-addr1 u1 is an allocated string,
\ and append resizes it (possibly moving it to a new address) to accomodate u characters.
\ .. use
\
\ HP71B
\ 20 STRING S1 " ABC" S1 S! S1 " DEF" S<& TYPE
\ ABCDEF OK { 0 } 
\
\ gforth
\ 20 STRING S1 S" ABCDEFG" S1 S! S1 S" HIJ" S<&  ok
\ .S <2> 140104583664352 10  ok
\ type ABCDEFGHIJ ok
\ s1 maxlen  ok
\ . 20  ok
\ --------------------------------------------------------------
\
\    
\ S= -----------------------------------------------------------
\
\ returns a true flag if two strings are equal, a false flag if not.
\ ( str1 str2 -- flag )
: S= STR= ;       
\ gforth  str= ( c-addr1 u1 c-addr2 u2 – f  ) gforth-0.6 “str-equals”
\ --------------------------------------------------------------
\
\
\       
\ S>& ----------------------------------------------------------
\
\ tested 18 Nov 2023
\ Adds a copy of one string to the beginning of another string.
\ Append the contents of the string specified by str2 to the end of the string specified by str1, and return
\ str3, the address and length of the resulting string. The address of str3 is the address of str2; the length of
\ str3 is the combined length of strl and str2. If the concatenation would exceed str2's maximum length, no
\ concatenation occurs and str3 = str2. Either str1 or str2 can specify a temporary string in the pad. The >
\ sign indicates that the right string will contain the result of the concatenation.
\ ( str1 str2 -- str3 )
: S>& 2DUP  ( addr1 n1 addr2 n2 addr2 n2 )
    MAXLEN  ( addr1 n1 addr2 n2 maxlen2 ) 
    OVER    ( addr1 n1 addr2 n2 maxlen2 n2 ) 
    5 PICK  ( addr1 n1 addr2 n2 maxlen2 n2 n1 ) 
    +       ( addr1 n1 addr2 n2 maxlen2 n1+n2 ) 
    < IF
	2NIP   ( addr2 n2 ) 
    ELSE
	( addr1 n1 addr2 n2 )
	3 PICK    ( addr1 n1 addr2 n2 n1 )
	CHARS	( addr1 n1 addr2 n2 n1Chars )
	3 PICK +  ( addr1 n1 addr2 n2 n1Chars+addr2 )
	3 PICK    ( addr1 n1 addr2 n2 n1Chars+addr2 addr2 )
	SWAP    ( addr1 n1 addr2 n2 addr2 n1Chars+addr2 )
	3 PICK    ( addr1 n1 addr2 n2 addr2 n1Chars+addr2 n2 )
	CMOVE>  ( addr1 n1 addr2 n2 )
	2OVER   ( addr1 n1 addr2 n2 addr1 n1 )
	4 PICK  ( addr1 n1 addr2 n2 addr1 n1 addr2 )
	SWAP    ( addr1 n1 addr2 n2 addr1 addr2 n1 )
	CMOVE   ( addr1 n1 addr2 n2 )
	ROT     ( addr1 addr2 n2 n1 )
	+       ( addr1 addr2 n1+n2 )
	ROT DROP ( addr2 n1+n2 )
	DUP     ( addr2 n1+n2 n1+n2 )
	3 PICK  ( addr2 n1+n2 n1+n2 addr2 )
	1 CHARS - C!  ( addr2 n1+n2 )
    THEN ;
\ use ..
\ HP71B
\ 20 STRING S1 " DEF" S1 S! " ABC" S1 S>& TYPE
\ ABCDEF OK { 0 }
\    S1 TYPE
\ ABCDEF OK { 0 } 
\ S1 MAXLEN .
\ 20  OK { 1 } 
\
\ gforth
\ first branch
\ 5 STRING S2 S" DEF" S2 S! S" ABC" S2 S>& TYPE DEF ok
\ S2 MAXLEN . 5  ok
\ second branch
\ 20 STRING S2 S" DEFGHIJ" S2 S! S" ABC" S2 S>& TYPE ABCDEFGHIJ ok
\ S2 MAXLEN . 20  ok
\ --------------------------------------------------------------
\
\
\           work this out for any effect in gforth; prio B
\ SCI ----------------------------------------------------------
\
\ set the display format.
\ Select scientific display mode with n + 1 significant digits displayed, for 0 <= n <= 11.
\ (n --)
\ see gforth https://gforth.org/manual/Floating_002dpoint-output.html
: SCI  DUP TO SDM SET-PRECISION  0 TO EDM  0 TO FDM
    CR CR ." SCI to be worked out in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ SCRFIB -------------------------------------------------------
\ Return the address of the variable SCRFIB, which contains the FIB# of
\ the currently active file (or 0 if no file is being loaded).
\ user variable containing the either the FIB# of the active file being interpreted by LOADF or else o.
\ >>>> SCRFIB
: SCRFIB  CR CR ." SCRFIB not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\            probably only in relation with HP71B and HP-IL therefore
\            not planned so far in a cross-development with gforth
\ SECONDARY ----------------------------------------------------
\ Return the address of the variable SECONDARY, which specifies the extended portion of an HP-IL address.
\ The valid range for SECONDARY is from 0 through 31, and the default value is o. (The contents of PRIMARY
\ and SECONDARY specify which HP-IL device to use with ENTER and OUTPUT. If system flag - 22 is clear,
\ the contents of PRIMARY specify a simple address; if system flag - 22 is set, the contents of PRIMARY
\ and SECONDARY specify an extended address.)
\ >>>> SECONDARY
\ --------------------------------------------------------------
\
\
\ SHRINK -------------------------------------------------------
\ Shrink the user's dictionary space (and consequently the FORTHRAM file) by n nibbles, and return a
\ true flag; or return a false flag if there are fewer than n free nibbles in the dictionary.
\ ( n -- flag )
\ >>>> SHRINK
: SCHRINK  CR CR ." SCHRINK not implemented in gforth .. HP71B specific?" CR ;
\ use EMU71
\ > CAT ALL
\  NAME   S TYPE   LEN    DATE    TIME PORT
\ FORTHRAM   FORTH 16947 01/01/00 00:00 
\ workfile   BASIC     0 01/01/00 00:00 
\ SNAKE      TEXT   2304 11/01/23 17:18 
\ VIDEO2     BASIC  6008 11/09/23 11:03 
\ REVERSI    BASIC  9662 11/09/23 15:39 
\ H71B1      TEXT   1024 11/24/23 19:20 
\ CLOCK      TEXT   3584 11/24/23 20:20 
\ > FORTH >>>> HP-71 FORTH 1A
\ 7000 SHRINK
\ BYE
\ >CAT ALL
\   NAME   S TYPE   LEN    DATE    TIME PORT
\ FORTHRAM   FORTH 13447 01/01/00 00:00 
\ workfile   BASIC     0 01/01/00 00:00 
\ SNAKE      TEXT   2304 11/01/23 17:18 
\ VIDEO2     BASIC  6008 11/09/23 11:03 
\ REVERSI    BASIC  9662 11/09/23 15:39 
\ H71B1      TEXT   1024 11/24/23 19:20 
\ CLOCK      TEXT   3584 11/24/23 20:20
\ .. FORTHRAM was reduced by 3500
\ --------------------------------------------------------------
\
\
\ SIGN ---------------------------------------------------------
\ Insert the ASCII minus sign - into the pictured numeric output string if n is negative.
\ Used between <# and #>.
\ SIGN same than gforth
\ ( n -- )
\ --------------------------------------------------------------
\
\ 
\ SIN ----------------------------------------------------------
\
\ tested 19 Sept 2023
\ ( r -- sin"r" )
\    X      Y      Z      T      L
\    SIN(X) Y      Z      T      X
: SIN    D_?SINIT    FDUP L F!    DEG IF 360.0E0 F/ 2.0E0 F* PI F* ELSE THEN   FSIN  FDUP X F!  ;
\ --------------------------------------------------------------
\
\
\                      work this out if required
\ SMOVE --------------------------------------------------------
\ ( str addr -- )
\ Store at addr and above (greater addresses) the characters in the string specified by str.
\ >>>> SMOVE     stores a string at a specified address.
: SMOVE CR CR ." SMOVE not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ SMUDGE -------------------------------------------------------
\ ( -- )
\ Toggle the smudge bit in the latest definition's name field.
\ >>>> SMUDGE
: SMUDGE  CR CR ." SMUDGE not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ SP! ----------------------------------------------------------
\ tested 21 Sept 2023
\ reset data stack to 0 items; different from gforth sp!
\ float stack untouched
: SP!  CLEARSTACK ;      
\ --------------------------------------------------------------
\
\
\                        test this
\ SP0 ----------------------------------------------------------
\
\ tested on emu71; same in gforth probably
\ ( -- addr)
\ Return the address of the system variable SPO, which contains the address of the bottom of the data
\ stack. (The address of the bottom of the data stack is greater than the address of the top.)
\ SP0       see variables and gforth
\ .. use
\ SP0 @   (.. same as S0 or TIP)
\ return the address of the bottom of the data stack
\ = start of TIB
\ which will be changed when GROW or SCHRINK is executed
\ see Forth manual page 30
: SP0 CR CR ." SP0 not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                           test this
\ SP@ ----------------------------------------------------------
\
\ Return addr, the address of the top of the data stack before SP@ was executed.
\ >>>> SP@       see gforth too ?
: SP@ SP@ CR CR ." to be tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ SPACE --------------------------------------------------------
\
\ Transmit an ASCII space to the current display device.
\ in gforth, too
\ --------------------------------------------------------------
\
\
\ SPACES -------------------------------------------------------
\
\ like gforth  spaces ( u –  ) core “spaces”
\ Display u spaces.
\ u negativ : nothing happens
\ --------------------------------------------------------------
\
\
\ SPAN ---------------------------------------------------------
\
\ see variable declaration above
\ address is 2FB5C of SPAN in HP71B. SPAN @ return the Number of characters read by E X P E C T 9 6.
\ >>>> SPAN
: SPAN CR CR ." SPAN not implemented in gforth" CR ;
\ .. use ?
\ --------------------------------------------------------------
\
\
\ SQRT ---------------------------------------------------------
\
\ tested 19 Sept 2023
\ ( r -- sqrt"r" )
\    X       Y      Z      T      L
\    SQRT(X) Y      Z      T      X
: SQRT  D_?SINIT FDUP L F! FSQRT FDUP X F! ;
\ --------------------------------------------------------------
\
\ ST* ----------------------------------------------------------
\ tested 
\ ( I: addr -- FV@I *X  )
: ST* DUP F@ X F@ F* F!
 D_RPNREC \ reconstitute the gforth stack based on the RPN stack X Y Z T
; 
\ ST+ ----------------------------------------------------------
\ tested 
\ ( I: addr -- FV@I *X  )
: ST+ DUP F@ X F@ F+ F!
 D_RPNREC \ reconstitute the gforth stack based on the RPN stack X Y Z T
; 
\ ST/ ----------------------------------------------------------
\ tested 
\ ( I: addr -- FV@I *X  )
: ST/ DUP F@ X F@ F/ F!
 D_RPNREC \ reconstitute the gforth stack based on the RPN stack X Y Z T
; 
\
\                            test this 
\ STATE --------------------------------------------------------
\
\ ( -- addr )
\ Return the address of the variable STATE, which contains a non-zero value if compilation is occurring (or zero if not).
\ >>>> STATE
: STATE CR CR ." STATE to be tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ STD ----------------------------------------------------------
\ 
\ Select the BASIC standard display format.
\ >>> STD
: STD  CR CR ." STD not implemented in gforth" CR ;
\ ( -- )
\ --------------------------------------------------------------
\
\
\ STO ----------------------------------------------------------
\
\ ( addr -- )
\ Store the contents of the X-register at addr.
\
\ tested with PERE12 on PC and EMU71
\ store the content of X at f-addr.
\ the GFORTH stack should not move
\    Xg      Yg      Zg      Tg      L 
\    Xg      Yg      Zg      Tg      L
\ ( f-addr -- )
\ : STO  D_?SINIT FDUP F! FDUP X F! ;
: STO  D_?SINIT FDUP F! D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ STOIND -------------------------------------------------------
\
\ ( addr -- )
\ Store the contents of the X-register at ind addr.
\
\ store the content of X at nth registerf-addr.
\ the GFORTH stack should not move
\    Xg      Yg      Zg      Tg      L 
\    Xg      Yg      Zg      Tg      L
\ ( f-addr -- )
\
\ test if X Y Z T L
\ 5.0 Y STO
\ Y RCL FTOI REG41 2.0 STOIND.. 2.0 in 5 REG41 to check with CLX 5 REG41 RCL FS.
\
\ test if register
\ 2.0 5 REG41 STOIND
\
: STOIND
    D_?SINIT
    DUP   ( addr addr  -- )
    DUP   ( addr addr addr -- )
    F@ F>S  ( addr addr N -- )
    SWAP      ( addr N addr -- )
    CELLS 8 = IF 1 CELLS ELSE 16 THEN
    -
    @    ( addr N POS -- )
    -   ( addr POS-N -- )
    CELLS 8 = IF 1 CELLS ELSE 16 THEN
    2 *
    *
    +
    FDUP F! FDUP X F! ;
\ --------------------------------------------------------------
\
\
\
\ STR$ ---------------------------------------------------------
\
\ tested on 21 Sept 2023
\ convert the number d into a temporary string in the PAD specified by str
\ ( d -- str ) 
: STR$   TUCK DABS <# #S ROT SIGN #>
D_PADSET ;
\ --------------------------------------------------------------
\
\
\ STRING ------------------------------------------------------
\ 
\ tested Nov 2023
\   modif 22 nov 2023: chars instead of cells
\                      MAXI 255 check
\ creates a string variable, create a directionnary entry for name,
\ alloting one byte for a maximum-length field (value = n),
\ one byte for a current-length field (value = 0)
\ and n bytes for the string characters
\ chapter 11 http://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm
\ : STRING   CREATE  DUP ,  0 DO 0 C, LOOP  DOES> COUNT   ;
: STRING  CREATE  DUP 255 > IF ( if >255 set it to 255 ) DROP 255
    ELSE THEN DUP C, 0 C, 0 DO 32 C, LOOP DOES> 1 CHARS + DUP 1 CHARS + SWAP C@  ;
\ .. use of form n STRING name
\
\ .. use gforth
\ DECIMAL 10 STRING STR1  ok
\ STR1  ok
\ . 0  ok
\ . 140528396302204  ok
\
\ .. use EMU71
\ 10 STRING STR3
\ OK { 0 } 
\ STR3
\ OK { 2 } 
\ .
\ 0  OK { 1 } 
\ .
\ 203481  OK { 0 }
\ 400 STRING STR2       No message it should not be longer than 255
\ OK { 0 } 
\ 5 STRING STR1
\ OK { 0 } 
\ 5 CONSTANT CON1
\ OK { 0 } 
\ 5 STRING STR2
\ OK { 0 } 
\ VARIABLE VAR1
\  OK { 0 } 
\ 10 VAR1 !
\ OK { 0 } 
\ " ABCDE" STR1 S!
\ OK { 0 } 
\ " ABCDE" STR2 S!
\ OK { 0 } 
\ STR1
\ OK { 2 } 
\ TYPE
\ ABCDE OK { 0 } 
\ STR2 TYPE
\ ABCDE OK { 0 } 
\ VAR1 @
\ OK { 1 } 
\ .
\ 10  OK { 0 } 
\ CON1
\ OK { 1 } 
\ .
\ 5  OK { 0 } 
\ STR2 TYPE
\ ABCDE OK { 0 } 
\ STR2
\ OK { 2 } 
\ .
\ 5  OK { 1 } 
\ .
\ 203422  OK { 0 }
\ " GHI" STR1 S!
\ OK { 0 } 
\ STR1 TYPE
\ GHI OK { 0 } 
\ " KLMNOPQ" STR1 S!
\ FTH ERR:S! string won't fit
\
\ 10 STRING S1
\ " 1234567891" S1 S!
\  OK { 0 } 
\ " ABC" S1 S!
\ OK { 0 } 
\ S1 TYPE
\ ABC OK { 0 }
\ S1
\ OK { 2 } 
\ .
\ 3  OK { 1 } 
\ 8 TYPE
\ ABC45678 OK { 0 }   .. the previous letters are still there
\
\ MEMORY setup..
\ 10 STRING S1 >>> OK { 0 } 
\ S1 MAXLEN >>> OK { 1 } .. which is 10 
\ S1 S.
\ [ 10 213653 0 ]  OK { 3 } 
\ DROP 1 CELLS - @ .
\ 174  OK { 1 } ..  why not 0 ? NOT 1 cell behind !!!
\ 213653 1 CHARS - C@ .
\ 0  OK { 0 } .. the expected 0 is 1 CHAR behind.. 
\ 213653 2 CHARS - C@ .
\ 10  OK { 0 } .. the MAXLEN is 2 CHARs behind..
\ " ABCD" S1 S! >>>> OK { 0 } 
\ 213653 2 CHARS - C@ .
\ 10  OK { 0 } 
\ 213653 1 CHARS - C@ .
\ 4  OK { 0 } 
\ S1 S.
\ [ 213653 4 ]  OK { 2 } 
\ TYPE
\ ABCD OK { 0 } 
\
\ string analysis with C@+
\ example .. FRAMBUF  C@+ . 27  ok
\ C@+ . 37  ok
\ C@+ . 1  ok
\ C@+ . 2  ok
\ C@+ . 124  ok
\ .. it scan all characters
\ --------------------------------------------------------------
\
\
\                     
\ STRING-ARRAY -------------------------------------------------
\
\ tested with GLOCK Nov 2023
\   modif 22 11 2023: Chars instead of celles
\ creates a string-array variable.
\ ( n1 n2 -- )
\ Create a dictionary entry for name, allotting one byte for the maximum-length field (value = nl), one byte for the dimension field (value = n2),
\ and (n1 + 2) bytes each for n2 string-array elements. = n2 strings of maximum length n1
\ STRING-ARRAYS fills in the maximum-length (value = n1) and current-length (value = 0) fields for each string-array element.
\ used in the form   n1  n2  STRING-ARRAY  name
\ Later execution of   n name   will return strn, the address and current length of the nth element of the string array.
\ look there ? http://www.forth.org/svfig/Len/arrays.htm
\ https://www.rosettacode.org/wiki/Create_a_two-dimensional_array_at_runtime#Forth
\ https://librehacker.com/2021/02/04/2d-array-problem-in-gforth/
\ 
\ : STRING-ARRAY  CREATE  2DUP SWAP , , 0 DO DUP , 0 , DUP ALLOT LOOP DROP
\ better?.. initialize with blank 
( n1 n2 --  = n2 strings of maximum length n1 )
: STRING-ARRAY  CREATE
    2DUP          ( n1 n2 n1 n2 )
    SWAP          ( n1 n2 n2 n1 )
    C, C,         ( n1 n2 )
    0 DO          ( n1 )
	DUP       ( n1 n1 )
	C, 0 C,   ( n1 )
	DUP       ( n1 n1 )
	\
        0 DO      ( n1 )
	  32 C, LOOP 
\	ALLOT  ( n1 )
\
  LOOP DROP
DOES>           ( n addr )
  DUP           ( n addr addr )
  1 CHARS +     ( n addr addr+1 )
  C@             ( n addr n2 )
  3 PICK SWAP   ( n addr n n2 )
  > IF
    ." requested nth string out of array reach" DROP DROP
  ELSE        ( n addr )
      DUP C@   ( n addr n1 )
	CHARS 2 CHARS + ( n addr n1-chars&2 chars )
	ROT 1 - * ( addr   n1-chars&2chars * n-1 )
	+ 4 CHARS + ( addrOfNthString )
	DUP 1 CHARS - C@
	    THEN ;
\ --------------------------------------------------------------
\
\
\ SUB$ ---------------------------------------------------------
\
\ tested 22 Nov 2023
\ Create a temporary string (specified by str2) consisting of the
\ n1th through n2th characters in the string specified by str1.
\ creates a temporary substring from the middle part of a string.
\ ( str1 n1 n2 -- str2 )
: SUB$         ( addr len n1 n2 -- addr len)
TUCK           ( addr len n2 n1 n2 )
- NEGATE 1 +   ( addr len n2 n2-n1+1 )
>R             ( w: addr len n2 r: n2-n1+1 )
LEFT$          ( w: addr2 n2 r: n2-n1+1 )
R>             ( addr2 n2 n2-n1+1 )
RIGHT$ ;       ( addr3 n2-n1+1 )
\ use ..
\ HP71B
\ " ABCDEFGHIJKLMNOP" 4 10 SUB$ TYPE
\ DEFGHIJ OK { 0 }
\ " ABCDEFGHIJKLMNOP" 2 10 SUB$
\ OK { 4 } 
\ PAD 1 CHARS + COUNT TYPE
\ BCDEFGHIJ OK { 4 } 
\ PAD 1 CHARS + COUNT DROP 14 TYPE
\ BCDEFGHIJJKLMN OK { 4 } 
\ = .. the PAD is not cleaned up
\ 
\ gforth
\ S" ABCDEFGHIJKLMNOP" 2 5 SUB$ TYPE BCDE ok  
\ S" ABCDEFGHIJKLMNOP" 2 10 SUB$ TYPE BCDEFGHIJ ok
\ S" ABCDEFGHIJKLMNOP" 7 LEFT$ TYPE ABCDEFG ok
\ S" ABCDEFGHIJKLMNOP" 4 RIGHT$ TYPE MNOP ok
\ --------------------------------------------------------------
\
\
\ SWAP ---------------------------------------------------------
\ swap ( w1 w2 – w2 w1 ) core “swap”
\ in GFORTH, too
\ --------------------------------------------------------------
\
\
\ SYNTAXF ------------------------------------------------------
\ ( str -- flag )
\ Return a true flag if the string specified by str is a valid HP-71 file name, or return a false flag if not.
\ If the specified string exceeds eight characters, SYNTAXF checks only the first eight characters.
\ >>>> SYNTAXF
: SYNTAXF  CR CR ." SYNTAXF not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ T ------------------------------------------------------------
\ stack address already above in fvariable declaration
\ Return the address of the floating-point T-register.
\ ( -- T-addr ) or simply ( -- 2FCOO )
\ --------------------------------------------------------------
\
\
\ TAN ----------------------------------------------------------
\
\ tested on 21 Sept 2023
\ ( r1 -- tan"r1" )
\   X      Y      Z      T      L
\   TAN(X) Y      Z      T      X
: TAN  D_?SINIT FDUP L F! ( default is radian. However DEG perhaps ON ) DEG IF 360.0E0 F/ 2.0E0 F* PI F* ELSE THEN  FATAN FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ TIB ----------------------------------------------------------
\ ( -- addr )
\ >>>> TIB
: TIB  CR CR ." TIB not implemented in gforth" CR ;
\ Return the address of the terminal input buffer. The terminal input buffer can hold up to 96 characters.
\ : TIB 1 BUFFER ;
\ could be for HP71B.. (GROW or SHRINK will change the value)
\ : TIB HEX 30D7C CONSTANT TIB ;
\ --------------------------------------------------------------
\
\
\ TOGGLE -------------------------------------------------------
\ Replace n2 (the contents at addr) with the bit-by-bit logical value of (n1 XOR n2).
\ ( addr n1 -- )
\ >>>> TOGGLE
: TOGGLE  CR CR ." TOGGLE not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ TRAVERSE -----------------------------------------------------
\ ( addr1 n -- addr2 )
\ Return the address of the opposite end (length byte or last character) of a definition's name field.
\ > If n=1, addr1 is the address of the length byte, and addr2 is address of the last character.
\ > If n=-1, addr1 is the address of the last character, and addr2 is the address of the length byte.
\ > If n doesn't equal 1 or -1, addr1=addr2
\ >>>> TRAVERSE
: TRAVERSE  CR CR ." TRAVERSE not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ TYPE ---------------------------------------------------------
\ (addr n -- ) transmit n
\ characters found at addr through addr + (2n -1) to the current display device.
\ TYPE transmit no characters for n<= 0
\ in gforth, too   type ( c-addr u –  ) core “type”
\ If u>0, display u characters from a string starting with the character stored at c-addr.
\ .. use
\ (EMU71)
\ " ABCD" TYPE
\ ABCD OK { 0 } .. on printer or 80 column terminal
\ (gforth)
\ S" ABCD" TYPE ABCD ok   
\ --------------------------------------------------------------
\
\
\ U. -----------------------------------------------------------
\
\ in gforth, too
\ Display un (according to BASE) as an unsigned number in a free-field format with one trailing blank.
\ ( un -- )
\ --------------------------------------------------------------
\
\
\ U< -----------------------------------------------------------
\
\ in gforth, too
\ Return a true flag if un1 < un2, or return a false flag if not.
\ ( un1 un2 -- flag )
\ --------------------------------------------------------------
\
\
\ UM* ----------------------------------------------------------
\
\ in gforth, too
\ Return the double-number product ud of two single numbers unl and un2. All numbers are unsigned.
\ ( un1 un2 -- ud )
\ --------------------------------------------------------------
\
\
\ UM/MOD -------------------------------------------------------
\
\ in gforth, too
\ Divide the double number udl by the single number un1 and return the single-number remainder un2 and
\ the single-number quotient un3. All numbers are unsigned.
\ ( ud1 un1 -- un2 un3 )
\ --------------------------------------------------------------
\
\
\ USE ----------------------------------------------------------
\
\ Return the address of the variable USE, which contains the address of the next mass storage buffer available for use.
\ ( -- addr )
\ is a user variable containing the address of the mass memory buffer to use next.
\ >>>> USE
: USE  CR CR ." not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ VAL ----------------------------------------------------------
\
\ in HP71B  ( addr u -- d )  or  ( addr u -- )    see page 20 of the forth manual
\           examine the counted string at addr and convert it into a double number d
\           - if the string contain a ".", tries to convert to float and put it in X and move the float stack
\             if it is not a valid float , it will show a data type error
\           - if no "," it will try to convert into an integer. 
\             if it is not a valid integer , it will show a data type error
: VAL 
    \ input will be  in gforth, S" ( compilation ’ccc"’ – ; run-time – c-addr u  ) core,file “s-quote”
    \ tested in gforth
    \ .. integer path
    \ S" 123456" DECIMAL .S <2> 94769981895760 6  ok
    \ 2DUP  ok
    \ S>NUMBER?  ok
    \ . -1  ok
    \ 2NIP  ok
    \ .S <2> 123456 0  ok
    \ .. float path
    \ S" 1.23456E0" DECIMAL .S <2> 94769981895728 9  ok
    \ >FLOAT  ok
    \ . -1  ok
    \ F.S <1> 1.234560000000E0  ok
    \ .S <0>  ok
\
    BASE @ >R   \ Save the current number base
    \
    DECIMAL     \ Set the number base to decimal
    \
    2DUP        \ duplication for a second try with float   ( addr u addr u )
    S>NUMBER?   \ Convert the string to a number integer    ( addr u d f )
                \ gforth  s>number? ( addr u – d f  ) 
                \ converts string addr u into d, flag indicates success 
    0=          \ no success
    IF
	\ CR ." Issue reading the integer number in the string. Now try float" CR
	2DROP   ( addr u )
	>FLOAT 
	\ >float ( c-addr u – f:... flag ) floating “to-float”
	\ Actual stack effect: ( c_addr u – r t | f ). Attempt to convert the character string c-addr u to internal floating-point representation.
	\ If the string represents a valid floating-point number, r is placed on the floating-point stack and flag is true.
	\ Otherwise, flag is false. A string of blanks is a special case and represents the floating-point number 0.
	\ test gforth
	\ S" 1.234567E0"  ok
	\ >FLOAT  ok
	\ . -1  ok
	\ F.S <1> 1.234567000000E0  ok
	0=
	IF
	    CR ." Issue reading the float number in the string. exit" EXIT
	ELSE
	    \ CR ." Changed to float" CR
	    D_SSET    \ update the 4 level stack with number in X
	THEN
    ELSE
	2NIP
	\ CR ." Changed to integer" CR
    THEN   
R> BASE !   \ Restore the original number base
;
\ .. use
\ (HP71B)
\ " 1.23456" VAL
\ F.
\ 1.23456
\
\ " 234567" VAL
\ .
\ 2
\ .
\ 34567
\
\ .. use gforth
\ S" 1.23456" VAL Changed to integer
\  ok
\ F.S <0>  ok  
\ .S <2> 123456 0  ok
\
\ S" 1.23456E0" VAL Issue reading the integer number in the string. Now try float
\ Changed to float
\ ok
\ .S <0>  ok
\ F.S <4> 0.000000000000E0 0.000000000000E0 0.000000000000E0 1.234560000000E0  ok
\ CLEARSTACKS  ok
\ S" 234567" VAL Changed to integer
\ ok
\ F.S <0>  ok
\ .S <2> 234567 0  ok
\ --------------------------------------------------------------
\
\
\ VARIABLE -----------------------------------------------------
\ see GFORTH: initialize to zero
\ dont initialize to zero on EMU71
\ --------------------------------------------------------------
\
\
\ VARID --------------------------------------------------------
\
\ Return the address of the variable VARID, in which the assembler stores the ID# of the general-purpose
\ buffer that it uses. If the value of VARID is non-zero, the FORTH system will preserve the buffer with
\ that ID#.
\ you can mark one buffer to be retained even
\ during these operations by storing its buffer ID
\ into the FORTH user variable VARID.
\ (The assembler uses this variable to save a buffer.)
\ >>>> VARID
: VARID  CR CR ." VARID not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ VOCABULARY ---------------------------------------------------
\ see forth manual page 23. see gforth, too
\ used at the top of this file
\ --------------------------------------------------------------
\
\
\                test this
\ WARN ---------------------------------------------------------
\ Return the address of the variable WARN.
\ If WARN contains a non-zero value, compiling a new word
\  whose name matches an existing word causes a name isnt unique message to be displayed; if
\ WARN contains 0, the message is suppressed.
\ ( -- addr )
\ >>>> WARN is declared as variable and will return the address 2FB48
\      see page 28 of the forth71b manual
\ .. use ?
\    0 WARN !  suppress all warnings
VARIABLE WARN
\ --------------------------------------------------------------
\
\
\ WIDTH --------------------------------------------------------
\ ( -- addr )
\ Return the address of the variable WIDTH, which determines the maximum allowable length for the
\  name of a word. The valid range for WIDTH is from 1 through 31.
\  so far no variable identified at gforth
\ WIDTH address is 2FB43 in Forth71
\ VARIABLE WIDTH 
\ DECIMAL 31 WIDTH !  
\ 31 because value in HP71B
\ WIDTH @
\ OK { 1 } 
\ .S
\ 1F  OK { 1 } 
\ DECIMAL
\ OK { 1 } 
\ .
\ 31  OK { 0 } 
: WIDTH  CR CR ." WIDTH not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\               test this on gforth and hp71B. use?
\ WORD ---------------------------------------------------------
\ in gforth
\ https://gforth.org/manual/The-Input-Stream.html#index-word-_0028-char-_0022_003cchars_003eccc_003cchar_003e_002d_002d-c_002daddr-_0029-core
\
\ ( c -- addr )
\ Receive characters from the input stream until the non-zero delimiting character c is encountered or the
\ input stream is exhausted, and store the characters in a counted string at addr. WORD ignores leading
\ delimiters. If the input stream is exhausted as WORD is called, a zero-length string results.
\ >>>> WORD
\ --------------------------------------------------------------
\
\
\ X ------------------------------------------------------------
\
\ use EMU71
\ X H.
\ 2FBD0  OK { 0 } 
\ X .
\ 195536  OK { 0 } 
\
\ already above in fvariable declaration
\ Return the address of the floating-point X-register.
\ ( -- X-addr ) or simply ( -- 2FBDO )
\ --------------------------------------------------------------
\
\ X+1 ----------------------------------------------------------
\
: X+1 1.0E0 F+ FDUP X F! ;
\ --------------------------------------------------------------
\
\ X<>Y ---------------------------------------------------------
\
\ tested 19 Sept 2023
\  swap the gforth float stack, then X
\ ( r1 r2 -- r2 r1 )  
\    X      Y      Z      T      L
\    Y      X      Z      T      L
: X<>Y ( ." X<>Y in  " D_RPNS ) D_?SINIT   FSWAP   X F@ TB F!   Y F@ X F!   TB F@ Y F!  ;
\ --------------------------------------------------------------
\
\
\ X#Y? ---------------------------------------------------------
\
\ function similar to F<> in gforth but here without stack move 
\ have a look again.. FDUP X and Y in gforth stack? No. +f is in the nstack.
\ the fstack will not move since X and Y are recalled
\ if checked intereactively in gforth, dont change the size of fstack to 4
\ ( r1 r2 -- r1 r2 +f )   
\    X      Y      Z      T      L
\    X      Y      Z      T      L
: X#Y?   D_?SINIT   Y F@    X F@    F<>  ;
\ --------------------------------------------------------------
\
\
\ X<=Y? --------------------------------------------------------
\
\ function similar to F>= in gforth but without stack move
\ ( r1 r2 -- r1 r2 +f )  
\    X      Y      Z      T      L
\    X      Y      Z      T      L
: X<=Y? D_?SINIT Y F@ X F@ F>=  ;
\ --------------------------------------------------------------
\
\
\ X<Y? ---------------------------------------------------------
\
\  function similar to F> in gforth but without stack move
 ( r1 r2 -- r1 r2 +f )   
\    X      Y      Z      T      L
\    X      Y      Z      T      L
: X<Y?  D_?SINIT  Y F@ X F@ F> ;
\ --------------------------------------------------------------
\
\
\ X=0? ---------------------------------------------------------
\
\ ( r -- r +f)
\    X      Y      Z      T      L
\    X      Y      Z      T      L
: X=0? D_?SINIT FDUP F0= ;
\ --------------------------------------------------------------
\
\
\ X=Y? ---------------------------------------------------------
\
\ function similar to F= in gforth but without stack move
\ ( r1 r2 -- r1 r2 +f ) 
\    X      Y      Z      T      L
\    X      Y      Z      T      L
: X=Y? D_?SINIT Y F@ X F@ F= ;
\ --------------------------------------------------------------
\
\
\ X>=Y? --------------------------------------------------------
\
\  function similar to F<= in gforth but without stack move
\ ( r1 r2 -- r1 r2 +f )  
\    X      Y      Z      T      L
\    X      Y      Z      T      L
: X>=Y?  D_?SINIT Y F@ X F@ F<= ;
\ --------------------------------------------------------------
\
\
\ X>Y? ---------------------------------------------------------
\
\  function similar to F< in gforth but here without stack move
\  ( r1 r2 -- r1 r2 +f )  
\    X      Y      Z      T      L
\    X      Y      Z      T      L
: X>Y?  D_?SINIT Y F@ X F@ F< ;
\ --------------------------------------------------------------
\
\
\ XOR ----------------------------------------------------------
\
\ in gforth too
\ Return the bit-by-bit exclusive OR of n1 and n2.
\ ( n1 n2 -- n3 )
\ .. use
\ https://wiki.forth-ev.de/doku.php/projects:4th_lesson_4
\ This is a bitwise XOR.  For example, if you type
\ 240 255 XOR     ( Hex F0 XOR FF = 0F )
\ the value 15 will be left on top of the stack.
\ --------------------------------------------------------------
\
\
\ X^2 ----------------------------------------------------------
\
\ ( r -- r^2 )
\  X      Y      Z      T      L
\  X^2    Y      Z      T      X
: X^2  D_?SINIT FDUP L F! FDUP F* FDUP X F! ;
\ --------------------------------------------------------------
\
\
\ Y --------------------------------------------------------------
\
\ stack address already above in fvariable declaration
\ Return the address of the floating-point Y-register.
\ ( -- Y-addr ) or simply ( -- 2FBEO )
\ --------------------------------------------------------------
\
\
\                 
\ Y^X ----------------------------------------------------------
\
\ test EMU71
\ 2.0 3.0 Y^X F. L RCL F.
\ 8.00000000000 3.00000000000  OK { 2 }
\
\ ( r1 r2 -- r1**r2 )  
\    T      Z      Y      X      L
\    T      T      Z     Y**X    X
: Y^X D_?SINIT FDUP L F! F**  X F! Z F@ Y F! T F@ Z F!  
D_RPNREC \ reconstitute the gforth fstack based on the RPN stack X Y Z T
;
\
\ test gforth
\ 2.0E 3.0E Y^X F. L RCL F. 8. 3.  ok
\ --------------------------------------------------------------
\
\
\ Z ------------------------------------------------------------
\
\ stack address already above in fvariable declaration
\ Return the address of the floating-point Z-register.
\ ( -- Z-addr ) or simply ( -- 2FBFO )
\ --------------------------------------------------------------
\
\
\                           test this
\ [ ------------------------------------------------------------
\ in gforth, too
\ IMMEDIATE. Suspend compilation. Subsequent text from the input stream will be executed.
\ .. use
\ in relation with LITERAL etc .. [ .. ] LITERAL ..
\ --------------------------------------------------------------
\
\
\ ['] ----------------------------------------------------------
\
\ tested with SNAKE
\ like in gforth
\ Used in the form:
\ name1 ['] name2 ..
\ COMPILE, IMMEDIATE. Compile the CFA of name2 as a literal. An error occurs if name2 is not found
\ in the currently active search order. Later execution of name! will return name2's CFA.
\ (gforth)
\ ['] ( compilation. "name" – ; run-time. – xt  ) core “bracket-tick”
\     xt represents name’s interpretation semantics. Perform -14 throw if the word has no interpretation semantics.
\     https://gforth.org/manual/Execution-token.html#index-_005b_0027_005d_0028--compilation_002e-_0022name_0022-_002d_002d-_003b-run_002dtime_002e-_002d_002d-xt--_0029-core

\ https://gist.github.com/ruv/7c0b6fae5d5f388dd54062b59b732118
\
\ When you apply "[']" or "'" to a not ordinary word,
\ which is not implemented as an immediate word,
\ you get a behavior that is different to what you get in Forth-83.
\ So, it's better to throw an exception in such cases.
\ If you need a Forth-83 xt for such a word, create a wrapper as:
\   : foo [compile] foo ; immediate
\ See area of the COMPILE Word
\ --------------------------------------------------------------
\
\
\                              test this
\ [COMPILE] ----------------------------------------------------
\ Used in the form: ... [COMPILE] name ...
\ IMMEDIATE, COMPILE. Compile name, even if name is an IMMEDIATE word.
\ ( -- )
\ : [COMPILE]  POSTPONE [COMP']  ; IMMEDIATE
\ gforth https://github.com/ForthHub/F83/blob/master/readme.pc

\ https://gist.github.com/ruv/7c0b6fae5d5f388dd54062b59b732118
\ This "[compile]" is applicable to not ordinary words only
\ see the COMPILE Word above in this file.
\ --------------------------------------------------------------
\
\
\ ] ------------------------------------------------------------
\ in gforth, too
\ ( -- )
\ Resume compilation. Subsequent text from the input stream is compiled.
\ --------------------------------------------------------------
\
\
\ *********************   THIRD PART    ************************
\
\                glossary HP71B  of the Forth utilities
\                see FTHUTILF print
\
\ **************************************************************
\
\
\ N-A ----------------------------------------------------------
\ convert a nibble to its hex ascii equivalent.  n -> )
\
\ test EMU71
\ 9 N-A
\ 9 OK { 0 } 
\ F N-A
\ F OK { 0 } 
\ G N-A
\ FTH ERR:G not recognized
\ FF N-A
\ 6 OK { 0 } 
\ 6 N-A
\ 6 OK { 0 } 
\ 2 N-A
\ 2 OK { 0 } 
DECIMAL
: N-A 10 - DUP 0< IF 58 + ELSE 65 + THEN EMIT ;
\
\ test gforth
\ 9 N-A 9 ok
\ F N-A 
\ :2: Undefined word
\ >>>F<<< N-A
\ Backtrace:
\ $7FA6EF6B9A68 throw 
\ $7FA6EF6CFDB0 no.extensions 
\ $7FA6EF6B9D28 interpreter-notfound1 
\ HEX F N-A F ok
\ HEX FF N-A 6 ok
\ --------------------------------------------------------------
\
\
\ PAUSE --------------------------------------------------------
\ PAUSE  Pause for the number of milliseconds stored in the variable PAUSELEN. (Does an empty DO LOOP).
\        Intended for use with outputs to the HP-71 display.
\ ( PAUSE : Pause for PAUSELEN/1000 seconds )
\ : PAUSE PAUSELEN @ 1+ 0 DO LOOP ;
: PAUSE PAUSELEN @ MS ;
\ --------------------------------------------------------------
\
\
\ DUMP ---------------------------------------------------------
\
\ testing results a bit different (PAD structure? nibbles?..)
\ output anyway different
\
\ test EMU71
\ " ABCD" DUMP
\ 1424 OK { 0 } 
\ " ABCD" . 9 DUMP    
\ 4 142434445 OK { 0 } 
\
\ similar gforth
\ Display n nibbles, starting at addr, as ASCII hex characters.
\ ( addr n -- )
\ : DUMP OVER + SWAP ( addr n -> print n nibs starting at addr)
\    DO I N@ N-A LOOP ;
\
\ standard gforth dump
\ S" ABCD" dump 
\ 556A9EF97960: 41 42 43 44              -                           ABCD
\ ok
\ S" ABCD" . 9 DUMP 4 
\ 556BC7AD1FC0: 41 42 43 44  00 00 00 00 - 00                        ABCD.....
\ ok
\ 
\ --------------------------------------------------------------
\
\
\               improve/test this for having same output
\ DUMP+ --------------------------------------------------------
\
\ test EMU71
\ " EFGH" DUMP+
\ 5464 OK { 1 } 
\ 5 DUMP+
\ 74845 OK { 1 } 
\
\ Display n nibbles, starting at addr, as ASCII hex characters.
\ Leave the next address (addr+n) on the stack.
\ ( addr n -- addr+n )
: DUMP+ 2DUP + ROT ROT DUMP ;
\
\ test gforth
\ S" EFGH" DUMP+   
\ 556BC7AD3890: 45 46 47 48              -                           EFGH
\ ok
\ 5 DUMP+ 
\ 556BC7AD3894: 00 00 00 00  00          -                           .....
\ ok
\
\ --------------------------------------------------------------
\
\
\         work this out when needed due to different output
\ SHOW ---------------------------------------------------------
\
\ EMU71 test
\ OK { 0 } 
\ " ABCD" SHOW
\ 353AE      34241 
\ 353B3      F0444 
\ 353B8      F0571 
\ 353BD      FDC0 
\ OK { 1 }
\
( SHOW: disp contents of n consecutive memory cells.  { addr n -> } )
: SHOW CR 1+ 1 DO DUP H. DUP @ 5 SPACES H. PAUSE CR 5+ LOOP ;
\
\ test gforth
\ S" ABCD" SHOW 
\ $5557F1C69720      $555744434241 
\ $5557F1C69728      $0 
\ $5557F1C69730      $0 
\ $5557F1C69738      $121 
\  ok
\
\ --------------------------------------------------------------
\
\
\ BASE? --------------------------------------------------------
\
\ test EMU71
\  OK { 0 } 
\  BASE?
\  10  OK { 0 }
\
( put current base in X and display)
: BASE? BASE @ DECIMAL . ;
\ : BASE? BASE @ DUP DECIMAL . DUP ITOF X F! ;
\
\ test gforth
\ BASE? 10  ok
\
\ --------------------------------------------------------------
\
\
\ DELAY00 ------------------------------------------------------
( DELAY00: Set DELAY0,0)
\ : DELAY00 " DELAY0,0" BASICX ;
: DELAY00 0 MS ;
\ --------------------------------------------------------------
\
\
\ D- -----------------------------------------------------------
\ not implemented since only relation to BASIC and HW HP71B
( D- {D P R *}: Do DISPLAY IS {DISPLAY  PRINTER RS232 *} )
\ : D-P " DISPLAY IS PRINTER" BASICX ;
\ : D-* " DISPLAYIS*" BASICX ;
\ : D-D " DISPLAYISDISPLAY" BASICX ;
\ : D-R " DISPLAYISRS232"BASICX ;
: D- CR CR ." D- not implemented in gforth" CR ;
: D-P CR CR ." D-P not implemented in gforth" CR ;
: D-* CR CR ." D-* not implemented in gforth" CR ;
: D-D CR CR ." D-D not implemented in gforth" CR ;
: D-R CR CR ." D-R not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ ROOM? --------------------------------------------------------
\ ROOM? Display the number of nibbles available in the FORTHRAM file
( ROOM?:  Display number of nibbles available in dictionary )
\ : ROOM? SP0 @ HERE - 458 - . ;
: ROOM? CR CR ." ROOM not implemented in gforth since no room limitation like in HP71B" CR ;
\ --------------------------------------------------------------
\
\
\ S. -----------------------------------------------------------
\ tested 2023 11 19
\
\ equivalent to standard gforth .S
\
( S.: Print integer stack contents bottom first )
: S. ." [ " DEPTH 0> IF DEPTH 1 SWAP DO I PICK U. -1 +LOOP THEN ." ] " ;
\ use..
\ HP71B
\ 1 2 3 4 5 S. .S
\ [ 1 2 3 4 5 ] 
\ 5 4 3 2 1  OK { 5 } 
\ gforth
\ 1 2 3 4 5 S. .S [ 1 2 3 4 5 ] <5> 1 2 3 4 5  ok
\ --------------------------------------------------------------
\
\
\ ADDR- --------------------------------------------------------
( ADDR-    get the addr of the namefield of the previous word.)
(          NFAddr1 -> NFAddr2 )
HEX
\ : ADDR- 5- @ ;
: ADDR-  5- @ CR CR ." ADDR- not tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ NFA ----------------------------------------------------------
\ https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
\ check behaviour in EMU71 and HP71B
( NFA:  GIVEN CFA, GET NFA.  [ CFA -> NFA ] )
\ CFA is code field address
\ : NFA 2- -1 TRAVERSE ;
: NFA >name ." NFA not released in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ NAME ---------------------------------------------------------
\ test on HP71B and EMU71
( NAME: From NFA, type name  [ nfa -> ] )
\ NFA is name field address
\ : NAME DUP C@ 1F AND 2DUP 1- SWAP 2+ SWAP TYPE
\    2* + C@ 7F AND EMIT ;
: NAME .name CR CR ." NAME not released in gforth" CR ;
\ test gforth
\ S" RAD" find-name .name RAD  ok
\ --------------------------------------------------------------
\
\ NFASTR -------------------------------------------------------
\ test on HP71B and EMU71 
( NFASTR: Convert NFA to name string.  [NFA -> str] )
\ : NFASTR DUP 2+ SWAP C@ 1F AND ;
: NFASTR name>string CR CR ." NFASTR not released in gforth" CR ;
\ test gforth
\ S" RAD" find-name   ok
\ .S <1> 139831737855256  ok
\ name>string  ok
\ type RAD ok
\
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ SPECIAL ------------------------------------------------------
( SPECIAL: Array containing list of words with remote CFA's. )
( 1st value is # of entries. )
\ CREATE SPECIAL 
\   D ,
\   E701A , ( COLON )
\   E71E8 , ( SEMI )
\   E1C54 , ( number )
\   E22ED , ( F-number )
\   E1C67 , ( DO )
\   E3FF1 , ( LOOP )
\   E3F81 , ( +LOOP )
\   E5D86 , ( IF / UNTIL / WHILE )
\   E5D99 , ( ELSE / REPEAT )
\   E0640 , ( " )
\   E0EFA , ( ." )
\   E580E , ( ABORT")
\ E0168 , ( J )
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ SPEC? --------------------------------------------------------
( SPEC?: Find CFA in SPECIAL. Return # of entry, or 0 if not present. )
( [ cfa -> cfa # ] )
\ : SPEC? 0 SPECIAL @ 1+ 1 DO  ( CFA 0 )
\   OVER SPECIAL I 5 * + @ =  ( CFA 0 CFA SPECi )
\	IF DROP I LEAVE THEN LOOP ; ( CFA # )
: SPEC? CR CR ." SPEC? not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ 'NAME --------------------------------------------------------
\ test this on EMU71 and HP71B
( 'NAME: Given CFA, type name.  { CFA -> } )
\ : 'NAME DUP NFA SWAP OVER - 2/ 1- SWAP NFASTR DUP 4 ROLL = 
\      IF 1- 2DUP TYPE 2* + C@ 7F AND EMIT 
\      ELSE 2DROP ." Unknown" THEN ;
: 'NAME >name .name CR CR ." 'NAME not released in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ HEREN --------------------------------------------------------
( HEREN: Find the start of the n+1th link field.  { n -> addr } )
\ : HEREN DUP C = IF DROP E6FAB ELSE 1+ 5 * E0000 + @  ( NFA )
\   BEGIN 5- DUP @ DUP 0 <> WHILE SWAP DROP REPEAT DROP THEN ;
: HEREN CR CR ." HEREN not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
VARIABLE ENDA VARIABLE HERE0
\
\
\                work this out when needed
\ 'END ---------------------------------------------------------
\
( END: given a CFA, find the addr of the start of the next word   )
( and store in ENDA.   CFA -> )
\ : 'END DUP E0000 U< IF LATEST HERE
\   ELSE DUP NFA C@ 1F AND  ( CFA n )
\      DUP 5 * E0000 + @ SWAP HEREN THEN
\   DUP HERE0 ! ENDA ! BEGIN 2DUP < WHILE DUP ENDA ! ADDR- REPEAT 
\   2DROP ENDA @ HERE0 @ <> IF -5 ENDA +! THEN ;
: 'END CR CR ." 'END not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ 5SP ----------------------------------------------------------
: 5SP 5 SPACES ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ +ADDR --------------------------------------------------------
\ how does it work under HP71B??
\ subroutine of WORDNAME therefore no immediate use so far...
( +ADDR: Type addr following control word; incr addr. { I -> I+5 } )
\ : +ADDR 5+ DUP DUP @ + ."  to " H. ;
: +ADDR CR CR ." +ADDR not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ "STR ---------------------------------------------------------
\ subroutine of WORDNAME therefore no immediate use so far...
( "STR: Type the compiled string following a " word [ I 4-or-2 ] )
\ : "STR SWAP 5+ DUP C@ 2DUP SWAP 5 ROLL + SWAP 22 EMIT SPACE TYPE 22 EMIT
\           2* + 1- ;
: "STR CR CR ." highSTR not implemented in gforth" CR ;
\ --------------------------------------------------------------
\     
\       
\ FTEMP --------------------------------------------------------
\ how does it work under HP71B??
( FTEMP: FVariable to hold X during decompilation of a FP word )
FVARIABLE FTEMP
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ WORDNAME -----------------------------------------------------
\ subroutine of WORD@ therefore no immediate use so far...
( WORDNAME: Given I and its WA, type the word identified; advance I)
( [ I WA -> I' ]  I' = next I -5 )
\ : WORDNAME DUP ABS 10000 > ( Is this a legitimate word addr? )
\        IF SPEC? CASE 0 OF 'NAME ENDOF SWAP DROP
\        1 OF ." :" ENDOF
\        2 OF ." ;" ENDOF 
\        3 OF 5+ DUP @ . ENDOF 
\        4 OF 5+ DUP FTEMP STO RDN RCL F. RDN FTEMP RCL B + ENDOF
\        5 OF ." DO" ENDOF
\        6 OF ." LOOP" +ADDR ENDOF
\        7 OF ." +LOOP" +ADDR ENDOF
\        8 OF DUP 5+ @ 0> 
\      IF OVER 5+ DUP @ + 5- DUP @ 0< SWAP 5- @ E5D99 = AND 
\         IF ." WHILE" ELSE ." IF" THEN
\      ELSE ." UNTIL" THEN +ADDR ENDOF
\        9 OF DUP 5+ @ 0> IF ." ELSE" ELSE ." REPEAT" THEN +ADDR ENDOF
\        A OF 4 "STR ENDOF
\        B OF ." ." 2 "STR 2- ENDOF
\        C OF ." ABORT" 2 "STR 2- ENDOF
\        D OF ." J" ENDOF
\      ENDCASE     ( addr')
\     ELSE DROP ( addr'=addr )
\     THEN ;
: WORDNAME CR CR ." WORDNAME not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\               work this out if needed
\ WORD@ --------------------------------------------------------
\ subroutine of UN:C therefore no immediate use so far...
( WORD@: Given an addr, type it, it's content, and the word identified. )
(    { addr -> addr'}  where addr' = addr of next I )
\ : WORD@ DUP H. 5SP DUP @ DUP H. ( addr cfa )
\   WORDNAME 5+ PAUSE CR ;
: WORD@ CR CR ." WORD@ not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ UN:C ---------------------------------------------------------
\ subroutine of UN: therefore no immediate use found in gforth
( UN:C  Decompile a word, omitting header [ cfa -> ] )
\ : UN:C ." CFA: " DUP 'END DUP DUP @ - -5 = IF H. 5SP ." Primitive" PAUSE CR
\   ELSE WORD@ BEGIN 5SP WORD@ DUP ENDA @ = UNTIL DROP THEN ;  ( )
: UN:C CR CR see-code ." UN:C not tested in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ UN: ----------------------------------------------------------
\ released 02 Jun 2024
\ Page 21-1 
\ Decompile the word following UN: in the input stream.
\ Used in the form: (not for ROM words)
\   UN: <word name>
( UN: Decompile the word named next, including the header )
\ : UN: ' DUP ." Word: " DUP 'NAME PAUSE CR NFA ( CFA NFA )
\     DUP 5- ." LFA: " DUP H. 5SP @ ." Link: " H. PAUSE CR ( CFA NFA )
\   DUP ." NFA: " H. 5SP NFASTR 1+ 2* SWAP 2- SWAP DUMP PAUSE CR ( CFA )
\   UN:C ;
: UN: see ;
\ --------------------------------------------------------------
\
\
\                          test this
\ RS. ----------------------------------------------------------
\ Decompiles the contents of the return stack.
\ RS. lists each item on the return stack, in bottom-to-top
\ order, each followed by the name of the word identified by the address.
\ The lowest two levels, which refer to the outer interpreter, are omitted.
\ ( -- )
\ : RS. RP@ RP0 @ 5-
\     DO I @ WORD@ DROP -5 +LOOP ;
: RTEST RP@ DUP >R RP@ RDROP SWAP - ;
RTEST CONSTANT RSTEP
RP@ CONSTANT RBIAS
: RS. \ --
  RP@ RBIAS 
  DO I @ U. RSTEP
  +LOOP  ." RS. not tested in gforth"  CR ;
\ https://stackoverflow.com/questions/27409852/show-the-return-stack
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ RTNSAVE ------------------------------------------------------
( RTNSAVE: Variable to hold SST environment. )
( Contents of addr: )
( RTNSAVE = I )
(      +5 = Orig. I --in word that calls DOSST )
(      +A = CFA of SST word )
(      +F = END of SST word )
(     +14 = >RTN )
(     +19 = >RBOT )
( RTNSAVE+E6 points to end of RTNSAVE--temp RP0@)
\ CREATE RTNSAVE E6 NALLOT
( " CREATE TEXT RTNSAVE:PORT[1],500" BASICX )
( : RTNSAVE " ADDR$['RTNSAVE']" BASIC$ DROP 2- NUMBER DROP 25 + ; )
: RTNSAVE CR CR ." debug word RTNSAVE not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ NEWRTN -------------------------------------------------------
( NEWRTN: Copy the RTN stack & >RTN to RTNSAVE )
\ : NEWRTN RP@ RP0 @ ( >RTN >RBOT)
\   OVER - SWAP OVER RTNSAVE E6 + 
\   SWAP - DUP RTNSAVE 14 + ! ( Save >RTN* ) ROT ( [ >RTN >RTN* # ] )
\   NMOVE   ( COPY RTN stack to RTNSAVE)
\   RTNSAVE 2FB7F ! ; ( Put RTNSAVE addr in 2FB7F )
: NEWRTN CR CR ." debug word NEWRTN not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ SSTERROR -----------------------------------------------------
( SSTERROR: On error routine for single step )
\ : SSTERROR 0 ONERR ! RTNSAVE 19 + @ RP0 ! ( Restore >RBOT )
\     2F7E4 4N@ DUP 2DUP ( 4 copies of errn )
\     2EFF > ( >2EFF ? )
\    SWAP 2F41 < ( <2F41 ? )
\     AND SWAP 0= OR  ( =0? )
\     IF R>  ( Go back to ABORT )
\    ELSE " BEEP" BASICX " 'ERR:'&MSG$(ERRN)"BASIC$ TYPE SP! RP! QUIT
\    THEN ;
: SSTERROR CR CR ." debug word SSTERROR not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                      use TBD / to be worked out
\ SSTOUT -------------------------------------------------------
( SSTOUT: Variable to hold CFA of word to be executed after DOSST )
VARIABLE SSTOUT
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ SST ----------------------------------------------------------
\ Display the name of the word identified by the next address in the current single-step word’s definition,
\ then execute the named word. Then execute the word whose CFA is stored in the variable SSTOUT,
\ The default SSTOUT word is S. which displays the stack in bottom-to-top order (reverse of .S ).
\ ( -- )
\ Single step the word identified by the instruction pointer I* stored )
( at RTNSAVE, unless it is a semi.  Then display the stack. )
\ : SST RTNSAVE @ DUP @ DUP E71E8 <> ( I WA flag )
\      IF WORDNAME DROP 5 SPACES
\      [ ' SSTERROR  ] ( Put SSTERROR cfa on stack for onerror )
\      LITERAL ONERR !
\      DOSST 0 ONERR ! SSTOUT @ EXECUTE
\         ELSE ." ;" 2DROP THEN ;
: SST CR CR ." debug word SST not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ ' S. SSTOUT ! ( Initialize SSTOUT to hold S.'s CFA )
\
\
\                work this out when needed
\ READYSTEP ----------------------------------------------------
( READYSTEP: Set up environment for STEP or BREAK )
( Save I*, END, CFA in RTNSAVE, do NEWRTN )
\ : READYSTEP ' DUP RTNSAVE A + !  ( Store CFA )
\   DUP 'END ENDA @ RTNSAVE F + !   ( Store word END )
\   5+ RTNSAVE ! ( Save new I )
\   NEWRTN ;
: READYSTEP CR CR ." debug word READYSTEP not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ STEP ---------------------------------------------------------
\ Create a single step environment for the word named next, then SST the first word following the : in
\ the word’s definition.
\ Used in the form STEP  <wordname>.
( STEP: Single step next word.)
\ : STEP READYSTEP SST ;
: STEP CR CR ." debug word STEP not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ BP -----------------------------------------------------------
\ Set a breakpoint at address n, for use with LUT.
\ ( n -- )
( BP: Set a breakpoint.  [ Ib -> ] )
\ : BP 2FB84 ! ;
: BP CR CR ." debug word BP not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ CONT ---------------------------------------------------------
\ Resume execution of a word that was interrupted with STEP, BREAK, or SST, Execute up to the
\ breakpoint address, or to the final " ; ", whichever comes first.
\ ( -- )
( CONT: Continue execution of a BREAKed or SSTed word to next breakpoint )
\ : CONT [ ' SSTERROR ] LITERAL ONERR ! 
\      BRRUN 0 ONERR ! SSTOUT @ EXECUTE  ;
: CONT CR CR ." CONT not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ FINISH -------------------------------------------------------
\ Complete execution of an interrupted word through the final " ; ".
\ ( -- )  
( FINISH: Complete execution of an interrupted word )
\ : FINISH 0 BP CONT ;
: FINISH CR CR ." debug word FINISH  not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\                work this out when needed
\ BREAK --------------------------------------------------------
\ Used in the form <addr> BREAK <wordname>.
\ Create a single step environment for the word named next, then execute the word, stopping when the
\ instruction pointer reaches the address on the top of the data stack. The addresses for BREAK. can be
\ obtained using UN: on the word to be single stepped. BREAK can stop at any word address in a
\ definition after the first address following the: ( use STEP if you want to stop on the first address) and
\ before the final ; (stopping on the ; is the same as executing the full word).
\ ( n -- )
( BREAK: Execute next word, stopping at Ib specified on stack or at the )
(        final ;  [ I -> ] )
\ : BREAK READYSTEP BP CONT ;
: BREAK CR CR ." debug word BREAK not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
\
\ LIST ---------------------------------------------------------
\
\ tested/released June 1st 2024
\ Display a list of user-dictionary words, starting with the most recently created.
( LIST: List current RAM dictionary.  Same as RAMLIST )
\ : LIST LATEST BEGIN 
\   DUP NAME DUP C@ 40 AND IF ."  Immediate" THEN PAUSE CR 5- @ 
\    DUP 0= UNTIL DROP ;
\ ( -- )
: LIST WORDS ;
\ --------------------------------------------------------------
\
\
\ FSCRATCH -----------------------------------------------------
( FSCRATCH : Floating point scratch variable used in TIMED )
FVARIABLE FSCRATCH
\ --------------------------------------------------------------
\
\
\ FINDW --------------------------------------------------------
( FINDW: Get a word from input stream and return its cfa [ -> cfa ] )
\ : FINDW BL WORD FIND 0= IF ABORT" Word not found" THEN ;
\ part of PRINT and TIMED which is not necessary so far
\ therefore, need not so far identified
\ -------------------------------------------------------------
\
\
\ PRINT -------------------------------------------------------
\ released 01 June 2024
\ PRINT  Used in the form PRINT xxxxx, which causes the display output of the FORTH word xxxxx to be
\        directed to the printer (:PRINTER(1)). The original DISPLAY IS device is restored automatically
\        after xxxxx has finished execution. PRINT UN: FRED, for example, will print the decompilation of
\        FRED on a printer instead of the display.
( PRINT:  Direct the display output of the next word to the printer )
( original HP71B code..                                             )             
\ : PRINT 2F78D 2FC79 7 NMOVE ( Save old Display device )
\     D-P ( Make printer the display )
\     FINDW EXECUTE CR ( Do it and print output)
\     2FC79 2F78D 7 NMOVE ( Restore old display )
\     7 2F7B1 N! ( Reset display type )
\     2FC79 7 0 NFILL ( Zero the assembler variables )
\     ;
\ gforth implementation not activ since the output
\ in a terminal window can be copy/paste into a text
\ editor and sent to any printer in the network
: PRINT CR CR ." PRINT not implemented in gforth; make copy/paste of the word output instead and send to the printer" CR ;
\ --------------------------------------------------------------
\
\
\                so far no need identified on PC / H71B
\ SKIP ---------------------------------------------------------
\ SKIP  Send ESC & 11L to the :PRINTER to set perforation skip mode.     
( SKIP : Set printer to skip over perf mode )
\ : SKIP " PRINT CHR$(27);'&l1L';"BASICX ;
: SKIP CR CR ." SKIP not implemented in gforth" CR ;
\ --------------------------------------------------------------
\
DECIMAL
\ --------------------------------------------------------------
\             see separate LIF files for these assembler words
\             any GH_xx must be in one line because will be
\             deleted by the awk filtering
\ --------------------------------------------------------------
\
\
\ TIME ---------------------------------------------------------
\ tested on gforth PC
\  "TIME" in HP71B translator module: H.MMSS into the float stack and hh:mm:ss into screen
\  "TIME" uploaded as assembler is giving seconds as output sssss.xx0 when tested therefore
\    the output here was modified to have something like sssss.yy
\    HP71B SW developper handbook page 91 of PDF..
\      Pushes the current HP-71 clock time onto the floating point stack. Time is expressed in seconds from
\      midnight, rounded to the nearest .01 second.
\    HP71B Owner Manual page 17 Setting Time and Date
\      BASIC   SETDATE"23/10/29"
\              DATE$ display the date 23/10/29
\              SETTIME"17:35:00"
\              TIME$ display 17:36:00 (if called 1 min after the setup)
\  https://gforth.org/manual/Keeping-track-of-Time.html#index-utime_0028--_002d_002d-dtime--_0029-gforth_002d0_002e5
\  TIME&DATE + + + + + S>F
\ ( -- float_time )
\ worked for SNAKE
\ : TIME  UTIME #10000 UM/MOD NIP S>F 100.00E0 F/  ;
\
\ introduction of 86399 MOD because the time is maximum 23h59min59s 86399 min since midnight
\ for test only on gforth cross-development (plus 99 for the milliseconds)
\ : TIME  UTIME #10000 UM/MOD NIP 8639999 MOD S>F 100.00E0 F/  ;
\ looks like 86399 is an integer accepted by the HP71B forth
\ according CHATGPT the largest integer on 20bits is 1.048.575
\
\ tested 13 Nov Time in SSSS.xx
\ revisited on 19Juni2024
\
\ extract the seconds for today
( -- r: SSSS.xx )
: TIME
( -- r: SSSS )
    TIME&DATE DROP DROP DROP 3600 * SWAP 60 * + +
    S>F
( r: SSSS -- f: SSSS.xx )
    UTIME #10000 UM/MOD NIP
    S>F 
    100.0E0 F/
    \
    \ Fractional part
    FDUP          \  rrr.rrr  rrr.rrr
    FTRUNC        \  rrr.rrr  int(rrr.)
    F-            \  0.rrr
    \
    F+ D_?SINIT
;
\ --------------------------------------------------------------
\
\
\ T71>41 -------------------------------------------------------
\ tested on gforth PC
\  change from SSSSS.ss into HH.MMSSss into the float stack
\  see HP71B ASM word
: T71>41
( r:SSSSS.ss -- r: HH.MMSSss )
\    TIME                 \ (0.0 0.0 0.0) SSSSS.ss
\    f.s cr
    FDUP                 \ SSSSS.ss   SSSSS.ss
\    f.s cr
    FDUP FTRUNC F-       \ SSSSS.ss   0.ss    
\    f.s cr
    10000.0E0 F/         \ SSSSS.ss   0.0000ss
\    f.s cr
    FSWAP                \ 0.0000ss   SSSSS.ss
\    f.s cr
    FTRUNC               \ 0.0000ss   SSSSS.00
\    f.s cr
    3600.0E0 F/          \ 0.0000ss   HH.hhhhhhh
\ f.s cr
    FDUP                 \ 0.0000ss   HH.hhhhhhh   HH.hhhhhhh
\ f.s cr
    FTRUNC               \ 0.0000ss   HH.hhhhhhh   HH.000
\ f.s cr
     FROT                \ HH.hhhhhhh  HH.000    0.0000ss  
\    f.s cr
    F+ FSWAP             \ HH.0000ss  HH.hhhhhhh        
\ f.s cr
    FDUP FTRUNC F-       \ HH.0000ss  0.hhhhhh   
\ f.s cr
    60.0E0 F*            \ HH.0000ss  MM.mmmmm
\ f.s cr
    FDUP                 \ HH.0000ss  MM.mmmmm     MM.mmmmm
\ f.s cr
    FTRUNC               \ HH.0000ss  MM.mmmmm     MM.000
\ f.s cr
    100.0E0 F/           \ HH.0000ss  MM.mmmmm     0.MM
\ f.s cr
    FROT F+ FSWAP        \ HH.MM00ss  MM.mmmmm        
\ f.s cr
    FDUP FTRUNC F-       \ HH.MM00ss  0.mmmmm   
\ f.s cr
    60.0E0 F*            \ HH.MM00ss  SS.0000xxx
\ f.s cr
    FTRUNC               \ HH.MM00ss  SS.00
\ f.s cr
    10000.0E0 F/ F+      \ HH.MMSSss
\    f.s cr
    D_?SINIT
;
\ --------------------------------------------------------------
\
\
\ X<>Z ---------------------------------------------------------
\ swap the gforth float stack, then X
\ ( r1 r2 -- r2 r1 )  
\    X      Y      Z      T      L
\    Z      Y      X      T      L
: X<>Z   D_?SINIT  X<>Y RDN X<>Y RUP X<>Y ;
\ --------------------------------------------------------------
\
\
\ X<>T ---------------------------------------------------------
\ ( r1 r2 r3 r4 -- r4 r2 r3 r1 )  
\    X      Y      Z      T      L
\    T      Y      Z      X      L
: X<>T   D_?SINIT X F@ TBX F! T F@ X F! TBX F@ T F! D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ X<> ---------------------------------------------------------
\ swap the gforth float variable value with X
\ ( addr -- )
\ (      -- )  
\    X      Y      Z      T      L
\    Val    Y      T      T      L
: X<>   D_?SINIT
    DUP     \ integer: addr addr              
    F@      \ integer: addr                float:  val
    X F@    \ integer: addr                float:  val    XValue 
    FSWAP   \ integer: addr                float:  XValue val 
    X F!    \ integer: addr                float:  XValue 
    F!
    D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ Y<>Z ---------------------------------------------------------
\ swap the gforth float stack, then X
\ ( r1 r2 r3 -- r1 r3 r2 )  
\    X      Y      Z      T      L
\    X      Z      Y      T      L
: Y<>Z   D_?SINIT  RDN X<>Y RUP ;
\ --------------------------------------------------------------
\
\
\ X*2 ----------------------------------------------------------
\ ( r1 r2 -- r2 r1 )  
\    X        Y      Z      T      L
\    2*X      Y      Z      T      X
: X*2   D_?SINIT  2.0E X F@ F* X F! D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ X/2 ----------------------------------------------------------
\ ( r1 r2 -- r2 r1 )  
\    X        Y      Z      T      L
\    X/2.     Y      Z      T      X
: X/2  D_?SINIT  X F@ 2.0E F/ X F! D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ X+2 ----------------------------------------------------------
\ ( r1 r2 -- r2 r1 )  
\    X        Y      Z      T      L
\    X+2      Y      Z      T      X
: X+2   D_?SINIT  2.0E X F@ F+ X F! D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ X<>L ---------------------------------------------------------
\ swap the gforth float stack, then X
\ from MAFO.TXT
\ ( r1 r2 -- r2 r1 )  
\    X      Y      Z      T      L
\    L      Y      Z      T      X
: X<>L D_?SINIT  X F@ TB F!  L F@ X F!  TB F@ L F!  D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ Z<>T ---------------------------------------------------------
\    X      Y      Z      T      L
\    X      Y      T      Z      L
: Z<>T D_?SINIT  Z F@ TBZ F!  T F@ Z F!  TBZ F@ T F!  D_RPNREC ;
\ --------------------------------------------------------------
\
\
\ X<0? ---------------------------------------------------------
\    X      Y      Z      T      L
\    X      Y      T      Z      L
: X<0? D_?SINIT FDUP F0< ;
\ --------------------------------------------------------------
\
\
\ FS. ----------------------------------------------------------
\ from file DISPLAY.fth 
\
\ show the Float Stack content mostly during debugging on the
\ HP71B hardware
\
\ Execution/Inputs: nothing
\ Outputs: the stack
\ Use: FS. <return>
\ T= 2 
\ Z= 7 
\ Y= -3 
\ X= 8 
\ L= 3 
\  OK { 0 } 
\
\ Modules used
\   FORTH/ASSEMBLER
\
\ under CC BY SA CreativeCommons 4.0
\
\ change log
\   see file DISPLAY.fth 
: FS. CR RUP ." T=  " F. 
    RUP CR ." Z=  " F. 
    RUP CR ." Y=  " F. 
    RUP CR ." X=  " F. 
    CR X<>L ." L=  " F. X<>L CR ;
\ --------------------------------------------------------------
\
\
\ FV. ----------------------------------------------------------
\ from file DISPLAY.fth 
\
\ show the Float variable content mostly during debugging on the
\ HP71B hardware
\ like VIEW of HP41
\
\ Execution/Inputs: nothing
\ Outputs: the stack
\ Use: FV. <return>
\ X FV.
\ 9.000000 
\ OK { 0 } 
\
\ Modules used
\   FORTH/ASSEMBLER
\
\ under CC BY SA CreativeCommons 4.0
\
\ change log
\   see file DISPLAY.fth 
: FV. DUP X<> ."  " F. X<> ;
\ --------------------------------------------------------------
\
\
\                         test this (no rest in stack?)
\ TIMED --------------------------------------------------------
\
\ test EMU71
\ 2.0 3.0 TIMED PERE12
\
\ Param B: 3.00000000000 
\ Param A: 2.00000000000 
\ ELLIPSE PERIM = 15.8654395893 1.46000000000  OK { 0 }
\
\ Used in the form TIMED xxxx, which displays the execution time of the word xxxx in seconds (to the
\ nearest . 01 second). For timing floating point words, be aware that TIMED will change the T-register on
\ input, and the T- and Z- registers on output.
( TIMED: Execute the next word and display its execution time )
(    The time is left in X.  T is lost on input, and Z & T are lost)
(    on output. )
\ : TIMED FINDW FSCRATCH
\     TIME STO FDROP EXECUTE TIME
\     FSCRATCH DUP DUP RCL F-
\     TIME STO FDROP TIME
\     F- RCL F+ F. ;
\
: TIMED 
    TIME FSCRATCH F! 
    ' EXECUTE 
    TIME FSCRATCH F@ F- 1.0E5 F/ D_?SINIT
    CR ." execution time: " 
    F. ." sec." CR ;
\
\ test gforth
\ 1.0E 9.0E TIMED AGM 
\ execution time: 0.0099999905 sec.

\ BASE !
\ --------------------------------------------------------------
\
\
\ ****     ENDE     ********************************************
