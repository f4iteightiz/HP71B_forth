(                       FTHUTILF                                         )   
( FORTH Utilities:  Secondaries.  FTHUTILA file should assembled prior to )
(     loading this file.                                                  )
BASE @

( PAUSELEN: Variable to hold pause length )
VARIABLE PAUSELEN 0 PAUSELEN !

( PAUSE : Pause for PAUSELEN/1000 seconds )
: PAUSE PAUSELEN @ 1+ 0 DO LOOP ;

( N-A: convert a nibble to its hex ascii equivalent.  n -> )
DECIMAL
: N-A 10 - DUP 0< IF 58 + ELSE 65 + THEN EMIT ;

: DUMP OVER + SWAP ( addr n -> print n nibs starting at addr)
    DO I N@ N-A LOOP ;

: DUMP+ 2DUP + ROT ROT DUMP ; ( Do DUMP; leave next addr on stack )

( SHOW: disp contents of n consecutive memory cells.  { addr n -> } )
: SHOW 1+ 1 DO DUP H. DUP @ 5 SPACES H. PAUSE CR 5+ LOOP ;

: BASE? BASE @ DUP DECIMAL . BASE ! ; ( put current base in X and display)

( DELAY00: Set DELAY0,0)
: DELAY00 " DELAY0,0" BASICX ;

( D- {D P R *}: Do DISPLAY IS {DISPLAY  PRINTER RS232 *} )
: D-P " DISPLAY IS PRINTER" BASICX ;
: D-* " DISPLAYIS*" BASICX ;
: D-D " DISPLAYISDISPLAY" BASICX ;
: D-R " DISPLAYISRS232"BASICX ;

( ROOM?:  Display number of nibbles available in dictionary )
: ROOM? SP0 @ HERE - 458 - . ;

( S.: Print stack contents bottom first )
: S. ." [ " DEPTH 0> IF DEPTH 1 SWAP DO I PICK U. -1 +LOOP THEN ." ] " ;

HEX
( ADDR-    get the addr of the namefield of the previous word.)
(          NFAddr1 -> NFAddr2 )
: ADDR- 5- @ ;

( NFA:  GIVEN CFA, GET NFA.  [ CFA -> NFA ] )
: NFA 2- -1 TRAVERSE ;

( NAME: From NFA, type name  [ nfa -> ] )
: NAME DUP C@ 1F AND 2DUP 1- SWAP 2+ SWAP TYPE
       2* + C@ 7F AND EMIT ;

( NFASTR: Convert NFA to name string.  [NFA -> str] )
: NFASTR DUP 2+ SWAP C@ 1F AND ;

( SPECIAL: Array containing list of words with remote CFA's. )
( 1st value is # of entries. )
CREATE SPECIAL 
   D ,
   E701A , ( COLON )
   E71E8 , ( SEMI )
   E1C54 , ( number )
   E22ED , ( F-number )
   E1C67 , ( DO )
   E3FF1 , ( LOOP )
   E3F81 , ( +LOOP )
   E5D86 , ( IF / UNTIL / WHILE )
   E5D99 , ( ELSE / REPEAT )
   E0640 , ( " )
   E0EFA , ( ." )
   E580E , ( ABORT")
   E0168 , ( J )

( SPEC?: Find CFA in SPECIAL. Return # of entry, or 0 if not present. )
( [ cfa -> cfa # ] )

: SPEC? 0 SPECIAL @ 1+ 1 DO  ( CFA 0 )
   OVER SPECIAL I 5 * + @ =  ( CFA 0 CFA SPECi )
      IF DROP I LEAVE THEN LOOP ; ( CFA # )

( 'NAME: Given CFA, type name.  { CFA -> } )
: 'NAME DUP NFA SWAP OVER - 2/ 1- SWAP NFASTR DUP 4 ROLL = 
      IF 1- 2DUP TYPE 2* + C@ 7F AND EMIT 
      ELSE 2DROP ." Unknown" THEN ;

( HEREN: Find the start of the n+1th link field.  { n -> addr } )
: HEREN DUP C = IF DROP E6FAB ELSE 1+ 5 * E0000 + @  ( NFA )
   BEGIN 5- DUP @ DUP 0 <> WHILE SWAP DROP REPEAT DROP THEN ;

VARIABLE ENDA VARIABLE HERE0

( END: given a CFA, find the addr of the start of the next word   )
( and store in ENDA.   CFA -> )
: 'END DUP E0000 U< IF LATEST HERE
   ELSE DUP NFA C@ 1F AND  ( CFA n )
      DUP 5 * E0000 + @ SWAP HEREN THEN
   DUP HERE0 ! ENDA ! BEGIN 2DUP < WHILE DUP ENDA ! ADDR- REPEAT 
   2DROP ENDA @ HERE0 @ <> IF -5 ENDA +! THEN ;

: 5SP 5 SPACES ;

( +ADDR: Type addr following control word; incr addr. { I -> I+5 } )
: +ADDR 5+ DUP DUP @ + ."  to " H. ;

( "STR: Type the compiled string following a " word [ I 4-or-2 ] )
: "STR SWAP 5+ DUP C@ 2DUP SWAP 5 ROLL + SWAP 22 EMIT SPACE TYPE 22 EMIT
           2* + 1- ;

( FTEMP: FVariable to hold X during decompilation of a FP word )
FVARIABLE FTEMP

( WORDNAME: Given I and its WA, type the word identified; advance I)
( [ I WA -> I' ]  I' = next I -5 )
: WORDNAME DUP ABS 10000 > ( Is this a legitimate word addr? )
        IF SPEC? CASE 0 OF 'NAME ENDOF SWAP DROP
        1 OF ." :" ENDOF
        2 OF ." ;" ENDOF 
        3 OF 5+ DUP @ . ENDOF 
        4 OF 5+ DUP FTEMP STO RDN RCL F. RDN FTEMP RCL B + ENDOF
        5 OF ." DO" ENDOF
        6 OF ." LOOP" +ADDR ENDOF
        7 OF ." +LOOP" +ADDR ENDOF
        8 OF DUP 5+ @ 0> 
      IF OVER 5+ DUP @ + 5- DUP @ 0< SWAP 5- @ E5D99 = AND 
         IF ." WHILE" ELSE ." IF" THEN
      ELSE ." UNTIL" THEN +ADDR ENDOF
        9 OF DUP 5+ @ 0> IF ." ELSE" ELSE ." REPEAT" THEN +ADDR ENDOF
        A OF 4 "STR ENDOF
        B OF ." ." 2 "STR 2- ENDOF
        C OF ." ABORT" 2 "STR 2- ENDOF
        D OF ." J" ENDOF
      ENDCASE     ( addr')
     ELSE DROP ( addr'=addr )
     THEN ;

( WORD@: Given an addr, type it, it's content, and the word identified. )
(    { addr -> addr'}  where addr' = addr of next I )
: WORD@ DUP H. 5SP DUP @ DUP H. ( addr cfa )
   WORDNAME 5+ PAUSE CR ;

( UN:C  Decompile a word, omitting header [ cfa -> ] )
: UN:C ." CFA: " DUP 'END DUP DUP @ - -5 = IF H. 5SP ." Primitive" PAUSE CR
   ELSE WORD@ BEGIN 5SP WORD@ DUP ENDA @ = UNTIL DROP THEN ;  ( )

( UN: Decompile the word named next, including the header )
: UN: ' DUP ." Word: " DUP 'NAME PAUSE CR NFA ( CFA NFA )
     DUP 5- ." LFA: " DUP H. 5SP @ ." Link: " H. PAUSE CR ( CFA NFA )
   DUP ." NFA: " H. 5SP NFASTR 1+ 2* SWAP 2- SWAP DUMP PAUSE CR ( CFA )
   UN:C ;

( RS.  Decompile the return stack, omitting the bottom two levels )
: RS. RP@ RP0 @ 5-
     DO I @ WORD@ DROP -5 +LOOP ;

( RTNSAVE: Variable to hold SST environment. )
( Contents of addr: )
( RTNSAVE = I )
(      +5 = Orig. I --in word that calls DOSST )
(      +A = CFA of SST word )
(      +F = END of SST word )
(     +14 = >RTN )
(     +19 = >RBOT )
( RTNSAVE+E6 points to end of RTNSAVE--temp RP0@)
CREATE RTNSAVE E6 NALLOT
( " CREATE TEXT RTNSAVE:PORT[1],500" BASICX )
( : RTNSAVE " ADDR$['RTNSAVE']" BASIC$ DROP 2- NUMBER DROP 25 + ; )

( NEWRTN: Copy the RTN stack & >RTN to RTNSAVE )
: NEWRTN RP@ RP0 @ ( >RTN >RBOT)
   OVER - SWAP OVER RTNSAVE E6 + 
   SWAP - DUP RTNSAVE 14 + ! ( Save >RTN* ) ROT ( [ >RTN >RTN* # ] )
   NMOVE   ( COPY RTN stack to RTNSAVE)
   RTNSAVE 2FB7F ! ; ( Put RTNSAVE addr in 2FB7F )

( SSTERROR: Onerror routine for single step )
: SSTERROR 0 ONERR ! RTNSAVE 19 + @ RP0 ! ( Restore >RBOT )
     2F7E4 4N@ DUP 2DUP ( 4 copies of errn )
     2EFF > ( >2EFF ? )
    SWAP 2F41 < ( <2F41 ? )
     AND SWAP 0= OR  ( =0? )
     IF R>  ( Go back to ABORT )
    ELSE " BEEP" BASICX " 'ERR:'&MSG$(ERRN)"BASIC$ TYPE SP! RP! QUIT
    THEN ;

( SSTOUT: Variable to hold CFA of word to be executed after DOSST )
VARIABLE SSTOUT

( SST: Single step the word identified by the instruction pointer I* stored )
( at RTNSAVE, unless it is a semi.  Then display the stack. )
: SST RTNSAVE @ DUP @ DUP E71E8 <> ( I WA flag )
      IF WORDNAME DROP 5 SPACES
      [ ' SSTERROR  ] ( Put SSTERROR cfa on stack for onerror )
      LITERAL ONERR !
      DOSST 0 ONERR ! SSTOUT @ EXECUTE
         ELSE ." ;" 2DROP THEN ;

' S. SSTOUT ! ( Initialize SSTOUT to hold S.'s CFA )

( READYSTEP: Set up environment for STEP or BREAK )

( Save I*, END, CFA in RTNSAVE, do NEWRTN )
: READYSTEP ' DUP RTNSAVE A + !  ( Store CFA )
   DUP 'END ENDA @ RTNSAVE F + !   ( Store word END )
   5+ RTNSAVE ! ( Save new I )
   NEWRTN ; 

( STEP: Single step next word.)
: STEP READYSTEP SST ;

( BP: Set a breakpoint.  [ Ib -> ] )
: BP 2FB84 ! ;

( CONT: Continue execution of a BREAKed or SSTed word to next breakpoint )
: CONT [ ' SSTERROR ] LITERAL ONERR ! 
      BRRUN 0 ONERR ! SSTOUT @ EXECUTE  ;

( FINISH: Complete execution of an interrupted word )
: FINISH 0 BP CONT ;

( BREAK: Execute next word, stopping at Ib specified on stack or at the )
(        final ;  [ I -> ] )
: BREAK READYSTEP BP CONT ;

( LIST: List current RAM dictionary.  Same as RAMLIST )
: LIST LATEST BEGIN 
   DUP NAME DUP C@ 40 AND IF ."  Immediate" THEN PAUSE CR 5- @ 
    DUP 0= UNTIL DROP ;

( FSCRATCH : Floating point scratch variable )
FVARIABLE FSCRATCH

( FINDW: Get a word from input stream and return its cfa [ -> cfa ] )
: FINDW BL WORD FIND 0= IF ABORT" Word not found" THEN ;

( PRINT:  Direct the display output of the next word to the printer )
: PRINT 2F78D 2FC79 7 NMOVE ( Save old Display device )
     D-P ( Make printer the display )
     FINDW EXECUTE CR ( Do it and print output)
     2FC79 2F78D 7 NMOVE ( Restore old display )
     7 2F7B1 N! ( Reset display type )
     2FC79 7 0 NFILL ( Zero the assembler variables )
     ;

( SKIP : Set printer to skip over perf mode )
: SKIP " PRINT CHR$(27);'&l1L';"BASICX ;

( TIMED: Execute the next word and display its execution time )
(    The time is left in X.  T is lost on input, and Z & T are lost)
(    on output. )
: TIMED FINDW FSCRATCH
     TIME STO FDROP EXECUTE TIME
     FSCRATCH DUP DUP RCL F-
     TIME STO FDROP         TIME
     F- RCL F+ F. ;

BASE !
