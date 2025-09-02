       FORTH
*
****************************************************************
*                           HEADER
*
* File CXEXTFCN.ASM
* Forth words simulating the HP41CX EXT FUNCTIONS
*
* under BY NC SA CreativeCommons 4.0
* https://creativecommons.org/licenses/by-nc-sa/4.0/deed.en
* pascaldagornet@yahoo.de
****************************************************************
*
* comments to be made after the word FORTH at the top of this file
* .. before FORTH, it would give the issue 
* " HOTD" ASSEMBLE
* PASS 1 .
* >> "missing/multiple file type"
* 
* commands for uploading file into HP71B (or EMU71)
*  in Debian11 PC terminal
*  commands, or create a script asmtoDAT_CXEXTFCN.sh to start with 
*  sh asmtoDAT_CXEXTFCN.sh
*    #
*    #!/bin/bash
*    grep -v '^*' /home/pascal/programming/hp71b/ASM/CXEXTFCN.ASM | cut -c1-23 | sed 's/[[:space:]]\+$//' > 
*      /home/pascal/programming/hp71b/ASM/CXEXTFCN.TXT
*    unix2dos -v /home/pascal/programming/hp71b/ASM/CXEXTFCN.TXT
*    wine '/home/pascal/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe' CXEXTFCN.TXT
*      CXEXTFCN.DAT /T
*    #
*  in forth prompt   FORGET FVTOI  (delete from this word)
*  put HOTD.DAT into DOSLINK
*  in basic prompt.. PURGE CXEXTFCN  (clean the previous version)
*                    COPY :DOSLINK
*                    FORTH
* in forth prompt    “ CXEXTFCN” ASSEMBLE
*
*
* columns..
* 1:label  8:mnemonic  15:modifier  from 24:comments
*
* System entry points: see 14-1 of IMS Vol 1
=POP1N  EQU #0BD1C           ?? Pop 1 Nb off D1 stack into A idsv3 pdf page 1362 use ??
=SIGCHK EQU #0BD98           check NaN in A; idsv3 pdf page 1366 use ??
=uMODES EQU #0BDB1           set the modes S8-11 idsv3 pdf page 1367 use ??
=ARGERR EQU #0BF19           for report invalid arg error
*=PI     EQU #0C000           PI 12digit form (LCHEX) it goes into fn1 which is 
*                             the FRTN1 and probably for BASIC use only..
=SUBONE EQU #0C327           ?? substract 1 to X (A,B) page 1392 of idsv3 use ??
=ADDONE EQU #0C330           ?? add 1 to X = (A,B) (AD15S..) pdf page 1391 idsv3 use ??
=IN2-15 EQU #0C33E           1/X where X=(A,B)
=X/Y15  EQU #0C34F           X/Y where X=(A,B) and Y=(C,D)
=AD2-12 EQU #0C35F           12 digit add = (A) + (C) result 15dig in (A,B) idsv3 pdf page 1394
=AD2-15 EQU #0C363           15-digit add = (A,B) + (C,D) result 15dig in (A,B) idsv3 pdf page 1394
=AD15M  EQU #0C366           page 1394 of idsv3
=AD15S  EQU #0C369           same + SB reset page 1394 of idsv3
=MP2-12 EQU #0C432           12 digit * (A) * (C) result 15dig in (A,B) idsv3 pdf page 1397
=MP1-12 EQU #0C436           15digit(A,B) * 12Digit(C) result 15digit(A,B) idsv3 pdf page 1397
=MP2-15 EQU #0C43A           15-digit multiply (A,B) * (C,D) result 15dig in (A,B) idsv3 pdf page 1397
=MP15S  EQU #0C440           ?? 15-digit multiply (A,B) * (C,D) use ??
=MULTF  EQU #0C446           ?? Multiply float use ??
=SHF10  EQU #0C486           ?? Normalized AB use ??
=DV2-12 EQU #0C4A8           12-digit divise (A) and (C) result in (A,B) idsv3 pdf page 1399
=DV2-15 EQU #0C4AC           15-digit divise (A,B) and (C,d)  result in (A,B) idsv3 pdf page 1399
=DV15S  EQU #0C4B2           ?? 15-digit divise SB not cleared page 1399 use ??
=SQR15  EQU #0C534           idsv3 pdf page 1399 SQR(A,B) result in (A,B)
=XYEX   EQU #0C697           Exchange (A,B) with (C,D)  pdf idsv3 page 1408
=SPLITA EQU #0C6BF           Extend (A) into (A,B)  pdf idsv3 page 1409
=CLRFRC EQU #0C6F4           (A,B) to (A,B) w/o fractio part page 1409 out dec mode
*                              carry set if no fractional part
=FRAC15 EQU #0C70E           frac of (A,B) into (A,B) page 1411 pdf ids3,
*                              works only if x,yyyy and not 0,yyyy
=IF12A  EQU #0C739           entry A. return where the decimal point is. 
*                              Out is the number in P. Page 1412 If E<0, P =14 etc.
*                              alter A,B,C(A),P,CARRY
=INFR15 EQU #0C73D           same as above, input (A,B), alter C(A),P,CARRY  Page 1412
=SPLTAC EQU #0C934           Extend (A) and (C) into (A,B) and (C,D)  pdf idsv3 page 1424
=SPLITC EQU #0C940           Extend (C) into (C,D) pdf idsv3 page 1425
=uRES12 EQU #0C994           Reduce (A,B) into (C) idsv3 pdf page 1426 
*                              uses R3 therefore avoid it
*                              only when =NUMST used?
=NRM12  EQU #0C9BB           ?? Round to 12 sig digits use ??
=YX2-12 EQU #0D274           Y^X & Reg 0 2 3 modified
=STAB1  EQU #0D3D9           Store AB into scratch1 (R0,R1) pdf idsv3 page 1471
=EXAB1  EQU #0D3E7           Exchange AB and scratch1 pdf idsv3 page 1471
=RCCD1  EQU #0D3F5           Recall scratch1 into CD pdf idsv3 page 1471
=STAB2  EQU #0D400           Store AB into scratch2 (R2,R3) pdf idsv3 page 1471
=EXAB2  EQU #0D40E           Exchange AB and scratch2 pdf idsv3 page 1471
=RCCD2  EQU #0D41C           Recall scratch2 into CD pdf idsv3 page 1471
=STCD2  EQU #0D427           Store CD into scratch2 (R2,R3) pdf idsv3 page 1471
=uTEST  EQU #0D435           ?? User real comparison A & C in 12digit pdf idsv3 Page 1484..6 use ??
*                                with carry which has the result
=TST15  EQU #0D47A           Compare numbers 15dg AB vs CD idsv3 pdf page 1486
*                             P has the cell# assoClated width the nUMber pair, 
*                              arg's in 15-dig forM unchanged. pdf page 1486 of idsv3
=ARG15  EQU #0D67F           ARG((A,B),(C,D)), result in (A,B) R0 R1 touched pdf ids v3 page 1498
*                             is it the angle?
=MAKEPI EQU #0D6F1           ?? Put PI into (C,D) pdf page 1499 idsv3 use ??
=SPLTA  EQU #0D706           = splita ids v3 page 1500
=SPLTB  EQU #0D70C           = splitb ids v3 page 1500
=SIN12  EQU #0D716           = splita ids v3 page 1502
=SIN15  EQU #0D71A           ids v3 page 1502
=COS12  EQU #0D721           = splita ids v3 page 1502
=COS15  EQU #0D725           ids v3 page 1502
=TWO*   EQU #0DB38           ?? Dbl precision doubler use ??
=PI/2   EQU #0DB77           load PI/2 into (C,D) idsv3 pdf page 1517
=ATAN12 EQU #0DBBA           atan of 12 digit args (A) result in (A,B) ids v3 page 1520
=ATAN15 EQU #0DBBE           atan of 15 digit args (A,B) result in (A,B) ids v3 page 1520
=SB15S  EQU #0E19A           substraction 15 digits (A,B) = (A,B) - (C,D) . needs SETDEC before
=DMP15S EQU #0E1B3           SETDEC then MP15S
=ures12 EQU #0E1C4           ?? use ids v3 page 1551. point to =IVP page 1391
=uRESD1  EQU #0E1EE          Reduce (A,B) into (C), similar uRES12, dont point to D1
*                            uses R3,P=14 ids3 pdf page 1553
=SPLTAX EQU #0E62B           SETDEC, then SPLITA Page 1585
=SIGTST EQU #0E636           ?? Handle signal NaN Page 1585 use?? 
=ARGPRP EQU #0E8EF           pop and normalize real number from top math stack ids3 pdf p1603, 
*                              exit with number in A/B. BASIC only ?
*                              1. call POP1R
*                              2. call SPLTAX
=POP1R  EQU #0E8FD           v3p1605
*                              RTN of POP1N #0BD1C Seite 1361
*                              which takes 1 arg off mathstack, Result in A
=BP     EQU #0EADF           make beep float A HZ Float C duration sec pdf ids3 page 1625
=STSCR  EQU #0E92C           ?? Push (A,B) into top math scratch stack page 1607 use??
=RCLW1  EQU #0E981           ?? recall 1 top math stack entry page 1610
*                             move (AB) to (CD) then recal into (AB) use??
=IDIV   EQU #0EC7B           full word integer divide; A/C Integer division pdf page 1635 ids3
*                            Quotien A, Reminder B and C
=HEXDEC EQU #0ECAF           Arg A(A) Hex to Dec use vs HDFLT? idsv3 pdf p1636
=HXDCW  EQU #0ECB4           Hex to decimal
=CMPT   EQU #125B2           System entry points:  Read time
=FLTDH  EQU #1B223           Convert 12digit flt A to 5 digit Hex Integ A(A) pdf 2627 idsv3, see 
*                              out with hex mode, see FTOI forth/asm ids page pdf 308
=HDFLT  EQU #1B31B           change hex integ A(A) to 12dig float in A(W) exit DEC mode idsv3 pdf p2631
=FLOAT  EQU #1B322           change dec integ to 12dig float in A(W)
*=FNRTN4 EQU #0F238           function return, page 1679 ONLY FOR BASIC
*                             entry D0=PC,D1 new stack pointer,C(W) to be pushed on stack
*=STORE  EQU #0F5F8           store number opr string in register.. use? v3p1698
=MTHSTK EQU #2F599           data stack
=FUNCD0 EQU #2F8BB           function scratch RAM allocation 5 nibbles
*                             see FUNCD1 FUNCR0..R1 too page 3089
=OL     EQU #2FBC0           L address
=OX     EQU #2FBD0           X address
=OY     EQU #2FBE0           Y address
=OZ     EQU #2FBF0           Z address
=OT     EQU #2FC00           T address
*
* FORTH entry points:
=LT     EQU #E01EF           ( n1 n2 --<-- flag ) ids asm pdf page 170
=GT     EQU #E0231           ( n1 n2 -->-- flag ) ids asm pdf page 169
=IP     EQU #E06B8           Integer part of X (clear the fractional part); page 192
=NOWADD EQU #E08AA           page 199
=F-     EQU #E08C5           substract page 199
=FSTRT  EQU #E08D0           SAVEFP uMODES STKDRP GETXLN. page 199
=FEND   EQU #E08E9           GOLONG =PUTABX (GETFP). page 199
=CHS    EQU #E1518           change X sign; dont change LastX; asm ids pdf page 259
=STO    EQU #E15B4           store X into address; asm ids pdf page 262
=RCL    EQU #E15EB           recall into X; p263
=NUMST  EQU #E1718           uMODES, SAVEFP,GETX+L.. GET X INTO (A,B); put X into L; asm ids pdf page 266
=RDN    EQU #E1839           page 269
=MOD    EQU #E1CA1           modulo ids Forth pdf page 287
=ABS    EQU #E1A23           (n -- |n|)
=XXYY   EQU #E212D           comparison of a(X?) and c(Y?) (or zero) pdf page 304 Forth/asm ids
=CMPST  EQU #E216C           comparison operator routine pdf page 304 Forth/asm ids
=ITOF   EQU #E21FD           ( n -- ) ( -- f(n) ) integ to float pdf page 307 Forth/asm ids
=FTOI   EQU #E2267           change float in X to integer pdf page 308 Forth/asm ids
=X<>Y   EQU #E23A5           pdf page 313 Forth/asm ids
=OVER   EQU #E2538           (n1 n2 -- n1 n2 n1) pdf page 319 Forth/asm ids
=DDUP   EQU #E255A           =2DUP; p319
=FDROP  EQU #E30FB           drop X content; X become Y value; dont change LastX 
*                              pdf page 371 Forth/asm ids
=FLTOH  EQU #E466D           Forth asm p453 .. FLTDH more (v3p2627)
=SAVEFP EQU #E717A           save Forth pointer; pdf page 602 Forth/asm ids
=GETFP  EQU #E71A5           restore Forth pointers; pdf page 602 Forth/asm ids
=GETX   EQU #E728A           Put X into (A,B) ; pdf page 608 Forth/asm ids
=GETXLN EQU #E72BF           put X Y into (A,B) & (C,D); pdf page 608 Forth/asm ids
*                              use with stack drop
=GETX+L EQU #E72DF           Put X into (A,B) and X in L; pdf page 608 Forth/asm ids
=PUTABX EQU #E72F5           Put (A,B) into X, uRES12 (AB into C),GETFP ; pdf page 609 Forth/asm ids
=STKLFT EQU #E7320           Stacklift Forth OM page 609 pdf LastX dont change
=STKDRP EQU #E734C           Stackdrop Forth OM page 609 pdf LastX change 
*                              A and C most likely overwritten
*
* general remarks:
*   P=0 must be restored before leaving a Word
*   RES12 MODES GETX+L using D1 therefore D1 has to be saved before its use
*   SETDEC has to be used before most of math functions
*   ADDONE and SUBONE has a warning message WRN
*     use XM=0 and SB=0 , see GAMMA word
*   X<>L and others could be deleted because it is the same like L X<>
*     or use L X <F>
*   GOSBVL NUMST is making issues with GETX+L where sign is checked. Avoid sometimes.
*   dont touch D0 outside of SAVEFP and GETFP
*   SAVEFP and GETFP: check where really necessary for optimizing code
*   GOSBVL =SAVEFP then =GETFP take the freeze and WRN message away
*   A label cannot start with 0 or 1 or.. 9 letters
*   2CYCL label use gives an error because 2 in it
*   ignore the messages * warning: word not unique, in line xx
*    (?dont appear when clean the HP71B from file and words, 
*    switch off, upload new file in :DOSLINK entry, switch on,
*    COPY :DOSLINK, FORTH, " FILENEW" ASSEMBLE)
*   the entries can have = in the front of them or not ( = is ignored)
*   uRESD1 use R3 therefore be carefull
*   nothing before the line "       FORTH"
*   P= 0   improve sometimes?  before LC(5) anyway. see P= 5 in TIME, too
*   use NUMST in the beginning with PUTABX in the end, and Res12 in the middle
*     else issues with ResD1 etc. by mixing.. see example below
*   avoid FRAC15 if 0,xxxxx already there; test before if =IF12A
*   use GONC after CLRFRC (use FLTOH) or put before XM=0 if use of FLTDH
*   0.16666666667 in A had an issue with SPLITA
*
*      1      12       3
*     +-+------------+---+
* A = |S|MMMMMMMMMMMM|EEE|
*     +-+------------+---+
* P    15      ..       0
* EEE = X
* MMM.. = M
* S = S
* all = W
*
*      1      10      5
*     +-+----------+-----+
* A = |S|??????????|EEEEE|
*     +-+----------+-----+
* B = |?|MMMMMMMMMMMMMMMM|
*     +-+----------+-----+
* EEEEE = A
*
*
* *******
* data type from IDS V1 9.10 p202 of 568 in pdf. 
*   See forth manual too p497 of 1081 pdf
* data types on stack (from IDS V1 7.4.3 p174):
*
*                  +-+----------+---+
* real number      |S| Mantissa |Exp|
*                  +-+----------+---+
*                   1     12      3
*
*                  +-+----------+---+-+----------+---+--+
* complex number   |S| Mantissa |Exp|S| Mantissa |Exp|0E| 
* (real/imag/tag)  +-+----------+---+-+----------+---+--+
*                   1     12      3  1     12      3  2
*
*                  +------------+-----+-------+------+--+
* string           | String...  |MaxLn|Address|Length|0F|
* (string/descr)   +------------+-----+-------+------+--+
*                                  4      5       5   2
*
* nonexistent      +------------+------+--+
* string array el  |Ele#|00|Name|Length|8F|E466D
*                  +------------+------+--+
*                     4   2   3     5   2
*
* Array            +-------+------+------+-+-+-+
* descriptor       |Pointer|rowmax|colmax|b|#|t|
* b=base, t=type   +-------+------+------+-+-+-+
*                     5       4      4    1 1 1
* type t: A=integer, B=short, C=real, D=shrt cmplx, E=cmplx
* base b: 0 or 1 , dim #: 1 or 2
* *******
*
*
****************************************************************
*
* TODO or to explain what is the similar word in Forth HP71B
*
* CLs         >> tbd Clear statistics registers (om en page 99)
* GRAD        >> tbd GRAD Mode. 360 degrees = 2 pi radians = 400 grads
* MEAN        >> tbd see statistics
* SDEV        >> tbd standard deviation statistics (om en page 101)
* s+          >> tbd statistics registers (om en page 99)
* s-          >> tbd statistics registers (om en page 99)
* sREG        >> tbd beginning of a block of six statistical registers (om en page 99)
* SST         >> tbd with Forth bebugger. Single step (om en page 129, 132). Not programmable.
*
* extend the Forth with new words
*
* stick back to HP41 wording and behaviour, example VIEW instead of FV.
*
*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* BACKLOG / REMARKS
*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
* 1/FV and all others float words like FVABS: action directly in Float variable
* HP15 & Warpcore HP41 like variable comparison
*   ?0= ?0# ?0< ?0<= ?0> ?0>= 
*   ?X= ?X# .. Y Z T L
* IRCL integer rcl 
* X=YZ?
* X=YZT?
* SST     sort x y z t stack
* 00..N Registers   
*             >> N REG41CR REG41 Creates an array REG41 of integer pointer of maximum N float register
*                   in the FORTHRAM. Another method for another place in RAM scratch or other?
*             >> NN REG41     Recall the address of the NNth register
*             .. further to be done for simulating the registers 00 to 329 on HP41
* STO and RCL should then still work
****************************************************************
* HP41 and HP71B Forth float functions (standard or new) 
* list created with HP41/ILPER with SF 15  CAT 3
* Legend: () means no need because covered by another function
*             or different behaviour
*         >> tbd word to be worked out
*         (Forth standard) mean word in the Forth of HP71B Forth/ASM module
*         (New ASM word) 
*             https://github.com/f4iteightiz/HP71B_forth/blob/main/MAFO.TXT 
*         (new Forth word) 
*             https://github.com/f4iteightiz/HP71B_forth/blob/main/DISPLAY.SRC
*             https://github.com/f4iteightiz/HP71B_forth/blob/main/H71B1.SRC
*
* .. regarding the ALPHA functions
* .. No ALPHA register on HP71B but several strings can be created and extensive output words can be used
* ARCL        () put reg data into the ALPHA register, om en page 70
*                STR$
* ASHF        () shift the left-most six characters in the ALPHA
*                SUB$ could create a new string from the character 7 on
* ASTO        () store ALPHA string into Register (max 6 letters), om en page 70
*                STRING define a new variable string then merge another string into it
*                with the use of S<&
* AVIEW       () show ALPHA string into screen, om en page 151
*                ." SOMETHING " 
*                INTEGERVAR @ .
*                STRINGVAR OUTPUT
*                FVAR1 F.
*                FS. the whole float stack
* CLA         () Clear ALPHA register (page 41).
*                Since several strings variables can be created and the display can be a line or
*                a screen, diverse methods can be defined
*                . Put the length of a string variable to zero means it would be cleared 
*                  : CLEARSTR DROP DUP 1 CHARS - 0 SWAP C! 0 ;
*                . Cleaning the HP71B screen line, use CR
*                . Cleaning a 80column screen 
*                  2 STRING RES2CHR 
*                  RES2CHR 27 CHR$ S<& 69 CHR$ S<& 2DROP
*                  RES2CHR OUTPUT .. OK { 0 } screen is clean 
*                  or : PAGE 27 EMIT 69 EMIT ;
*
*
* HP41        HP71B Forth word
* ....        ....
* +           F+ (Forth standard)
* -           F- (Forth standard)
* *           F* (Forth standard)
* /           F/ (Forth standard)
* 1/X         1/X (Forth standard)
* 10^X        10^X (Forth standard)
* ABS         ABS (Forth standard) for integer from integer stack
*             FABS (Forth standard) for float number in X
* ACOS        ACOS (Forth standard) 
* ADV         CR (Forth standard)
*                advance paper if printer in system om en HP41 page 105
*                activate the printer as output in Forth with D-P then use CR
* AOFF        () takes the HP-41C out of ALPHA mode; HP71 entry are num AND ALPHA
*                therefore no need for a switch in Forth
* AON         () function places the HP-41C into ALPHA mode; HP71 entry are num AND ALPHA
*                therefore no need for a switch in Forth
* ARCL        see above
* ASHF        see above
* ASIN        ASIN (Forth standard)
* ASN         () Assign. not programmable on HP41, om en page 270
* ASTO        see above
* ATAN        ATAN (Forth standard)
* AVIEW       see above
* BEEP        BEEP41 (New ASM word)
* BST         () Back step (om en page 132). Not programmable. See HP71B Forth debugger
* CAT         LIST ( = CAT 3)
* CF          ST=x  y  look at the ST Forth status flags in HP71B
* CHS         CHS for the X register (Forth standard)
*             FVCHS (new ASM word for float variable CHS)
* CLA         see above. PAGE word would clean the screen.
* CLD         () clear display (example something displayed with AVIEW): 
*                CR (Forth Word) or PAGE (Forth Word)
* CLP         FORGET (Word).. and all words after   -1 REG41 ?
* CLRG        CLRG (New Forth word) clear all activated register
*             CLFV (New ASM word) clear single register
* CLs         >> Clear statistics registers (om en page 99)
* CLST        CLST (New ASM word) clear stack; lastX not cleaned
* CLX         CLX (New ASM word)  clear X
* COPY        () " XYZ" LOADF, load xyz into FORTHRAM.. 
*             HP41 Copy (download or copy). 
*             Requires ALPHA program name input (om en page 260). 
*             Not programmable in HP41.
* COS         COS (Forth standard)
* D-R         DEG-RAD (New ASM word)
* DEC         () Oct to Dec conversion page pdf 111 of om em manual 
*                or https://rosettacode.org/wiki/Non-decimal_radices/Convert#Forth
*                in Forth, use the 8 BASE ! NUMBERINTEGER1 DECIMAL for changing the values
*                in the INTEGER stack from OCT base to DECIMAL. 
*                example 8 BASE ! 326 DECIMAL .
*                        214  OK { 0 } 
*                        which mean 326 in OCT is 214 in decimal
*                The float stack is everytime DECIMAL and any FTOI will
*                be a change DECIMAL to BASE
* DEG         DEGREES
* DEL         () see editor
* DSE         DSE (New ASM word) see I DO LOOP of Forth.
*                (of Variable FVARx X Y Z T L..)
* END         ; (Forth standard)
* ENG         ENG (Forth standard)
* ENTER^      FENTER (Forth standard)
* E^X         E^X (Forth standard)
* E^X-1       () see E^X
* FACT        FACT  (New ASM word)
*             FACX  (new Forth word) factorial of X, fractional part ignored
*             FACI  (new Forth word) factorial for integer in integer stack
*             GAMMA (New ASM word)
* FC?         () look at the ST Forth status flags in HP71B ASM
* FC?C        () look at the ST Forth status flags in HP71B ASM
* FIX         FIX (Forth standard)E466D
* FRC         FP  (Forth standard)
* FS?         () look at the ST Forth status flags in HP71B
* FS?C        () look at the ST Forth status flags in HP71B
* GRAD        >> tbd GRAD Mode. 360 degrees = 2 pi radians = 400 grads page pdf 98 hp41 manual
*             GRA>RAD ? since no GRADE mode exists in HP71B. Only RADIANS and DEGREES mode so far defined in HP71B
*             GRA>DEG RAD>GRAD DEG>GRAD ?
* GTO         ()
* HMS         HMS  (New ASM word)
* HMS+        HMS+ (New ASM word)
* HMS-        HMS- (New ASM word)
* HR          HR   (New ASM word)
* INT         IP (Forth standard)
* ISG         ISG (New ASM word) see I DO LOOP of Forth.
*                (of Variable FVARx X Y Z T L..)
* LASTX       LASTX (Forth standard)
* LBL         () look at the Forth word ":"
* LN          LN (Forth standard)
* LN1+X       () see LN
* LOG         LGT (Forth standard)
* MEAN        >> tbd see statistics
* MOD         PMOD (New Forth word)
*             >>
*             78 7 PMOD .   > 1  OK { 0 } 
*             -78 7 PMOD .  > 6  OK { 0 } 
*             78 -7 PMOD .  > -6  OK { 0 } 
*             -78 -7 PMOD . > -1  OK { 0 }
*             .. because MOD of HP41
*             78 7 MOD      > 1
*             -78 7 MOD     > 6
*             78 -7 MOD     > -6
*             -78 -7 MOD    > -1
*             .. and MOD HP71 (standard word)
*             78 7 MOD .    > 1  OK { 0 } 
*             -78 7 MOD .   > -1  OK { 0 } 
*             78 -7 MOD .   > 1  OK { 0 } 
*             -78 -7 MOD .  > -1  OK { 0 } 
*             .. and MOD Gforth
*             78 7 MOD .    > 1  ok
*             -78 7 MOD .   > 6  ok
*             78 -7 MOD .   > -6  ok
*             -78 -7 MOD .  > -1  ok
*             <<
* OCT         () Dec to Oct conversion page pdf 111 of om em manual
*                in Forth, use the 8 BASE ! NUMBERINTEGER1 DECIMAL for changing the values
*                in the INTEGER stack from OCT base to DECIMAL. 
*                example 8 BASE ! 326 DECIMAL .
*                        214  OK { 0 } 
*                        which mean 326 in OCT is 214 in decimal
* OFF         () function simply turns the HP-41C power off
* ON          () When you execute the function ((xEQ) ON (APHA] ), 
*             the turn-off feature is disabled and the HP-41C will no
*             longer automatically turn itself off
* P-R         P-R (New ASM word) Polar to Radian coordinates (om en manual page 94).
* PACK        () Pack program memory (page 141). Not programmable in 41
*                look at Forth 71B SHRINK and GROW for similar function
* %           %OF (New ASM word). "percent" function is a two-number function that 
*                   allows you to compute percentages.HP41 om en manual page 83.
* %CH         %CH (New ASM word). "percent of change" function is a two-number function 
*                   that calculates the percent increase or decrease from a number 
*                   in the Y-register to the number in the X-register.
*                   HP41 om en manual page 83.
* PI          PI (New ASM word)
* PROMPT      () displays the contents of the ALPHA register 
*                and stops program execution (om en manual page 51).
*                wait for data input.
*                EXPECT96 in Forth
* PSE         PAUSE (Forth standard)
* R^          RUP (Forth standard)
* R-D         DEG-RAD (New ASM word)
* R-P         R-P (New ASM word) Radial to Polar conversion HP41 om en page 92
* RAD         RADIANS (Forth standard)
* RCL         RCL (Forth standard)
*             >> RCLIND (New ASM word)
* RDN         RDN (Forth standard)
* RND         RND (New Forth word) round according N in integer stack and 
*                  not accordingly the activ SCI or ENG stored value 
*                  Round HP41 om en page 78. 
* RTN         ; (Forth standard)
* SDEV        >> tbd standard deviation statistics (om en page 101)
* SCI         SCI, scientific notation display om en page 33
* SF          () look at the ST Forth status flags in HP71B
* s+          >> tbd statistics registers (om en page 99)
* s-          >> tbd statistics registers (om en page 99)
* sREG        >> tbd beginning of a block of six statistical registers (om en page 99)
* SIN         SIN (Forth standard)
* SIGN        XSIGN ( New ASM; put 1 in X if X>=0, else -1)
*             SIGN in Forth is for integer only (not float)
* SIZE        SIZE (New Forth Word) allocate X registers (variable). Not programmable in HP41
*              use SIZE REG41 14 create array of 14 elements
*                  4 REG41 RCL is like RCL 04 in HP41
*                  -1 REG41 return the size of REG41 which is here 14
*                    this is like SIZE? in HP41
*                  -2 REG41 clear all list REG41  
*                    this is like CLRG in HP41 (manual CX omv2 odf p277)
*                  2,010 -3 REG41 clear all registers 2..10 of REG41
*                    this is like 2,010 CLRGX in HP41 (manual CX omv2 odf p277)
* SQRT        SQRT  (Forth standard)
* SST         >> tbd with Forth bebugger. Single step (om en page 129, 132). Not programmable.
* ST+         ST+ (New ASM word)
* ST-         ST- (New ASM word)
* ST*         ST* (New ASM word)
* ST/         ST/ (New ASM word)
* STO         STO (Forth standard)
*             >> STOIND (New ASM word)
* STOP        () HP71B key ATTN Stops program execution (hp41 om en page 145).
* TAN         TAN (standard Forth)
* TONE        TONE (New ASM word)
*
* VIEW        FV.  (New Forth word)
*             ..
*             X FV.
*               9.000000 
*             OK { 0 } 
*
* X=0?        X=0?  (standard Forth)
* X#0?        X#0?  (New ASM word)
* X<0?        X<0?  (New ASM word)
* X<=0?       X<=0? (New ASM word)
* X>0?        X>0?  (New ASM word)
* X=Y?        X=Y?  (standard Forth)
* X#Y?        X#Y?  (standard Forth)
* X<Y?        X<Y?  (standard Forth)
* X<=Y?       X<=Y? (standard Forth)
* X>Y?        X>Y?  (standard Forth)
* X<>         X<>   (New ASM word)
* X<>Y        X<>Y  (standard Forth)
* XEQ         () call the word in the Forth prompt
* X^2         X^2   (standard Forth)
* Y^X         Y^X   (standard Forth)
*
****************************************************************
* list created with HP41/ILPER with..
*   SF 15 CAT 2 R/S ENTER
*
*
*-TIME  2C   
*ADATE       
*ALMCAT      
*ALMNOW      
*ATIME       
*ATIME24     
*CLK12       CLK12 (new Forth Word)
*CLK24       CLK12 (new Forth Word)
*CLKT        
*CLKTD       
*CLOCK       
*CORRECT     
*DATE        
*DATE+       
*DDAYS       
*DMY         
*DOW         
*MDY         
*RCLAF       
*RCLSW       
*RUNSW       
*SETAF       
*SETDATE     
*SETIME      
*SETSW       
*STOPSW      
*SW          
*T+X         
*
* TIME        TIME (ASM in FTHUTILA) with different output format
*
*  HP41 XEQ TIME
*  7:47:19 PM   
*  19.47198500 ***
*  XEQ PRSTK
*
*  T=  0.000000000
*  Z=  0.000000000
*  Y=  0.000000000
*  X=  19.47198500
*  X is in HH.MMSSss format
*
*  HP71B Forth
*  TIME
*  OK { 0 } 
*  FS.
*  X=  42443.44 
*  X is in SSSSSS.ss format
*
*  ASM Word T71>41 change SSSSSS.ss into HH.MMSSss format
*  >>> new ASM Word T41>71 ?? need?
*
*XYZALM      
*
*
*-CX TIME    
*CLALMA      
*CLALMX      
*CLRALMS     
*RCLALM      
*SWPT        
*..
*
*-PRINTER 2E see IR Module manual pdf92
*ACA         Accumulate contents of ALPHA register into print buffer
*ACCHR       Accumulate standard character into print buffer
*ACCOL       Accumulate graphics column into print buffer
*ACSPEC      Accumulate special character into print buffer
*ACX         Accumulate contents of X-register into print buffer
*BLDSPEC     Build special character in X- and Y-registers
*LIST        List program lines.(Not programmable.)
*PRA         Print contents of ALPHA register.
*PRAXIS      Print and label y-axis of plot.
*PRBUF       Print contents of print buffer left-justified.
*PRFLAGS     Plot function noninteractively.
*PRKEYS      Print list of reassigned keys.
*PRP         Print program. (Not programmable.)
*PRPLOT      Plot function interactively.
*PRPLOTP     Plot function noninteractively.
*
* PRREG      PRREG  (new Forth word)
*            Print contents of all storage registers.
*   HP41.. XEQ PRREG
*   blank line
*   R00=  0.000000000
*   R01=  0.000000000
*   R02=  0.000000000
*   R03=  0.000000000
*   R04=  0.000000000
*   R05=  0.000000000
*   R06=  0.000000000
*   R07=  1.456791031
*   R08=  1.456791031
*   R09=  4.492639890
*   R10=  32.00000000
*   R11=  0.000000000
*   R12=  0.000000000
*   ..
*   R98=  0.000000000
*   R99=  0.000000000
*
*   HP71B Forth
*   15 SIZE REG41 
*   PRREG REG41
*   blank line
*   R0 = 0 
*   R1 = 0 
*   R2 = 0 
*   ..
*   R13 = 0 
*   R14 = 0  OK { 0 } 
*             
*
*PRREGX      Print contents of specified storage registers.
*PRs         Print contents of statistics registers.
*
* PRSTK       FS.  (New in Forth; see DISPLAY.SRC)
*             Print contents of X-, Y-, Z-, T-registers
*             ..
*             HP41.. XEQ PRSTK
*             blank line
*             T=  1.456791031
*             Z=  9.688448224
*             Y=  0.000000000
*             X=  1.000000000+78
*             HP71B Forth.. D-P FS.
*             T=  2.000000 
*             Z=  7.000000 
*             Y=  -3.000000 
*             X=  9.000000 
*             L=  4.000000 
*             OK { 0 } 
*
* PRX         D-P F.  (standard Forth)
*             ..
*             HP41.. XEQ PRX
*             0.000000000 ***
*             HP71B Forth.. D-P F.
*             0.000000000  OK { 0 }
*
*REGPLOT     Plot single function value using storage registers.
*SKPCHR      Accumulate skipped characters into print buffer.
*SKPCOL      Accumulate skipped dot columns into print buffer.
*STKPLOT     Plot single function value using stack registers.
*FMT         Specify formatting option for printed output.
*--          
*
*
*-MASS ST 1H 
*CREATE      
*DIR         
*NEWM        
*PURGE       
*READA       
*READK       
*READP       
*READR       
*READRX      
*READS       
*READSUB     
*RENAME      
*SEC         
*SEEKR       
*UNSEC       
*VERIFY      
*WRTA        
*WRTK        
*WRTP        
*WRTPV       
*WRTR        
*WRTRX       
*WRTS        
*ZERO        
*--          
*
*
*-CTL FNS    
*AUTOIO      
*FINDID      
*INA         
*IND         
*INSTAT      
*LISTEN      
*LOCAL       
*MANIO       
*OUTA        
*PWRDN       
*PWRUP       
*REMOTE      
*SELECT      
*STOPIO      
*TRIGGER     
*
*
*-SNDMTH'4X4 
*2^X-1       
*s1/N        
*sN^X        
*AGM         AGM (New ASM word); EPER file
*AINT        
*BS>D        
*CBRT        
*CEIL        
*CF2V        
*CHSYX       
*CROOT       
*D>BS        
*D>H         
*DERV        
*E3/E+       
*FLOOR       
*GEU         
*GHM         
*GMSLVR      
*H>D         
*HMS*        
*LOGYX       
*MKEYS       
*PVL         
*QREM        
*QROOT       
*QROUT       
*R>P         
*R>S         
*S>R         
*STLINE      
*s^123       
*VMANT       
*X^3         
*X=1?        
*X=YR?       
*X>=0?       
*X>=Y?       
*Y^1/X       
*Y^^X        
*PRGM        
*-FRC        
*D>F         
*F+          
*F-          _
*F*          
*F/          
*FRC?        
*INT?        
*-HYP        
*HACOS       
*HASIN       
*HATAN       
*HCOS        
*HSIN        
*HTAN        
*-RCLIO      
*AIRCL       
*RC^         RC^ (New ASM word)
*RC+         RC+ (New ASM word)
*RC-         RC- (New ASM word)
*RC*         RC* (New ASM word)
*RC/         RC/ (New ASM word)
*
*
*-HL MATH+   
*1/GMF       
*sFL         
*sF#         
*sF$         
*BETA        
*CHBAP       
*CI          
*DSHT        
*EI          
*ELIPK       
*ENX         
*ERF         
*FFOUR       
*FINTG       
*FROOT       
*
*GAMMA       GAMX  (New Forth word)
*            GAMMA (New Forth/ASM word based on GAMMA from Math module
*
*HCI         
*HGF+        
*HSI         
*IBS         
*ICBT        
*ICGM        
*IERF        
*IGMMA       
*JBS         
*KBS         
*LINX        
*LNGM        
*LOBACH      
*PSI         
*PSIN        
*SI          
*SJBS        
*SYBS        
*TAYLOR      
*WL0         
*YBS         
*ZETA        
*ZETAX       
*-PB/STS     
*%T          
*CORR        
*COV         
*`CURVE      
*EVEN?       
*GCD         
*LCM         
*LR          
*LRY         
*NCR         
*NPR         
*ODD?        
*PDF         
*PFCT        
*PRIME?      
*RAND        
*RGMAX       
*RGSORT      
*RGSUM       
*SEEDT       
*ST<>s       
*STSORT      
*TVM$        
*
*
*-EXT FCN 2D 
*ALENG       
*ANUM        
*APPCHR      
*APPREC      
*ARCLREC     
*AROT        
*ATOX        
*CLFL        
*CLKEYS      
*CRFLAS      
*CRFLD       
*DELCHR      
*DELREC      
*EMDIR       
*FLSIZE      
*GETAS       
*GETKEY      
*GETP        
*GETR        
*GETREC      
*GETRX       
*GETSUB      
*GETX        
*INSCHR      
*INSREC      
*PASN        
*PCLPS       
*POSA        
*POSFL       
*PSIZE       
*PURFL       
*RCLFLAG     
*RCLPT       
*RCLPTA      
*REGMOVE     
*REGSWAP     
*SAVEAS      
*SAVEP       
*SAVER       
*SAVERX      
*SAVEX       
*SEEKPT      
*SEEKPTA     
*
*SIZE?       SIZE? (new Forth word) Returns the number of data storage registers in the main memory to the X register
*              on HP71B dont return it into 
*            15 SIZE REG41 .. it creates an array of 15 float
*            SIZE? REG41
*            output..  STORAGE SIZE IS: 15 , REGISTERs ID 0..14  OK 
*
*STOFLAG     
*X<>F        
*XTOA        
*
*
*-CX EXT FCN 
*ASROOM      
*
*CLRGX       CLRGX (New Forth word) clear the register bbb when
*               bbb.eeeii in X
*
*ED          
*EMDIRX      
*EMROOM      
*GETKEYX     
*RESZFL      
*sREG?       
*
* X=NN?      X=NN? (New ASM word)
* X#NN?      X#NN? (New ASM word)
* X<NN?      X<NN? (New ASM word)
* X<=NN?     X<=NN? (New ASM word)
* X>NN?      X>NN? (New ASM word)
* X>=NN?     X>=NN? (New ASM word)
*
*
*
* RAMBOX64 functions
*
* others
* XYZT>XZTT
* XYZT>XYTT
*
*
****************************************************************
*X=NN?       
*X#NN?       
*X<NN?       
*X<=NN?      
*X>NN?       
*X>=NN?      
* check if the value in X with the register then put 0 (false) 
*   or -1 (true) into the integer stack
*   similar to HP41; EXT FCN 2D ;
*   however use of an integer stack instead of program counter 
*   jump or not (like the Forth word X=0?)
*   no float stack change
*   tested 13 Mai 2025
*     1 REG41 RCL FS.    
*    (see the FORTH word SIZE from SIZE.SRC which creates REG41)
*     T=  10.3033000000E0 
*     Z=  10.2527000000E0 
*     Y=  23.5000000000E0 
*     X=  160.875000000E0 
*     L=  23.3000000000E0 
*      OK { 0 } 
*     X<>Y
*      OK { 0 } 
*     1 REG41 X<NN?
*      OK { 1 } 
*     .
*     -1  OK { 0 } 
*
       WORD 'X=NN?'
       GOSUB  ENTAC
       P=     2              see TST15 idsv3p1486
       GOTO   CMP1
* X#NN?
       WORD 'X#NN?'
       GOSUB  ENTAC
       P=     13             see TST15 idsv3p1486
*                            5 was used in X#0?
       GOTO   CMP1
* X<NN?
       WORD 'X<NN?'
       GOSUB  ENTAC
       P=     4              see TST15 idsv3p1486
       GOTO   CMP1
* X<=NN?
       WORD 'X<=NN?'
       GOSUB  ENTAC
       P=     6              see TST15 idsv3p1486
       GOTO   CMP1
* X>NN?
       WORD 'X>NN?'
       GOSUB  ENTAC
       P=     1              see TST15 idsv3p1486
       GOTO   CMP1
* X>=NN?
       GOSUB  ENTAC
       P=     3              see TST15 idsv3p1486
*
CMP1   GOSBVL =uTEST
       SETHEX
       A=0    A
       GONC   TRO
       A=A-1  A
TRO    GOSBVL =GETFP
       DAT1=A A
       RTNCC
*
ENTAC  GOSBVL =SAVEFP
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 (the instruction pointer) for now reading the values
       A=DAT0 W              copy data at address in D0 (in C(A)) into W in A
       R0=A                  R0: value of data at address for later
       P=     0
       LC(5)  =OX            put X-address into field A of register C low order 5 nibbles
       CD0EX                 put data pointer D0 into C(A)
       C=DAT0 W              C = value of X (which is in D0)
       A=R0
       RTN
*
****************************************************************
*
****************************************************************
       END
