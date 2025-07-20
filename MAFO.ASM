       FORTH
****************************************************************
*                           HEADER
*
* under BY NC SA CreativeCommons 4.0
* https://creativecommons.org/licenses/by-nc-sa/4.0/deed.en
* pascaldagornet@yahoo.de
****************************************************************
*
* columns..
* 1:label  8:mnemonic  15:modifier  from 24:comments
*
* System entry points: see 14-1 of IMS Vol 1
=OUTRES EQU #0BC84           ures12 (.. A/B into C) & FN4  v3p1357
=uMODES EQU #0BDB1           set the modes v3p1367
=FN4    EQU #0BDE0           FNRTN4 v3p1368
=SUBONE EQU #0C327           substract 1 to (A,B) page 1392 of idsv3
=ADDONE EQU #0C330           add 1 to (A,B) (AD15S..) pdf page 1391 idsv3
=IN2-15 EQU #0C33E           1/X where X=(A,B)
=X/Y15  EQU #0C34F           X/Y where X=(A,B) and Y=(C,D)
=AD2-12 EQU #0C35F           12 digit add = (A) + (C) 15dig result in (A,B) idsv3 pdf page 1394
=AD2-15 EQU #0C363           15-digit add = (A,B) + (C,D) idsv3 pdf page 1394
=AD15S  EQU #0C369           same + SB reset pdf page 1394 of idsv3
=MP2-12 EQU #0C432           12 digit * (A) * (C) result 15dig in (A,B) idsv3 pdf page 1397
=MP1-12 EQU #0C436           12 digit * (A,B) * (C) result 15dig in (A,B) idsv3 pdf page 1397
=MP2-15 EQU #0C43A           15-digit multiply (A,B) * (C,D) result 15dig in (A,B) idsv3 pdf page 1397
=DV2-12 EQU #0C4A8           12-digit divise (A) and (C) result in (A,B) idsv3 pdf page 1399
=DV2-15 EQU #0C4AC           15-digit divise (A,B) and (C,d) result in (A,B) idsv3 pdf page 1399
=SQR15  EQU #0C534           idsv3 pdf page 1401 SQR(A,B) result in (A,B)
=XYEX   EQU #0C697           Exchange (A,B) with (C,D)  pdf idsv3 page 1408
=SPLITA EQU #0C6BF           Extend (A) into (A,B) pdf idsv3 page 1408
=CLRFRC EQU #0C6F4           (A,B) to (A,B) w/o fractio part page 1409 
*                              exit in DECMODE carry set if no frac part
=FRAC15 EQU #0C70E           frac of (A,B) into (A,B) page 1411 pdf ids3
=IF12A  EQU #0C739           entry A. return where the decimal point is. 
*                              Out is the number in P. Page 1412 If E<0, P =14 etc.
*                              alter A,B,C(A),P,CARRY
=INFR15 EQU #0C73D           same as above, input (A,B), alter C(A),P,CARRY  Page 1412
=SPLTAC EQU #0C934           Extend (A) and (C) into (A,B) and (C,D) pdf idsv3 page 1424
=SPLITC EQU #0C940           Extend (C) into (C,D) pdf idsv3 page 1425
=uRES12 EQU #0C994           Reduce (A,B) into (C) idsv3 pdf page 1426 uses R3 therefore avoid it
=YX2-12 EQU #0D274           Y^X & Reg 0 2 3 modified
=STAB1  EQU #0D3D9           Store AB into scratch1 (R0,R1) pdf idsv3 page 1471
=EXAB1  EQU #0D3E7           Exchange AB and scratch1 pdf idsv3 page 1471
=RCCD1  EQU #0D3F5           Recall scratch1 into CD pdf idsv3 page 1471
=STAB2  EQU #0D400           Store AB into scratch2 (R2,R3) pdf idsv3 page 1471
=EXAB2  EQU #0D40E           Exchange AB and scratch2 pdf idsv3 page 1471
=RCCD2  EQU #0D41C           Recall scratch2 into CD pdf idsv3 page 1471
=STCD2  EQU #0D427           Store CD into scratch2 (R2,R3) pdf idsv3 page 1471
=uTEST  EQU #0D435           User real comparison A & C in 12digit pdf idsv3 Page 1484..6 
*                             with carry which has the result
*                             with predicat in page 1486; < is 1; <= is 3; ..
=SPLTA  EQU #0D706           = splita ids v3 page 1500
=SPLTB  EQU #0D70C           = splitb ids v3 page 1500
=SIN12  EQU #0D716           = splita ids v3 page 1502
=SIN15  EQU #0D71A           ids v3 page 1502 alter R0 R1
=COS12  EQU #0D721           = splita ids v3 page 1502
=COS15  EQU #0D725           ids v3 page 1502 alter R0 R1
=PI/2   EQU #0DB77           load PI/2 into (C,D) idsv3 pdf page 1517
=ATAN15 EQU #0DBBE           atan of 15 digit args (A,B) result in (A,B) ids v3 page 1520
=uRESD1 EQU #0E1EE           Reduce (A,B) into (C), similar uRES12, exi: R3 touched, S8-11 ..
=outres EQU #0E3D9           ures12 & FNRTN4 v3p1570   BASIC ONLY
*                            dont alter D1,P=14 ids3 pdf page 1553
* =FNRTN4 EQU #0F238          function return, v3p1679   ONLY FOR BASIC, put C(W) on stack
=SPLTAX EQU #0E62B           SETDEC, Extend (A) into (A,B) Page 1585
=FAC15S EQU #0E72B           factorial 15digit (A,B) into (A,B), v3 page 1591
=BP     EQU #0EADF           make beep float A HZ Float C duration sec
=HDFLT  EQU #1B31B           change hex integ A(A) to 12dig float in A(W) exit DEC mode idsv3 pdf p2631
=FLOAT  EQU #1B322           change integ to 12dig float in A page 2631 ids pdf vol3
=FLTDH  EQU #1B223           Convert 12digit flt A to 5 digit Hex Integ A(A) pdf 2627 idsv3, see 
*                             out with hex mode, see FTOI forth/asm ids page pdf 308
=OL     EQU #2FBC0           L address
=OX     EQU #2FBD0           X address
=OY     EQU #2FBE0           Y address
=OZ     EQU #2FBF0           Z address
=OT     EQU #2FC00           T address
*
* FORTH entry points:
=$X     EQU #E0091           put addr var into stack ids asm pdf p160
=$Y     EQU #E00AA
=$Z     EQU #E00C3
=$T     EQU #E00DC
=$L     EQU #E00F5
=FEND   EQU #E08E9           PUTABX,GETFP
=CHS    EQU #E1518           change X sign; dont change LastX; asm ids pdf page 259
=NUMST  EQU #E1718           uMODES, SAVEFP,GETX+L.. GET X INTO (A,B); put X into L; asm ids pdf page 266
=ABS    EQU #E1A23           (n -- |n|) ids Forth pdf page 275
=MOD    EQU #E1CA1           modulo ids Forth pdf page 287
=XXYY   EQU #E212D           comparison of a(X?) and c(Y?) (or zero) 
*                              pdf page 304 Forth/asm ids, out with GETFP
=CMPST  EQU #E216C           comparison operator routine pdf page 304 Forth/asm ids, out with SAVEFP
=X<>Y   EQU #E23A5           X<>Y pdf page 313 Forth/asm
=SWAP   EQU #E24DE           p317
=OVER   EQU #E2538           (n1 n2 -- n1 n2 n1)
=FDROP  EQU #E30FB           drop X content; X become Y value; dont change LastX 
*                              pdf page 371 Forth/asm ids
=SAVEFP EQU #E717A           save Forth pointer
=GETFP  EQU #E71A5           restore Forth pointers
=GETX   EQU #E728A           Put X into (A,B) 
=GETX+L EQU #E72DF           Put X into (A,B) and X in L
=PUTABX EQU #E72F5           Put (A,B) into X, .. uRES12 GETFP pdf page 609 Forth/asm ids
=STKLFT EQU #E7320           Stacklift Forth OM page 609 pdf
=STKDRP EQU #E734C           Stackdrop Forth OM page 609 pdf LastX change 
*                              A and C most likely overwritten
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
* general remarks:
*   use scripts for deleting all * comments and after column 30 or 31
*    be carefull the long LCHEX commands are not shortened during this process
*   uRES12 MODES GETX+L using D1 therefore D1 has to be saved before its use
*   SETDEC has to be used before most of math functions
*   ADDONE and SUBONE has a warning message WRN; workaround so far not found
*   dont touch D0 outside of SAVEFP and GETFP
*   SAVEFP and GETFP: check where really necessary for optimizing code
*   X<>L and others could be deleted because it is the same like L X<>
*     or use L X <F>
*   GOSBVL NUMST is making issues with GETX+L where sign is checked. Avoid sometimes.
*   A label cannot start with 0 or 1 or.. 9 letters
*   the entries can have = in the front of them or not ( = is ignored)
*   freeze if D1 wildly changed in a word
*   P=     0 before LC(5)  =OY ! or X Z..
*   C then A has to be loaded prior any AD MP DIV (?)
*   avoid FRAC15 if 0,xxxxx already there; test before if =IF12A
*   ignore the messages * warning: word not unique, in line xx
*    (?dont appear when clean the HP71B from file and words, 
*    switch off, upload new file in :DOSLINK entry, switch on,
*    COPY :DOSLINK, FORTH, " FILENEW" ASSEMBLE)
*   the entries can have = in the front of them or not ( = is ignored)
*   uRESD1 use R3 therefore be carefull
*   nothing before the line "       FORTH"
*   use NUMST in the beginning with PUTABX in the end, and Res12 in the middle
*     else issues with ResD1 etc. by mixing.. see example below
*
****************************************************************
*                           WORDS
****************************************************************
*
*
****************************************************************
* CLFV : initialize float variable to zero.
* FVAR1 CLFV .. will put 0 into the FVARIABLE FVAR1
* tested 18 Sept 2024
       WORD 'CLFV'
       GOSBVL =SAVEFP        save pointers: CPU reg, D0, D1
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 (the instruction pointer)
       A=0    W
       DAT0=A W
       GOSBVL =GETFP         restore Forth pointers
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* CLX : set X to zero. LastX not modified (see HP41).
* tested 18 Sept 2024
       WORD 'CLX'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX            put X-address into field A of register C low order 5 nibbles
       CD0EX                 exchange data pointer address D0 with C(A)
       A=0    W
       DAT0=A W
       GOSBVL =GETFP         restore Forth pointers
       RTNCC
****************************************************************
*
*
****************************************************************
* CLST : set X Y Z T to zero. LastX not modified (see HP41).
* testes 02 Aug 2024
       WORD 'CLST'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX
       CD0EX
       A=0    W
       DAT0=A W              X set to zero
       D0=D0+ 16
       DAT0=A W              Y set to zero
       D0=D0+ 16
       DAT0=A W              Z set to zero
       D0=D0+ 16
       DAT0=A W              T set to zero
       GOSBVL =GETFP         restore Forth pointers
       RTNCC
****************************************************************
*
*
****************************************************************
* 1TOFV : initialize float variable to 1. tested 26 July 2024
* FVAR1 1TOFV .. will put 1 into the FVARIABLE FVAR1
       WORD '1TOFV'
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 (the instruction pointer)
       A=0    W              clear A
       P=     14
       A=A+1  P              put 1 at P=14 of the variable
       DAT0=A W
       CD0EX                 restore D0 the instruction pointer
       D1=D1+ 5              throw the D1 entry (seen in integer stack) away
       RTNCC
****************************************************************
*
*
****************************************************************
* 1TOX : put 1 into X . no stack move. LastX not modified. tested 17 July 2024
       WORD '1TOX'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX            put X-address into field A of register C low order 5 nibbles
       CD0EX                 exchange data pointer address D0 with C(A)
       A=0    W
       P=     14
       A=A+1  P
       DAT0=A W
       GOSBVL =GETFP         restore Forth pointers
       RTNCC
****************************************************************
*
*
****************************************************************
* X-2 : substract 2 to X ; X transfered to LASTX.
* next: test if 2x SUBONE works 
       WORD 'X-2'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
*       GOSBVL =SUBONE        WRN: appear, therefore better next
       C=0    W
       C=-C-1 S              negativ 2in (C,D)
       D=0    W
       P=     14
       D=D+1  P              
       D=D+1  P              2 in mantissa of D 
       GOSBVL =AD2-15
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* X-1 : substract 1 to X ; X transfered to LASTX.
* next: test if 1x SUBONE works 
       WORD 'X-1'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
*       GOSBVL =SUBONE        WRN: appear, therefore better next 
*                             could be ok if SB and XM to 0. test this
       C=0    W
       C=-C-1 S
       D=0    W
       P=     14
       D=D+1  P              1 in Register D
       GOSBVL AD2-15
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* X+1 : add 1 to X ; X transfered to LASTX.
* next: test if 1x ADDONE works 
       WORD 'X+1'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
*       GOSBVL =ADDONE        WRN: appear, therefore better next
*                             could be ok if SB and XM to 0. test this
       C=0    W
       D=0    W
       P=     14
       D=D+1  P              1 in Register B
       GOSBVL =AD2-15
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* X+2 : add 2 to X ; X transfered to LASTX. Tested 10 July 2024
* next: test if 2x ADDONE works 
       WORD 'X+2'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
       C=0    W
       D=0    W
       P=     14
       D=D+1  P              
       D=D+1  P              2 in mantissa of D
       GOSBVL =AD2-15
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* X*2 : multiply X by 2; X transfered to LASTX. Tested 11July2024
       WORD 'X*2'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
       C=B    W
       D=C    W
       C=A    W              copy (A,B) to (C,D). use STAB1 and RCCD1
       GOSBVL =AD2-15        (A,B) + (C,D) and put result in (A,B) = X+X = 2*X
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* X/2 : divise X by 2; X transfered to LASTX. Tested 10July2024
       WORD 'X/2'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
       C=0    W
       D=C    W
       P=     14
       D=D+1  P
       D=D+1  P              2 in Register D
       GOSBVL =DV2-15        (A,B) / (C,D) = and result in (A,B)
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* X<> exchange the value in the variable with the value in X
* use: FVAR1 X<>
* tested 16Jul2025
       WORD 'X<>'
       GOSBVL =$X
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* X<>Z exchange the HP41 registers, similar to X<>Y. 
*   tested 16Jul2025
       WORD 'X<>Z'
       GOSBVL =$X
       GOSBVL =$Z
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* X<>T exchange the HP41 registers, tested 16Jul2025
       WORD 'X<>T'
       GOSBVL =$X
       GOSBVL =$T
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* X<>L exchange the HP41 registers, tested 16Jul2025
       WORD 'X<>L'
       GOSBVL =$X
       GOSBVL =$L
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* Y<> exchange the value in the variable with the value in Y
*   tested 16Jul2025
*   use: FVAR1 Y<>
       GOSBVL =$Y
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* Y<>Z exchange the HP41 registers tested 16Jul2025
       WORD 'Y<>Z'
       GOSBVL =$Y
       GOSBVL =$Z
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* Y<>T exchange the HP41 registers tested 16Jul2025
       WORD 'Y<>T'
       GOSBVL =$Y
       GOSBVL =$T
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* Y<>L exchange the HP41 registers tested 16Jul2025
       WORD 'Y<>L'
       GOSBVL =$Y
       GOSBVL =$L
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* Z<> exchange the value in the variable with the value in Z 
*   tested 16Jul2025
*   use: FVAR1 Z<>
       WORD 'Z<>'
       GOSBVL =$Z
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* Z<>T exchange the HP41 registers, tested 16Jul2025
       WORD 'Z<>T'
       GOSBVL =$T
       GOSBVL =$Z
       GOTO  FVE
****************************************************************
*
*
****************************************************************
* Z<>L exchange the HP41 registers tested 16Jul2025
       WORD 'Z<>L'
       GOSBVL =$L
       GOSBVL =$Z
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* T<> exchange the value in the variable with the value in T. 
*   tested 16Jul2025
*   use: FVAR1 T<>
       WORD 'T<>'
       GOSBVL =$T
       GOTO  FVE
****************************************************************
*
*
****************************************************************
* T<>L exchange the HP41 registers. tested 16Jul2025
       WORD 'T<>L'
       GOSBVL =$T
       GOSBVL =$L
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* L<> exchange the value in the variable with the value in L 
*   tested 16Jul2025
*   use: FVAR1 L<>
       WORD 'L<>'
       GOSBVL =$L
       GOTO   FVE
****************************************************************
*
*
****************************************************************
* <F> exchange the values of the 2 float variables
* tested 16Jul2025
* use: FVAR1 FVAR2 <F> 
       WORD '<F>'
FVE    GOSBVL =SAVEFP        save pointers: CPU reg, D0, D1
       C=DAT1 A              copy data at 1st address in D1 into C(A)
       CD0EX                 exchange C(A) with D0
       A=DAT0 W              value into A
       R0=A                  R0 = Value1
       D1=D1+ 5
       C=DAT1 A              copy data at 2nd address in D1 into C(A)
       CD0EX
       A=DAT0 W
       R1=A                  R1 = value2
       A=R0
       DAT0=A W              Store value1 at addr2
       D1=D1- 5
       C=DAT1 A
       CD0EX
       A=R1
       DAT0=A W              Store value2 at addr1
       GOSBVL =GETFP         restore pointers: CPU reg, D0, D1
       D1=D1+ 10             return nothing on data stack
       RTNCC
****************************************************************
*
*
****************************************************************
* XYZ>ZXY Stack manipulation = "X<>Y then X<>Z"
* tested 6 Sept 2024
       WORD 'XYZ>ZXY'
* first X<>Y
       GOSBVL =X<>Y
* then X<>Z
       P=     0
       LC(5)  =OX            put X-address into field A of register C low order 5 nibbles
       CD0EX                 put data pointer D0 into C(A)
       R1=C                  save D0 into R1
       C=DAT0 W              C = value of X (which is in D0)
       D0=D0+ 16             D0 has now Y addr
       D0=D0+ 16             D0 has now Z addr
       A=DAT0 W              put value of Z into A via addr in D0
       DAT0=C W              Z = value of X
       D0=D0- 16
       D0=D0- 16             D0 -> X
       DAT0=A W              X = value of Z
       C=R1
       D0=C                  restore D0
       RTNCC
****************************************************************
*
*
****************************************************************
* XYZ>YZX Stack manipulation = "X<>Y then Y<>Z"
* tested 6 Sept 2024
****************************************************************
       WORD 'XYZ>YZX'
* first X<>Y
       GOSBVL =X<>Y
* then Y<>Z
       P=     0
       LC(5)  =OY            put Y-address into field A of register C low order 5 nibbles C(A)
       CD0EX                 exchange data pointer address D0 with C(A)
       R1=C                  save D0 into R1
       C=DAT0 W              C = value of Y from its address in C(A)
       D0=D0+ 16             D0 show now towards Z
       A=DAT0 W              put value of Z (from address D0) into A
       DAT0=C W              Z = value of Y
       D0=D0- 16             D0 -> Y
       DAT0=A W              Y = value of Z
       C=R1
       D0=C                  restore D0
       RTNCC
****************************************************************
*
*
****************************************************************
* ST- substract X value in the reg address, no lastX
* use> FVARNAME1 ST- will substract the X value within FVARNAME1
* tested 18 Sept 2024
       WORD 'ST-'
       GOSBVL =CHS           change sign of X for later calc
       GOSBVL =SAVEFP        save pointers: CPU reg, D0, D1
       C=DAT1 A              copy data at address in D1 into C(A) = field A of reg C D1 is the data pointer
       CD0EX                 exchange C(A) with D0 (the instruction pointer) for now reading the values
       A=DAT0 W              copy data at address in D0 (in C(A)) into W in A
       R0=A                  R0: value of data at address for later
       P=     0
       LC(5)  =OX            load of X register addr into C(A)
       CD0EX                 exchange C(A) with D0 (the instruction pointer)
       A=DAT0 W              copy data of X into A
       C=A    W              (-XValue) into (C)
       A=R0                  (RegValue) into (A)
       GOSBVL =SPLTAC        (A) in (A,B) and (C) in (C,D)
       GOSBVL =AD2-15        (A,B) + (C,D) and result in (A,B) = RegVal -X
       GOSBVL =uRES12        (A,B) into (C)
       A=C    W              put C into A
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 (the instruction pointer)
       DAT0=A W              Write value (OrgRegVal -X) to reg back
       GOSBVL =GETFP         restore pointers: CPU reg, D0, D1
       GOSBVL =CHS           change back from -X to X
       D1=D1+ 5              return nothing on data stack
       RTNCC
****************************************************************
*
*
****************************************************************
* ST+ add X value in the reg address, no lastX
* use> FVARNAME1 ST+ will add the X value within FVARNAME1
* released 23 july 2024
       WORD 'ST+'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W              A=X
       R0=A
       C=DAT1 A              Field A (=address) in D1 into C(A)
       R1=C                  R1=D1
       CD0EX                 Put it into D0
       A=DAT0 W              A=RegValue
       B=A    W
       C=B    W              C=X
       A=R0                  A=RegValue
       SETDEC
       GOSBVL =AD2-12
       SETHEX
       GOSBVL =uRESD1
       A=C    W
       C=R1
       CD0EX
       DAT0=A W
       GOSBVL =GETFP
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* ST* multiply X value into the reg address, no lastX
* use: FVARNAME1 ST* will multiply FVARNAME1 by X
* released 24 july 2024
       WORD 'ST*'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W              A=X
       R0=A
       C=DAT1 A              Field A (=address) in D1 into C(A)
       R1=C                  R1=D1
       CD0EX                 Put it into D0
       A=DAT0 W              A=RegValue
       B=A    W
       C=B    W              C=X
       A=R0                  A=RegValue
       SETDEC
       GOSBVL =MP2-12
       SETHEX
       GOSBVL =uRESD1
       A=C    W
       C=R1
       CD0EX
       DAT0=A W
       GOSBVL =GETFP         restore pointers: CPU reg, D0, D1
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* ST/ divise the value in reg address by X, no lastX
* use: FVARNAME1 ST/ will divise FVARNAME1 by X
* released 24 july 2024
       WORD 'ST/'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W              A=X
       R0=A
       C=DAT1 A              Field A (=address) in D1 into C(A)
       R1=C                  R1=D1
       CD0EX                 Put it into D0
       A=DAT0 W              A=RegValue
       C=R0                  C=X
       SETDEC
       GOSBVL =DV2-12
       SETHEX
       GOSBVL =uRESD1
       A=C    W
       C=R1
       CD0EX
       DAT0=A W
       GOSBVL =GETFP          restore pointers: CPU reg, D0, D1
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* ST^ exponent the value in reg address by X, no lastX
* test
       WORD 'ST^'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W              A=X
       R0=A
       C=DAT1 A              Field A (=address) in D1 into C(A)
       R1=C                  R1=D1
       CD0EX                 Put it into D0
       A=DAT0 W              A=RegValue
       C=R0                  C=X
       SETDEC
       GOSBVL =YX2-12
       SETHEX
       GOSBVL =uRESD1
       A=C    W
       C=R1
       CD0EX
       DAT0=A W
       GOSBVL =GETFP         restore pointers: CPU reg, D0, D1
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* RC- upload value from register then substract it to X  
* tested 22 July 2024
       WORD 'RC-'
       GOSBVL =SAVEFP        save pointers: CPU reg, D0, D1
       C=DAT1 A
       CD0EX
       A=DAT0 W
       R1=A                  RegVal into R1
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W
       D0=D0- 16
       DAT0=A W              X into L
       GOSBVL =CHS           -X
       D0=(5)  =OX
       A=DAT0 W
       C=A    W              -X into C
       A=R1                  RegVal into A
       GOSBVL =AD2-12        -X+RegVal
       GOSBVL =uRES12
       A=C    W              A = -Result
       P=     0
       LC(5)  =OX
       CD0EX
       DAT0=A W              -Result into X
       GOSBVL =GETFP         restore pointers: CPU reg, D0, D1
       GOSBVL =CHS           Result now in X
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* RC+ upload value from register then add it to X 
* tested 23 july 2024
       WORD 'RC+'
       GOSBVL =SAVEFP
       C=DAT1 A
       CD0EX
       A=DAT0 W
       R0=A                  RegVal into R0
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W              A = X
       D0=D0- 16
       DAT0=A W              X into L
       B=A    W
       C=B    W
       A=R0                  X in C, RegVal in A.. C=R0 better?
       GOSBVL =SPLTAC
       SETDEC
       GOSBVL =AD2-15
       SETHEX
       GOSBVL =uRESD1
       A=C    W              A = Result
       D0=(5) =OX
       DAT0=A W              Store result into X
       GOSBVL =GETFP          restore pointers: CPU reg, D0, D1
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* RC/ upload value from register then divide X by it 
* tested 23 july 2024
       WORD 'RC/'
       GOSBVL =SAVEFP
       C=DAT1 A
       CD0EX
       A=DAT0 W
       R0=A                  RegVal into R0
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W
       D0=D0- 16
       DAT0=A W              X into L, A=X
       C=R0
       GOSBVL =SPLTAC
       SETDEC
       GOSBVL =DV2-15
       SETHEX
       GOSBVL =uRESD1
       A=C    W              A = Result
       D0=(5) =OX
       DAT0=A W              Result into X
       GOSBVL =GETFP
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* RC* upload value from register then multiply X with it
* tested 23 July 2024
       WORD 'RC*'
       GOSBVL =SAVEFP
       C=DAT1 A
       CD0EX
       A=DAT0 W
       R0=A                  RegVal into R0
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W
       D0=D0- 16
       DAT0=A W              X into L, A=X
       B=A    W
       C=B    W 
       A=R0                  A=Regval, C=X
       GOSBVL =SPLTAC
       SETDEC
       GOSBVL =MP2-15
       SETHEX
       GOSBVL =uRESD1
       A=C    W              A = Result
       D0=(5) =OX
       DAT0=A W              Result into X
       GOSBVL =GETFP
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* RC^ upload value from register then exponent X by it 
* test
       WORD 'RC^'
       GOSBVL =SAVEFP
       C=DAT1 A
       CD0EX
       A=DAT0 W
       R0=A                  RegVal into R0
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W
       D0=D0- 16
       DAT0=A W              X into L, A=X
       C=R0
       SETDEC
       GOSBVL =YX2-12
       SETHEX
       GOSBVL =uRESD1
       A=C    W              A = Result
       D0=(5) =OX
       DAT0=A W              Result into X
       GOSBVL =GETFP
       D1=D1+ 5
       RTNCC
****************************************************************
*
*
****************************************************************
* FVCHS change sign of value in the float var address
* use: FVARNAME1 FVCHS will minus(value) within FVARNAME1 which is a FVARIABLE
* tested 24 July 2024
       WORD 'FVCHS'
       GOSBVL =SAVEFP
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX
       A=DAT0 W              copy data at address in D0 (in C(A)) into W in A
       SETDEC
       A=-A-1 S
       SETHEX
       DAT0=A W
       CD0EX                 exchange C(A) with D0
       GOSBVL =GETFP
       D1=D1+ 5              return nothing on data stack
       RTNCC
****************************************************************
*
*
****************************************************************
* DEG-RAD : Deg to Rad conversion (D-R in HP41)
* Result in X is X(old) * (PI/2) / (2* 45)
* X(old) go into LastX
* tested 02 Aug 2024
       WORD 'DEG-RAD'            
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
       GOSBVL =PI/2
       SETDEC
       GOSBVL =MP2-15        Resut in A,B
       SETHEX
* create 90 in (C,D)
       C=0    W
       D=C    W
       P=     0
       C=C+1  P              Exponent 10**1
       P=     14
       D=D+1  P
       D=D+1  P
       D=D+D  P
       D=D+D  P
       D=D+1  P              9 in D mantissa
*
       SETDEC
       GOSBVL =DV2-15        Result in A,B
       SETHEX
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* RAD-DEG : Rad to Deg conversion (R-D in HP41)
* Result in X is X(old) * (2* 45) / (PI/2)
* X(old) go into LastX
* tested 2nd Aug 2024
       WORD 'RAD-DEG'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
* create 90 in (C,D)
       C=0    W
       D=C    W
       P=     0
       C=C+1  P              Exponent 10**1
       P=     14
       D=D+1  P
       D=D+1  P
       D=D+D  P
       D=D+D  P
       D=D+1  P              9 in D mantissa
*
       SETDEC
       GOSBVL =MP2-15        Resut in A,B
       SETHEX
       GOSBVL =PI/2
       SETDEC
       GOSBVL =DV2-15        Result in A,B
       SETHEX
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* %CH : calculate [(x —-y) 100] / y, see %CH HP41 manual page 84
* Y untouched
* X result, lastx updated
* tested 02 Aug 2024
       WORD '%CH'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W
       D0=D0- 16
       DAT0=A W              X into L, A=X
       R0=A
       P=     0
       LC(5)  =OY
       CD0EX
       A=DAT0 W              A = Y
       SETDEC
       A=-A-1 S              A = -Y
       SETHEX
       B=A    W
       C=B    W 
       A=R0                  A=X, C=-Y
       SETDEC
       GOSBVL =AD2-12        (X-Y) in (A,B)
       SETHEX
       C=0    W
       D=C    W
       P=     0
       C=C+1  P              
       C=C+1  P              Exponent 10**2
       P=     14
       D=D+1  P              1 in D mantissa.. = 100 in (C,D)
       SETDEC
       GOSBVL =MP2-15        Resut in A,B
       SETHEX
       GOSBVL =EXAB1         [(x —-y) 100] into (R0,R1)
       P=     0
       LC(5)  =OY
       CD0EX
       A=DAT0 W              A = Y
       GOSBVL =SPLTAX         (A,B) = Y
       C=B    W
       D=C    W
       C=A    W              (C,D) = Y
       GOSBVL =EXAB1         (A,B) now = [(x —-y) 100]
       SETDEC
       GOSBVL =DV2-15        (A,B) = [(x —-y) 100] / Y
       GOSBVL =uRESD1
       SETHEX
       A=C    W              A = Result
       D0=(5) =OX
       DAT0=A W              Result into X
       GOSBVL =GETFP
       RTNCC
****************************************************************
*
*
****************************************************************
* %OF : calculate (X*Y)/100, see % in HP41 manual page 83, =pdf 89
* use: 500.0 94.0 %OF  result  470.00 in X
*                              500.00 still in Y
*                              94.00  in LASTX
* tested 02 Aug 2024
       WORD '%OF'
       GOSBVL =SAVEFP
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W
       D0=D0- 16
       DAT0=A W              X into L, A=X
       R0=A
       P=     0
       LC(5)  =OY
       CD0EX
       A=DAT0 W              A = Y
       B=A    W
       C=B    W 
       A=R0                  A=X, C=Y
       SETDEC
       GOSBVL =MP2-12         (X*Y) in (A,B)
       SETHEX
* reduce exponent nb by 2 for divide by 100? not tested..
       C=0    W
       D=C    W
       P=     0
       C=C+1  P              
       C=C+1  P              Exponent 10**2
       P=     14
       D=D+1  P              1 in D mantissa.. = 100 in (C,D)
       SETDEC
       GOSBVL =DV2-15        Result in A,B
       GOSBVL =uRESD1
       SETHEX
       A=C    W              A = Result
       D0=(5) =OX
       DAT0=A W              Result into X
       GOSBVL =GETFP
       RTNCC
****************************************************************
*
*
****************************************************************
* XSIGN : put +1.00 (X>=0) or -1.00 into X. Put old X into LASTX
* tested 2 Aug 2024
       WORD 'XSIGN'
       GOSBVL =SAVEFP        take the WRN: message away
       LC(5)  =OX                                               P=     0 before, missing?
       CD0EX
       A=DAT0 W              X into A
       D0=D0- 16
       DAT0=A W              X into L
       D0=D0+ 16             point again to X
       A=0    A
       A=0    M
       P=     14
       A=A+1  P              Create "1" with the same sign than X
       DAT0=A W
       GOSBVL =GETFP         take the WRN: message away
       RTNCC
****************************************************************
*
*
****************************************************************
* PI : put PI into X in the stack. uplift the float stack. lastx not changed (like HP41)
* tested July 20 2025
       WORD 'PI'
       GOSBVL =SAVEFP
       C=0    S
       P=     0
       LCHEX  314159265359000
       R0=C                  R0 = Result
       GOSBVL =STKLFT
       A=R0
       D0=(5) =OX
       DAT0=A W              Result into X
*       GOSBVL =GETFP
*       RTNCC
       GOVLNG =GETFP
****************************************************************
*
*
****************************************************************
* create HP41 like tone function with input 0 (low)..9 (high)
* input in the integer stack (in HP41 it is as a line command TONE x)
* ( n -- ) TONE
* 0 .. 394 Hz  >> Value 394 + (43.777 * N) 
* 1 .. 437.8
* ..
* 8 .. 744.2
* 9 .. 788 HZ
* length 0,28s
* however inputs higher than 9 will be accepted
* negative values -8 (example) will be taken as 8
* tested July 20 2025
* use  5 TONE in Forth prompt
       WORD 'TONE'
*       GOSBVL =ABS           for positive whatever input was
       GOSBVL =SAVEFP
       A=DAT1 A
       GOSBVL =HDFLT         change A(A) to float A(W) 12 digits for further calc. exit with DEC mode
       C=0    S
       P=     0
       LCHEX  437777777777001
       GOSBVL =MP2-12        result into (A,B)
       GOSBVL =uRESD1        result into C
       R0=C
       C=0    S 
       P=     0
       LCHEX  394000000000002
       A=R0
       GOSBVL =AD2-12        result into (A,B)
       GOSBVL =uRESD1        result into C as 12 digit
       R0=C                  R0 Hz 
       C=0    S
       P=     0
       LCHEX  280000000000999 C duration
       SETDEC
       A=R0
       GOSBVL =BP
       GOSBVL =GETFP
       D1=D1+ 5              take the value out of the stack
       RTNCC
****************************************************************
*
*
****************************************************************
* BEEP41  the HP41 beep sound on HP71B
* https://www.hpcalc.org/details/7826 
* .. The 41's BEEP is TONE 7, TONE 5, TONE 8, TONE 7
* According to the Wickes book on synthetic programming the frequencies are as follows:
* TONE 7 = 629 Hz
* TONE 5 = 394 Hz
* TONE 8 = 788 Hz
* TONE 7 = 629 Hz
* The book also says that the duration of the standard tones is 0,28 seconds.
* tested July 20 2025
       WORD 'BEEP41'
       GOSBVL =SAVEFP
       C=0    W
* Tone 7
       LCHEX  629000000000002
       A=C    W
       R0=C
       LCHEX  280000000000999
       R1=C
       GOSBVL =BP
* Tone 5 
       LCHEX  394000000000002
       A=C    W
       C=R1
       GOSBVL =BP
* Tone 8
       LCHEX  788000000000002
       A=C    W
       C=R1
       GOSBVL =BP
* Tone 7
       A=R0
       C=R1
       GOSBVL =BP
*       GOSBVL =GETFP
*       RTNCC
       GOVLNG =GETFP
****************************************************************
*
*
****************************************************************
* check if the value in X with 0 then put 0 (false) 
*   or -1 (true) into the integer stack
*   similar to HP41; however use of an integer stack instead of
*   program counter jump or not (like the Forth word X=0?)
*   no float stack change
*   tested 21Aug2024
* X<0?
       WORD 'X<0?'
       ST=1   0
       GOSBVL =CMPST
       P=     1
       GOTO   CMP0
* X<=0?
       WORD 'X<=0?'
       ST=1   0
       GOSBVL =CMPST
       P=     3
       GOTO   CMP0
* X#0?
       WORD 'X#0?'
       ST=1   0
       GOSBVL =CMPST
       P=     5
       GOTO   CMP0
* X=>0?                      added 15 Mai 2025
       WORD 'X=>0?'
       ST=1   0
       GOSBVL =CMPST
       P=     6              see idsv3p1486 TST15
       GOTO   CMP0
* X>0?
       WORD 'X>0?'
       ST=1   0
       GOSBVL =CMPST
       P=     4
CMP0   GOVLNG =XXYY
****************************************************************
*
*
****************************************************************
* R-P : Rad to Polar conversion
* see HP41 manual page 92
* 1.0 2.0 R-P gives 2.2361 (SQRT(5)) in X and 26.56° or 0.4636 Rad in Y
* change the output if use of the words RADIANS or DEGREES
* tested 3 sept 2024.
* WRN: coming; reason unknown; however result ok
*   for this SB and XM to zero could solve
* use perhaps stscr / rcscr instead of several uRES12
****************************************************************
       WORD 'R-P'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
       GOSBVL =uRES12        C = (A,B)
       A=C    W
       R0=A                  X in A and R0
       SETDEC
       GOSBVL =MP2-12
       GOSBVL =uRES12        C = (A,B)
       A=C    W
       R2=A                  X**2 in A and R2
       P=     0
       LC(5)  =OY
       CD0EX
       A=DAT0 W
       R1=A                  Y in A and R1
       C=R0                  X in C
       SETDEC
       GOSBVL =DV2-12
       GOSBVL =ATAN15
       GOSBVL =uRES12        C = (A,B)
       A=C    W
       P=     0
       LC(5)  =OY
       CD0EX
       DAT0=A W              copy data of A into Y
       A=R1
       C=A    W
       SETDEC
       GOSBVL =MP2-12
       GOSBVL =uRES12        C = (A,B)
       A=C    W              Y**2 in A
       C=R2                  X**2 in C
       SETDEC
       GOSBVL =AD2-12
       GOSBVL =SQR15
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* P-R : Polar to Rad conversion
* see HP41 manual page 92
* use..
* 26.56° or 0.4636 Rad in Y   then 
*  2.2361 (SQRT(5)) in X    P-R
*  gives 1.0 in Y and 2.0 in X
* tested 03 Sept 2024
* use perhaps stscr / rcscr instead of several uRES12
****************************************************************
       WORD 'P-R'
       GOSBVL =NUMST         GET X INTO (A,B), uMODES;SAVEFP;GETX+L
       P=     0
       LC(5)  =OY
       CD0EX
       A=DAT0 W
       R2=A                  Angle in R2
       SETDEC
       GOSBVL =SIN12
       GOSBVL =uRES12        uRES12 modify R3 therefore avoid R3
       A=C    W
       P=     0
       LC(5)  =OX
       CD0EX                 exchange data pointer address D0 with C(A)
       C=DAT0 W              C = value of X from its address in C(A)
       SETDEC
       GOSBVL =MP2-12        Modulus*sin(angle) in (A,B)
       GOSBVL =uRES12
       A=C    W
       P=     0
       LC(5)  =OY
       CD0EX
       DAT0=A W              copy data of A (modulus * sin(angle)) into Y
       A=R2
       GOSBVL =COS12         cos(angle) in (A,B)
       GOSBVL =uRES12
       A=C    W
       P=     0
       LC(5)  =OX
       CD0EX                 exchange data pointer address D0 with C(A)
       C=DAT0 W              C = value of X from its address in C(A)
       SETDEC
       GOSBVL =MP2-12        Modulus*cos(angle) in (A,B)
       GOSBVL =PUTABX        Modulus*cos(angle) into X
       RTNCC
****************************************************************
*
*
****************************************************************
* CLCV : control looping creation in variable
* use IIIII FFF CC FVAR CLCR where 
*       IIIII is the counter value integer
*       FFF   is the counter test value integer
*       CC    is the increment value integer
*       FVAR  is the float variable where IIII.FFFCC will be stored
*     IIIII FFF CC 
*         1     22    3    X    CLCV  will put  1.02203  into X
*   D1+20 D1+15 D1+10 D1+5 D1 (?)
*
* tested 2024 Oct 16
* 777 888 11 Y CLCV FS.   result  Y: 777.888110000 
*
* use perhaps stscr / rcscr instead of several uRESD1
****************************************************************
       WORD 'CLCV'
       GOSBVL =SAVEFP
*
       P=     0              Set to 0 made it.
       D1=D1+ 5              CC; go to the first address of the variable in the integ stack 
       A=DAT1 A              copy data at address in D1 into A(A)
       GOSBVL =HDFLT         change A(A) to float A(W) 12 digits for further calc. exit with DEC mode
       R0=A
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  995            0,00001 now in (C)
       A=R0
       SETDEC
       GOSBVL =MP2-12        0,000CC into (A,B)
       GOSBVL =uRESD1        into (C)
       R1=C
*
       D1=D1+ 5
       A=DAT1 A              copy data at address in D1 into A(A)
       GOSBVL =HDFLT         change A(A) to float A(W) 12 digits for further calc. exit with DEC mode
       R0=A
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  3              1000 in (C)
       A=R0
       SETDEC
       GOSBVL =DV2-12        0,FFF into (A,B)
       GOSBVL =uRESD1        into (C)
       A=R1
       SETDEC
       GOSBVL =AD2-12        0,FFFCC into (A,B)
       GOSBVL =uRESD1        into (C)
       R1=C
*
       D1=D1+ 5              IIIII
       A=DAT1 A              copy data at address in D1 into A(A)
       GOSBVL =HDFLT         change A(A) to float A(W) 12 digits for further calc. exit with DEC mode
       R0=A
       C=R1
       A=R0
       SETDEC
       GOSBVL =AD2-12        IIIII,FFFCC into (A,B)
       SETHEX
       GOSBVL =uRESD1        into (C)
       R1=C                  IIIII,FFFCC into R1
*
       D1=D1- 15
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 Put it into D0
       A=R1                  IIIII,FFFCC into A
       DAT0=A W              Store value IIIII,FFFCC into variable
       D0=C
*
       GOSBVL =GETFP
       D1=D1+ 15
       D1=D1+ 5
       RTNCC
*
****************************************************************
*
*
****************************************************************
* CLRV : control looping parameters retrieving from variable
*        = inverse of CLCV
* support function for easier use of CLRGX of HP41CX
* use FVAR CLRV will put IIIII FFF CC into integer stack 
*       IIIII is the initial counter value integer
*       FFF   is the counter test value integer
*       CC    is the increment value integer
*       FVAR  is the float variable where IIII.FFFCC is stored
*                IIIII FFF CC 
* when 1.02203 in X
* X CLRV will put  1    22  3     into integer stack
*
* test
* 777.888110000 FENTER Y CLRV S. 
*  >> [ 777 888 11 ]  OK { 3 } 
*
* tested ok 21 April 2025
* FS.
* T=  0.16666666667 
* Z=  111.333020000 
* Y=  111.666050000 
* X=  333.888000000 
* L=  6.00000000000 
*  OK { 0 } 
* L CLRV S.
* [ 6 0 1 ]  OK { 3 } 
* X CLRV S.
* [ 333 888 1 ]  OK { 3 } 
* Y CLRV S.
* [ 111 666 5 ]  OK { 3 } 
*
* FS.
* T=  0.16666666667 
* Z=  111.333020000 
* Y=  111.666050000 
* X=  333.888000000 
* L=  6.00000000000 
*  OK { 0 } 
* Y CLRV L CLCV FS.
* T=  0.16666666667 
* Z=  111.333020000 
* Y=  111.666050000 
* X=  333.888000000 
* L=  111.666050000 
*
* was an issue (minor) when using SPLITA
* T=  0.16666666667 
* T CLRV S.
* [ 0 0 1 ]  OK { 3 }
* X=  1.16666666000 
* X CLRV S. FS.
* [ 1 166 66 ]
****************************************************************
       WORD 'CLRV'
       GOSBVL =SAVEFP
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 pointer
       A=DAT0 W              value ssss,eeeiixxx into A
       XM=0                  ERR at execution if not included
*       GOSBVL =SPLITA        A into (A,B)
*       GOSBVL =STAB1         store ssss,eeeiixxx (A,B) into (R0,R1)
       R0=A                                                     better
* 10000 in (C)
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  5              just retracting 5x 1 in X was not making it
*       GOSBVL =RCAB1         ssss,eeeiixxx into (A,B)
       A=R0                                                     better
       SETDEC
       GOSBVL =MP2-12        sssseeeii,xxx into (A,B)
       GOSBVL =CLRFRC        sssseeeii,000 into (A,B)
       GOSBVL =STAB1         store sssseeeii,000 (A,B) into (R0,R1)
*
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  998            0,01 now in C
*
       GOSUBL =RCAB1         sssseeeii,000 into (A,B)
       SETDEC
       GOSBVL =MP1-12        sssseee,ii000 into (A,B)
       GOSBVL =CLRFRC        sssseee,00000 into (A,B); carry set if ii=00
       GONC   nadd1          if 0,ii000 # 0,00 then dont add one
* BRANCH: ii is ZERO .. then add 1
       GOSUB  =RCAB1         sssseee00,000 into (A,B)
       GOSBVL =ADDONE
       GOSBVL =STAB1         store sssseee01,000 (A,B) into (R0,R1)
*
* BRANCH: ii IS NOT ZERO from here
*
nadd1  C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  995            0,00001 now in C
       GOSUB  =RCAB1         sssseeeii,000 into (A,B)
       SETDEC
       GOSBVL =MP1-12        ssss,eeeii000 into (A,B)
       GOSBVL =STAB1         store ssss,eeeii000 (A,B) into (R0,R1)
*
       GOSBVL =INFR15
       ?P#    14             test if (A,B) is not like 0,eeeii000 which means ssss # 0
       GOYES  SNZ0           ssss is NOT ZERO
       A=0    W
       GOTO   SSZ0
SNZ0   GOSBVL =CLRFRC        ssss,000 into (A,B)
       GOSBVL =uRESD1        into (C)
       A=C    W
SSZ0   GOSBVL =FLTDH         A into 5 nibs hex A(A)
*
       DAT1=A A              put ssss into integer stack
*
       D1=D1- 5
       GOSUB  =RCAB1         value ssss,eeeii000 into (A,B)
       GOSBVL =INFR15
       ?P=    14             test if (A,B) is like 0,eeeii000 which means ssss = 0
       GOYES  S0Z0           ssss is ZERO
       GOSUB  =RCAB1         value ssss,eeeii000 into (A,B)
       GOSBVL =FRAC15        0,eeeii000 in (A,B)
       GOSBVL =STAB1         store 0,eeeii000 (A,B) into (R0,R1)
S0Z0   C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  3              1000 in (C)
       GOSUB  =RCAB1         value 0,eeeii000 into (A,B)
       SETDEC                dont forget this before MP1-12
       GOSBVL =MP1-12        eee,ii000 into (A,B)
       GOSBVL =STAB1         store eee,ii000 (A,B) into (R0,R1)
       GOSBVL =CLRFRC        eee,00000 into (A,B)
       GOSBVL =uRESD1        eee,00 into (C)
       A=C    W
       GOSBVL =FLTDH
       DAT1=A A              put eee into integer stack
* fine till here
       D1=D1- 5
       GOSUB  =RCAB1         value eee,ii000 into (A,B)
       GOSBVL =INFR15
       ?P=    14             test if A is like 0,ii000 which means eee = 0
       GOYES  SNZ3           eee is ZERO
       GOSUB  =RCAB1         value eee,ii000 into (A,B)
       GOSBVL =FRAC15        0,ii000 in (A,B)
       GOSBVL =STAB1         0,ii000 in (R0,R1)
SNZ3   C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  2              100 in (C)
       GOSUB  =RCAB1         0,ii000 in (A,B)
       SETDEC                dont forget this before MP1-12
       GOSBVL =MP1-12        ii,000 into (A,B)
       GOSBVL =uRESD1        ii,000 into (C)
       A=C    W              ii,000 into (A)
       GOSBVL =FLTDH
       DAT1=A A              put ii into integer stack
       GOSBVL =GETFP
       D1=D1- 10
       RTNCC
*
****************************************************************
*
* standalone routine 
* recall the values of R0/R1 into A/B
* in additional to the existing =STAB1 =EXAB1 =RCCD1 =STAB2 
*   =EXAB2 =RCCD2 =STCD2
*
RCAB1  A=R1
       B=A    W
       A=R0
       RTN
*
****************************************************************
*
*
*
****************************************************************
* ISG ( addr -- flag ) increment value
* need? since I DO LOOP or +LOOP exists, it should cover the needs
* HP41 like "increment and skip if greater". It increment sss and would "skip", what
*   we can interpret as put a NO/false (0) in the integer stack, if it becomes equal or greater than eee
*   hp41 om en page 164 (pdf 170)
* BUT here, no truncated according display
* use.. FVAR1 ISG (FVAR1 is a float variable) ; ssss,eeeiixxx in FVAR1 will 
*         return  0 (false) if sss+ii >= eee
*         return -1 (true) if sss+ii < eee
*       ii default is 1 if not given (00)
*
*       0.0 X ISG X FV. . >> Value: 1.000000000  &  0 in Intg Stack 
*       1.01 X ISG will increase X by default 1 with 
*          result 2.01000 in X and -1 in Intg Stack
*       -10.00102345 Y STO Y ISG will increase Y by 2 with 
*          result -8.00102345 in Y  and -1 in Intg Stack 
*       10.02003 X ISG      gives  13.02003 in X, -1 in Integ Stack
*       10.020001234 X ISG  gives  11.020001234 in X, -1 in Integ stack
*       10.020001234 X ISG X FV. .  Value in X: 11.020001234 -1  OK { 0 } 
*       10.020103456 X ISG  gives  20.020103456 in X, 0 in Integ stack
*       -10.02010 X ISG     gives  0.02010 in X, -1 in Integ Stack
*       0.0201 X ISG      gives  10.02010 in X, -1 in Integ Stack
*       10.02010 X ISG      gives  20.02010 in X, 0 in Integ Stack
*       20.02010 X ISG      gives  30.02010 in X, 0 in Integ Stack
*       -1.02010 X ISG FS. .   gives 9.02010 in X, -1 in Integ stack
*       0.00005 X ISG X FV. . >> 5.00005 in X and 0 in Integ stack
*       0.010405678 X ISG X FV. . >> 40.010405678 in X and 0 in Integ stack
*       -10.02010777 X ISG FS. . >> 0.02010777 in X and -1 in integ stack
*
* tested 07.10.2024
*
* comments/improvements:
* use perhaps stscr / rcscr instead of several uRESD1
****************************************************************
       WORD 'ISG' 
       GOSBVL =SAVEFP
       C=DAT1 A              copy data at address in D1 into C(A)
       R0=C                  copy C(A) into R0
       CD0EX                 exchange C(A) with D0 pointer
       A=DAT0 W              value ssss,eeeiixxx into A
*
* saving the original value of the float variable
*
       R1=A                  value ssss,eeeiixxx into   >> R1 <<
*
* 10000 in (C)
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  5              just retracting 5x 1 in X was not making it
*
       A=R1                  seems to make it better; see %OF, too
*                            = upload A after C prior MV DV ADD..
       SETDEC
       GOSBVL =MP2-12        sssseeeii,xxx into (A,B)
       GOSBVL =CLRFRC        sssseeeii,000 into (A,B)
       GOSBVL =uRESD1        into (C)
       R2=C                  sssseeeii,000 into R2
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  998            0,01 now in C
*
       A=R2                  sssseeeii,000 into A
       SETDEC
       GOSBVL =MP2-12        sssseee,ii000 into (A,B)
       GOSBVL =CLRFRC        sssseee,00000 into (A,B); carry set if ii=00
*
       GONC   nadd0          if 0,ii000 # 0,00 then dont add one
*
* BRANCH: ii is ZERO .. then add ONE
*
       A=R2                  sssseee00,000 into A
       C=0    W
       P=     14
       C=C+1  P              1 in Register C
       C=A    S              it makes -1 if A<0; +1 if A>0
       A=R2                  sssseee00,000 into A
       GOSBVL =AD2-12        sssseee01,000 in (A,B)
       GOSBVL =uRESD1        sssseee01,000 into (C)
       R2=C                  sssseee01,000 into (R2)
*
* BRANCH: ii IS NOT ZERO from here
*
nadd0  C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  998            0,01 now in C
       A=R2                  from here, sssseeeii,000 into A
       SETDEC
       GOSBVL =MP2-12        sssseee,ii000 into (A,B)
       GOSBVL =FRAC15        0,ii000 in (A,B). ii#00 for sure here  issue there if sssseee = 0?
       GOSBVL =uRESD1        into (C)
       A=C    W              0,ii000 into A
       P=     15
       A=0    P              +0,ii000 in (A)
       SETDEC
       R2=A                  +0,ii000 in R2 now
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  2              100 in C
       A=R2                  +0,ii000 in (A)
*
       SETDEC                dont forget this before MP DV .. 2-12
       GOSBVL =MP2-12        +ii,000 into (A,B)
       GOSBVL =uRESD1        +ii,000 into (C)
*
       R2=C                  +ii,000 into (R2)
*
* ssss could be zero and could make an issue later with the use of FRAC15
* which is not necessary in case this is already 0,eeeiixxxx
*
       A=R1                  ssss,eeeiixxx into A
       GOSBVL =IF12A
       ?P#    14             test if A is like 0,eeeiixxx which means ssss = 0
       GOYES  SNZ            ssss is NOT ZERO (tested)
*
* BRANCH: ssss is ZERO; -ii is from here definitively NOT ZERO
*
       C=R1                  0,eeeiixxx is in C now (ssss is Zero tested before)
       A=R2                  +ii,000 (which is NOT ZERO) into A
       C=A    S              (C) should have the same sign than (A)
       GOSBVL =AD2-12        value (ssss=zero)+ii,eeeiixx in (A,B)
       GOSBVL =uRESD1        in C
       R1=C
       GOTO WEI
*
* Branch: ssss is not ZERO (tested)
*
SNZ    A=R1                  A is like ssss,eeeiixxx
       GOSBVL =SPLITA        A into (A,B)
       GOSBVL =CLRFRC        ssss,00 into (A,B)
       GOSBVL =uRESD1        ssss,00 into (C)
       A=R2                  +ii,000 into (A)
       SETDEC                dont forget this before MP DV .. 2-12
       GOSBVL =AD2-12        value ssss+ii,00 in (A,B)
       GOSBVL =uRESD1        ssss+ii,00 in C
       A=C    W              ssss+ii,00 in A
       R2=C                  ssss+ii,00 in R2
*
       ?A#0   M              (ssss+ii) not ZERO ?
       GOYES  WEIT           goto WEIT if (ssss+ii) not zero
*
* BRANCH: (ssss+ii) is ZERO
*
       A=0    W              necessary(?)
       R2=A                  necessary(?)
*
       A=R1                  ssss,eeeiixxx (NOT like 0,eeeiixxx) in A
       GOSBVL =SPLITA        A into (A,B)
       GOSBVL =FRAC15        0,eeeiixxx in (A,B)
       GOSBVL =uRESD1        into (C)
       C=0    S              +0,eeeiixxx (= ssss+ii,eeeiixxx) in (C)
       R1=C                  +0,eeeiixxx (= ssss+ii,eeeiixxx) in (R1)
       GOTO   WEI
*
*
* BRANCH: (ssss+ii) and (ssss) are both NOT ZERO
*
WEIT   A=R1                  ssss,eeeiixxx into A
       GOSBVL =SPLITA        ssss,eeeiixxx into (A,B)
       GOSBVL =FRAC15        0,eeeiixxx in (A,B)
       GOSBVL =uRESD1        0,eeeiixxx into (C)
       A=R2                  ssss+ii,00 into (A)
       C=A    S
       GOSBVL =AD2-12        value ssss+ii,eeeiixxx in (A,B)
       GOSBVL =uRESD1        ssss+ii,eeeiixxx into C
       R1=C                  ssss+ii,eeeiixxx in (R1)
*
* Upload the ISG action back into the variable
*
WEI    A=R1
       C=R0
       CD0EX                 exchange C(A) with D0
       DAT0=A W              Store increased value ssss+ii,eeeiixxx back into variable
*
* Now upload the TRUE FALSE into the integer stack
* So far, R2 has ssss+ii,00
* So far, R1 has ssss+ii,eeeiixxx
*
* isolate eee
*
       A=R1                  ssss+ii,eeeiixxx in (A)
*
       GOSBVL =IF12A
       ?P=    14             test if A like 0,eeeiixxx
       GOYES  SIZ            ssss+ii is ZERO (tested)
*
* BRANCH (again) ssss+ii IS NOT ZERO
*
       A=R1 
       GOSBVL =SPLITA        A into (A,B)
       GOSBVL =FRAC15        0,eeeiixxx into (A,B)
       GOSBVL =uRESD1        0,eeeiixxx (A,B) into C
       R1=C                  0,eeeiixxx in R1
*
* BRANCH (again) ssss-ii IS ZERO
* 0,eeeiixxx
*
SIZ    C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  3              1000 in (C)
       A=R1                  0,eeeiixxx into A
       SETDEC                dont forget this before MP2-12
       GOSBVL =MP2-12        eee,iixxx into (A,B)
       GOSBVL =CLRFRC        eee,00000 into (A,B)
       GOSBVL =uRESD1        eee,00 into (C)
       C=0    S              +eee,00 in (C)
       R1=C                  +eee,00 in (R1)
*
       C=R1                  +eee,00    in C
       A=R2                  ssss+ii,00 in A
       P=     1
       GOSBVL =uTEST
       SETHEX
       A=0    A
       GONC   TRO1
       A=A-1  A
TRO1   GOSBVL =GETFP
       DAT1=A A
       RTNCC
*
****************************************************************
*
*
****************************************************************
* DSE : decrement value  
* HP41 like; manual page 163; decrement and skip if equal
* It decrement sss and would "skip", what we can interpret as 
*   put a NO/false (0) in the integer stack, if it becomes equal or less than eee
* (ssss,eeeii in X) X DSE return  0  (false) if ssss <= eee
*                                 -1 (true) if ssss > eee
* use: 20.01001 X DSE  >> output .. X: 19.010010000  ..  0  OK { 0 } 
*   this value can be saved into any variable Z T or FVARX
*   then FVARX DSE can be used
*
* use cases for testing:
*   0.0 X DSE            -1.0 in X    & 0 in integ stack
*   20.00020 X DSE       0.00020 in X & 0 in integ stack
*   20.010010000 X DSE   19.010010000 in X & -1 in integer stack
*   20.010001234 X DSE   19.010001234 in X & -1 in integer stack
*   20.010023456 X DSE   18.010023456 in X & -1 in integer stack
*   0.010023456 X DSE   -2.010023456 in X & 0 in integer stack
*   1.010023456 X DSE   -1.010023456 in X & 0 in integer stack
*   20.010103456 X DSE   10.010103456 in X & 0 in integer stack
*   10.010103456 X DSE   0.010103456 in X & 0 in integer stack
*   -10.0000566666 X DSE   -15.000056667 in X & 0  OK { 0 }
*   -10.000000000  X DSE FS. .  >> X: -11.000000000 >> 0  OK { 0 } 
*   0.010003456 X DSE FS. .     >> -1.010003456 .. 0  OK { 0 }
*   1.0200234567 X DSE FS. .    >> -1.020023457 .. 0  OK { 0 } 
*   0.00000345678 X DSE FS. .   >> -1.00000345678 .. 0  OK { 0 } 
*   0.01002 X DSE FS. .         >> -2.01002        0
*   0.01 X DSE FS. .  >> -1.01                    0 
*   0.00002 X DSE FS. . >> -2.00002               0
*
* tested 07 Oct 2024
*
* comments/improvements:
* use perhaps stscr / rcscr instead of several uRESD1
****************************************************************
       WORD 'DSE'
       GOSBVL =SAVEFP
       C=DAT1 A              copy data at address in D1 into C(A)
       R0=C                  copy C(A) into R0
       CD0EX                 exchange C(A) with D0 pointer
       A=DAT0 W              value ssss,eeeiixxx into A
*
* saving the original value of the float variable
*
       R1=A                  value ssss,eeeiixxx into   >> R1 <<
*
* ssss or sssseee or sssseeeii = 0 seems to be an issue because the numbers after ii , are gone. 
*               lets consider that case to bypass later calculations
*               >> FRAC15 dont like something like 0,xxxx
*
* 10000 in (C)
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  5              just retracting 5x 1 in X was not making it
*
       A=R1                  seems to make it better; see %OF, too
*                            = upload A after C prior MV DV ADD..
       SETDEC
       GOSBVL =MP2-12        sssseeeii,xxx into (A,B)
       GOSBVL =CLRFRC        sssseeeii,000 into (A,B)
       GOSBVL =uRESD1        into (C)
       R2=C                  sssseeeii,000 into R2
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  998            0,01 now in C
*
       A=R2                  sssseeeii,000 into A
       SETDEC
       GOSBVL =MP2-12        sssseee,ii000 into (A,B)
       GOSBVL =CLRFRC        sssseee,00000 into (A,B); carry set if ii=00
*
       GONC   nadd2          if 0,ii000 # 0,00 then dont add one
*
* BRANCH: ii is ZERO .. then add ONE
*
       A=R2                  sssseee00,000 into A
       C=0    W
       P=     14
       C=C+1  P              1 in Register C
       C=A    S              it makes -1 if A<0; +1 if A>0
       A=R2                  sssseee00,000 into A
       GOSBVL =AD2-12        sssseee01,000 in (A,B)
       GOSBVL =uRESD1        sssseee01,000 into (C)
       R2=C                  sssseee01,000 into (R2)
*
* BRANCH: ii IS NOT ZERO from here
*
nadd2  C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  998            0,01 now in C
       A=R2                  from here, sssseeeii,000 into A
       SETDEC
       GOSBVL =MP2-12        sssseee,ii000 into (A,B)
       GOSBVL =FRAC15        0,ii000 in (A,B). ii#00 for sure here  issue there if sssseee = 0?
       GOSBVL =uRESD1        into (C)
       A=C    W              0,ii000 into A
       P=     15
       A=0    P              +0,ii000 in (A)
       SETDEC
       A=-A-1 S              A = -0,ii000
       R2=A                  -0,ii000 in R2 now
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  2              100 in C
       A=R2                  -0,ii000 in (A)
*
       SETDEC                dont forget this before MP DV .. 2-12
       GOSBVL =MP2-12        -ii,000 into (A,B)
       GOSBVL =uRESD1        -ii,000 into (C)
*
       R2=C                  -ii,000 into (R2)
*
* ssss could be zero and could make an issue later with the use of FRAC15
* which is not necessary in case this is already 0,eeeiixxxx
*
       A=R1                  ssss,eeeiixxx into A
       GOSBVL =IF12A
       ?P#    14             test if A is like 0,eeeiixxx which means ssss = 0
       GOYES  SNZ2           ssss is NOT ZERO (tested)
*
* BRANCH: ssss is ZERO; -ii is from here definitively NOT ZERO
*
       C=R1                  0,eeeiixxx is in C now (ssss is Zero tested before)
       A=R2                  -ii,000 (which is NOT ZERO) into A
       C=A    S              (C) should have the same sign than (A)
       GOSBVL =AD2-12        value (ssss=zero)-ii,eeeiixx in (A,B)
       GOSBVL =uRESD1        in C
       R1=C
       GOTO WEI2
*
* Branch: ssss is not ZERO (tested)
*
SNZ2   A=R1                  A is like ssss,eeeiixxx
       GOSBVL =SPLITA        A into (A,B)
       GOSBVL =CLRFRC        ssss,00 into (A,B)
       GOSBVL =uRESD1        ssss,00 into (C)
       A=R2                  -ii,000 into (A)
       SETDEC                dont forget this before MP DV .. 2-12
       GOSBVL =AD2-12        value ssss-ii,00 in (A,B)
       GOSBVL =uRESD1        ssss-ii,00 in C
       A=C    W              ssss-ii,00 in A
       R2=C                  ssss-ii,00 in R2
*
       ?A#0   M              (ssss-ii) not ZERO ?
       GOYES  WEI3           goto WEI3 if (ssss-ii) not zero
*
* BRANCH: (ssss-ii) is ZERO
*
       A=0    W              necessary(?)
       R2=A                  necessary(?)
*
       A=R1                  ssss,eeeiixxx (NOT like 0,eeeiixxx) in A
       GOSBVL =SPLITA        A into (A,B)
       GOSBVL =FRAC15        0,eeeiixxx in (A,B)
       GOSBVL =uRESD1        into (C)
       C=0    S              +0,eeeiixxx (= ssss-ii,eeeiixxx) in (C)
       R1=C                  +0,eeeiixxx (= ssss-ii,eeeiixxx) in (R1)
       GOTO   WEI2
*
*
* BRANCH: (ssss-ii) and (ssss) are both NOT ZERO
*
WEI3   A=R1                  ssss,eeeiixxx into A
       GOSBVL =SPLITA        ssss,eeeiixxx into (A,B)
       GOSBVL =FRAC15        0,eeeiixxx in (A,B)
       GOSBVL =uRESD1        0,eeeiixxx into (C)
       A=R2                  ssss-ii,00 into (A)
       C=A    S
       GOSBVL =AD2-12        value ssss-ii,eeeiixxx in (A,B)
       GOSBVL =uRESD1        ssss-ii,eeeiixxx into C
       R1=C                  ssss-ii,eeeiixxx in (R1)
*
* Upload the DSE action back into the variable
*
WEI2   A=R1
       C=R0
       CD0EX                 exchange C(A) with D0
       DAT0=A W              Store increased value ssss-ii,eeeiixxx back into variable
*
* Now upload the TRUE FALSE into the integer stack
* So far, R2 has ssss-ii,00
* So far, R1 has ssss-ii,eeeiixxx
*
* isolate eee
*
       A=R1                  ssss-ii,eeeiixxx in (A)
*
       GOSBVL =IF12A
       ?P=    14             test if A like 0,eeeiixxx
       GOYES  SIZ2           ssss-ii is ZERO (tested)
*
* BRANCH (again) ssss-ii IS NOT ZERO
*
       A=R1 
       GOSBVL =SPLITA        A into (A,B)
       GOSBVL =FRAC15        0,eeeiixxx into (A,B)
       GOSBVL =uRESD1        0,eeeiixxx (A,B) into C
       R1=C                  0,eeeiixxx in R1
*
* BRANCH (again) ssss-ii IS ZERO
* 0,eeeiixxx
*
SIZ2   C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  3              1000 in (C)
       A=R1                  0,eeeiixxx into A
       SETDEC                dont forget this before MP2-12
       GOSBVL =MP2-12        eee,iixxx into (A,B)
       GOSBVL =CLRFRC        eee,00000 into (A,B)
       GOSBVL =uRESD1        eee,00 into (C)
       C=0    S              +eee,00 in (C)
       R1=C                  +eee,00 in (R1)
*
       C=R2                  ssss-ii,00 in C
       A=R1                  +eee,00    in A
       P=     1
       GOSBVL =uTEST
       SETHEX
       A=0    A
       GONC   TRO2
       A=A-1  A
TRO2   GOSBVL =GETFP
       DAT1=A A
       RTNCC
*
****************************************************************
*
*
****************************************************************
* FACT : factorial function (see HP41)
* tested 18 Oct 2024
* use: 6.0 FACT FS.
* > X: 720.000000000 
* > L: 6.000000000 
       WORD 'FACT'
       GOSBVL =NUMST
       SB=0                  issue WRN gone when included
       XM=0                  same
       SETDEC                same
       GOSBVL =FAC15S
       GOSBVL =PUTABX
       RTNCC
****************************************************************
*
*
****************************************************************
* HR
*
* see JPC ROM HR page 73
* https://github.com/hp71b/jpcrom/blob/main/src/hms.as
*
* HP41 manual page 88 (page 94 pdf)
* hh.mmssddddd HR hh.DDDDDDDD
*
* strategy convert value hh,mmssddddd in X into hh.DDDDDDDD
*   (no test if mm<60)
*   (no test if ss<60)
*   hh,mmssddddd from X into A/B                     167.221568
*   put A/B into L (.. NUMST?)
*   store A/B into R0/R1 (=STAB1)
*   R0/R1 (RCL) =CLRFRC R2/R3 STO (=STAB2)
*     (store hh in R2/R3)                            167.0
*   R0/R1 RCL (=EXAB1 =STAB1)
*   test if hh#0 with =INFR15 then 
*     (11) =FRAC15 
*     else
*     (22)
*   100 * (mm,ssddddd in A/B)
*   R0/R1 STO (mm,ssddddd into R0/R1 =STAB1)
*   =CLRFRC (..mm) 60 /           (0,3666666 in A/B)
*   (=RCCD2) R2/R3 ST+ (=AD2-15 =STAB2)              167.3666666
*   R0/R1 RCL (=EXAB1 =STAB1) 
*   test if mm#0 with =INFR15 then 
*     (33) =FRAC15 
*     else
*     (22)
*   100 *  (ss,ddddd in A/B)
*   (60*60) /                                         +0.0043555
*   (=RCCD2) R2/R3 ST+ (=AD2-15 =STAB2)
*   (R2/R3)                                        = 167.3710222
*   (A/B) into X (=PUTABX)
*
* tested 30 Jan 2025
*   167.221568 HR result 167.3710222
*     167hr  22min 15sec 0,25sec = 167.3710222 hr
*   167.22 HR result 167.3666667
*   167.5 HR result 167.8333333
*
* ST Status Flag             comment
*      0      0              0 SIX(60 /) ; 1 SIX(60 *)
*      0      1              0 no HMS+/-   1  yes
*      1      2              0 HMS-   1 HMS+
****************************************************************
       WORD 'HR'
       GOSBVL =NUMST         GETFP, .., hh.mmssddddd into A/B and L
*
       ST=0   1
COND   ST=0   0
       GOSBVL =STAB1         hh.mmssddddd into (R0,R1)
       GOSBVL =CLRFRC        hh in (A,B)
       GOSBVL =STAB2         hh into (R2,R3), save result 
*
       GOSUBL =RCAB1         hh.mmssddddd into A/B
       GOSBVL =INFR15
       ?P=    14             test if A/B like 0.xxxxxx which means hh = 0
       GOYES  SN0            hh is ZERO (tested)
       GOSUBL =RCAB1         hh.mmssddddd into A/B
       GOSBVL =FRAC15        0.mmssddddd  in (A,B)
       GOSBVL =STAB1         0.mmssddddd into (R0,R1) for later
*
SN0    GOSUBL =RCAB1         0.mmssddddd into A/B
*
       A=A+1  A
       A=A+1  A              mm.ssddddd into A/B
       GOSBVL =STAB1         mm.ssddddd into (R0,R1) for later
       GOSBVL =CLRFRC        mm in A/B
       GOSUB  SIX            mm/60 = 0.DDDDDD in A/B
       GOSBVL =RCCD2         hh in C/D
       GOSBVL =AD2-15        hh.DDDDDD in A/B
       GOSBVL =STAB2         back to R2/R3
       GOSBVL =EXAB1         mm.ssddddd into (A,B)
       GOSBVL =STAB1         mm.ssddddd into (A,B)
*
       GOSBVL =INFR15
       ?P=    14             test if A/B like 0,xxxxxx which means mm = 0
       GOYES  SN1            mm is ZERO (tested)
       GOSUBL =RCAB1         mm.ssddddd into A/B
       GOSBVL =FRAC15        0.ssddddd in (A,B)
       GOSBVL =STAB1         0.ssddddd into (R0,R1)
*
SN1    GOSUBL =RCAB1         0.ssddddd into A/B
       A=A+1  A
       A=A+1  A              ss.ddddd in A/B
       GOSUB  SIX            ss.ddddd / 60 in A/B
       GOSUB  SIX            ss.ddddd / (60*60) in A/B
       GOSBVL =RCCD2         hh.DDDDDDD in C/D
       GOSBVL =AD2-15        hh.EEEEEEE in A/B
*
       ?ST=1  1
       RTNYES
       GOVLNG =PUTABX
*
* standalone routine 
* creates 60 in (C,D)
* divide by 60 or multiply by 60
* in and out (A,B)
*
SIX    C=0    W
       P=     14
       LCHEX  6
       P=     0
       C=C+1  P
       D=C    W
       D=0    P
       P=     14
       C=0    P
       SETDEC
       ?ST=0  0
       GOYES  DV
       GOVLNG =MP2-15
*
DV     GOVLNG =DV2-15
*
****************************************************************
* HMS
*
* see JPC ROM HMS page 70
* https://github.com/hp71b/jpcrom/blob/main/src/hms.as
*
* HP41 manual page 88 (page 94 pdf)
* HMS format is hh.mmssdd..
*
* hh,DDDDDDDDD HMS hh,mmssdddddd
* 
* strategy
*   hh,DDDDDDDDD from X into A/B                         21.57
*   put A/B into L (=NUMST)
*   store A/B into R0/R1 (=STAB1)
*   R0/R1 RCL =CLRFRC R2/R3 STO (=STAB2) (hh in R2/R3)   21.
*   R0/R1 RCL (=EXAB1 =STAB1) 
*   test if hh#0 with =INFR15 then 
*     (11) =FRAC15 
*     else
*     (22)
*   (0,DDDDDDDDD) 60 *
*   (mm,EEEEEEE) R0/R1 STO (=STAB1) 
*      (mm,EEEEE in R0/R1)                               34.20
*   =CLRFRC 100 /
*   R2/R3 ST+  ( hh,mm in R2/R3 )
*   R0/R1 RCL 
*   test if mm#0 with =INFR15 then 
*     (33) =FRAC15 
*     else
*     (44)
*   (0,EEEEEEE) 60 * 10000 /   (= 0,00ssFFFFFFF)          0.0012
*   R2/R3 ST+ (0,00ssFFFFFFF added in R2/R3 )
*
* testcase HP41 or HP71B
*   21.57 HMS result 21.3412
*   21.5  HMS result 21.30
*
* ST Status Flag             comment
*      1      0              0 SIX(60 /) ; 1 SIX(60 *)
*      0      1              0 no HMS+/-   1  yes
*      0      2              0 HMS-   1 HMS+
****************************************************************
       WORD 'HMS'
*
       GOSBVL =NUMST
*
       ST=0   1
CONH   ST=1   0
       GOSBVL =STAB1         hh.DDDDDDDDD into (R0,R1)
       GOSBVL =CLRFRC        hh in (A,B)
       GOSBVL =STAB2         hh into (R2,R3)
*
       GOSBVL =RCAB1         hh.DDDDDDDDD into A/B
       GOSBVL =INFR15
       ?P=    14             test if A/B like 0.xxxxxx which means hh = 0
       GOYES  SN2            hh is ZERO (tested)
       GOSBVL =RCAB1
       GOSBVL =FRAC15        value 0.DDDDDDDD  in (A,B)
       GOSBVL =STAB1         0.DDDDDDDD into (R0,R1) for later
*
SN2    GOSBVL =RCAB1         0.DDDDDDDD into A/B
*
       GOSUB  SIX            0.DDDDDDDD*60, mm.EEEEEEE into A/B
       GOSBVL =STAB1         mm.EEEEEEE into (R0,R1) for later
       GOSBVL =CLRFRC        mm in A/B
       A=A-1  A
       A=A-1  A              0.mm into A/B
       GOSBVL =RCCD2         hh in C/D
       GOSBVL =AD2-15        hh.mm in A/B
       GOSBVL =STAB2         hh.mm to R2/R3
       GOSBVL =RCAB1         mm.EEEEEEE into A/B
*
       GOSBVL =INFR15
       ?P=    14             test if A/B like 0,xxxxxx which means mm = 0
       GOYES  SN3            mm is ZERO (tested)
       GOSBVL =RCAB1         mm.EEEEEEE in A/B
       GOSBVL =FRAC15        0.EEEEEEE  now in (A,B)
       GOSBVL =STAB1         0.EEEEEEE into (R0,R1)
*
SN3    GOSBVL =RCAB1         0.EEEEEEE in A/B
       GOSUB  SIX            0.EEEEEEE * 60 = ss.FFFFFF in A/B
       A=A-1  A
       A=A-1  A
       A=A-1  A
       A=A-1  A              0.00ssFFFFFF now in A/B
       GOSBVL =RCCD2         hh.mm in C/D
       GOSBVL =AD2-15        hh.mmssFFFFFF in A/B
*
       ?ST=1  1
       RTNYES
       GOVLNG =PUTABX
*
****************************************************************
* HMS-
*
* see JPC ROM HMSSUB page 72
* https://github.com/hp71b/jpcrom/blob/main/src/hms.as
*
* HP41 manual page 96 pdf (page 90)
* HMS format is hh.mmssddd
*
* strategy
*   store value of X into A/B and L
*   Change sign of A/B (none if HMS+)
*   change value of A/B(X) to format hh,DDDDDDDDD and store it
*    into X back (= HR of it)
*   store value of Y into A/B and change to format hh,ddddddddd 
*    then store it back to Y (= HR of it)
*   Put value of X register in A
*   Put value of Y register in C
*   add A and C , result into A/B
*   change A/B to format HMS
*   put result back to X
*   X<>Y
*   FDROP
*
* use test HP41 
*   70.0002 7.2311 HMS-      X 62.36510000, X into LASTX
*
* tested HP71 30Jan2025
*   70.0002 7.2311 HMS- FS.  X=  62.365100000 
*
*      1      1              0 no HMS+/-   1  yes
*      0      2              0 HMS-   1 HMS+
****************************************************************
       WORD 'HMS-'
       GOSBVL =NUMST
       ST=0   2
       GOTO   ST2
*
*
****************************************************************
* HMS+
*
* see JPC ROM HMSADD page 71
* https://github.com/hp71b/jpcrom/blob/main/src/hms.as
* HP41 manual page 96 pdf (page 90)
* HMS format is hh.mmssdd
*
* use test HP41
*   45.105076 24.491095 HMS+ FS.  result 70.000171
*
* tested HP71 30Jan2025
*   45.105076 24.491095 HMS+ FS.  result 70.000171
*
*      1      1              0 no HMS+/-   1  yes
*      1      2              0 HMS-   1 HMS+
*
* comments/improvements:
* use perhaps stscr / rcscr instead of several uRES12
****************************************************************
       WORD 'HMS+'
       GOSBVL =NUMST         Get X into A/B and X into L
       ST=1   2
ST2    ST=1   1
       GOSUB  COND           Convert A/B with HR
*
       ?ST=1  2
       GOYES  HM0
       A=-A-1 S              change to -A/B in X if HMS-
*
HM0    GOSBVL =uRES12        A/B into C
       A=C    W              Value from C into A
       R0=A
*
       P=     0
       LC(5)  =OX
       CD0EX
       A=R0
       DAT0=A W              Put converted value back to X
*
       D0=D0+ 16
       A=DAT0 W              Get value from Y into A
       GOSBVL =SPLITA
       GOSUB  COND           Convert A/B with HR
       GOSBVL =uRES12        A/B into C
       A=C    W              Value from C into A
       R0=A
*
       P=     0
       LC(5)  =OY            Point to Y
       CD0EX
       A=R0
       DAT0=A W              Put converted value back to Y
       D0=D0- 16             Point to X
       C=DAT0 W              Put X value into C
*
       GOSBVL =AD2-12        Add A(=X) and C(=Y); result in A/B
       GOSBVL CONH           Convert result with HMS into A/B
       GOSBVL =PUTABX        Put result into X
       GOSBVL =X<>Y
       GOVLNG =FDROP
*
************************************************************
*
*
****************************************************************
* FVTOI
*
* Float variable to integer
*
* similar to INT in HP41 (FTOI in Forth HP71B) which change 
* only the X register
*
* it takes the float value of a float variable and put the 
*   result into the integer stack
*
* use case HP41
* 10 STO 02                   store 10 in register 02
* 2.54 STO IND 02             store 2.54 in register 10
*
* use test HP71
* 50 SIZE REG41               create a register area of 50
*                               .. index 0..49
* 10 REG41 RCL FS.            show what is in register 10 
* 2 REG41 10.0 STO            store 10 in register 02
* 2 REG41 FVTOI               put 10 into the integer stack
* REG41 2.54 STO              store 2.54 in register 10
* 10 REG41 RCL FS.            show what is now in register 10 
* 
* command comparison
* HP41                         HP71B
* 10 STO 02                    10.0 2 REG41 STO
* 2.54 STO IND 02              2 REG41 FVTOI REG41 2.54 STO
*
* tested 18 April 2025
* X=  111.333020000 
* X FVTOI S.
* [ 111 ]  OK { 1 } 
* 111.66605 X FVTOI S.
* [ 112 ]  OK { 1 }
* 333.888 FSCRATCH STO
* FSCRATCH FVTOI S.
* [ 334 ]  OK { 1 } 
*
* comments/improvements:
* 
*
****************************************************************
       WORD 'FVTOI'
       GOSBVL =SAVEFP        save pointers: CPU reg, D0, D1
*
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 (the instruction pointer)
       A=DAT0 W
       GOSBVL =FLTDH
*
       GOSBVL =GETFP
       DAT1=A A
       RTNCC
*
****************************************************************
*
*
****************************************************************
* FVIP
* float variable integer part 
*
* put the integer part of a float variable into the integer stack
*
* tested ok 21.04.2025
* FS.
* ..
* Y=  111.666050000 
* ..
* L=  6.00000000000 
* L FVIP S.
* [ 6 ]  OK { 1 } 
* Y FVIP S.
* [ 111 ]  OK { 1 } 
****************************************************************
       WORD 'FVIP'
       GOSBVL =SAVEFP        save pointers: CPU reg, D0, D1
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 (the instruction pointer)
       A=DAT0 W
       XM=0
       GOSBVL =SPLITA        A into (A,B)
       GOSBVL =CLRFRC        xxx,00000 into (A,B)
       GOSBVL =uRESD1        xxx,00000 into (C)
       A=C    W
       GOSBVL =FLTDH         FLTOH was fine, too
       GOSBVL =GETFP
       DAT1=A A
       RTNCC
****************************************************************
*
****************************************************************
* CFV copy the value of the 1st in stack float variable to the second
* tested  20.05.2025
* use: FVAR1 FVAR2 CFV will put float value of FVAR2  into FVAR1
       WORD 'CFV'
CFVL   GOSBVL =SAVEFP        save pointers: CPU reg, D0, D1
       C=DAT1 A              copy data at 1st stack address (addr1) in D1 into C(A)
       CD0EX                 exchange C(A) with D0
       A=DAT0 W              value of 1st stack address into A
       D1=D1+ 5
       C=DAT1 A              copy 2nd address (addr2) on stack in D1 into C(A)
       CD0EX
       DAT0=A W              Store value1 at addr2
       GOSBVL =GETFP         restore pointers: CPU reg, D0, D1
       D1=D1+ 10             return nothing on data stack
       RTNCC
****************************************************************
*
****************************************************************
* DFV download the value of the 2nd float variable in the stack 
*   into the first address on the stack
* tested  20.05.2025
* use: FVAR1 FVAR2 DFV will put float value of FVAR1 into FVAR2
       WORD 'DFV'
       GOSBVL =SWAP
       GOTO   CFVL
****************************************************************
*
****************************************************************
* CRRV : Control Register parameters Retrieving from Variable
* support function for easier use of REGMOVE and REGSWAP of HP41CX
* use FVAR CRRV will put SSS DDD NNN into integer stack 
*       SSS   is the start register address integer
*       DDD   is the destination test value integer
*       NNN   is the increment value integer
*       FVAR  is the float variable where SSS.DDDNNN is stored
* when 1.022003 (sss.dddnnn) in X register
* X CRRV will put  1  22  3     into integer stack
*
* test 20 May 2025
* 777.888110000 FENTER Y CRRV S. 
*  >> [ 777 888 110 ]  OK { 3 } 
*
****************************************************************
       WORD 'CRRV'
       GOSBVL =SAVEFP
       C=DAT1 A              copy data at address in D1 into C(A)
       CD0EX                 exchange C(A) with D0 pointer
       A=DAT0 W              value sss,dddnnnxxx into A
       XM=0                  ERR at execution if not included
       R0=A
* 10000 in (C)
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  6
       A=R0
       SETDEC
       GOSBVL =MP2-12        sssdddnnn,xxx into (A,B)
       GOSBVL =CLRFRC        sssdddnnn,000 into (A,B)
       GOSBVL =STAB1         store sssdddnnn,000 (A,B) into (R0,R1)
*
       C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  997            0,001 now in C
*
       GOSUBL =RCAB1         sssdddnnn,000 into (A,B)
       SETDEC
       GOSBVL =MP1-12        sssddd,nnn000 into (A,B)
       GOSBVL =CLRFRC        sssddd,00000 into (A,B); carry set if nnn=00
       GONC   nadd3          if 0,nnn000 # 0,00 then dont add one
* BRANCH: nnn is ZERO .. then add 1
       GOSUBL =RCAB1         sssddd000,000 into (A,B)
       GOSBVL =ADDONE
       GOSBVL =STAB1         store sssddd001,000 (A,B) into (R0,R1)
*
* BRANCH: nnn IS NOT ZERO from here
*
nadd3  C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  994            0,000001 now in C
       GOSUBL =RCAB1         sssdddnnn,000 into (A,B)
       SETDEC
       GOSBVL =MP1-12        sss,dddnnn000 into (A,B)
       GOSBVL =STAB1         store sss,dddnnn000 (A,B) into (R0,R1)
*
       GOSBVL =INFR15
       ?P#    14             test if (A,B) is not like 0,dddnnn000 which means ssss # 0
       GOYES  SNZ1           sss is NOT ZERO
       A=0    W
       GOTO   SSZ1
SNZ1   GOSBVL =CLRFRC        sss,000 into (A,B)
       GOSBVL =uRESD1        into (C)
       A=C    W
SSZ1   GOSBVL =FLTDH         A into 5 nibs hex A(A)
*
       DAT1=A A              put sss into integer stack
*
       D1=D1- 5
       GOSUBL =RCAB1         value sss,dddnnn000 into (A,B)
       GOSBVL =INFR15
       ?P=    14             test if (A,B) is like 0,dddnnn000 which means ssss = 0
       GOYES  S0Z1           sss is ZERO
       GOSUBL =RCAB1         value sss,dddnnn000 into (A,B)
       GOSBVL =FRAC15        0,dddnnn000 in (A,B)
       GOSBVL =STAB1         store 0,dddnnn000 (A,B) into (R0,R1)
S0Z1   C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  3              1000 in (C)
       GOSUBL =RCAB1         value 0,dddnnn000 into (A,B)
       SETDEC                dont forget this before MP1-12
       GOSBVL =MP1-12        ddd,nnn000 into (A,B)
       GOSBVL =STAB1         store ddd,nnn000 (A,B) into (R0,R1)
       GOSBVL =CLRFRC        ddd,00000 into (A,B)
       GOSBVL =uRESD1        ddd,00 into (C)
       A=C    W
       GOSBVL =FLTDH
       DAT1=A A              put ddd into integer stack
* fine till here
       D1=D1- 5
       GOSUBL =RCAB1         value ddd,nnn000 into (A,B)
       GOSBVL =INFR15
       ?P=    14             test if A is like 0,nnn000 which means ddd = 0
       GOYES  SNZ4           ddd is ZERO
       GOSUBL =RCAB1         value ddd,nnn000 into (A,B)
       GOSBVL =FRAC15        0,nnn000 in (A,B)
       GOSBVL =STAB1         0,nnn000 in (R0,R1)
SNZ4   C=0    W
       P=     14
       C=C+1  P
       P=     0
       LCHEX  3              1000 in (C)
       GOSUBL =RCAB1         0,nnn000 in (A,B)
       SETDEC                dont forget this before MP1-12
       GOSBVL =MP1-12        nnn,000 into (A,B)
       GOSBVL =uRESD1        nnn,000 into (C)
       A=C    W              nnn,000 into (A)
       GOSBVL =FLTDH
       DAT1=A A              put nnn into integer stack
       GOSBVL =GETFP
       D1=D1- 10
       RTNCC
*
****************************************************************
*
* standalone routine 
* recall the values of R0/R1 into A/B
* in additional to the existing =STAB1 =EXAB1 =RCCD1 =STAB2 
*   =EXAB2 =RCCD2 =STCD2
*
*RCAB1  A=R1
*       B=A    W
*       A=R0
*       RTN
*
****************************************************************
*
       END
