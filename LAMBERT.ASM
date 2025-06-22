       FORTH
*
* columns..
* 1:label  8:mnemonic  15:modifier  from 24:comments
*
* System entry points: see 14-1 of IMS Vol 1
=RSTD0  EQU #06832           v3p822  restore D0 from F-R0-0 in SYSTEM RAM, use C(A)
=COLLAP EQU #091FB           collapse math stack v3 p1109 use?
=OUTRES EQU #0BC84           ures12 & FN4  idsv3 pdf page 1357 use for basic ?
*=ures12 EQU #0BC7E           call uRES12, idsv3 pdf page 1357
=CNFLCT EQU #0BD15           GOVLNG =RDATTY, idsv3 pdf page 1361; use ??
=POP1N  EQU #0BD1C           Pop 1 Nb off D1 math stack idsv3 pdf page 1362 use most likely BASIC only result in A
=MPOP1N EQU #0BD8D           similar like above.. idsv3 pdf page 1366 use most likely BASIC only
*                            uMODES/POP1N/RTNC
=uMODES EQU #0BDB1           set the modes idsv3 pdf page 1367
=FN4    EQU #0BDE0           goto FNRTN4, idsv3 pdf page 1368 use ??
=ARGERR EQU #0BF19           for report invalid arg error, idsv3 pdf page 1373 use ??
*=PI     EQU #0C000           PI 12digit form (LCHEX) it goes into fn1 which is 
*                             the FRTN1 and probably for BASIC use only
=SUBONE EQU #0C327           substract 1 to X (A,B) page 1392 of idsv3  uses A,B,C,D
=ADDONE EQU #0C330           add 1 to X = (A,B) (AD15S..) pdf page 1392 idsv3  uses A,B,C,D
=INX15  EQU #0C33E           1/X where X=(A,B) idsv3 pdf page 1393
=X/Y15  EQU #0C34F           X/Y where X=(A,B) and Y=(C,D) idsv3 pdf page 1393
=AD2-12 EQU #0C35F           12 digit add = (A) + (C) result 15dig in (A,B) idsv3 pdf page 1394
=AD2-15 EQU #0C363           15-digit add = (A,B) + (C,D) result 15dig in (A,B) idsv3 pdf page 1394
=AD15S  EQU #0C369           same + SB reset page 1394 of idsv3
=ADDF   EQU #0C372           add finites args; pdf idsv3 page 1395; use ??
=MP2-12 EQU #0C432           12 digit * (A) * (C) result 15dig in (A,B) idsv3 pdf page 1397
=MP1-12 EQU #0C436           12 digit * (A,B) * (C) result 15dig in (A,B) idsv3 pdf page 1397
=MP2-15 EQU #0C43A           15-digit multiply (A,B) * (C,D) result 15dig in (A,B) idsv3 pdf page 1397
=MP15S  EQU #0C440           15-digit multiply (A,B) * (C,D) idsv3 pdf page 1397 use ??
=MULTF  EQU #0C446           Multiply finite args; idsv3 pdf page 1397 use ?? 
=SHF10  EQU #0C486           ?? Normalized AB idsv3 pdf page 1397 use ?? 
=DV2-12 EQU #0C4A8           12-digit divise (A) and (C) result in (A,B) idsv3 pdf page 1399
=DV2-15 EQU #0C4AC           15-digit divise (A,B) and (C,d)  result in (A,B) idsv3 pdf page 1399
=DV15S  EQU #0C4B2           ?? 15-digit divise SB not cleared page 1399 use ??
=DIVF   EQU #0C4B8           div finites args; pdf idsv3 page 1400; use ??
=SQR15  EQU #0C534           idsv3 pdf page 1401 SQR(A,B) result in (A,B)
=PWEEDS EQU #0C5D3           pull weeds page 1403 use BASIC only?
=XYEX   EQU #0C697           Exchange (A,B) with (C,D) pdf idsv3 page 1408
=SPLITA EQU #0C6BF           Extend (A) into (A,B)  pdf idsv3 page 1409
=CLRFRC EQU #0C6F4           (A,B) to (A,B) w/o fractio part page 1409 exit in DECMODE carry set if no frac part
=FRAC15 EQU #0C70E           frac of (A,B) into (A,B) page 1411 pdf ids3
=INFR15 EQU #0C73D           same as above, input (A,B), alter C(A),P,CARRY  Page 1412
=SPLTAC EQU #0C934           Extend (A) and (C) into (A,B) and (C,D) pdf idsv3 page 1424
=SPLITC EQU #0C940           Extend (C) into (C,D) pdf idsv3 page 1425
=uRES12 EQU #0C994           Reduce (A,B) into (C) idsv3 pdf page 1426 uses R3
=NRM12  EQU #0C9BB           ?? Round to 12 sig digits pdf idsv3 page 1427 use ??
=uRND>P EQU #0C9CF           Round A/B into C, P=2 12form , p=9 5 form..
=RNDNRM EQU #0CAB1           Round A/B into C/D, mantissa, P=2 12 digits , p=9 5digits..
=FINITA EQU #0CD03           is A/B non finite? pdf idsv3 page 1443 use ??
=FINITC EQU #0CD0F           is C/D non finite? pdf idsv3 page 1443 use ??
=LN1+15 EQU #0CD44           LN( 1+ X=A/B ); pdf idsv3 page 1444
=LN15   EQU #0CD81           pdf idsv3 page 1446.. destroy A..D and R0
=EXP15  EQU #0CF5A           pdf idsv3 page 1454.. destroy A..D and R0 and R1
=YX2-12 EQU #0D274           Y^X & Reg 0 2 3 modified
=FNPWDS EQU #0D3C0           Weeds out NaNs and Infs; pdf idsv3 page 1469 see LN15, FAC15S of MAFO
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
=TST12A EQU #0D476           12dg A & C
=TST15  EQU #0D47A           Compare numbers 15dg AB vs CD idsv3 pdf page 1486
*                             P has the cell# assoClated width the nUMber pair, 
*                             arg's in 15-dig form unchanged. pdf page 1486 of idsv3
=MSN12  EQU #0D553           find most significant NaN, pdf ids v3 page 1490 use ??
=SETSB  EQU #0D641           set SB, pdf ids v3 page 1495 use ??
=SAVESB EQU #0D66E           save SB into sIX (page 1391 =sIX EQU 7), pdf ids v3 page 1497 use ??
=ARG15  EQU #0D67F           ?? ARG((A,B),(C,D)), result in (A,B) R0 R1 touched pdf ids v3 page 1498 use ??
=MAKEPI EQU #0D6F1           ?? Put PI into (C,D) pdf page 1499 idsv3 use ??
=SPLTA  EQU #0D706           = SPLITA ids v3 page 1500
=SPLTB  EQU #0D70C           = SPLTAC ids v3 page 1500
=SIN12  EQU #0D716           ids v3 page 1502
=SIN15  EQU #0D71A           ids v3 page 1502 alter R0 R1
=COS12  EQU #0D721           = splita ids v3 page 1502
=COS15  EQU #0D725           ids v3 page 1502 alter R0 R1
=IVARG  EQU #0D749           p1502
=TWO*   EQU #0DB38           ?? Dbl precision doubler use ??
=PI/2   EQU #0DB77           load PI/2 into (C,D) idsv3 pdf page 1517
=ATAN12 EQU #0DBBA           atan of 12 digit args (A) result in (A,B) ids v3 page 1520
=ATAN15 EQU #0DBBE           atan of 15 digit args (A,B) result in (A,B) ids v3 page 1520
=SB15S  EQU #0E19A           substraction 15 digits (A,B) = (A,B) - (C,D) . needs SETDEC before
=DMP15S EQU #0E1B3           SETDEC then MP15S
=uRESD1 EQU #0E1EE           Reduce (A,B) into (C), similar uRES12, dont point to D1
*                            dont alter R3, exit with P=14 and HEXMODE ids3 pdf page 1553
=SPLTAX EQU #0E62B           SETDEC, then SPLITA Page 1585
=SIGTST EQU #0E636           ?? Handle signal NaN Page 1585 use?? 
=FAC15S EQU #0E72B           factorial 15digit (A,B) into (A,B), v3 page 1591
=FCSTRT EQU #0E757           Factorial Page 1592 use ?? 
=ARGPRP EQU #0E8EF           pop and normalize real number ids3 pdf page 1603
=POP1R  EQU #0E8FD           RTN of POP1N #0BD1C Seite 1361 take 1 arg off mathstack; only for BASIC words
=STSCR  EQU #0E92C           push (A,B) into top cratch stack page 1607. uses C
=RCSCR  EQU #0E954           pull 15from into C/D (A/B unchanged); idsv3 pdf page 1608
=RCLW1  EQU #0E981           recall 1 top math scratch stack entry page 1610 move (AB) to (CD) then recal into (AB)
=RCL*   EQU #0E983           nb P 
=RCLW2  EQU #0E9BE           page 1611
=RCLW3  EQU #0E9C4           same
=BP     EQU #0EADF           make beep float A HZ Float C duration sec pdf ids3 page 1625
=IDIV   EQU #0EC7B           full word integer divide; A/C Integer division pdf page 1635 ids3 Quotien A, Reminder B and C
=HEXDEC EQU #0ECAF           Arg A(A) Hex to Dec use vs HDFLT?
=FNRTN4 EQU #0F238           function return, page 1679 ONLY FOR BASIC
*                             entry D0=PC,D1 new stack pointer,C(W) to be pushed on stack
=RDATTY EQU #17CC6           system fct.. exits to BASIC main loop (= not for Forth?); pdf 2385 idsv3, use ??
=RND-12 EQU #1B01F           A to be rounded in P digits (P=15 none) v3p2617
=FLTDH  EQU #1B223           Convert 12digit flt A to 5 digit Hex Integ A(A) pdf 2627 idsv3, see 
*                             out with hex mode, see FTOI forth/asm ids page pdf 308
=HDFLT  EQU #1B31B           change hex integ A(A) to 12dig float in A(W) exit DEC mode page 2631
=FLOAT  EQU #1B322           change dec integ to 12dig float in A(W)
=CSLC5  EQU #1B435           perform 5x of circular left shift of C v3p2637, CSLC ASMp62
=CSRC5  EQU #1B41B           right, same CRLC ASMp62
=SAVD0  EQU #1C587           SAVE D0 into F-R0-0 v3p2820, use C(A)
=MTHSTK EQU #2F599           data stack
=FUNCD0 EQU #2F8BB           function scratch RAM allocation 5 nibbles
*                            see FUNCD1 FUNCR0..R1 too page 3089
=OL     EQU #2FBC0           L address
=OX     EQU #2FBD0           X address
=OY     EQU #2FBE0           Y address
=OZ     EQU #2FBF0           Z address
=OT     EQU #2FC00           T address
*
* FORTH entry points:
=LT     EQU #E01EF           ( n1 n2 --<-- flag ) ids asm pdf page 170
=GT     EQU #E0231           ( n1 n2 -->-- flag ) ids asm pdf page 169
=IP     EQU #E06B8           Integer part ids asm pdf page 192
=FEND   EQU #E08E9           GOLONG =PUTABX (..uRES12..GETFP) ids asm pdf page 199
=CHS    EQU #E1518           change X sign; dont change LastX, asm ids pdf page 259
=NUMST  EQU #E1718           uMODES/SAVEFP/GETX+L; GET X INTO (A,B); put X into L, asm ids pdf page 266
=MOD    EQU #E1CA1           modulo ids Forth pdf page 287
=ABS    EQU #E1A23           (n -- |n|)
=XXYY   EQU #E212D           comparison of a(X?) and c(Y?) (or zero) pdf page 304 Forth/asm ids
=CMPST  EQU #E216C           comparison operator routine pdf page 304 Forth/asm ids
=ITOF   EQU #E21FD           ( n -- ) ( -- f(n) ) integ to float pdf page 307 Forth/asm ids
=X<>Y   EQU #E23A5           X<>Y pdf page 313 Forth/asm
=OVER   EQU #E2538           (n1 n2 -- n1 n2 n1)
=FDROP  EQU #E30FB           drop X content; X become Y value pdf page 371 Forth/asm ids
=CLAP   EQU #E4A85           collapse math stack
=SAVEFP EQU #E717A           save Forth pointer; pdf page 602 Forth/asm ids
=GETFP  EQU #E71A5           restore Forth pointers; pdf page 602 Forth/asm ids
=GETX   EQU #E728A           Put X into (A,B) ; pdf page 608 Forth/asm ids; D1 to MTHSTK; destroy C, too
=GETX+L EQU #E72DF           Put X into (A,B) and X in L; pdf page 608 Forth/asm ids
=PUTABX EQU #E72F5           Put (A,B) into X, uRES12 (AB into C),GETFP ; pdf page 609 Forth/asm ids
=STKLFT EQU #E7320           Stacklift Forth OM page 609 pdf LastX dont change
=STKDRP EQU #E734C           Stackdrop Forth OM page 609 pdf LastX change
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
* B = |?|MMMMMMMMMM MMMMM|
*     +-+----------+-----+
* EEEEE = A
*
* general remarks:
*   uRES12 MODES GETX+L using D1 therefore D1 has to be saved before its use
*   SETDEC has to be used before most of math functions
*   ADDONE and SUBONE has a warning message WRN; workaround so far not found
*     use XM=0 before ? 
*   dont touch D0 outside of SAVEFP and GETFP
*   SAVEFP and GETFP: check where really necessary for optimizing code
*   X<>L and others could be deleted because it is the same like L X<>
*     or use L X <F>
*   GOSBVL NUMST is making issues with GETX+L where sign is checked. Avoid sometimes.
*   A label cannot start with 0 or 1 or.. 9 letters
*   the entries can have = in the front of them or not ( = is ignored)
*   freeze if D1 wildly changed in a word
*   P=     0 before LC(5)  =OY ! or X Z..
*   C then A has to be loaded prior any AD MP DV TEST
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
* load 0.5 in 15-form in (C,D)
****************************************************************
GET.5  C=0    W
       D=0    W
       P=     14
       LCHEX  #5
       CDEX   W
       C=-C-1 A
       RTN
*
****************************************************************
* load 4.0 in 15-form in (C,D)
****************************************************************
GET4.  C=0    W
       D=0    W
       P=     14
       LCHEX  #4
       CDEX   W
       RTN
*
****************************************************************
* load 2.0 in 15-form in (C,D)
****************************************************************
*GET2.  C=0    W
*       D=0    W
*       P=     14
*       LCHEX  #2
*       CDEX   W
*       RTN
*
****************************************************************
* save A/B in X 
* not tested
****************************************************************
*STABX   D1=(5) =MTHSTK
*        CDEX   W
*        C=DAT1 A
*        D1=C
*        CDEX   W
*        GOSUB  ures12
*        D0=(5) (=OX)
*        DAT0=C W
*        SETHEX
*        RTN
*
STABX   R4=C
        GOSUB  ures12
        A=C    W
        LC(5) =OX
        CD0EX
        DAT0=A W
        CD0EX
        C=R4
        RTN
*
****************************************************************
* save A/B in Y 
* not tested
****************************************************************
*STABY   D1=(5) =MTHSTK
*        CDEX   W
*        C=DAT1 A
*        D1=C
*        CDEX   W
*        GOSUB  =ures12
*        D0=(5) (=OY)
*        DAT0=C W
*        SETHEX
*        RTN
*
STABY   R4=C
        GOSUB ures12
        A=C    W
        LC(5) =OY
        CD0EX
        DAT0=A W
        CD0EX
        C=R4
        RTN
*
****************************************************************
* Get Y in A/B 
* not tested
****************************************************************
GETY   D0=(5) =OY
       A=DAT0 W
       SETDEC
       D1=(5) =MTHSTK
       C=DAT1 A
       D1=C
       GOSBVL =SIGTST
       GONC   GX02
       GOSBVL =uRES12
       A=C    W
GX02   GOVLNG =SPLITA
*
****************************************************************
* Get X in A
****************************************************************
GETXA  D0=(5) =OX
       A=DAT0 W
       RTN 
*
****************************************************************
* Get X in A/B
****************************************************************
GETXAB D0=(5) =OX
       A=DAT0 W
       GOVLNG =SPLITA
*
****************************************************************
* Get X in C
* destroys A & C
****************************************************************
GETXC  D0=(5) =OX
*
*       A=DAT0 W
*       C=A    W
* test
       C=DAT0 W
       RTN
*
****************************************************************
* standalone routine  ALREADY IN MAFO = dont transfer there
* recall the values of R0/R1 into A/B
* in additional to the existing =STAB1 =EXAB1 =RCCD1 =STAB2 
*   =EXAB2 =RCCD2 =STCD2
*
RCAB1  A=R1
       B=A    W
       A=R0
       RTN
*
RCAB2  A=R3
       B=A    W
       A=R2
       RTN
*
****************************************************************
* standalone routine
* recall the parameter at deepest place of the scratch
* in additional to the existing =RCLW1 =RCLW2 =RCLW3
* not tested
RCLW4  GOSBVL =SAVD0         uses C(A)
       P=     3              scratch level 4 
       GOSUB  rcl*           scratch level x into A/B, 
*                              A/B was moved to C/D
       RSTK=C
       GOSBVL =RSTD0         uses C(A)
       C=RSTK
       P=     0
       RTN
*
****************************************************************
* jump table
****************************************************************
stab1  GOVLNG =STAB1
rccd1  GOVLNG =RCCD1
stcd2  GOVLNG =STCD2
exab2  GOVLNG =EXAB2
stab2  GOVLNG =STAB2
exab1  GOVLNG =EXAB1
rccd2  GOVLNG =RCCD2
putabx GOVLNG =PUTABX
splita GOVLNG =SPLITA
mp2-15 GOVLNG =MP2-15
mp2-12 GOVLNG =MP2-12
dv2-15 GOVLNG =DV2-15
ad2-15 GOVLNG =AD2-15
ures12 GOVLNG =uRES12
stkdrp GOVLNG =STKDRP
rcl*   GOVLNG =RCL*
xyex   GOVLNG =XYEX
inx15  GOVLNG =INX15
ln15   GOVLNG =LN15
addone GOVLNG =ADDONE
getfp  GOVLNG =GETFP
numst  GOVLNG =NUMST
exp15  GOVLNG =EXP15
sqr15  GOVLNG =SQR15
tst15  GOVLNG =TST15
tst12a GOVLNG =TST12A
tst12  GOVLNG =uTEST
getx   GOVLNG =GETX
splitc GOVLNG =SPLITC
spltac GOVLNG =SPLTAC
ivarg  GOVLNG =IVARG
*
rcscr  GOVLNG =RCSCR
stscr  GOVLNG =STSCR         destroy C; not only C[A]; 
*                            therefore preferably developp a stscr which would not touch C
*                            or use a workaround to avoid C/D when using stscr
rclw3  GOVLNG =RCLW3         scratch level 3 into A/B, 
*                             A/B was moved to C/D
rclw2  GOVLNG =RCLW2
rclw1  GOVLNG =RCLW1         No D0 saving here is fine
*
****************************************************************
* LAMBERT
* lambert W function calculation
* for HP71B with FORTH/ASM module
*
* numerical method based on wiki https://en.wikipedia.org/wiki/Lambert_W_function#Numerical_evaluation
* and https://www.researchgate.net/publication/362219191_Guaranteed-_and_high-precision_evaluation_of_the_Lambert_W_function#pfc
* calculate wN=wn-(((wn*exp(wn))-X)/(exp(wn)+wn*exp(wn)))
*   simplified >>> wN= wn - ((wn-X*exp(-wn))/(1+wn)) <<<
* or quicker with 
*   wN=wn-((wn*exp(wn)-X)/((exp(wn)*(wn+1))-(((wn+2)*(wn*exp(wn)-X))/(2*wn+2))))
*   but larger code would be necessary 
* or quicker (quadratic convergence) HERE IN THIS CODE the new iteration with starting values
*   wN=(wn/(1+wn))*(1+ln(X/wn))
*
* install (debian linux)
*   unix2dos -v LAMBERT.ASM
*   wine '/home/user/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe' LAMBERT.ASM LAMBERT.DAT /T
*   upload LAMBERT.DAT into "In" field of DosLink of EMU71 / ILPER
*   in BASIC prompt: COPY :DOSLINK
*   go into the FORTH prompt.. " LAMBERT" ASSEMBLE
*
* use: a(float) LW0 (0 branch from -1/e to infinite)
*      a(float) LW1 (-1 branch from -1/e to 0)
*
* tested 22 June 2025
*
* 1.0 LW0 FS.
* X: 0.567143290 
*
* on HP41 with Sandmath manual V5 pdf page 156
* 2 XEQ WL0 result 0.852605502
*
* -0.3 XEQ (sum)F$  ( keys.. ShiftF F ShiftP )
* WL1 result -1.7813414
*
* or in https://www.had2know.org/academics/lambert-w-function-calculator.html
*
****************************************************************
* issues/remarks/improvements:
*   observe if unstable calculation point? 
*       (clearing bits 0 1 2 avoided this)
*   optimize the code
*       take ures12 away but use more xyex and GETXAB
*       or just keep all because tst12 tst12a tst15 seen all together
****************************************************************
*
       WORD 'LW0'
       GOSBVL =NUMST         SAVEFP etc. X to L and (A,B)
       SB=0                  better for taking WRN away when X=0
       XM=0
*
*      X = 0? go out
       GOSUB  xyex
       A=0    W
       B=0    W
       P=     2
       GOSUB  tst15
       GONC   LMOR0
       GOTO   OUTLW          carry set = yes then just go out 
*
* check if X smaller than -1/e; then nothing (could be improved with error message "out of range")
*
LMOR0  A=0    W
       B=0    W
       P=     14
       B=B+1  P              A/B = 1
       GOSUB  exp15          A/B = e; R0/R1 destroyed
       GOSUB  inx15          A/B = 1/e
       A=-A-1 S              set sign of A/B to -1/e
       GOSUB  xyex
       GOSUB  GETXAB       
       P=     1
       GOSUB  tst15
       GONC   LMOR1
       GOTO   OUTLWE         carry set = yes then go out Error
*
* else further calculation; calculate startpoint 
*   if X <=0  w0=(e*X*log(1+sqrt(1+e*X)))/(1+e*X+sqrt(1+e*X)).. w01
*
LMOR1  GOSUB  GETXC          put X into C
       A=0    W
       P=     6                                                     better instead 4
       GOSUB  tst12a          X<0? then carry set
       GONC   LMOR2
       GOTO   LBLW01          carry set = yes then calculate w0=(e*X*log(1+sqrt(1+e*X)))/(1+e*X+sqrt(1+e*X))
*
*   if X>e  then      w0=log(X)-log(log(X)).. w02
*           equal to  w0=log(X/log(X))
LMOR2  A=0    W
       B=0    W
       P=     14
       B=B+1  P              A/B = 1
       GOSUB  exp15          A/B = e
       GOSUB  xyex
       GOSUB  GETXAB       
       P=     4
       GOSUB  tst15
       GONC   LMOR3
       GOTO   LBLW02           carry set = yes then calculate w0=log(X/log(X))
*      
*   else (0<= X <= e) w0=X/e
*
LMOR3  A=0    W
       B=0    W
       P=     14
       B=B+1  P              A/B = 1
       GOSUB  exp15          A/B = e; destroy R0 & R1
       GOSUB  xyex           e into C/D
       GOSUB  GETXAB         put X into A/B
       SETDEC                                               better due to issue LW0 0..1.06 ?
       GOSUB  dv2-15         X/e in A/B
*
* just in case for testing the path
*       GOTO   putabx
*
       GOTO   LW             global calculation; same
***********************************************************************
*
       WORD 'LW1'
       GOSBVL =NUMST         X to L and (A,B)
       SB=0                  better for taking WRN away when X=>0
       XM=0
*
*      X => 0? just go out
*
       GOSUB  xyex
       A=0    W    
       B=0    W
       P=     3
       GOSUB  tst15
       GONC   LMOR4
       GOTO   OUTLWE         carry set = yes then go out Error Argument
*
* check if X smaller than -1/e; then go out Error Argument
*
LMOR4  A=0    W
       B=0    W
       P=     14
       B=B+1  P              A/B = 1
       GOSUB  exp15          A/B = e; destroy R0 and R1
       GOSUB  inx15          A/B = 1/e
       A=-A-1 S              change sign of A/B to -1/e
*
       GOSUB  ures12
       R0=C
       GOSUB  GETXC          put X into C
       A=R0                  A=-1/e
       P=     4
       GOSUB  tst12          X<-1/e ?
       GONC   LMOR6
       GOTO   OUTLWE         carry set = yes then go out Error Argument
*
* else further calculation; calculate startpoint
* all fine tested
*   if X < -1/4 w0=-1-sqrt(2(1+e*X))).. W03
*
LMOR6  A=0    W
       B=0    W
       P=     14
       B=B+1  P
       B=B+1  P
       B=B+B  P              4 in A/B
       GOSUB  inx15          1/4 in A/B
       A=-A-1 S              -1/4 in A/B
*
       GOSUB  stab1          
       GOSUB  GETXAB
       GOSUB  xyex           X in C/D
       GOSUB  RCAB1          -1/4 in A/B
       P=     4
       GOSUB  tst15
*
* dont work
*       GOSUB  xyex
*       GOSUB  GETXAB
*       P=     4
*       GOSUB  tst15
*
       GONC   LMOR7
       GOTO   LBLW03         carry set = yes then go there  
*
* else (-1/4 <= X < 0) w0=log(-X)-log(-log(-X)) or
*                      w0=log(X/log(-X))
*
LMOR7  GOSUB  GETXAB         put X into A/B 
       A=-A-1 S             -X in A/B
       GOSUB  ln15           ln(-X) in A/B; destroy R0
       GOSUB  inx15          1/ln(-X) in A/B
       GOSUB  stab1          1/ln(-X) in R0/R1
       GOSUB  GETXAB         put X into A/B 
       GOSUB  xyex           put X into C/D
       GOSUB  RCAB1          1/ln(-X) in A/B
       SETDEC
       GOSUB  mp2-15         X/ln(-X) in A/B
       GOSUB  ln15           ln(X/ln(-X)) in A/B; destroy R0
*
* was just for testing the error -0.01 LW1 which blocked
*       GOTO   OUTLW
*
LW     GOSUB  stab1          store w0 into R0/R1
       GOSUB  GETXAB         put X into A/B
       GOSUB  stscr          store X into stack for rclw2/4 later; easier than letting him in X
       GOSUB  RCAB1          put w0 into A/B
       GOSUB  stscr          store w0 into stack for rclw1/4 later
*
*   wN=(wn/(1+wn))*(1+ln(X/wn))
*
LOO2   GOSUB  rcscr          pull stack value last w(n) into C/D (A/B unchanged)         
       GOSUB  RCAB1          put wn from R0/R1 into A/B
       GOSUB  stscr          store wn into stack, C destroyed
       GOSUB  rclw1          put wn into A/B, move A/B to C/D
       GOSUB  rclw2          put X into A/B (move A/B = wn to C/D)
       SETDEC
       GOSUB  dv2-15         put X/wn into A/B
       GOSUB  ln15           ln(X/wn) in A/B; destroy R0
       GOSUB  addone         1+ln(X/wn) in A/B
       GOSUB  stab2          store 1+ln(X/wn) into R2/R3
       GOSUB  rclw1          put wn into A/B, 1+ln(X/wn) into C/D
       GOSUB  addone         1+wn in A/B, C/D gone
       GOSUB  rclw1          put wn into A/B, 1+wn into C/D
       SETDEC
       GOSUB  dv2-15         wn/(1+wn) into A/B
       GOSUB  xyex           move wn/(1+wn) to C/D
       GOSUB  RCAB2          1+ln(X/wn) in A/B
       SETDEC
       GOSUB  mp2-15         wN=(wn/(1+wn))*(1+ln(X/wn)) in A/B
       GOSUB  stab1          store new wN into R0/R1
       GOSUB  rclw1          put wn into A/B, wN into C/D
*
* now compare A/B to C/D; if (nearly) equal go out
*
*       B=0    B
*       D=0    B              clear nibs 0 and 1   use was nOK: LW1 blocked at X -0.01
*
* better: less instability probably due to mantissa X 12digits injected into into A/B 
* during calculation which could make instable mantissa 15digits calc from LW label on 
       B=0    X
       D=0    X              clear nibs 0, 1 and 2
*
       ?A#C   A
       GOYES  LOO2
       A=B    W
       C=D    W
       ?A#C   M
       GOYES  LOO2
       ?A#C   X
       GOYES  LOO2
*
       GOSUB  rcscr          destroy 1 pos of stack
       GOSUB  rcscr          destroy 2nd pos of stack; now stack is empty
       GOSUB  RCAB1          wN in A/B
       GOTO   putabx
*       
OUTLW  GOSUB  GETXAB
       GOTO   putabx         getfp was a problem; putabx if numst used
*
OUTLWE GOSUB  GETXAB
       GOSUB  ivarg          Invalid Argument
       GOTO   putabx
*
* w0=(e*X*log(1+sqrt(1+e*X)))/(1+e*X+sqrt(1+e*X)).. w01
*
LBLW01 A=0    W
       B=0    W
       P=     14
       B=B+1  P
       GOSUB  exp15          e in A/B; destroy R0 and R1
       GOSUB  stab1          e in R0/R1
       GOSUB  GETXAB         X in A/B
       GOSUB  xyex           X in C/D 
       GOSUB  RCAB1          e in A/B   
       SETDEC
       GOSUB  mp2-15         e*X in A/B
       GOSUB  addone         1+e*X in A/B
       GOSUB  stab1          1+e*X in R0/R1
       GOSUB  sqr15          SQRT(1+e*X) in A/B
       GOSUB  xyex           SQRT(1+e*X) in C/D
       GOSUB  RCAB1          1+e*X in A/B   
       GOSUB  ad2-15         SQRT(1+e*X) + 1+e*X in A/B
       GOSUB  inx15          1/(SQRT(1+e*X) + 1+e*X ) in A/B
       GOSUB  stscr          into stack
       GOSUB  exab1          1+e*X in A/B
       GOSUB  sqr15          SQRT(1+e*X) in A/B
       GOSUB  addone
       GOSUB  ln15           log(1+sqrt(1+e*X)) in A/B; destroy R0
       GOSUB  stscr          into stack
       A=0    W
       B=0    W
       P=     14
       B=B+1  P
       GOSUB  exp15          e in A/B; destroy R0 and R1
       GOSUB  stab1          e in R0/R1
       GOSUB  GETXAB         X in A/B; destroy R3
       GOSUB  xyex           X in C/D 
       GOSUB  RCAB1          e in A/B   
       SETDEC
       GOSUB  mp2-15         e*X in A/B
       GOSUB  rcscr          log(1+sqrt(1+e*X) in C/D (e*X in A/B untouched)
       SETDEC
       GOSUB  mp2-15         e*X*log(1+sqrt(1+e*X) into A/B
       GOSUB  rcscr          1/(SQRT(1+e*X) + 1+e*X ) in C/D
       SETDEC
       GOSUB  mp2-15         w01 in A/B
       GOTO   LW
* 
* w0=log(X/log(X))
*
LBLW02 GOSUB  GETXAB         X in A/B
       GOSUB  stab2          X in R2/R3
       GOSUB  ln15           destroy R0
       GOSUB  xyex           log(X) into C/D
       GOSUB  RCAB2          X into A/B
       SETDEC
       GOSUB  dv2-15         X/log(X) into A/B
       GOSUB  ln15           log(X/log(X)) into A/B
       GOTO   LW
*
* w0=-1-sqrt(2*(1+e*X)).. W03
*
LBLW03 A=0    W
       B=0    W
       P=     14
       B=B+1  P
       GOSUB  exp15          e in A/B; destroy R0 and R1
       GOSUB  stab1
       GOSUB  GETXC          X in C
       GOSUB  splitc         X in C/D
       GOSUB  RCAB1          e in A/B
       SETDEC
       GOSUB  mp2-15         e*X in A/B
       GOSUB  addone         1+e*X in A/B
       C=B    W
       D=C    W
       C=A    W              1+e*X in C/D
       GOSUB  ad2-15         2*(1+e*X) in A/B
       GOSUB  sqr15          SQRT(2*(1+e*X)) in A/B
       GOSUB  addone         1+SQRT(2*(1+e*X)) in A/B
       A=-A-1 S              -1-SQRT(2*(1+e*X)) in A/B
       GOTO   LW
*
****************************************************************
*
       END
