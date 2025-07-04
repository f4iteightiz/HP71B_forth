       FORTH
*
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
=SUBONE EQU #0C327           substract 1 to X (A,B) page 1392 of idsv3
=ADDONE EQU #0C330           add 1 to X = (A,B) (AD15S..) pdf page 1392 idsv3
=IN2-15 EQU #0C33E           1/X where X=(A,B)
=X/Y15  EQU #0C34F           X/Y where X=(A,B) and Y=(C,D)
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
PWEEDS  EQU #0C5D3           pull weeds page 1403 use BASIC only?
=XYEX   EQU #0C697           Exchange (A,B) with (C,D) pdf idsv3 page 1408
=SPLITA EQU #0C6BF           Extend (A) into (A,B)  pdf idsv3 page 1409.
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
=LN15   EQU #0CD81           pdf idsv3 page 1446
=EXP15  EQU #0CF5A           pdf idsv3 page 1454
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
=TST15  EQU #0D47A           Compare numbers 15dg AB vs CD idsv3 pdf page 1486
*                             P has the cell# assoClated width the nUMber pair, 
*                             arg's in 15-dig forM unchanged. pdf page 1486 of idsv3
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
=GETX   EQU #E728A           Put X into (A,B) ; pdf page 608 Forth/asm ids
=GETX+L EQU #E72DF           Put X into (A,B) and X in L; pdf page 608 Forth/asm ids
=PUTABX EQU #E72F5           Put (A,B) into X, uRES12 (AB into C),GETFP ; pdf page 609 Forth/asm ids
=STKLFT EQU #E7320           Stacklift Forth OM page 609 pdf LastX dont change
=STKDRP EQU #E734C           Stackdrop Forth OM page 609 pdf LastX change
*
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
*        GOSUB  =ures12
*        D0=(5) (=OX)
*        DAT0=C W
*        SETHEX
*        RTN
*
STABX   R4=C
        GOSUB =ures12
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
        GOSUB =ures12
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
GETY   D0=(5) OY
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
*
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
cslc5  GOVLNG =CSLC5
csrc5  GOVLNG =CSRC5
*
rcscr  GOVLNG =RCSCR
* look at MATH ROM for GAMMA which has an improved fct rcscr+ ??
rcscr+ C=RSTK
       GOSUB  =cslc5
       CD0EX
       GOSUB  =cslc5
       R4=C
       GOSBVL =RCSCR
       RSTK=C               save C[A] on RSTK
       CR4EX                and the rest in R4
       GOSUB  =csrc5
       D0=C
       C=RSTK               get back C[A]
       GOSUB  =csrc5
       RSTK=C
       GOSUB  =cslc5
       RSTK=C               save again C[A]
       C=R4                 restore C from R4
       C=RSTK               with C[A]
       RTN
*
ures12 GOVLNG =uRES12
stkdrp GOVLNG =STKDRP
rcl*   GOVLNG =RCL*
xyex   GOVLNG =XYEX
*
stscr  GOVLNG =STSCR         destroy C; not only C[A]; 
*                            therefore preferably developp a stscr which would not touch C
*                            or use a workaround to avoid C/D when using stscr
* look at MATH ROM for GAMMA which has an improved fct stscr+ ??
stscr+ C=RSTK
       GOSUB  =cslc5
       CD0EX
       GOSUB  =cslc5
       R4=C
       GOSBVL =STSCR
       C=R4
       GOSUB  =csrc5
       CD0EX
       GOSUB  =csrc5
       RSTK=C
       RTN
* 
* tested. Use R4 for not destroying C
*stscr  RSTK=C
*       GOSBVL =SAVD0         uses C(A)
*       R4=C
*       GOSBVL =STSCR         put A/B into top of scratch stack; looks like it uses all C
*       GOSBVL =RSTD0         uses C(A)
*       C=R4                  restore C
*       C=RSTK
*       RTN
*
* tested 18Feb 2025 shorter
*stscr  R4=C                   Use R4 for not destroying all C
*                              R4 is a bit an issue due to interrupts
*       GOSBVL =STSCR
*       C=R4 
*       RTN
*
* seems also ok. check if SAVD0 and RSTD0 necessary.
*rclw3  GOSBVL =SAVD0         uses C(A)
*       GOSBVL =RCLW3         scratch level 3 into A/B, 
*                              A/B was moved to C/D
*       RSTK=C
*       GOSBVL =RSTD0         uses C(A)
*       C=RSTK
*       RTN
*
*                                                               test this
rclw3  GOVLNG =RCLW3         scratch level 3 into A/B, 
*                             A/B was moved to C/D
*                            No D0 saving here is fine
*
* tested
*rclw2  RSTK=C
*       GOSBVL =SAVD0         save D0; uses C(A)
*       GOSBVL =RCLW2         scratch 2 into A/B, A/B was moved to C/D
*       R4=C                  save the C which is the previous A
*       GOSBVL =RSTD0         restore D0; uses C(A)
*       C=RSTK
*       C=R4                  restore C = previous A
*       RTN
*
rclw2  GOVLNG =RCLW2
*
* tested
*rclw1  RSTK=C
*       GOSBVL =SAVD0         uses C(A)
*       C=RSTK
*       GOSBVL =RCLW1         scratch top into A/B, 
*                               A/B was moved to C/D
*       RSTK=C
*       GOSBVL =RSTD0         restore D0 from F-R0-0 in 
*                               SYSTEM RAM, use C(A)
*       C=RSTK
*       RTN
*
rclw1  GOVLNG =RCLW1         No D0 saving here is fine
*
****************************************************************
* EPER 
* ellipse perimeter calculation = 2*PI* MAGM(a^2,b^2) / AGM(a,b)
* for HP71B with FORTH/ASM module
* See file ELPER.TXT for HP41 https://github.com/f4iteightiz/ellipse_iso_perimeter/blob/main/ELPER.TXT
* based on AMS publication http://www.ams.org/notices/201208/rtx120801094p.pdf
* of https://semjonadlaj.com/
* 
* install (debian linux)
*   unix2dos -v EPER.TXT
*   wine '/home/user/.wine/drive_c/Program Files (x86)/HP-Emulators/alifhdr32/alifhdr.exe' EPER.TXT EPER.DAT /T
*   upload EPER.DAT into "In" field of DosLink of EMU71 / ILPER
*   in BASIC prompt: COPY :DOSLINK
*   go into the FORTH prompt.. " EPER" ASSEMBLE
*
* use: b(float) a(float) EPER
*
* tested Feb 18 2025
*
* 0.5 18.0 EPER FS.
* X=  72.1241927329
* L=  18.0000000000 
* OK { 0 } 
*
* on HP41 with Sandmath
* 0.5 ENTER 18.0
* XEQ (sum)F$  ( keys.. ShiftF F ShiftP )
* EPER
* XEQ PRX
* 72.12419272 ***
*
* https://goodcalculators.com/arithmetic-geometric-mean-calculator/
* https://www.mathsisfun.com/geometry/ellipse-perimeter.html#tool
****************************************************************
* issues/remarks:
*   rclw1 2 3 4: use of 4 level fifo stack
*   retest with higher precision = clear only bit 0
*       P=     0
*       B=0    P
*       D=0    P
****************************************************************
*
       WORD 'AGM'
       ST=1  0
       GOTO  EPER
*
       WORD 'EPER'
*
       ST=0  0
EPER   GOSBVL =NUMST         X(=a) to L and A/B
       XM=0                  WRN gone
       SB=0                  WRN gone
       GOSUB  ures12         make a 12digit mantissa for easier 0 testing
       A=C    W
       ?A=0   M              Mantissa of a is zero?
       GOYES  A0             a is zero
       GOSUB  splita
       GOSUB  stab1          X(=a) into R0/R1
       P=     0              why not. just to be sure
       LC(5)  =OY
       CD0EX
       A=DAT0 W              A=Y
       CD0EX
       ?A=0   M              b=0?
       GOYES  B0
       GOTO   AGM
*
A0     GOSUB  stkdrp         a=0 then RTN with 4*b in X
       ?ST#1  0
       GOYES  WA0
       A=0    W
       B=A    W
       GOTO   putabx
*
WA0    P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W              A = b
       CD0EX
       GOSUB  splita         A/B =b
       GOSUB  GET4.
       GOSUB  mp2-15
       GOTO   putabx         4*b into X
*
B0     GOSUB  stkdrp         b=0, now RTN with 4*a in X
       ?ST#1  0
       GOYES  WB0
       A=0    W              necessary if already b=0?
       B=A    W
       GOTO   putabx
*
WB0    GOSBVL RCAB1          X(=a) into A/B
       GOSUB  GET4.
       GOSUB  mp2-15
       GOTO   putabx         4*a into X
*
*
* AGM calculation path
*
AGM    GOSUB  splita         A/B = Y =b
       GOSUB  stab2          Y(=b) into R2/R3
LOO1   GOSUB  RCAB1          an=R0/R1 into A/B
       GOSUB  rccd2          bn=R2/R3 into C/D
       GOSUB  ad2-15         (an+bn) into A/B
       GOSUB  GET.5          .5 into C/D
       GOSUB  mp2-15         ((an+bn)/2) into A/B
       GOSUB  stscr          put bN (an+bn)/2 into scratch
       GOSUB  RCAB1          an=R0/R1 into A/B
       GOSUB  rccd2          bn=R2/R3 into C/D
       GOSUB  mp2-15         (an*bn) in A/B
       GOSBVL =SQR15         aN = sqrt(an*bn) in A/B 
       GOSUB  stab1          aN into R0/R1
       GOSUB  rcscr          bN from scratch into C/D 
       GOSUB  stcd2          bN into R2/R3
*
* round A/B and C/D according p digits = truncate
* now just Put at P = 0 and 1 .. 3?? of register B and D (Mantissa) to 
* zero for avoiding precision instability
* tested
*       P=     0
*       B=0    P
*       D=0    P
*       P=P+1
*       B=0    P
*       D=0    P
*       P=P+1
*       B=0    P
*       D=0    P
*
* tested. shorter.
       B=0    B
       D=0    B              clear nibs 0 and 1
*or
*       B=0    X
*       D=0    X              clear nibs 0, 1 and 2
*
       ?A#C   A
       GOYES  LOO1
       A=B    W
       C=D    W
       ?A#C   M
       GOYES  LOO1
       ?A#C   X
       GOYES  LOO1
*
* calculation agm done. Go OUT if only AGM calc was requested
       ?ST#1  0
       GOYES  WMAGM          go further for EPER calculation
       GOSUBL RCAB2          retrieve agm (bN) from R2/R3 into A/B
       GOSUB  putabx         agm into X
       GOSBVL =X<>Y
       GOVLNG =FDROP
*
WMAGM  GOSUBL RCAB2          retrieve agm (bN) from R2/R3 into A/B
       GOSUB  stscr          put agm into scratch for later recall
*
* MAGM calculation path
*
       P=     0
       LC(5)  =OX
       CD0EX
       A=DAT0 W              A = a
       CD0EX
       C=A    W
       GOSUB  mp2-12         results a^2 (=a0 now) in A/B
       GOSUB  stab1          Store A/B into R0/R1 
       GOSUB  stscr          a0 into scratch stack for rclw3/4
       P=     0
       LC(5)  =OY            Y = b
       CD0EX
       A=DAT0 W              A = b
       CD0EX
       C=A    W
       GOSUB  mp2-12         results b^2 (=b0 now) in A/B
       GOSUB  stab2          Store A/B (=b0) into R2/R3 
       GOSUB  stscr          bn into scratch stack for rclw2/4 later
       A=0    W
       B=A    W              Put zero into A/B
       GOSUB  stscr          Store A/B (Z0; zero for now) into scratch
*                              for rclw1/4 later
*
* scratch structure now 
*   (in the next steps, only level 1 2 3 to be updated)
*   (4th level still keep agm)
* possibilities,
* retrieve agm via RCLW4
*          an (=a^2 here) via rclw3
*          bn (=b^2 here) via rclw2
*          zn             via rclw1
*
* aN calc.. (an/bn)/2
*
*
LOO3   GOSUB  rclw3          an into A/B
       GOSUB  rclw2          bn into A/B, an moved to C/D
       GOSUB  ad2-15         (an+bn) into A/B
       GOSUB  GET.5          .5 into C/D
       GOSUB  mp2-15         ((an+bn)/2 =aN) into A/B 
       GOSUB  stab1          and into R0/R1 (=aN)
*
* bN calc.. (SQRT(an-zn)*(bn-zn))+zn
*
       GOSUB  rclw3          an into A/B
       GOSUB  rclw1          zn into A/B, an now in C/D
       A=-A-1 S              Change A/B to -zn
       GOSUB  ad2-15         (an–zn) in A/B
       GOSUB  stab2          (an–zn) into R2/R3
       GOSUB  rclw2          bn into A/B
       GOSUB  rclw1          zn into A/B and bn now in C/D
       A=-A-1 S              Change sign of A/B
       GOSUB  ad2-15         (bn-zn) in A/B
       GOSUB  rccd2          retrieve R2/R3 (an–zn) into C/D
       GOSUB  mp2-15         (an-zn)*(bn-zn) in A/B 
       GOSBVL =SQR15         SQRT(an-zn)*(bn-zn) in A/B 
       GOSUB  rclw1          zn into A/B; SQRT(an-zn)*(bn-zn) now in C/D
       GOSUB  ad2-15         (SQRT(an-zn)*(bn-zn))+zn in A/B (=bN)
       GOSUB  stab2          (SQRT(an-zn)*(bn-zn))+zn in R2/R3 (=bN)
*
* zN calc.. zn-(SQRT(an-zn)*(bn-zn))
*      bN already in A/B from previous steps
*
       A=-A-1 S              now -(SQRT(an-zn)*(bn-zn))-zn in A/B
       GOSUB  rclw1          zn into A/B; -(SQRT(an-zn)*(bn-zn))-zn in C/D
       GOSUB  ad2-15         -(SQRT(an-zn)*(bn-zn)) now in A/B
       GOSUB  rcscr          zn into C/D, -(SQRT(an-zn)*(bn-zn)) still in A/B
       GOSUB  ad2-15         zn-(SQRT(an-zn)*(bn-zn)) (=zN) in A/B
       GOSUB  rcscr          take scratch level away (bn into C/D); A/B (=zN) not changed
       GOSUB  rcscr          take scratch level away (an into C/D); 
*        all level except 1 for agm, gone. A/B (=zN) not changed
*
* put new aN bN zN into scratch 
*
* workaround tested (solution in case stscr  GOVLNG =STSCR is used; which destroy C)
       GOSUB  exab1          aN in A/B, zN which was in A/B now in R0/R1
       GOSUB  stscr          aN into scratch (later put again into R0/R1)
       GOSUB  RCAB2          bN in A/B
       GOSUB  stscr          bN into scratch, use C
       GOSUB  RCAB1          zN from temporary R0/R1, into A/B
       GOSUB  stscr          zN into scratch, use C
       GOSUB  rclw3          aN from scratch into A/B
       GOSUB  stab1          aN back into R0/R1 for further loops
*
* tested with new stscr version which use R4 for not destroying C
*       GOSUB  xyex           zN which was in A/B, now in C/D
*       GOSUB  RCAB1          aN into A/B
*       GOSUB  stscr          aN into scratch
*       GOSUB  RCAB2          bN in A/B
*       GOSUB  stscr          bN into scratch
*       GOSUB  xyex           zN which was in C/D now in A/B
*       GOSUB  stscr          zN into scratch
*       GOSUB  RCAB1          aN into A/B
*
* now compare A/B and C/D
       GOSUB  rccd2          bN into C/D; C was destroyed by stscr in previous code
*
* round A/B and C/D according p digits = truncate the mantissa
* now just Put at P = 0 and 1 .. 3?? of register B and D (Mantissa) to 
* zero for avoiding precision instability
* tested
*       P=     0
*       B=0    P
*       D=0    P
*       P=P+1
*       B=0    P
*       D=0    P
*       P=P+1
*       B=0    P
*       D=0    P
*
* tested. shorter.
       B=0    B
       D=0    B              clear nibs 0 and 1
*
       ?A#C   A
       GOYES  LOO2
       A=B    W
       C=D    W
       ?A#C   M
       GOYES  LOO2
       ?A#C   X
       GOYES  LOO2
       GOTO   OUT1
*
LOO2   GOLONG LOO3
*
* go out
* take scratch away
*
OUT1   GOSUB  rcscr          zN out from scratch stack = into C/D
       GOSUB  rcscr          bN=magm in C/D
       GOSUB  rcscr          aN=magm in C/D
*
* final calculation
       GOSUB  xyex           aN=magm now in A/B
       GOSUB  rcscr          agm into C/D
       GOSUB  dv2-15         magm/agm in A/B
       GOSUB  GET4.          Put 4. into C/D
       GOSUB  mp2-15         4*magm/agm into A/B
       GOSBVL =PI/2          pi/2 into C/D
       GOSUB  mp2-15         PI*2*magm/agm into A/B
       GOSUB  putabx         ellipse perimeter into X
       GOSBVL =X<>Y
       GOVLNG =FDROP
*
****************************************************************
*
       END
