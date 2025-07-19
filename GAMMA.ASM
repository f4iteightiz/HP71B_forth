       FORTH
*
* columns..
* 1:label  8:mnemonic  15:modifier  from 24:comments
*
* System entry points: see 14-1 of IMS Vol 1
=COLLAP EQU #091FB             collapse math stack v3 p1109 use?
=OUTRES EQU #0BC84             ures12 & FN4  idsv3 pdf page 1357 use for basic ?
=ures12 EQU #0BC7E             call uRES12, idsv3 pdf page 1357
=CNFLCT EQU #0BD15             GOVLNG =RDATTY, idsv3 pdf page 1361; use ??
=POP1N  EQU #0BD1C             Pop 1 Nb off D1 math stack idsv3 pdf page 1362 use most likely BASIC only result in A
=MPOP1N EQU #0BD8D             similar like above.. idsv3 pdf page 1366 use most likely BASIC only
*                              uMODES/POP1N/RTNC
=uMODES EQU #0BDB1             set the modes idsv3 pdf page 1367, modify D(A), uses S8-11
=FN4    EQU #0BDE0             goto FNRTN4, idsv3 pdf page 1368 use ??
=ARGERR EQU #0BF19             for report invalid arg error, idsv3 pdf page 1373 use ??
*=PI     EQU #0C000             PI 12digit form (LCHEX) it goes into fn1 which is 
*                               the FRTN1 and probably for BASIC use only
=SUBONE EQU #0C327             substract 1 to X (A,B) page 1392 of idsv3
=ADDONE EQU #0C330             add 1 to X = (A,B) (AD15S..) pdf page 1392 idsv3
=IN2-15 EQU #0C33E             1/X where X=(A,B)
=X/Y15  EQU #0C34F             X/Y where X=(A,B) and Y=(C,D)
=AD2-12 EQU #0C35F             12 digit add = (A) + (C) result 15dig in (A,B) idsv3 pdf page 1394
=AD2-15 EQU #0C363             15-digit add = (A,B) + (C,D) result 15dig in (A,B) idsv3 pdf page 1394
=AD15S  EQU #0C369             same + SB reset page 1394 of idsv3
=ADDF   EQU #0C372             add finites args; pdf idsv3 page 1395; use ??
=MP2-12 EQU #0C432             12 digit * (A) * (C) result 15dig in (A,B) idsv3 pdf page 1397
=MP1-12 EQU #0C436             12 digit * (A,B) * (C) result 15dig in (A,B) idsv3 pdf page 1397
=MP2-15 EQU #0C43A             15-digit multiply (A,B) * (C,D) result 15dig in (A,B) idsv3 pdf page 1397
=MP15S  EQU #0C440             15-digit multiply (A,B) * (C,D) idsv3 pdf page 1397 use ??
=MULTF  EQU #0C446             Multiply finite args; idsv3 pdf page 1397 use ?? 
=SHF10  EQU #0C486             ?? Normalized AB idsv3 pdf page 1397 use ?? 
=DV2-12 EQU #0C4A8             12-digit divise (A) and (C) result in (A,B) idsv3 pdf page 1399
=DV2-15 EQU #0C4AC             15-digit divise (A,B) and (C,d)  result in (A,B) idsv3 pdf page 1399
=DV15S  EQU #0C4B2             ?? 15-digit divise SB not cleared page 1399 use ??
=DIVF   EQU #0C4B8             div finites args; pdf idsv3 page 1400; use ??
=SQR15  EQU #0C534             idsv3 pdf page 1401 SQR(A,B) result in (A,B)
PWEEDS  EQU #0C5D3             pull weeds page 1403 use BASIC only?
=XYEX   EQU #0C697             Exchange (A,B) with (C,D) pdf v3p1408
=SPLITA EQU #0C6BF             Extend (A) into (A,B)  pdf idsv3 page 1409.
=CLRFRC EQU #0C6F4             (A,B) to (A,B) w/o fractio part page 1409 exit in DECMODE carry set if no frac part
=FRAC15 EQU #0C70E             frac of (A,B) into (A,B) page 1411 pdf ids3
=INFR15 EQU #0C73D             same as above, input (A,B), alter C(A),P,CARRY  Page 1412
=SPLTAC EQU #0C934             Extend (A) and (C) into (A,B) and (C,D) pdf idsv3 page 1424
=SPLITC EQU #0C940             Extend (C) into (C,D) pdf idsv3 page 1425
=uRES12 EQU #0C994             Reduce (A,B) into (C) idsv3 pdf page 1426 uses R3
=NRM12  EQU #0C9BB             ?? Round to 12 sig digits pdf idsv3 page 1427 use ??
=uRND>P EQU #0C9CF             Round A/B into C, P=2 12form , p=9 5 form..
=RNDNRM EQU #0CAB1             Round A/B into C/D, mantissa, P=2 12 digits , p=9 5digits..
=FINITA EQU #0CD03             is A/B non finite? pdf idsv3 page 1443 use ??
=FINITC EQU #0CD0F             is C/D non finite? pdf idsv3 page 1443 use ??
=LN1+15 EQU #0CD44             LN( 1+ X=A/B ); pdf idsv3 page 1444
=LN15   EQU #0CD81             pdf idsv3 page 1446
=EXP15  EQU #0CF5A             pdf idsv3 page 1454
=YX2-12 EQU #0D274             Y^X & Reg 0 2 3 modified
=FNPWDS EQU #0D3C0             Weeds out NaNs and Infs; pdf idsv3 page 1469 see LN15, FAC15S of MAFO
=STAB1  EQU #0D3D9             Store AB into scratch1 (R0,R1) pdf idsv3 page 1471
=EXAB1  EQU #0D3E7             Exchange AB and scratch1 pdf idsv3 page 1471
=RCCD1  EQU #0D3F5             Recall scratch1 into CD pdf idsv3 page 1471
=STAB2  EQU #0D400             Store AB into scratch2 (R2,R3) pdf idsv3 page 1471
=EXAB2  EQU #0D40E             Exchange AB and scratch2 pdf idsv3 page 1471
=RCCD2  EQU #0D41C             Recall scratch2 into CD pdf idsv3 page 1471
=STCD2  EQU #0D427             Store CD into scratch2 (R2,R3) pdf idsv3 page 1471
=uTEST  EQU #0D435             User real comparison A & C in 12digit pdf idsv3 Page 1484..6 
*                               with carry which has the result
*                               with predicat in page 1486; < is 1; <= is 3; ..
=TST15  EQU #0D47A             Compare numbers 15dg AB vs CD idsv3 pdf page 1486
*                               P has the cell# assoClated width the nUMber pair, 
*                               arg's in 15-dig forM unchanged. pdf page 1486 of idsv3
=MSN12  EQU #0D553             find most significant NaN, pdf ids v3 page 1490 use ??
=SETSB  EQU #0D641             set SB, pdf ids v3 page 1495 use ??
=SAVESB EQU #0D66E             save SB into sIX (page 1391 =sIX EQU 7), pdf ids v3 page 1497 use ??
=ARG15  EQU #0D67F             ?? ARG((A,B),(C,D)), result in (A,B) R0 R1 touched pdf ids v3 page 1498 use ??
=MAKEPI EQU #0D6F1             ?? Put PI into (C,D) pdf page 1499 idsv3 use ??
=SPLTA  EQU #0D706             = SPLITA ids v3 page 1500
=SPLTB  EQU #0D70C             = SPLTAC ids v3 page 1500
=SIN12  EQU #0D716             ids v3 page 1502
=SIN15  EQU #0D71A             ids v3 page 1502 alter R0 R1
=COS12  EQU #0D721             = splita ids v3 page 1502
=COS15  EQU #0D725             ids v3 page 1502 alter R0 R1
=TWO*   EQU #0DB38             ?? Dbl precision doubler use ??
=PI/2   EQU #0DB77             load PI/2 into (C,D) idsv3 pdf page 1517
=ATAN12 EQU #0DBBA             atan of 12 digit args (A) result in (A,B) ids v3 page 1520
=ATAN15 EQU #0DBBE             atan of 15 digit args (A,B) result in (A,B) ids v3 page 1520
=SB15S  EQU #0E19A             substraction 15 digits (A,B) = (A,B) - (C,D) . needs SETDEC before
=DMP15S EQU #0E1B3             SETDEC then MP15S
=uRESD1 EQU #0E1EE             Reduce (A,B) into (C), similar uRES12, dont point to D1
*                              dont alter R3, exit with P=14 and HEXMODE ids3 pdf page 1553
=SPLTAX EQU #0E62B             SETDEC, then SPLITA Page 1585
=SIGTST EQU #0E636             ?? Handle signal NaN Page 1585 use?? 
=FAC15S EQU #0E72B             factorial 15digit (A,B) into (A,B), v3 page 1591
=FCSTRT EQU #0E757             Factorial Page 1592 use ?? 
=ARGPRP EQU #0E8EF             pop and normalize real number ids3 pdf page 1603
=POP1R  EQU #0E8FD             RTN of POP1N #0BD1C Seite 1361 take 1 arg off mathstack; only for BASIC words
=STSCR  EQU #0E92C             Push (A,B) into top cratch stack page 1607
=RCSCR  EQU #0E954             push 15from into C/D (A/B unchanged); idsv3 pdf page 1608
=RCLW1  EQU #0E981             ?? recall 1 top math scratch stack entry page 1610 move (AB) to (CD) then recal into (AB) use??
=RCL*   EQU #0E983             ?? nb P 
=RCLW2  EQU #0E9BE             ?? page 1611
=RCLW3  EQU #0E9C4             ?? same
=BP     EQU #0EADF             make beep float A HZ Float C duration sec pdf ids3 page 1625
=IDIV   EQU #0EC7B             full word integer divide; A/C Integer division pdf page 1635 ids3 Quotien A, Reminder B and C
=HEXDEC EQU #0ECAF             Arg A(A) Hex to Dec use vs HDFLT?
=FNRTN4 EQU #0F238             function return, page 1679 ONLY FOR BASIC
*                               entry D0=PC,D1 new stack pointer,C(W) to be pushed on stack
=RDATTY EQU #17CC6             system fct.. exits to BASIC main loop (= not for Forth?); pdf 2385 idsv3, use ??
=RND-12 EQU #1B01F             A to be rounded in P digits (P=15 none) v3p2617
=FLTDH  EQU #1B223             Convert 12digit flt A to 5 digit Hex Integ A(A) pdf 2627 idsv3, see 
*                               out with hex mode, see FTOI forth/asm ids page pdf 308
=HDFLT  EQU #1B31B             change hex integ A(A) to 12dig float in A(W) exit DEC mode page 2631
=FLOAT  EQU #1B322             change dec integ to 12dig float in A(W)
=CSLC5  EQU #1B435             perform 5x of circular left shift of C page 2637 idsv3
=CSRC5  EQU #1B41B             right, same
=MTHSTK EQU #2F599             data stack
=FUNCD0 EQU #2F8BB             function scratch RAM allocation 5 nibbles
*                              see FUNCD1 FUNCR0..R1 too page 3089
=OL     EQU #2FBC0             L address
=OX     EQU #2FBD0             X address
=OY     EQU #2FBE0             Y address
=OZ     EQU #2FBF0             Z address
=OT     EQU #2FC00             T address
*
* FORTH entry points:
=LT     EQU #E01EF             ( n1 n2 --<-- flag ) ids asm pdf page 170
=GT     EQU #E0231             ( n1 n2 -->-- flag ) ids asm pdf page 169
=IP     EQU #E06B8             Integer part ids asm pdf page 192
=FEND   EQU #E08E9             GOLONG =PUTABX (..uRES12..GETFP) ids asm pdf page 199
=CHS    EQU #E1518             change X sign; dont change LastX, asm ids pdf page 259
=NUMST  EQU #E1718             uMODES/SAVEFP/GETX+L; GET X INTO (A,B); put X into L, asm ids pdf page 266
=MOD    EQU #E1CA1             modulo ids Forth pdf page 287
=ABS    EQU #E1A23             (n -- |n|)
=XXYY   EQU #E212D             comparison of a(X?) and c(Y?) (or zero) pdf page 304 Forth/asm ids
=CMPST  EQU #E216C             comparison operator routine pdf page 304 Forth/asm ids
=ITOF   EQU #E21FD             ( n -- ) ( -- f(n) ) integ to float pdf page 307 Forth/asm ids
=OVER   EQU #E2538             (n1 n2 -- n1 n2 n1)
=FDROP  EQU #E30FB             drop X content; X become Y value pdf page 371 Forth/asm ids
=CLAP   EQU #E4A85             collapse math stack
=SAVEFP EQU #E717A             save Forth pointer; pdf page 602 Forth/asm ids
=GETFP  EQU #E71A5             restore Forth pointers; pdf page 602 Forth/asm ids
=GETX   EQU #E728A             Put X into (A,B) ; pdf page 608 Forth/asm ids
=GETX+L EQU #E72DF             Put X into (A,B) and X in L; pdf page 608 Forth/asm ids
*                              SIGTST/uRES12/SPLITA
=PUTABX EQU #E72F5             Put (A,B) into X, uRES12 (AB into C),GETFP ; pdf page 609 Forth/asm ids
=STKLFT EQU #E7320             Stacklift Forth OM page 609 pdf LastX dont change
=STKDRP EQU #E734C             Stackdrop Forth OM page 609 pdf LastX change
*
****************************************************************
* see notes.txt for the word mapping of the MATH module
* https://www.jeffcalc.hp41.eu/emu71/mathrom.html
****************************************************************
*
*  tbl	0008	04e2   tables
*  err	04E2	06E8   error messages and functions
*  hnd	06E8	09F7   poll handler
*  nmf	09F7	13F3   numerical fcns noparam/hyperb/log2/gamma/etc
*  bas	13F3	1839   base conv & utl - BSTR$, BVAL, NAN$, TYPE
*  com	1839	20FD   CONJ, COMPLEX, complex utilities, MAT execute, MAT ZER/CON/IDN
*  mio	20FD	2E7A   mtrx i/o - MAT DISP/PRINT/INPUT, complex image handlers
*  par     2E7A	32C6   MAT entry,parse&decomp
*  jp1	32C6	330C   jmp table 1
*  arf	330C	38E2   array fcns
*  mat	38E2	4434   mtrx algebra MAT A= (X), (-)B, B+/-C, B*C, TRN(B)*C
*  mut	4434	4E2F   mtrx utilities
*  mat5	4E2F 	5736   DET entry, INV execute
*  mat6	5736	5F9B   SYS execute
*  mat7	5F9B	6C75   SYS subs
*  mat8	6C75	70F9   SYS subs
*  jp2	70F9	712B   jmp table 2
*  cpr	712B	84CE   complex routines
*  fft	84CE	8CD2   FFT (FOUR)
*  fir	8CD2 	953D   FNROOT and INTEGRAL entries, routines
*  fnr	953D  	9C62   solver (FNROOT) execute
*  fnr2	9C62 	A415   solver subs
*  fnr3	A415	AFB8   solver subs
*  int	AFB8	BEDA   INTEGRAL execute
*  prt	BEDA 	D871   Polynomial rootfinder (PROOT)
*  sym	-	-      global symbols
*
* then go back to the other files according theier address
* like in bas.a below.
* the =Cslc5 is in the imds v3  pdf page 1266 (address 0A924)
* which refer to CSLC5 imds v3 page 2636 (address 1B435)
* you will find the entry address in the local file HP71ENTR.TXT
* entry points from here https://github.com/hp71b/areuh/blob/master/equ/hp71.ep
* or here http://www.jeffcalc.hp41.eu/emu71/files/hp71ep.txt
*
****************************************************************
* from MATH ROM bas.a
* https://www.jeffcalc.hp41.eu/emu71/mathrom.html
****************************************************************
*
* standalone entry point
* call to STSCR with saving of 1 RSTK level and D0
* math3: cd0ex rstk=c stscr c=rstk cd0ex
stscr  GOVLNG =STSCR
stscr+ C=RSTK                  not used here
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
* standalone entry point
* call to RCSCR with saving of 1 RSTK level and D0
* math3: cd0ex rstk=c rcscr cd0ex c=rstk cd0ex
rcscr  GOVLNG =RCSCR
rcscr+ C=RSTK                  not used here
       GOSUB  =cslc5
       CD0EX
       GOSUB  =cslc5
       R4=C
       GOSBVL =RCSCR
       RSTK=C                  save C[A] on RSTK
       CR4EX                   and the rest in R4
       GOSUB  =csrc5
       D0=C
       C=RSTK                  get back C[A]
       GOSUB  =csrc5
       RSTK=C
       GOSUB  =cslc5
       RSTK=C                  save again C[A]
       C=R4                    restore C from R4
       C=RSTK                  with C[A]
       RTN
*
****************************************************************
*
****************************************************************
* jump table
****************************************************************
*
infr15 GOVLNG =INFR15
fnpwds GOVLNG =FNPWDS          Weed out NaNs and Infs
stab1  GOVLNG =STAB1
rccd1  GOVLNG =RCCD1
ln15   GOVLNG =LN15
stcd2  GOVLNG =STCD2
exab2  GOVLNG =EXAB2
stab2  GOVLNG =STAB2           Store AB into scratch 2
exab1  GOVLNG =EXAB1
rccd2  GOVLNG =RCCD2           Recall CD from scratch 2
addone GOVLNG =ADDONE
subone GOVLNG =SUBONE
multf  GOVLNG =MULTF
addf   GOVLNG =ADDF
divf   GOVLNG =DIVF
xyex   GOVLNG =XYEX
cslc5  GOVLNG =CSLC5           not used here
csrc5  GOVLNG =CSRC5           not used here
*
****************************************************************
* load 0.5 in 15-form in (C,D)
****************************************************************
*
GET.5  C=0    W
       D=0    W
       P=     14
       LCHEX  #5
       CDEX   W
       C=-C-1 A
       RTN
*
****************************************************************
*
* standalone routine  ALREADY IN MAFO = dont transfer there
* recall the values of R0/R1 into A/B
* in additional to the existing =STAB1 =EXAB1 =RCCD1 =STAB2 
*   =EXAB2 =RCCD2 =STCD2
*
=RCAB1 A=R1
       B=A    W
       A=R0
       RTN
*
****************************************************************
* GAMMA
* GAMMA is in MATH ROM file nmf.a
* https://www.jeffcalc.hp41.eu/emu71/mathrom.html
* algorithm, doc based on 75 Math ROM source files (NMF,p21, src.p125):
*  1)  
*  2) if x is a positive integer,
*       then returns (x-1)!
*  3)
*  5) if 7<X<255 and non-integral, 
*       then returns gamma(x)
*  6) if -259<=x<7 and non-integral, 
*       then returns gamma(x+m)/(x+1)*(x+2)*...*(x+m)
*       where m=int(7-x)
****************************************************************
* issues:
*  ..
*
* solutions:
*  ..
* 
* test March 1 2025
* 5.0 GAMMA FS.
* T=  204.348449465 
* Z=  51.0539977268 
* Y=  1.00000000000 
* X=  24.0000000000 
* L=  5.00000000000 
*  OK { 0 } 
* 6.0 GAMMA FS.
* T=  51.0539977268 
* Z=  1.00000000000 
* Y=  24.0000000000 
* X=  120.000000000 
* L=  6.00000000000 
*  OK { 0 } 
* 5.3 GAMMA FS.
* T=  1.00000000000 
* Z=  24.0000000000 
* Y=  120.000000000 
* X=  38.0779764499 
* L=  5.30000000000 
*  OK { 0 } 
* https://www.symbolab.com/solver/gamma-function-calculator/%5CGamma%5Cleft(5.3%5Cright)?or=input
****************************************************************
*
       WORD 'GAMMA'
       GOSBVL =NUMST           X in A/B and in L.. was mpop1n before
gamma  GOSUB  dogam
*       GOTO   outres                                        old
       GOVLNG =PUTABX          A/B into X
*
dogam  XM=0
*       GOSUB  fnpwds           make sure no NaN/Inf           old
*         probably not necessary due to NUMST before
*       GONC   dogam1           ok, go on                      old
       SB=0
       GOTO   dogam1
*
       ?A=0   S 
       RTNYES
gnan2  A=0    S
       A=-A-1 S
gnan3  P=     0
       LCHEX  #0A              eGAM 'GAMMA=Inf' Error
       P=     14
       A=0    WP
       SETHEX
       A=A-1  XS               F indicator for NaN
       SETDEC
       B=0    W
       B=-B-1 M                mantissa=999..9
       P=     2
       LCHEX  #002             LC(2)  =LEXMTH
       P=     3
       RTNSXM                  Page 58 of Forth/ASM manual
dogam1 GOSUB  infr15           returns position of decimal in P
       ?P=    15               NaN or Inf?
       GOYES  dogam2
       ?B#0   WP               fract part is 0?
       GOYES  dogam3           no, go to non-integer processing
dogam2 ?B=0   M                zero?
       GOYES  gnan3
       ?A#0   S                negative?
       GOYES  gnan2
       GOSUB  subone
       GOVLNG =FCSTRT          computes fact(n-1) and returns
dogam3 C=A    A                non-integer path
       C=C+C  A
       GOC    findm            go there if negative exponent
       GOSUB  stab1            save X(A/B) in (R0,R1)
       A=0    S                sign=0
       C=0    W                load (C,D)=-270
       P=     13
       LCHEX  #27
       D=0    W
       D=D+1  A
       D=D+1  A
       D=-D-1 S
       CDEX   W
       GOSUB  addf             X-270
       ?A#0   S                negative? (X<270?)
       GOYES  dogam5           yes, go on
       GOSUB  exab1            load back X
       GOSBVL =CLRFRC
       A=0    A
       A=A+1  XS
       ASL    A
       ?A=0   S 
       GOYES  dogam4
       A=-A   A
       BSRB
       ?B=0   P 
       GOYES  dogam4  
       A=0    S
dogam4 B=0    W
       P=     14
       B=B+1  P
       RTN
dogam5 GOSUB  exab1            load back X
       ?A#0   S                negative?
       GOYES  findm            yes, then go
       ?A#0   A                >10 ?
       GOYES  dogam6           yes, use gamma(x) directly
       P=     14
       LCHEX  #7
       ?B<C   P                <7?
       GOYES  findm            yes, use gamma(x+m)
dogam6 GOSUB  gamsub           otherwise computes gamma
dogame XM=0
       GOVLNG =SETSB
*
* X is <7.
* findm calculates X+M where M=int(8-X).
* Let X=N+E where N is an integer < 6 inclusive and 0<E<1.
* Then X+M = N+E+INT(8-N-E) = N+E+7-N = 7+E so that
* 7<X+M<8
*
findm  GOSUB  stab1            save X
       A=-A-1 S                negate   -X
       C=0    W                load (C,D)=8
       P=     14
       LCHEX  #8
       D=0    W
       CDEX   W
       GOSUB  addf             8-X
       GOSBVL =CLRFRC          INT(8-X)
       GOSUB  stab2            save it in (R2,R3)
       GOSUB  rccd1            X
       GOSUB  addf
*       GOSUB  =stscr+          store it to scratch stack. uses R4
       GOSUB  =stscr           damage but shorter                 ok
       GOSUB  exab2            get back INT(8-X)
       BSL    W
findm2 BSLC                    right justified INT(8-X) in B[X]
       A=A-1  A                loop until done
       GONC   findm2
       ABEX   X
       A=A-1  X
       R3=A                    save counter in R3
       GOSUB  rccd1
       GOSUB  xyex
*
findm3 C=R3
       C=C-1  X
       R3=C
       GOC    findm4  
*       GOSUB  =stscr+          store it to scratch stack
       GOSUB  =stscr           damage C but shorter               ok
       GOSUB  rccd1
       GOSUB  xyex
       GOSUB  addone
       GOSUB  stab1
*       GOSUB  =rcscr+          recall from scratch stack
       GOSUB  =rcscr           shorter                            ok
       GOSUB  multf
       GOTO   findm3
*
findm4 GOSUB  =rcscr           shorter                            ok
*       GOSUB  =rcscr+          recall from scratch stack
*       GOSUB  =rcscr
       GOSUB  stcd2
*       GOSUB  =stscr+          store it to scratch stack
       GOSUB  =stscr           damages                            ok
       GOSUB  exab2
       GOSUB  gamsub           computes gamma
*       GOSUB  =rcscr+          recall from scratch stack
       GOSUB  =rcscr           shorter                            ok
       GOSUB  divf
       GOTO   dogame           exit
*
****************************************************************
* gamsub
* computes gamma(X). X>7
* algorithm, doc based on 75 Math ROM source files (NMF):
* gamma(z) is evaluated as
* EXP( (Z-0.5)*LN(Z) - Z + K )
*
*            Z
* K = A(0) + ---------------
*            P + A(1) - A(2)
*                       ---------------
*                       P + A(3) - A(4)
*                                  ---------------
*                                  P + A(5) - A(6)
*                                             --------
*                                             P + A(7)
* where P=12*Z^2
*       A(0)=.918938533204673
*       A(1)=.4
*       A(2)=1.21142857142857
*       A(3)=9.33584905660377
*       A(4)=76.5594818140208
*       A(5)=30.3479606073615
*       A(6)=495.920119017593
*       A(7)=60
****************************************************************
*
gamsub GOSUB  stab2            save X in scr2
       GOSUB  rccd2            recall X in (C,D)
       GOSUB  multf            X^2
       C=0    W                load (C,D) = 12
       P=     13
       LCHEX  #12
       D=0    W
       D=D+1  A
       CDEX   W
       GOSUB  multf            12*X^2  = P
       GOSUB  stab1            save P in scr1
       C=0    W                load (C,D)= A(7) = 60
       P=     14
       LCHEX  #6
       D=0    W
       D=D+1  A
       CDEX   W
       GOSUB  addf             P+A(7)
       P=     0                load (C,D)= -A(6)
       LCHEX  #0495920119017593
       D=0    W
       D=D+1  P
       D=D+1  P
       D=-D-1 S                negate
       CDEX   W
       GOSUB  xyex
       GOSUB  divf             -A(6)/(P+A(7))
       P=     0                load (C,D)= A(5)
       LCHEX  #0303479606073615
       D=0    W
       D=D+1  P
       CDEX   W
       GOSUB  addf             A(5)-A(6)/(P+A(7))
       GOSUB  rccd1            recall P in (C,D)
       GOSUB  addf             P+A(5)+A(6)/(P+A(7))
       P=     0                load (C,D)= -A(4)
       LCHEX  #0765594818140208
       D=0    W
       D=D+1  P
       D=-D-1 S                negate
       CDEX   W
       GOSUB  xyex
       GOSUB  divf             -A(4)/(P+A(5)+A(6)/(P+A(7)))
       P=     0                load (C,D)= A(3)
       LCHEX  #0933584905660377
       D=0    W
       CDEX   W
       GOSUB  addf             A(3)-A(4)/(P+A(5)+A(6)/(P+A(7)))
       GOSUB  rccd1            recall P in (C,D)
       GOSUB  addf             P+A(3)-A(4)/(P+A(5)+A(6)/(P+A(7)))
       P=     0                load (C,D)= -A(2)
       LCHEX  #0121142857142857
       D=0    W
       D=-D-1 S                negate
       CDEX   W
       GOSUB  xyex
       GOSUB  divf             -A(2)/(P+A(3)-A(4)/(P+A(5)+A(6)/(P+A(7))))
*       P=     0                                                 better?
       GOSUB  =GET.5           load (C,D)=0.5
       D=D-1  P                (C,D)=0.4 = A(1)
       GOSUB  addf             A(1)-A(2)/(P+A(3)-A(4)/(P+A(5)+A(6)/(P+A(7))))
       GOSUB  rccd1            recall P in (C,D)
       GOSUB  addf             P+A(1)-A(2)/(P+A(3)-A(4)/(P+A(5)+A(6)/(P+A(7))))
       GOSUB  rccd2            recall X in (C,D)
       GOSUB  xyex
       GOSUB  divf             X/(P+A(1)-A(2)/(P+A(3)-A(4)/(P+A(5)+A(6)/(P+A(7)))))
       P=     0                load (C,D)= A(0)
       LCHEX  #0918938533204673
       D=0    W
       D=-D-1 A
       CDEX   W
       GOSUB  addf             this is K
       GOSUB  rccd2            recall X in (C,D)
       C=-C-1 S                negate
       GOSUB  addf             -X+K
*       GOSUB  =stscr+         store it to math scratch stack
       GOSUB  =stscr           damages C                          ok
       GOSUB  rccd2            recall X in (C,D)
       GOSUB  xyex
       GOSUB  ln15             LN(X)
       GOSUB  exab2            (A,B)=X  (R2,R3)=LN(X)
       GOSUB  =GET.5           load (C,D)=0.5
       C=-C-1 S                negate
       GOSUB  addf             X-0.5
       GOSUB  rccd2            recall LN(X) in (C,D)
       GOSUB  multf            (X-0.5)*LN(X)
       GOSUB  =rcscr                                            still ok
*       GOSUB  =rcscr+         recall -X+K from math scratch stack
       GOSUB  addf             (X-0.5)*LN(X)-X+K
       GOVLNG =EXP15           GAMMA(X)=EXP((X-0.5)*LN(X)-X+K)  and return
       RTNCC
*
****************************************************************
*
       END
