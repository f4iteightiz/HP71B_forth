( both lines below are only for gforth pc use                  )
S" 71B2G.fth" INCLUDED
S" H71B1.fth" INCLUDED
S" HP41.fth" INCLUDED
( ) 
( start with gforth LIN.fth                                    )
( leave gforth with the word "bye" and return key              ) 
( )
( ******************** lets print LIN ************************ )
( SF 15 )
( CF 16 )
( XEQ PRP ALPHA LIN ALPHA )
( )
( )
( ******************** word LIN ****************************** )
( )
( MicroRevue Nr 8 page 20.. )
( Copyright Pascal DAGORNET )
( )
(  01*LBL "LIN" )
( "RANG?" PROMPT FIX 0 )
( SF 29 STO 00 STO 07 )
( STO Y  E + STO 05 * )
( 9 + PSIZE 9.9 RCL 00 )
( E3 /  E + RCL X )
( )
( 23*LBL 00 )
( "A" ARCL Y ARCL X .. )
( / + RCL [  E6 / + )
( SF 25 REGSWAP FS? 25 )
( GTO 06 "NR" PROMPT )
( )
( 163*LBL 03 )
( "`=?" PROMPT STO IND T )
( RDN ISG Z END )
( )
( ******************** lets print LIN ************************ )
( CF 15 )
( CF 16 )
( XEQ PRP ALPHA LIN ALPHA )
( )
( )
( ******************** word LIN ****************************** )
( )
( MicroRevue Nr 8 page 20.. )
( Copyright Pascal DAGORNET )
( )
( .. [ )
FVARIABLE M
( .. \ )
FVARIABLE N
( .. ] )
FVARIABLE O
( 01*LBL "LIN" )
: LIN
( )
( 02 "RANG?" )
CR ." RANG?" 
( )
( 03 PROMPT )
EXPECT96
( )
( 04 FIX 0 )
0 FIX VAL ITOF
( )
( 05 SF 29 ) 
( 3digit separator or not )
( )
( 06 STO 00 )
0 REG41 STO
( )
( 07 STO 07 )
7 REG41 STO
( )
( 08 STO Y )
Y STO
( )
( 09  E )
1.0E0
( )
( 10 + )
F+
( )
( 11 STO 05 )
5 REG41 STO
( )
( 12 * )
F*
( )
( 13 9 )
9.0E0
( )
( 14 + )
F+
FTOI
( )
( 15 PSIZE )                                    ( silent label )
SIZE? REG41
<
IF
( )
( 16 9.9 )
9.9E0
( )
( 17 RCL 00 )
0 REG41
( )
( 18  E3 )
1000.0E0
( )
( 19 / )
F/
( )
( 20 E )
1.0E0
( )
( 21 + )
F+
( )
( 22 RCL X )
X RCL
( )
( 23*LBL 00 )          ( 00 erl )
BEGIN BEGIN
( )
( 24 "A" )
" A" ALPHA41 S!
( )
( 25 ARCL Y )
ALPHA41 X<>Y FSTR$ S<& X<>Y 
( )
( 26 ARCL X )
FSTR$ S<&
( )
( 27 XEQ 03 )
( )
( )
( from LBL 03 )
( )
( 164 "`=?" )
"  =?" S<&
( )
( 165 PROMPT )
TYPE EXPECT96 ??????
VAL
( )
( 166 STO IND T )
T IND REG41 STO
( )
( 167 RDN )
RDN
( )
( 168 ISG Z )
( )
DROP ( nobody cares if 0 or -1 )
( )
( )
( 28 ISG X )
X ISG
( )
( 29 GTO 00 )          ( 00 erl )
NOT UNTIL
( )
( 30 "C" )
S" C"
PAD 1 ALPHA41 S!
( )
( 31 ARCL Y )
ALPHA41 X<>Y FSTR$ X<>Y S<&
( )
( 32 XEQ 03 )
( )
( )
( from LBL 03 )
( 164 "`=?" )
S"  =?" ALPHA41 S<&
( )
( 165 PROMPT )
TYPE EXPECT96 ??????
( )
( 166 STO IND T )
T IND REG41 STO
( )
( 167 RDN )
RDN
( )
( 168 ISG Z )
Z ISG
( )
DROP ( nobody cares if 0 or -1 )
( )
( )
( 33 FRC )
FRC
( )
( 34 E )
1.0E0
( )
( 35 + )
F+
( )
( 36 ISG Y )
Y ISG
( )
( 37 GTO 00 )          ( 00 erl )
NOT UNTIL ( will leave here if true )
( )
( 38 9 )
9.0E0
( )
( 39 STO 08 )
8 REG41 STO
( )
( 40 RCL 00 )
0 REG41 RCL
( )
( 41 + )
F+
( )
( 42 STO 04 )
4 REG41 STO
( )
( 43 CLA )
PAGE
( )
( 44 RCL 05 )
5 REG41 RCL
( )
( 45 STO [ )
M STO
( )
( 46*LBL 11 )                                  ( 11 )
( )
( 47 SF 02 )
( )
( 48 ISG \ )
N ISG
( )
( 49*LBL 06 )                        ( 06 )
( )
( 50 RCL 04 )
4 REG41 RCL
( )
( 51 RCL 07 )
7 REG41 RCL
( )
( 52 - )
F-
( )
( 53 RCL 04 )
4 REG41 RCL
( )
( 54  E3 )
1000.0E0
( )
( 55 / )
F/
( )
( 56 + )
F+
( )
( 57 RCL IND X )
X IND REG41 RCL
( )
( 58 X=0? )
X#0?
IF 
( )
( 59 GTO 05 )                      ( 05 )
( )
( 60*LBL 12 )                                             ( 12 erl )
BEGIN 
( )
( 61 ST/ IND Y )
Y IND REG41 ST/
( )
( 62 ISG Y )
Y ISG
( )
( 63 GTO 12 )                                             ( 12 erl )
NOT UNTIL
( )
( 64 RCL 08 )
8 REG41 RCL
( )
( 65 STO 02 )
2 REG41 STO
( )
( 66 RCL 00 )
0 RCL
( )
( 67 STO 06 )
6 REG41 STO
( )
( 68*LBL 13 )        ( 13 erl )
BEGIN
( )
( 69 RCL 04 )
4 REG41 RCL
( )
( 70 STO 01 )
1 REG41 STO
( )
( 71 RCL 07 )
7 REG41 RCL
( )
( 72 STO 05 )
5 REG41 STO
( )
( 73 RCL 02 )
2 REG41 RCL
( )
( 74 + )
F+
( )
( 75 STO 03 )
3 REG41 STO
( )
( 76 RCL 01 )
1 REG41 RCL
( )
( 77 X=Y? )
X#Y?
( )
( 78 GTO 15 )                               ( 15 erl )
IF
( )
( 79 RCL IND 02 )
2 IND REG41 RCL
( )
( 80 X=0? )
X#0?
IF
( )
( 81 GTO 15 )                               ( 15 erl )
( )
( 82*LBL 14 )                ( 14 erl )
BEGIN
( )
( 83 RCL IND 01 )
1 IND REG41 RCL
( )
( 84 RCL IND 02 )
2 IND REG41 RCL
( )
( 85 * )
F*
( )
( 86 ST- IND 03 )
3 IND REG41 ST-
( )
( 87 E )
1.0E0
( )
( 88 ST- 01 )
1 REG41 ST-
( )
( 89 ST- 03 )
3 REG41 ST-
( )
( 90 DSE 05 )
5 REG41 DSE NOT
( )
( 91 GTO 14 )                ( 14 erl )
UNTIL
( )
( 92*LBL 15 )                               ( 15 erl )
THEN THEN
( )
( 93 RCL [ )
M RCL
( )
( 94 ST+ 02 )
2 REG41 ST+
( )
( 95 DSE 06 )
6 REG41 DSE
( )
( 96 GTO 13 )        ( 13 erl )
NOT UNTIL
( )
( 97 ST+ 04 )
4 REG41 ST+
( )
( 98  E )
1.0E0
( )
( 99 ST+ 08 )
8 REG41 ST+
( )
( 100 DSE 07 )
7 REG41 DSE
( )
( 101 GTO 11 )                                  ( 11 )
( )
( 102*LBL 02 )    ( 02 erl )
BEGIN
( )
( 103 RCL 00 )
0 REG41 RCL
( )
( 104 E )
1.0E0
( )
( 105 + )
F+
( )
( 106  E5 )
100000.0E0
( )
( 107 / )
F/
( )
( 108 9.9 )
9.9E0
( )
( 109 + )
F+
( )
( 110 RCL 00 )
( )
( 111 + )
F+
( )
( 112 LASTX )
LASTX
( )
( 113  E3 )
1000.0E0
( )
( 114 / )
F/
( )
( 115  E )
1.0E0
( )
( 116 + )
F+
( )
( 117 CF 02 )
( )
( 118*LBL 04 )       ( 04 erl )
BEGIN
( )
( 119 "X" )
S" X"
( )
( 120 FIX 0 )
0 FIX
( )
( 121 ARCL X )
X ARCL ALPHA41
( )
( 122 "`=" )
S" = "
( )
( 123 FIX 4 )
4 FIX
( )
( 124 ARCL IND Y )
Y IND REG41 ARCL ALPHA41
( )
( 125 PROMPT )
ACCEPT96 VAL 
( )
( 126 ISG Y )
Y ISG
( )
0=?
IF 
( 127 ISG X )
X ISG
( )
( 128 GTO 04 )       ( 04 erl )
NOT UNTIL
( )
( 129 GTO 02 )    ( 02 erl )
NOT UNTIL
( )
( 130*LBL 05 )                      ( 05 )
( )
( 131 FC?C 02 )
( )
( 132 GTO 08 )             ( 08 )
( )
( 133 0 )
0.0E0
( )
( 134 STO ] )
O RCL
( )
( 135*LBL 08 )             ( 08 )
( )
( 136 ISG ] )
O ISG
( )
( 137 "" )
( )
( 138 RCL \ )
N RCL
( )
( 139  E )
1.0E0
( )
( 140 - )
F-
( )
( 141 RCL [ )
M RCL
( )
( 142 * )
F*
( )
( 143 9 )
9.0E0
( )
( 144 + )
F+
( )
( 145 STO Y )
Y STO
( )
( 146 RCL [ )
M RCL
( )
( 147 RCL ] )
O RCL
( )
( 148 * )
F*
( )
( 149 + )
F+
( )
( 150  E3 )
1000.0E0
( )
( 151 / )
F/
( )
( 152 + )
F+
( )
( 153 RCL [ )
( )
( 154 E6 )
1000000.0E0
( )
( 155 / )
F/
( )
( 156 + )
F+
( )
( 157 SF 25 )
( )
( check if regswap can be done... else abort.. sf25 could be difficult here )
( 158 REGSWAP )
REGSWAP REG41
( )
( 159 FS? 25 )
( )
( 160 GTO 06 )                        ( 06 )
( )
( 161 "NR" )
ABORT" No solution"
( )
( 162 PROMPT )
( )
( 163*LBL 03 )
( 164 "`=?" )
( 165 PROMPT )
( 166 STO IND T )
( 167 RDN )
( 168 ISG Z )
( )
( from LBL00 )
THEN THEN                                       ( silent label )
( 169 END )
;
( END OF FILE )
