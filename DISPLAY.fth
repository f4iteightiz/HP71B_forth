\ ***************************** Basics *************************
\
( both lines below are only for gforth pc use                  )
( when it is used with the command gforth DISPLAY.fth          )
( leave gfoth with the word "bye" and return key               ) 
S" 71B2G.fth" INCLUDED
S" H71B1.fth" INCLUDED
\ 
\ start with
\   gforth DISPLAY.fth
\
\ ****************** word FV. *********************
\
\ view the Float content of a variable
\ no move of the float stack
\
\ Execution/Inputs: a FVARIABLE
\ Outputs: the value at the screen
\ use: FVAR1 FV.
\
\ Modules used
\   FORTH/ASSEMBLER
\
\ under CC BY SA CreativeCommons 4.0
\
\ change log
\   date 2024 07 22 creation
\
: FV. DUP X<> ."  " F. X<> ; 
\
\ ****************** word FS. *********************
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
\   date 2024 07 10 creation
\   date 2024 07 12 released
\                                                  
: FS. CR RUP ." T= " F. 
    RUP CR ." Z= " F. 
    RUP CR ." Y= " F. 
    RUP CR ." X= " F. 
    CR X<>L ." L= " F. X<>L CR ; 
\
\ *************************  NEXT  *********************
\
