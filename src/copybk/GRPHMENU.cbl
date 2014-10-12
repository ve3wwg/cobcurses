*>******************************************************************************************
*>
*>  BUILTIN MENU TO ALLOW CHOOSING AND DRAWING OF GRAPHIC CHARACTERS
*>
*>  THIS DEFINITION IS USING FREE FORMAT COBOL (Compile option -free)
*>
*>  Warren W. Gay
*>******************************************************************************************
01  GRAPHICS-CHAR-MENU.
    05  PIC X(16) VALUE "GRAPHICS-CHAR".                        *> THIS IS EFFECTIVELY IGNORED
    05  PIC 99 VALUE 01.                                        *> MENU TYPE 01 (STATIC)
    05  PIC 9(03) VALUE 029.                                    *> COLUMN #
    05  PIC 9(03) VALUE 013.                                    *> LINE #
    05  PIC X VALUE "Y".                                        *> OPTION ONEVALUE
    05  PIC X VALUE "Y".                                        *> OPTION ROWMAJOR
    05  PIC X VALUE "Y".                                        *> OPTION IGNORECASE
    05  PIC X VALUE "N".                                        *> OPTION SHOWDESC
    05  PIC X VALUE "N".                                        *> OPTION NONCYCLIC
    05  PIC X VALUE "Y".                                        *> OPTION SHOWMATCH
    05  PIC 99 VALUE 11.                                        *> OPTION FORMAT ROWS
    05  PIC 99 VALUE 04.                                        *> OPTION FORMAT COLS
    05  PIC 9(02) VALUE 08.                                     *> MENU TITLE LENGTH
    05  PIC X(08) VALUE "Graphics".                             *> MENU TITLE
    05  FILLER.
        10  PIC X(01) VALUE "Y".                                *> ITEM IS SELECTABLE
        10  PIC 99    VALUE 8.                                  *> ITEM NAME LENGTH
        10  PIC X(08) VALUE "ULCORNER".                         *> ITEM NAME
        10  PIC 99    VALUE 21.                                 *> ITEM DESCRIPTION LENGTH
        10  PIC X     VALUE X'01'.                              *> START OF ITEM DESCRIPTION (GRAPHIC)
        10  PIC X(20) VALUE " (Upper left corner)".             *> REMAINDER OF ITEM DESCRIPTION
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 8.
        10  PIC X(08) VALUE "LLCORNER".
        10  PIC 99    VALUE 21.
        10  PIC X     VALUE X'02'.
        10  PIC X(20) VALUE " (Lower left corner)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 8.
        10  PIC X(08) VALUE "URCORNER".      
        10  PIC 99    VALUE 22.
        10  PIC X     VALUE X'03'.
        10  PIC X(21) VALUE " (Upper right corner)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 8.
        10  PIC X(08) VALUE "LRCORNER".
        10  PIC 99    VALUE 22.
        10  PIC X     VALUE X'04'.
        10  PIC X(21) VALUE " (Lower right corner)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 4.
        10  PIC X(04) VALUE "LTEE".       
        10  PIC 99    VALUE 22.
        10  PIC X     VALUE X'05'.
        10  PIC X(21) VALUE " (Tee pointing right)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 4.
        10  PIC X(04) VALUE "RTEE".          
        10  PIC 99    VALUE 21.
        10  PIC X     VALUE X'06'.
        10  PIC X(20) VALUE " (Tee pointing left)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 4.
        10  PIC X(04) VALUE "BTEE".          
        10  PIC 99    VALUE 19.
        10  PIC X     VALUE X'07'.
        10  PIC X(18) VALUE " (Tee pointing up)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 4.
        10  PIC X(04) VALUE "TTEE".          
        10  PIC 99    VALUE 21.
        10  PIC X     VALUE X'08'.
        10  PIC X(20) VALUE " (Tee pointing down)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 5.
        10  PIC X(05) VALUE "HLINE".         
        10  PIC 99    VALUE 19.
        10  PIC X     VALUE X'09'.
        10  PIC X(18) VALUE " (Horizontal line)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 5.
        10  PIC X(05) VALUE "VLINE".         
        10  PIC 99    VALUE 17.
        10  PIC X     VALUE X'0A'.
        10  PIC X(16) VALUE " (Vertical line)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 4.
        10  PIC X(04) VALUE "PLUS".          
        10  PIC 99    VALUE 27.
        10  PIC X     VALUE X'0B'.
        10  PIC X(26) VALUE " (Large plus or crossover)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 2.
        10  PIC X(02) VALUE "S1".          	
        10  PIC 99    VALUE 15.
        10  PIC X     VALUE X'0C'.
        10  PIC X(14) VALUE " (Scan line 1)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 2.
        10  PIC X(02) VALUE "S9".            
        10  PIC 99    VALUE 16.
        10  PIC X     VALUE X'0D'.
        10  PIC X(15) VALUE " (Scan line 9)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 7.
        10  PIC X(07) VALUE "DIAMOND".       
        10  PIC 99    VALUE 11.
        10  PIC X     VALUE X'0F'.
        10  PIC X(10) VALUE " (Diamond)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 7.
        10  PIC X(07) VALUE "CKBOARD".       
        10  PIC 99    VALUE 27.
        10  PIC X     VALUE X'10'.
        10  PIC X(26) VALUE " (Checker board (stipple))".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 6.
        10  PIC X(06) VALUE "DEGREE".        
        10  PIC 99    VALUE 17.
        10  PIC X     VALUE X'11'.
        10  PIC X(16) VALUE " (Degree symbol)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 7.
        10  PIC X(07) VALUE "PLMINUS".       
        10  PIC 99    VALUE 14.
        10  PIC X     VALUE X'12'.
        10  PIC X(13) VALUE " (Plus/minus)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 6.
        10  PIC X(06) VALUE "BULLET".        
        10  PIC 99    VALUE 10.
        10  PIC X     VALUE X'13'.
        10  PIC X(09) VALUE " (Bullet)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 6.
        10  PIC X(06) VALUE "LARROW".        
        10  PIC 99    VALUE 23.
        10  PIC X     VALUE X'14'.
        10  PIC X(22) VALUE " (Arrow pointing left)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 6.
        10  PIC X(06) VALUE "RARROW".        
        10  PIC 99    VALUE 24.
        10  PIC X     VALUE X'15'.
        10  PIC X(23) VALUE " (Arrow pointing right)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 6.
        10  PIC X(06) VALUE "DARROW".        
        10  PIC 99    VALUE 23.
        10  PIC X     VALUE X'16'.
        10  PIC X(22) VALUE " (Arrow pointing down)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 6.
        10  PIC X(06) VALUE "UARROW".        
        10  PIC 99    VALUE 21.
        10  PIC X     VALUE X'17'.
        10  PIC X(20) VALUE " (Arrow pointing up)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 5.
        10  PIC X(05) VALUE "BOARD".         
        10  PIC 99    VALUE 20.
        10  PIC X     VALUE X'18'.
        10  PIC X(19) VALUE " (Board of squares)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 7.
        10  PIC X(07) VALUE "LANTERN".       
        10  PIC 99    VALUE 18.
        10  PIC X     VALUE X'19'.
        10  PIC X(17) VALUE " (Lantern symbol)".
    05  FILLER.
        10  PIC X(01) VALUE "Y".
        10  PIC 99    VALUE 5.
        10  PIC X(05) VALUE "BLOCK".         
        10  PIC 99    VALUE 22.
        10  PIC X     VALUE X'1A'.
        10  PIC X(21) VALUE " (Solid square block)".
    05  FILLER.
        10  PIC X(01) VALUE 'X'.                                *> END OF MENU DEFINITION

*>******************************************************************************************
*>
*>  END OF COPY BOOK GRPHMENU
*>
*>  $Source: /cvsroot/cobcurses/cobcurses/src/copybk/GRPHMENU.cbl,v $
*>******************************************************************************************

