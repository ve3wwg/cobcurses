*>********************************************************************************
*>  MENU REFERENCES RECORD :
*>
*>  FILE:
*>      ${COBCURSES_DATADIR}/MENUREFS.X
*>
*>********************************************************************************
*>
*>  THIS FILE LISTS SCREEN PROGRAMS THAT WISH TO HAVE CERTAIN MENU'S GENERATED
*>  THAT ARE NOT REFERENCED BY SCREEN FIELDS.
*>

*>  01  MENUREF-RECORD.
        05  MREF-KEY.
            10  MREF-SCREEN-NAME            PIC X(16).
            10  MREF-MENU-NAME              PIC X(16).
        05  MREF-DATA.
            10  FILLER                      PIC X(16).

*>********************************************************************************
*>  END MENU REFERENCES RECORD.
*>********************************************************************************


