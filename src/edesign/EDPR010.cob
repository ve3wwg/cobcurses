IDENTIFICATION DIVISION.
PROGRAM-ID. EDPR010.
*>
*>  Parallel Resistance Calculator
*>  Warren W. Gay
*>
*>  $Id: EDPR010.cob,v 1.1 2007/10/31 03:05:42 ve3wwg Exp $
*>
*>  THIS SOURCE MUST BE COMPILED WITH OPEN-COBOL'S -free
*>  OPTION FOR FREE-FORM SOURCE CODE.
*>
DATA DIVISION.
WORKING-STORAGE SECTION.

    COPY COBCATTR.
    COPY COBCCOLOUR.
    COPY COBCKEYS.
    COPY COBCURSL.                                      *> LOCAL STUFF FOR COBCURSQ PARAGRAPHS
    COPY EDPR010-WS.                                    *> SCREEN LOCALS

LINKAGE SECTION.
    COPY COBCURSG.                                      *> DEFINE COBCURSES GLOBALS

PROCEDURE DIVISION USING NC-COBCURSES.

MAIN.
    PERFORM 1000-INITIALIZE.
    PERFORM 5000-PROCESS.
    PERFORM 9000-FINALIZE.
    GOBACK.

1000-INITIALIZE.
    PERFORM NC-INIT.                                    *> INITIALIZE LOCALS
    COPY EDPR010-PD.
    PERFORM NC-DRAW-SCREEN.                             *> PAINT SCREEN BACKGROUND
    EXIT.

5000-PROCESS.
    PERFORM NC-PAUSE.
    EXIT.

9000-FINALIZE.
    EXIT.

COPY COBCURSQ.                                          *> OTHER SCREEN STUFF
COPY NULLEVENTS.

END PROGRAM EDPR010.

*> $Source: /cvsroot/cobcurses/cobcurses/src/edesign/EDPR010.cob,v $
