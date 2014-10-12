IDENTIFICATION DIVISION.
PROGRAM-ID. ED000010.
*>
*>  Main Splash Screen for E-Design (Launcher)
*>  Warren W. Gay
*>
*>  $Id: ed000010.cob,v 1.2 2007/10/31 14:56:29 ve3wwg Exp $
*>
*>  THIS SCREEN IS THE START OF THE E-DESIGN UTILITY.
*>  IT LAUNCHES THE MAIN MENU AFTER THE SPLASH SCREEN
*>  IS SHOWN.
*>
*>  THIS SOURCE MUST BE COMPILED WITH OPEN-COBOL'S -free
*>  OPTION FOR FREE-FORM SOURCE CODE.
*>
DATA DIVISION.
WORKING-STORAGE SECTION.

    COPY COBCATTR.
    COPY COBCCOLOUR.
    COPY COBCKEYS.
    COPY COBCURSG.                                      *> DEFINE COBCURSES GLOBALS
    COPY COBCURSL.                                      *> LOCAL STUFF FOR COBCURSQ PARAGRAPHS
    COPY ED000010-WS.                                   *> SPLASH SCREEN DEFNS

    01  WS-FLAGS.
        05  WS-SPLASH-FLAG                  PIC X.
            88  WS-SPLASH                   VALUE 'Y' FALSE IS 'N'.
        05  WS-DO-EXIT-FLAG                 PIC X.
            88  WS-DO-EXIT                  VALUE 'Y' FALSE IS 'N'.

    01  FILLER.
        05  WS-SELECTION                    PIC X(20).
        05  WS-SELECTION-LENGTH             PIC 9999.

    01  WS-SHELL                            PIC X(16).

PROCEDURE DIVISION.

MAIN.
    MOVE 'Y' TO NC-NO-RECOVERY.                         *> WHILE WE ARE DEBUGGING ONLY (DELETE ME)
    PERFORM 1000-INITIALIZE.
    PERFORM 5000-PROCESS.
    PERFORM 9000-FINALIZE.
    GOBACK.

1000-INITIALIZE.
    PERFORM NC-OPEN.                                    *> OPEN THE TERMINAL INTERFACE
    PERFORM NC-INIT.                                    *> INITIALIZE LOCALS
    COPY ED000010-PD.
    PERFORM NC-DRAW-SCREEN.                             *> PAINT SCREEN BACKGROUND
    SET WS-DO-EXIT TO FALSE.
    ACCEPT WS-SHELL FROM ENVIRONMENT "XTERM_SHELL".     *> SEE IF A USER PREFERENCE EXISTS
    IF WS-SHELL = SPACES THEN
        ACCEPT WS-SHELL FROM ENVIRONMENT "USER_SHELL"   *> ELSE GO WITH CONFIGURED DEFAULT
    END-IF.
    EXIT.

5000-PROCESS.
    PERFORM UNTIL WS-DO-EXIT
        MOVE "Press Enter to continue:" TO NC-MSGBUF    *> PAUSE FOR THE SPLASH SCREEN
        PERFORM NC-PUT-MESSAGE-CR
        PERFORM 5100-COMMAND-LOOP                       *> ISSUE THE MENU
    END-PERFORM.
    EXIT.

5100-COMMAND-LOOP.
    SET WS-SPLASH TO FALSE.
    PERFORM UNTIL WS-DO-EXIT OR WS-SPLASH
        MOVE LENGTH OF WS-SELECTION TO WS-SELECTION-LENGTH
        CALL "COBCURSES-SHOW-MENU" USING                *> POPUP THE MENU
            NC-COBCURSES, MENU-ED000010-STARTUP, WS-SELECTION, WS-SELECTION-LENGTH
        IF RETURN-CODE = ZERO THEN                      *> DID WE GET A SELECTION?
            EVALUATE WS-SELECTION                       *> YES, THEN DISPATCH
                WHEN " "
                    SET WS-SPLASH TO TRUE
                WHEN "!"                                *> OPEN A COMMAND SHELL
                    PERFORM NC-SUSPEND
                    CALL "SYSTEM" USING WS-SHELL
                    DISPLAY "DONE, PRESS CR: "
                    ACCEPT WS-SELECTION
                    PERFORM NC-RESUME
                WHEN "PR"                               *> PARALLEL RESISTANCE SCREEN
                    CALL "EDPR010" USING NC-COBCURSES
                WHEN "E"
                    SET WS-DO-EXIT TO TRUE
                WHEN OTHER                              *> EH? SHOULD NOT GET HERE
                    CONTINUE
            END-EVALUATE
        END-IF
    END-PERFORM.
    EXIT.

9000-FINALIZE.
    PERFORM NC-CLOSE.                                   *> CLOSE THE TERMINAL INTERFACE
    STOP RUN.

COPY COBCURSP.                                          *> THIS PROVIDES OPEN/CLOSE SUPPORT PARAGRAPHS
COPY COBCURSQ.                                          *> OTHER SCREEN STUFF
COPY NULLEVENTS.

END PROGRAM ED000010.

*> $Source: /cvsroot/cobcurses/cobcurses/src/edesign/ed000010.cob,v $
