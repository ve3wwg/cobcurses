        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-CURSES.

        ENVIRONMENT DIVISION.

        DATA DIVISION.

        WORKING-STORAGE SECTION.

            COPY COBCURSG.

            01  WORK-VARS.
                10  WS-PAIRS                PIC 999.
                10  WS-MS                   PIC 9(9).

        PROCEDURE DIVISION.

        MAIN-PROG.
            PERFORM NC-OPEN.
            PERFORM NC-CLOSE.

            DISPLAY "COBCURSES REPORTS:".
            DISPLAY "  COLUMNS.........", NC-COLUMNS.
            DISPLAY "  LINES...........", NC-LINES.
            DISPLAY "  HAS-COLOUR......", NC-CAP-COLOUR.
            DISPLAY "  CHG-COLOUR......", NC-CHG-COLOUR.
            MOVE NC-COLOUR-PAIRS TO WS-PAIRS.
            DISPLAY "  COLOUR-PAIRS....", WS-PAIRS.
            DISPLAY "  HAS-UNDERLINE...", NC-HAS-UNDERLINE-FLAG.
            DISPLAY "  MOUSE-SUPPORT...", NC-MOUSE-SUPPORT.
            MOVE NC-MOUSE-CLICK-MS TO WS-MS.
            DISPLAY "  MOUSE-CLICK-MS..", WS-MS.

            STOP RUN.

            COPY COBCURSP.

        END PROGRAM TEST-CURSES.
