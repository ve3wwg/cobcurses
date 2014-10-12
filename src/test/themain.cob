        IDENTIFICATION DIVISION.
        PROGRAM-ID. MAIN-PROG.

        ENVIRONMENT DIVISION.

        DATA DIVISION.

        WORKING-STORAGE SECTION.

            COPY COBCURSG.

        01  SCR-LOGIN-DATA.
            10  FLD-USERID              PIC X(8).
            10  FLD-PASSWORD            PIC X(16).
            10  FLD-GB                  PIC X(16).
            10  LOGIN-SUCCESS           PIC X.

        PROCEDURE DIVISION.

        MAIN-PROG.
            PERFORM NC-OPEN.
            CALL "SCR-LOGIN" USING NC-COBCURSES, SCR-LOGIN-DATA.
            PERFORM NC-CLOSE.
            DISPLAY "LOGIN SUCCESS =", LOGIN-SUCCESS.
            DISPLAY "MOUSE SUPPORT =", NC-MOUSE-SUPPORT.
            DISPLAY "MOUSE CLICK MS=", NC-MOUSE-CLICK-MS.
            STOP RUN.

            COPY COBCURSP.

        END PROGRAM MAIN-PROG.
