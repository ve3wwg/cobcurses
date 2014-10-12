        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST008.
      *> 
      *> THIS PROGRAM TESTS CALL "COBCURSES-ECVT-COMP-2"
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

        01  WS-TEST-X                       PIC 999 COMP.
        01  WS-COMP-2                       COMP-2.
        01  WS-TEST-DATA.
            05  WS-DATA-INIT.
                10  FILLER                  COMP-2 VALUE 2.3.
                10  FILLER                  COMP-2 VALUE 34.5.
                10  FILLER                  COMP-2 VALUE 456.07.
                10  FILLER                  COMP-2 VALUE 1258.9.
                10  FILLER                  COMP-2 VALUE 0.9.
                10  FILLER                  COMP-2 VALUE 0.42.
                10  FILLER                  COMP-2 VALUE 0.062.
                10  FILLER                  COMP-2 VALUE 0.00782.
                10  FILLER                  COMP-2 VALUE 0.000461.
            05  WS-TEST-ENTRY REDEFINES WS-DATA-INIT
                OCCURS 1 TO 9 TIMES.
                10  WS-TEST-VALUE           COMP-2.  

        01  WS-BUFFER                       PIC X(13).
        01  WS-BUFLEN                       PIC 99.
        01  WS-EXPONENT                     PIC S999.
        01  WS-SB-RC                        PIC S9(9).
        01  WS-FAILED-COUNT                 PIC 9999 VALUE 0.

        PROCEDURE DIVISION.
        MAIN-PROG.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            STOP RUN.

      *> 
      *> MAIN PROCESSING LOOP
      *> 
        5000-PROCESS.
            PERFORM 6100-TEST-1.
            PERFORM 6300-TEST-3.
            EXIT.

        5100-TEST.
            DISPLAY "SUB-TEST # ", WS-TEST-X, " :".
            MOVE LENGTH OF WS-BUFFER TO WS-BUFLEN
            CALL "COBCURSES-ECVT-COMP-2"
              USING
                WS-COMP-2,
                BY REFERENCE WS-BUFFER,
                WS-BUFLEN,
                BY REFERENCE WS-EXPONENT.
            PERFORM 5300-DISPLAY-RESULTS-OK.
            EXIT.

        5200-RETURN-CODE.
            DISPLAY "  RETURN-CODE= ", RETURN-CODE.
            IF RETURN-CODE NOT = WS-SB-RC THEN
                ADD 1 TO WS-FAILED-COUNT
                DISPLAY "  *** FAILED ***"
            ELSE
                DISPLAY "  --- CORRECT ---"
            END-IF.
            EXIT.

        5200-RETURN-CODE-OK.
            MOVE NC-RET-OK TO WS-SB-RC.
            PERFORM 5200-RETURN-CODE.
            EXIT.

        5300-DISPLAY-RESULTS.
            IF RETURN-CODE = NC-RET-OK THEN
                DISPLAY "  WS-BUFFER   = ", WS-BUFFER, ";"
                DISPLAY "  WS-EXPONENT = ", WS-EXPONENT
            ELSE
                DISPLAY "  *UNDEFINED RESULTS DUE TO RC."
            END-IF.
            EXIT.

        5300-DISPLAY-RESULTS-OK.
            PERFORM 5200-RETURN-CODE-OK.
            PERFORM 5300-DISPLAY-RESULTS.
            EXIT.

        6100-TEST-1.
            DISPLAY "*** ECVT-COMP-2 TESTS ***".

            PERFORM VARYING WS-TEST-X FROM 1 BY 1
              UNTIL WS-TEST-X > 9
                MOVE WS-TEST-VALUE(WS-TEST-X) TO WS-COMP-2
                PERFORM 5100-TEST
            END-PERFORM.
            PERFORM 8000-END-TEST.
            EXIT.

        6300-TEST-3.
            DISPLAY "*** OMITTED PARAMETER TESTS ***".
            PERFORM 7100-OMITTED-1.
            PERFORM 7200-OMITTED-2.
            PERFORM 7400-OMITTED-4.
            EXIT.        

        7100-OMITTED-1.
            DISPLAY "  -- OMITTED WS-BUFFER => RET-BADPARM --".
            
            CALL "COBCURSES-ECVT-COMP-2"
              USING
                WS-COMP-2,
                BY REFERENCE OMITTED,
                WS-BUFLEN,
                BY REFERENCE WS-EXPONENT.
            MOVE NC-RET-BADPARM TO WS-SB-RC.
            PERFORM 5200-RETURN-CODE.
            PERFORM 8000-END-TEST.
            EXIT.

        7200-OMITTED-2.
            DISPLAY "  -- OMITTED WS-BUFLEN => RET-OK --".
            DISPLAY "  SHOULD ASSUME LAST BUFFER LENGTH USED.".

            MOVE 326.05 TO WS-COMP-2.
            INITIALIZE WS-BUFFER.

            CALL "COBCURSES-ECVT-COMP-2"
              USING
                WS-COMP-2,
                BY REFERENCE WS-BUFFER,
                OMITTED,
                BY REFERENCE WS-EXPONENT.
            PERFORM 5300-DISPLAY-RESULTS-OK.
            PERFORM 8000-END-TEST.
            EXIT.

        7400-OMITTED-4.
            DISPLAY "  -- OMITTED WS-EXPONENT => RET-OK --".

            MOVE 3700.5 TO WS-COMP-2.
            INITIALIZE WS-BUFFER.
            MOVE ZERO TO WS-EXPONENT.

            CALL "COBCURSES-ECVT-COMP-2"
              USING
                WS-COMP-2,
                BY REFERENCE WS-BUFFER,
                OMITTED,
                BY REFERENCE OMITTED.
            PERFORM 5300-DISPLAY-RESULTS-OK.
            IF WS-EXPONENT NOT = ZERO THEN
                DISPLAY "  *** INCORRECT EXPONENT ***"
                ADD 1 TO WS-FAILED-COUNT
            END-IF.
            PERFORM 8000-END-TEST.
            EXIT.

        8000-END-TEST.
            DISPLAY " ".
            EXIT.

      *> 
      *> PROGRAM CLEANUP
      *> 
        9000-FINALIZE.
            DISPLAY "FAILED TESTS: ", WS-FAILED-COUNT.
            IF WS-FAILED-COUNT > 0 THEN
                MOVE 1 TO RETURN-CODE
            ELSE
                MOVE ZERO TO RETURN-CODE
            END-IF.
            EXIT.

        END PROGRAM TEST008.
