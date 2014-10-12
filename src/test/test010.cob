        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST010.
      *>
      *> THIS PROGRAM TESTS COBCURSES-INIT-PATHNAME
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

        01  WS-BEFORE-IMAGES.
            10  WS-BEFORE-CASE-1            PIC X(80)
                VALUE "${TEST_CASE_1}/$WHATEVER$".
            10  WS-BEFORE-CASE-2            PIC X(80)
                VALUE "${TEST_CASE_2}/${TEST_CASE_1}/END".
            10  WS-BEFORE-CASE-3            PIC X(30)
                VALUE "${TEST_CASE_2}/TRUNCATES".

        77  WS-SB-RC                        PIC S9(9).
        77  WS-FAIL-COUNT                   PIC 9999.

        01  WS-LENGTHS.
            10  WS-LENGTHS-1-2              PIC 9999.
            10  WS-LENGTH-3                 PIC 9999.

        01  WS-AFTER-IMAGES.
            10  WS-CASE-1                   PIC X(80).
            10  WS-CASE-2                   PIC X(80).
            10  WS-CASE-3                   PIC X(30).

        PROCEDURE DIVISION.
            PERFORM 1000-INITIALIZE.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            GOBACK.

        1000-INITIALIZE.
            MOVE WS-BEFORE-IMAGES TO WS-AFTER-IMAGES.
            MOVE LENGTH OF WS-CASE-1 TO WS-LENGTHS-1-2.
            MOVE LENGTH OF WS-CASE-3 TO WS-LENGTH-3.
            EXIT.

        5000-PROCESS.
            PERFORM 6000-CASE-0.
            PERFORM 6050-CASE-1.
            PERFORM 6100-CASE-2.
            PERFORM 6200-CASE-3.
            EXIT.

        6000-CASE-0.
            DISPLAY "*** TEST CASE 0 ***".
            DISPLAY "  NO PRIOR LENGTH GIVEN AND",
                " LENGTH IS OMITTED.."
            MOVE NC-RET-BADPARM TO WS-SB-RC.
            CALL "COBCURSES-INIT-PATHNAME"
              USING
                BY REFERENCE WS-CASE-1,
                OMITTED.
            PERFORM 7000-RETURN-CODE.
            DISPLAY " ".
            EXIT.

        6050-CASE-1.
            DISPLAY "*** TEST CASE 1 ***".
            MOVE NC-RET-OK TO WS-SB-RC.
            CALL "COBCURSES-INIT-PATHNAME"
              USING
                BY REFERENCE WS-CASE-1,
                WS-LENGTHS-1-2.
            PERFORM 7000-RETURN-CODE.
            DISPLAY "BEFORE '", WS-BEFORE-CASE-1, "'".
            DISPLAY "AFTER  '", WS-CASE-1, "'".
            DISPLAY " ".
            EXIT.

        6100-CASE-2.
            DISPLAY "*** TEST CASE 2 ***".
            MOVE NC-RET-OK TO WS-SB-RC.
            CALL "COBCURSES-INIT-PATHNAME"
              USING
                BY REFERENCE WS-CASE-2,
                OMITTED.
            PERFORM 7000-RETURN-CODE.
            DISPLAY "BEFORE '", WS-BEFORE-CASE-2, "'".
            DISPLAY "AFTER  '", WS-CASE-2, "'".
            DISPLAY " ".
            EXIT.

        6200-CASE-3.
            DISPLAY "*** TEST CASE 3 ***".
            MOVE NC-RET-TRUNCATED TO WS-SB-RC.
            CALL "COBCURSES-INIT-PATHNAME"
              USING
                BY REFERENCE WS-CASE-3,
                WS-LENGTH-3.
            PERFORM 7000-RETURN-CODE.
            DISPLAY "BEFORE '", WS-BEFORE-CASE-3, "'".
            DISPLAY "AFTER  '", WS-CASE-3, "'".
            DISPLAY " ".
            EXIT.

        7000-RETURN-CODE.
            DISPLAY "  RETURN-CODE = ", RETURN-CODE.
            IF RETURN-CODE NOT = WS-SB-RC THEN
                DISPLAY "    *** FAILED: EXPECTED RC=", WS-SB-RC
                ADD 1 TO WS-FAIL-COUNT
            ELSE
                DISPLAY "    --- CORRECT ---"
            END-IF.
            EXIT.

        9000-FINALIZE.
            DISPLAY "FAILURES DETECTED: ", WS-FAIL-COUNT.
            IF WS-FAIL-COUNT > 0 THEN
                MOVE 1 TO RETURN-CODE
            ELSE
                MOVE ZERO TO RETURN-CODE
            END-IF.
            EXIT.

        END PROGRAM TEST010.
