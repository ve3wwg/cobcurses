        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST011.
      *>
      *> THIS PROGRAM TESTS COBCURSES-EDIT-COMP-2 :
      *> AND COBCURSES-FORMAT-COMP-2.
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT INPUT-FILE
                ASSIGN TO "test011.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.

            FD  INPUT-FILE.
            01  INP-RECORD.
                10  INP-TEXT                    PIC X(32).
                10  INP-UNITS-CONFIG            PIC X(60).
                                
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

            01  WS-MISC.
                10  WS-LENGTH                   PIC 9999.
                10  WS-SB-RC                    PIC S9(9).
                10  WS-FAILED-COUNT             PIC 9999 VALUE 0.
                10  WS-UNITS-CONFIG             PIC X(60)
                    VALUE "NO UNITS CONFIG VALUE ".

            01  WS-FLAGS.
                10  WS-INPUT-EOF-FLAG           PIC X.
                    88  WS-INPUT-EOF            VALUE 'Y'
                        FALSE IS                'N'.

            01  WS-COMP-2-ITEM                  COMP-2.
            01  WS-OUT-BUFFER                   PIC X(32).
            01  WS-BUFFER-LENGTH                PIC 99.
            01  WS-SAVED-OUTPUT                 PIC X(32).
            01  WS-ENG-FORMAT-FLAG              PIC X.
            01  WS-DIGITS                       PIC 99 VALUE 14.

        PROCEDURE DIVISION.
            PERFORM 1000-INITIALIZE.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            GOBACK.

        1000-INITIALIZE.
            OPEN INPUT INPUT-FILE.
            SET WS-INPUT-EOF TO FALSE.
            MOVE LENGTH OF INP-TEXT TO WS-LENGTH.
            MOVE LENGTH OF WS-OUT-BUFFER TO WS-BUFFER-LENGTH.
            EXIT.

        5000-PROCESS.
            PERFORM 6000-READ-INPUT.
            PERFORM UNTIL WS-INPUT-EOF
                PERFORM 5100-TEST
                PERFORM 6000-READ-INPUT
            END-PERFORM.
            EXIT.

        5100-TEST.
            DISPLAY " ".
            DISPLAY "INPUT TEXT:   '", INP-TEXT, "'"
            DISPLAY "UNITS CONFIG: '", WS-UNITS-CONFIG, "'"
            MOVE 0.25 TO WS-COMP-2-ITEM.
            CALL "COBCURSES-EDIT-COMP-2" USING
                INP-TEXT,
                WS-LENGTH,
                WS-UNITS-CONFIG,
                WS-COMP-2-ITEM.
            MOVE ZERO TO WS-SB-RC.
            PERFORM 6100-RETURN-CODE.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                DISPLAY "COMP-2 ITEM : ", WS-COMP-2-ITEM
            END-IF
            MOVE 'Y' TO WS-ENG-FORMAT-FLAG.
            PERFORM 5200-FORMAT-RESULT.
            MOVE WS-OUT-BUFFER TO WS-SAVED-OUTPUT.
            MOVE 'N' TO WS-ENG-FORMAT-FLAG.
            PERFORM 5200-FORMAT-RESULT.
      *>
      *>     SHOULD BE ABLE TO RE-PROCESS OUR OUTPUT AS INPUT
      *>
            DISPLAY "REPROCESSING '", WS-SAVED-OUTPUT, "' ",
                "AS INPUT:".
            MOVE WS-SAVED-OUTPUT TO INP-TEXT.

            MOVE 0.25 TO WS-COMP-2-ITEM.
            CALL "COBCURSES-EDIT-COMP-2" USING
                INP-TEXT,
                WS-LENGTH,
                WS-UNITS-CONFIG,
                WS-COMP-2-ITEM.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                DISPLAY "WHOA!!! COMP-2 ITEM : ", WS-COMP-2-ITEM,
                    " DID NOT REPROCESS!!"
                ADD 1 TO WS-FAILED-COUNT
            ELSE
                MOVE 'Y' TO WS-ENG-FORMAT-FLAG
                PERFORM 5200-FORMAT-RESULT
                IF WS-OUT-BUFFER NOT = WS-SAVED-OUTPUT THEN
                    DISPLAY "WHOA!!! I SMELL A WUMPUS!!!"
                    DISPLAY "  REPROCESSED RESULTS DIFFER!!"
                    ADD 1 TO WS-FAILED-COUNT
                ELSE
                    DISPLAY "  REPROCESSED RESULTS MATCHED."
                END-IF
            END-IF.
            DISPLAY " "
            EXIT.

        5200-FORMAT-RESULT.
            DISPLAY "  ((( FORMATTING RESULT, ENG FLAG=", 
                WS-ENG-FORMAT-FLAG, ", DIGITS=", WS-DIGITS,
                " )))".
            IF WS-UNITS-CONFIG NOT = "-" THEN
                CALL "COBCURSES-FORMAT-COMP-2" USING
                    WS-COMP-2-ITEM,
                    WS-ENG-FORMAT-FLAG,
                    WS-DIGITS,
                    WS-UNITS-CONFIG,
                    WS-OUT-BUFFER,
                    WS-BUFFER-LENGTH
            ELSE
                DISPLAY "  <<<UNITS CONFIG OMITTED>>>"
                CALL "COBCURSES-FORMAT-COMP-2" USING
                    WS-COMP-2-ITEM,
                    WS-ENG-FORMAT-FLAG,
                    WS-DIGITS,
                    OMITTED,
                    WS-OUT-BUFFER,
                    WS-BUFFER-LENGTH
            END-IF.
            PERFORM 6100-RETURN-CODE.
            DISPLAY "RESULT :      '", WS-OUT-BUFFER, "'"
            EXIT.

        6000-READ-INPUT.
            READ INPUT-FILE
                AT END
                    SET WS-INPUT-EOF TO TRUE
                NOT AT END
                    SET WS-INPUT-EOF TO FALSE
                    IF INP-UNITS-CONFIG NOT = SPACES THEN
                        MOVE INP-UNITS-CONFIG TO WS-UNITS-CONFIG
                    END-IF
            END-READ.
            EXIT.

        6100-RETURN-CODE.
            DISPLAY "  RETURN-CODE= ", RETURN-CODE.
            IF RETURN-CODE = WS-SB-RC THEN
                DISPLAY "  -- AS EXPECTED : SUCCESSFUL --"
            ELSE
                DISPLAY "  ** INCORRECT : FAILED **"
                ADD 1 TO WS-FAILED-COUNT
            END-IF.
            EXIT.

        9000-FINALIZE.
            CLOSE INPUT-FILE.
            DISPLAY "--"
            DISPLAY "FAILURES DETECTED: ", WS-FAILED-COUNT.
            IF WS-FAILED-COUNT > 0 THEN
                MOVE 1 TO RETURN-CODE
            ELSE
                MOVE ZERO TO RETURN-CODE
            END-IF.
            EXIT.

        END PROGRAM TEST011.
