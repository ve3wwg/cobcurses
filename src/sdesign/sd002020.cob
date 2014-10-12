        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002020.
      *>
      *> THIS SCREEN IS A TEMPORARY "FIRST TIME" SCREEN. IT GIVES SOME 
      *> PRELIMINARY INSTRUCTIONS TO THE USER FOR THE FIRST TIME, SO THAT
      *> THEY'LL KNOW WHAT TO EXPECT IN THE SCREEN PAINTER.
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.
            COPY COBCATTR.
            COPY COBCCOLOUR.
            COPY COBCURSL.

            COPY SD002020-WS.

        01  MISC.
            10  FIRST-TIME                  PIC X VALUE 'Y'.

        77  WS-SCREEN-TOO-SMALL-FLAG        PIC X VALUE 'N'.
            88  WS-SCREEN-TOO-SMALL         VALUE 'Y'.

        LINKAGE SECTION.

            COPY COBCURSG.

        01  SCREEN-IMAGE.
            COPY SCREEN-SI.

        01  SCN-IMAGE-CHANGED               PIC X.
        01  SCN-STRIP-CHARACTER             PIC X.

        PROCEDURE DIVISION USING
            NC-COBCURSES, SCREEN-IMAGE,
            SCN-IMAGE-CHANGED, SCN-STRIP-CHARACTER.

        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZE.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            EXIT PROGRAM.

        1000-INITIALIZE.
            PERFORM NC-INIT
            COPY SD002020-PD.
            PERFORM NC-CLEAR.
            EXIT.

        5000-PROCESS.
            IF FIRST-TIME = 'Y'
                PERFORM NC-DRAW-SCREEN
                IF RETURN-CODE NOT = NC-RET-OK THEN
                    SET WS-SCREEN-TOO-SMALL TO TRUE
                END-IF
                IF NOT WS-SCREEN-TOO-SMALL THEN
                    MOVE "Press RETURN to start:" TO NC-MSGBUF
                    PERFORM NC-PUT-MESSAGE-CR
                    MOVE 'N' TO FIRST-TIME
                    PERFORM NC-CLEAR
                END-IF
            END-IF
            IF NOT WS-SCREEN-TOO-SMALL THEN
                CALL "SD002030" USING
                    NC-COBCURSES, SCREEN-IMAGE,
                    SCN-IMAGE-CHANGED, SCN-STRIP-CHARACTER
            END-IF
            PERFORM NC-CLEAR.
            EXIT.

        9000-FINALIZE.
            PERFORM NC-CLEAR.
            PERFORM NC-FINALIZE.
            EXIT.

            COPY NULLEVENTS.
            COPY COBCURSQ.

        END PROGRAM SD002020.
