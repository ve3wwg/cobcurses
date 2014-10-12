        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002035.
      *>
      *> THIS MODULE PERFORMS THE COMPLEX OPERATION OF SAVING A SCREEN IMAGE
      *> TO THE SCREEN BACKGROUND FRAGMENTS INDEXED FILE.
      *>
      *> INPUTS:
      *>
      *>     SCREEN-NAME         THE NAME OF THE SCREEN TO SAVE IMAGE FOR
      *>     SCREEN-IMAGE        THE SCREEN IMAGE ITSELF
      *>     SCRNBG-FILE-NAME    THE PATHNAME OF THE INDEXED FILE TO SAVE INTO
      *>
      *> OUTPUTS:
      *>
      *>     SCREEN-SAVED        SET TO 'Y' IF THE SAVE WAS SUCCESSFUL
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT SCRNBG-FILE
                ASSIGN TO SCRNBG-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCRBG-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  SCRNBG-FILE.
        01  SCRNBG-RECORD.
            COPY SCREEN-BG.

        WORKING-STORAGE SECTION.

            COPY COBCATTR.

            01  WORK-AREAS.
                10  SEGMENT-NO              PIC 999.
                10  LINE-NO                 PIC 999.
                10  NUM-SCREEN-COLS         PIC 9999 COMP-5
                                            SYNCHRONIZED.
                10  NUM-SEGLENGTH           PIC 9999 COMP-5
                                            SYNCHRONIZED.
                10  NUM-COLUMN              PIC 9999 COMP-5
                                            SYNCHRONIZED.
                10  NUM-OTLEN               PIC 9999 COMP-5
                                            SYNCHRONIZED.
                10  TEMP-SEGMENT            PIC X(300).

            01  SAVE.
                10  SV-LINE                 PIC 999.
                10  SV-COLUMN               PIC 999.
                10  SV-LENGTH               PIC 99.
                10  SV-TEXT                 PIC X(32).

            01  FLAGS.
                10  REWRITE-FLAG            PIC X.
                    88  NO-REWRITE          VALUE 'N'.
                    88  NEEDS-REWRITE       VALUE 'Y'.
                10  DELETE-FLAG             PIC X.
                    88  NO-DELETE           VALUE 'N'.
                    88  NEEDS-DELETE        VALUE 'Y'.

        LINKAGE SECTION.

            01  SCREEN-NAME                 PIC X(16).

            01  SCREEN-IMAGE.
                COPY SCREEN-SI.

            01  SCREEN-SAVED                PIC X.

            01  SCRNBG-FILE-NAME            PIC X(256).

        PROCEDURE DIVISION
          USING
            SCREEN-NAME,
            BY REFERENCE SCREEN-IMAGE,
            BY REFERENCE SCREEN-SAVED,
            SCRNBG-FILE-NAME.

        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZE.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            EXIT PROGRAM.

        1000-INITIALIZE.
            INITIALIZE SCRNBG-RECORD.
            MOVE 'Y' TO SCREEN-SAVED.
            OPEN I-O SCRNBG-FILE.
            EXIT.

      *>
      *> UPDATE LOGIC :
      *>
      *>     THIS PROGRAM SAVES SUCCESSIVE SEGMENTS OVER THE EXISTING
      *>     ONES WHENEVER POSSIBLE. THIS IS THOUGHT TO BE SAFER THAN
      *>     DELETING EXISTING ONES AND THEN ADDING THE REVISED SEGMENTS
      *>     IN CASE OF A PROGRAM ABORT OR INTERRUPTION.
      *>
      *>     AFTER ALL REWRITES ARE COMPLETED, AND EXTRA SEGMENTS ARE
      *>     STRICTLY DELETED.
      *>
        5000-PROCESS.
            MOVE SCREEN-COLUMNS TO NUM-SCREEN-COLS.
            MOVE 1 TO SEGMENT-NO.
            IF SCREEN-HAS-TITLE = 'Y'
                MOVE 1 TO LINE-NO
            ELSE
                MOVE 2 TO LINE-NO
            END-IF.
            MOVE SCREEN-LINE(LINE-NO) TO TEMP-SEGMENT.
            PERFORM UNTIL LINE-NO >= SCREEN-LINES
                MOVE SCREEN-NAME TO SCRBG-NAME
                MOVE SEGMENT-NO TO SCRBG-SEGMENT-NO
                MOVE LENGTH OF SCRBG-SEGMENT TO NUM-SEGLENGTH
                CALL "NC_EXTRACT_SEGMENT" USING
                    TEMP-SEGMENT,
                    NUM-SCREEN-COLS,
                    SV-TEXT,
                    NUM-SEGLENGTH,
                    NUM-COLUMN,
                    NUM-OTLEN
                MOVE LINE-NO TO SV-LINE
                MOVE NUM-COLUMN TO SV-COLUMN
                MOVE NUM-OTLEN TO SV-LENGTH
                IF SV-LENGTH > 0 THEN
                    PERFORM 5300-SAVE-SEGMENT
                ELSE
                    ADD 1 TO LINE-NO
                    MOVE SCREEN-LINE(LINE-NO) TO TEMP-SEGMENT
                END-IF
            END-PERFORM.
            PERFORM 5400-REMOVE-REMAINING.
            EXIT.

        5200-INITIALIZE-KEY.
            INITIALIZE SCRBG-KEY.
            MOVE SCREEN-NAME TO SCRBG-NAME.
            MOVE SEGMENT-NO TO SCRBG-SEGMENT-NO.
            EXIT.

        5300-SAVE-SEGMENT.
            PERFORM 5200-INITIALIZE-KEY.
            READ SCRNBG-FILE
                INVALID KEY
                    SET NO-REWRITE TO TRUE
                NOT INVALID KEY
                    SET NEEDS-REWRITE TO TRUE
            END-READ.

            MOVE SV-LINE TO SCRBG-LINE.
            MOVE SV-COLUMN TO SCRBG-COLUMN.
            MOVE SV-LENGTH TO SCRBG-LENGTH.
            MOVE SV-TEXT TO SCRBG-SEGMENT.
            MOVE NC-ATTR-NORMAL TO SCRBG-ATTRIBUTE.
            MOVE ZERO TO SCRBG-COLOUR-PAIR.

            IF NO-REWRITE THEN
                WRITE SCRNBG-RECORD
                    INVALID KEY
                        MOVE 'N' TO SCREEN-SAVED
                    NOT INVALID KEY
                        ADD 1 TO SEGMENT-NO
                END-WRITE
            ELSE
                REWRITE SCRNBG-RECORD
                    INVALID KEY
                        MOVE 'N' TO SCREEN-SAVED
                    NOT INVALID KEY
                        ADD 1 TO SEGMENT-NO
                END-REWRITE
            END-IF.
            EXIT.

        5400-REMOVE-REMAINING.
            PERFORM 5200-INITIALIZE-KEY.
            START SCRNBG-FILE KEY IS >= SCRBG-KEY
                INVALID KEY
                    SET NO-DELETE TO TRUE
                NOT INVALID KEY
                    SET NEEDS-DELETE TO TRUE
            END-START.
            PERFORM UNTIL NO-DELETE
                READ SCRNBG-FILE NEXT RECORD
                    AT END
                        SET NO-DELETE TO TRUE
                    NOT AT END
                        IF SCRBG-NAME = SCREEN-NAME
                            SET NEEDS-DELETE TO TRUE
                        ELSE
                            SET NO-DELETE TO TRUE
                        END-IF
                END-READ
                IF NEEDS-DELETE THEN
                    DELETE SCRNBG-FILE
                        INVALID KEY
                            ADD 0 TO SEGMENT-NO
                    END-DELETE
                END-IF
            END-PERFORM.
            EXIT.

        9000-FINALIZE.
            CLOSE SCRNBG-FILE.
            EXIT.

        END PROGRAM SD002035.
