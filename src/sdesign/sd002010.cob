        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002010.
      *>
      *> THIS IS THE MAIN SCREEN. THIS SCREEN IS RESPONSIBLE FOR MAINTAINING
      *> THE MAIN SCREEN HEADER RECORD. ALL OTHER SCREENS ARE LAUNCHED FROM
      *> THIS MAIN SCREEN.
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
            SELECT SCREEN-FILE
                ASSIGN TO SCREEN-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCN-NAME.

            SELECT SCRNBG-FILE
                ASSIGN TO SCRNBG-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCRBG-KEY.

            SELECT SCRFDEF-FILE
                ASSIGN TO SCRFDEF-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FDEF-KEY.

            SELECT SCRFSTA-FILE
                ASSIGN TO SCRFSTA-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FST-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  SCREEN-FILE.
        01  SCREEN-RECORD.
            COPY SCREEN-01.

        FD  SCRNBG-FILE.
        01  SCRNBG-RECORD.
            COPY SCREEN-BG.

        FD  SCRFDEF-FILE.
        01  SCRFDEF-RECORD.
            COPY SCREEN-FD.

        FD  SCRFSTA-FILE.
        01  SCRFSTA-RECORD.
            COPY SCREEN-FS.

        WORKING-STORAGE SECTION.

            COPY COBCRETC.
            COPY COBCATTR.
            COPY COBCCOLOUR.
            COPY COBCURSL.

            COPY SD002010-WS.

        01  FILE-NAMES.
            10  FILE-NAME-LENGTH            PIC 9999.
            10  SCREEN-FILE-NAME            PIC X(256)
                VALUE "${COBCURSES_DATADIR}/SCREENS.X".
            10  SCRNBG-FILE-NAME            PIC X(256)
                VALUE "${COBCURSES_DATADIR}/SCRNBG.X".
            10  SCRFDEF-FILE-NAME           PIC X(256)
                VALUE "${COBCURSES_DATADIR}/SCRFDEF.X".
            10  SCRFSTA-FILE-NAME           PIC X(256)
                VALUE "${COBCURSES_DATADIR}/SCRFSTA.X".

        01  RECORD-STATE.
            10  RCD-DEFINED                 PIC X.
                88  DEFINED-RECORD          VALUE 'Y'.
            10  RCD-NEW                     PIC X.
                88  NEW-RECORD              VALUE 'Y'.
            10  RCD-CHANGES                 PIC X.
                88 UNSAVED-CHANGES          VALUE 'Y'.

        01  OTHER-FLAGS.
            10  RCD-DELETED-FLAG            PIC X.
                88  RCD-DELETED             VALUE 'Y'
                    FALSE IS                'N'.
            10  FDEF-RECORD-FOUND-FLAG      PIC X.
                88  FDEF-RECORD-FOUND       VALUE 'Y'
                    FALSE IS                'N'.
            10  SCRNBG-RECORD-FOUND-FLAG    PIC X.
                88  SCRNBG-RECORD-FOUND     VALUE 'Y'
                    FALSE IS                'N'.
            01  FSTA-RECORD-FOUND-FLAG      PIC X.
                88  FSTA-RECORD-FOUND       VALUE 'Y'
                    FALSE IS                'N'.

        01  IMAGE-SAVED                     PIC X.
        01  LINEX                           PIC 999.

        01  SCREEN-IMAGE.
            COPY SCREEN-SI.

        01  WS-FLAGS.
            10  WS-SCREEN-TOO-SMALL-FLAG    PIC X VALUE 'N'.
                88  WS-SCREEN-TOO-SMALL     VALUE 'Y'.

        LINKAGE SECTION.

            COPY COBCURSG.

        PROCEDURE DIVISION
            USING NC-COBCURSES.

        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZATION.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5000-PROCESS
            END-IF.
            PERFORM 9000-FINALIZE.
            EXIT PROGRAM.

        1000-INITIALIZATION.
            INITIALIZE SCREEN-IMAGE.
            
            MOVE LENGTH OF SCREEN-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING SCREEN-FILE-NAME, FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING SCRFDEF-FILE-NAME, FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING SCRNBG-FILE-NAME, FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING SCRFSTA-FILE-NAME, FILE-NAME-LENGTH.

            PERFORM 9100-OPEN-FILES.
            PERFORM 1010-RECORD-INIT.
            PERFORM 1020-COBCURSES-INIT.
            MOVE FNO-ACTION TO NC-FSEQ-STATE.
            EXIT.

        1010-RECORD-INIT.
            MOVE 'N' TO RCD-DEFINED, RCD-NEW, RCD-CHANGES.
            EXIT.

        1020-COBCURSES-INIT.
            PERFORM NC-INIT.
            PERFORM NC-CLEAR.
            PERFORM 1030-SCREEN-INIT.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        1030-SCREEN-INIT.
            COPY SD002010-PD.
            PERFORM 1040-DRAW-SCREEN.
            EXIT.

        1040-DRAW-SCREEN.
            PERFORM NC-DRAW-SCREEN.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                SET WS-SCREEN-TOO-SMALL TO TRUE
            END-IF.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM NC-DRAW-FIELDS
            END-IF.
            EXIT.

        4000-MOVE-TO-RECORD.
            MOVE FLD-SCREEN-NAME TO SCN-NAME.
            MOVE FLD-SCREEN-DESCRIPTION TO SCN-DESCRIPTION.
            MOVE FLD-AUTHOR-NAME TO SCN-AUTHOR.
            MOVE FLD-NOTES TO SCN-NOTES.
            MOVE FLD-SCREEN-TITLE TO SCN-TITLE.
            MOVE FLD-SHOW-DATE TO SCN-SHOW-DATE.
            MOVE FLD-SHOW-TIME TO SCN-SHOW-TIME.
            MOVE FLD-ACTION-FIELD-FLAG TO SCN-ACTION-REQUIRED.
            MOVE FLD-MINIMUM-COLUMNS TO SCN-COLUMNS-MIN.
            MOVE FLD-MINIMUM-LINES TO SCN-LINES-MIN.
            MOVE FLD-MINIMUM-COLOUR-PAIRS TO SCN-PAIRS.
            MOVE FLD-WS-SECTION-COPYBOOK TO SCN-WS-SECTION.
            MOVE FLD-PROC-DIV-COPYBOOK TO SCN-PROCEDURE-DIVISION.
            MOVE FLD-STRIP-CHARACTER TO SCN-STRIP-CHARACTER.
            EXIT.

        4100-MOVE-FROM-RECORD.
            MOVE SCN-NAME TO FLD-SCREEN-NAME.
            MOVE SCN-DESCRIPTION TO FLD-SCREEN-DESCRIPTION.
            MOVE SCN-AUTHOR TO FLD-AUTHOR-NAME.
            MOVE SCN-NOTES TO FLD-NOTES.
            MOVE SCN-TITLE TO FLD-SCREEN-TITLE.
            MOVE SCN-SHOW-DATE TO FLD-SHOW-DATE.
            MOVE SCN-SHOW-TIME TO FLD-SHOW-TIME.
            MOVE SCN-ACTION-REQUIRED TO FLD-ACTION-FIELD-FLAG.
            MOVE SCN-COLUMNS-MIN TO FLD-MINIMUM-COLUMNS.
            MOVE SCN-LINES-MIN TO FLD-MINIMUM-LINES.
            MOVE SCN-PAIRS TO FLD-MINIMUM-COLOUR-PAIRS.
            MOVE SCN-WS-SECTION TO FLD-WS-SECTION-COPYBOOK.
            MOVE SCN-PROCEDURE-DIVISION TO FLD-PROC-DIV-COPYBOOK.
            MOVE SCN-COLUMNS-MIN TO SCREEN-COLUMNS.
            MOVE SCN-LINES-MIN TO SCREEN-LINES.
            MOVE SCN-STRIP-CHARACTER TO FLD-STRIP-CHARACTER.
            EXIT.

        5000-PROCESS.
            PERFORM NC-FIELD-STATE-MACHINE.
            EXIT.

        5100-ACTION-C.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM NC-RESET-CHANGES.
            PERFORM 1010-RECORD-INIT.
            PERFORM 6100-UPDATE-SCREEN.
            EXIT.

        5150-ACTION-O.
            MOVE SPACES TO NC-MSGBUF.
            STRING "Other actions: P=paint Q=fields ",
                "T=states, G=Generate and R=character sets."
                INTO NC-MSGBUF.
            PERFORM NC-PUT-MESSAGE-OVERRIDE.
            EXIT.

        5200-ACTION-N.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                PERFORM 5100-ACTION-C
                MOVE 1 TO NC-FSEQ-NEXT
            END-IF
            PERFORM VARYING LINEX FROM 1 BY 1
                UNTIL LINEX > 50
                MOVE SPACES TO SCREEN-LINE(LINEX)
            END-PERFORM.
            EXIT.

        5300-ACTION-E.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                MOVE 9999 TO NC-FSEQ-NEXT
            END-IF.
            EXIT.

        5400-ACTION-S.
            IF DEFINED-RECORD THEN
                IF NEW-RECORD THEN
                    PERFORM 5410-ADD-RECORD
                ELSE
                    PERFORM 5420-UPDATE-RECORD
                END-IF
                PERFORM 9200-CLOSE-FILES

                MOVE SCN-LINES-MIN TO SCREEN-LINES
                MOVE SCN-COLUMNS-MIN TO SCREEN-COLUMNS
                MOVE 'N' TO IMAGE-SAVED
                CALL "SD002035" USING
                    SCN-NAME, SCREEN-IMAGE,
                    IMAGE-SAVED, SCRNBG-FILE-NAME
                IF IMAGE-SAVED NOT = 'Y' THEN
                    MOVE "Error: Saving the screen background."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
                PERFORM 9100-OPEN-FILES
            ELSE
                MOVE "No record to save." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5410-ADD-RECORD.
            PERFORM 4000-MOVE-TO-RECORD
            WRITE SCREEN-RECORD
                INVALID KEY
                    MOVE "Error: Adding a new screen record."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                NOT INVALID KEY
                    MOVE 'N' TO RCD-CHANGES, RCD-NEW
                    MOVE "A new record was added (saved)."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-MESSAGE-OVERRIDE
            END-WRITE.
            PERFORM 9300-FLUSH-FILES.
            EXIT.

        5420-UPDATE-RECORD.
            PERFORM 4000-MOVE-TO-RECORD
            REWRITE SCREEN-RECORD
                INVALID KEY
                    MOVE "Error: Updating the screen record."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                NOT INVALID KEY
                    MOVE 'N' TO RCD-CHANGES
                    MOVE "Your record was updated (saved)."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-MESSAGE-OVERRIDE
            END-REWRITE.
            PERFORM 9300-FLUSH-FILES.
            EXIT.

        5500-ACTION-D.
            IF DEFINED-RECORD AND NOT NEW-RECORD THEN
                INITIALIZE SCREEN-RECORD
                MOVE FLD-SCREEN-NAME TO SCN-NAME

                DELETE SCREEN-FILE
                    INVALID KEY
                        SET RCD-DELETED TO FALSE
                    NOT INVALID KEY
                        SET RCD-DELETED TO TRUE
                END-DELETE

                IF RCD-DELETED THEN
                    PERFORM 5540-DELETE-SCNBG-RCDS
                    PERFORM 5510-DELETE-FIELD-RCDS
                    PERFORM 5570-DELETE-STATE-RCDS
                    PERFORM 9300-FLUSH-FILES
                    PERFORM 5100-ACTION-C
                    MOVE "The record was deleted." TO NC-MSGBUF
                    PERFORM NC-PUT-MESSAGE-OVERRIDE
                ELSE
                    MOVE "Error: Unable to delete this record."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF

            ELSE
                STRING "The current record has not been saved, "
                    "and thus cannot be deleted. Do a Clear."
                    INTO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5510-DELETE-FIELD-RCDS.
            INITIALIZE SCRFDEF-RECORD.
            MOVE SCN-NAME TO SCR-FDEF-SCREEN-NAME.
            MOVE ZERO TO SCR-FDEF-NO.

            START SCRFDEF-FILE KEY IS >= SCR-FDEF-KEY
                INVALID KEY
                    SET FDEF-RECORD-FOUND TO FALSE
                NOT INVALID KEY
                    SET FDEF-RECORD-FOUND TO TRUE
            END-START.

            IF FDEF-RECORD-FOUND THEN
                PERFORM 5520-DELETE-FIELD-RCDS
            END-IF.
            EXIT.

        5520-DELETE-FIELD-RCDS.
            PERFORM 5530-READ-NEXT-FDEF-RECORD.
            PERFORM UNTIL NOT FDEF-RECORD-FOUND
                DELETE SCRFDEF-FILE
                    INVALID KEY
                        CONTINUE
                END-DELETE
                PERFORM 5530-READ-NEXT-FDEF-RECORD
            END-PERFORM.
            EXIT.

        5530-READ-NEXT-FDEF-RECORD.
            READ SCRFDEF-FILE NEXT RECORD
                AT END
                    SET FDEF-RECORD-FOUND TO FALSE
                NOT AT END
                    IF SCR-FDEF-SCREEN-NAME = SCN-NAME THEN
                        SET FDEF-RECORD-FOUND TO TRUE
                    ELSE
                        SET FDEF-RECORD-FOUND TO FALSE
                    END-IF
            END-READ.
            EXIT.

        5540-DELETE-SCNBG-RCDS.
            INITIALIZE SCRNBG-RECORD.
            MOVE SCN-NAME TO SCRBG-NAME.
            MOVE ZERO TO SCRBG-SEGMENT-NO.

            START SCRNBG-FILE KEY IS >= SCRBG-KEY
                INVALID KEY
                    SET SCRNBG-RECORD-FOUND TO FALSE
                NOT INVALID KEY
                    SET SCRNBG-RECORD-FOUND TO TRUE
            END-START.
            IF SCRNBG-RECORD-FOUND THEN
                PERFORM 5550-DELETE-SCNBG-RCDS
            END-IF.
            EXIT.

        5550-DELETE-SCNBG-RCDS.
            PERFORM 5560-READ-NEXT-SCRNBG-RECORD.
            PERFORM UNTIL NOT SCRNBG-RECORD-FOUND
                DELETE SCRNBG-FILE
                    INVALID KEY
                        CONTINUE
                END-DELETE
                PERFORM 5560-READ-NEXT-SCRNBG-RECORD
            END-PERFORM.
            EXIT.

        5560-READ-NEXT-SCRNBG-RECORD.
            READ SCRNBG-FILE NEXT RECORD
                AT END
                    SET SCRNBG-RECORD-FOUND TO FALSE
                NOT AT END
                    IF SCRBG-NAME = SCN-NAME THEN
                        SET SCRNBG-RECORD-FOUND TO TRUE
                    ELSE
                        SET SCRNBG-RECORD-FOUND TO FALSE
                    END-IF
            END-READ.
            EXIT.

        5570-DELETE-STATE-RCDS.
            INITIALIZE SCRFSTA-RECORD.
            MOVE SCN-NAME TO SCR-FST-SCREEN-NAME.
            MOVE ZERO TO SCR-FST-STATE-NO.

            START SCRFSTA-FILE KEY IS >= SCR-FST-KEY
                INVALID KEY
                    SET FSTA-RECORD-FOUND TO FALSE
                NOT INVALID KEY
                    SET FSTA-RECORD-FOUND TO TRUE
            END-START.
            IF FSTA-RECORD-FOUND THEN
                PERFORM 5580-DELETE-FSTA-RCDS
            END-IF.
            EXIT.

        5580-DELETE-FSTA-RCDS.
            PERFORM 5590-READ-NEXT-FSTA-RECORD.
            PERFORM UNTIL NOT FSTA-RECORD-FOUND
                DELETE SCRFSTA-FILE
                    INVALID KEY
                        CONTINUE
                END-DELETE
                PERFORM 5590-READ-NEXT-FSTA-RECORD
            END-PERFORM.
            EXIT.

        5590-READ-NEXT-FSTA-RECORD.
            READ SCRFSTA-FILE NEXT RECORD
                AT END
                    SET FSTA-RECORD-FOUND TO FALSE
                NOT AT END
                    IF SCR-FST-SCREEN-NAME = SCN-NAME THEN
                        SET FSTA-RECORD-FOUND TO TRUE
                    ELSE
                        SET FSTA-RECORD-FOUND TO FALSE
                    END-IF
            END-READ.
            EXIT.

        5600-ACTION-F.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                STRING "Enter search value + CR, or cursor up/down",
                    "to browse." INTO NC-MSGBUF
                PERFORM NC-PUT-MESSAGE-OVERRIDE
                MOVE FSEQ-FIND TO NC-FSEQ-NEXT
                MOVE FNO-SCREEN-NAME TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT)
            END-IF.
            EXIT.

        5610-ACTION-F-CONTD.
            PERFORM 1010-RECORD-INIT
            INITIALIZE SCREEN-RECORD
            MOVE FLD-SCREEN-NAME TO SCN-NAME
            IF NC-FIELD-EXIT-CU THEN
                PERFORM 5620-LT-SEARCH
            END-IF
            IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB THEN
                PERFORM 5630-GE-SEARCH
            END-IF
            IF NC-FIELD-EXIT-CD THEN
                PERFORM 5640-GT-SEARCH
            END-IF.
            EXIT.

        5620-LT-SEARCH.
            START SCREEN-FILE KEY IS < SCN-NAME
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5630-GE-SEARCH.
            START SCREEN-FILE KEY IS >= SCN-NAME
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5640-GT-SEARCH.
            START SCREEN-FILE KEY IS > SCN-NAME
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5650-NEXT-RECORD.
            IF DEFINED-RECORD THEN
                READ SCREEN-FILE NEXT RECORD
                    AT END 
                        PERFORM 5100-ACTION-C
                    NOT AT END 
                        PERFORM 4100-MOVE-FROM-RECORD
                        SET DEFINED-RECORD TO TRUE
                        MOVE 'N' TO RCD-NEW, RCD-CHANGES
                        CALL "SD002033" USING
                            SCREEN-RECORD, SCREEN-IMAGE,
                            SCRNBG-FILE-NAME
                        PERFORM 6100-UPDATE-SCREEN
                END-READ
            ELSE
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        5700-ACTION-P.
            IF DEFINED-RECORD THEN
                PERFORM 9200-CLOSE-FILES
                MOVE SCN-COLUMNS-MIN TO SCREEN-COLUMNS
                MOVE SCN-LINES-MIN TO SCREEN-LINES
                IF SCN-TITLE NOT = SPACES
                    MOVE SCN-TITLE TO SCREEN-LINE(1)
                    MOVE 'Y' TO SCREEN-HAS-TITLE
                    MOVE SCN-SHOW-DATE TO SCREEN-SHOW-DATE
                    MOVE SCN-SHOW-TIME TO SCREEN-SHOW-TIME
                ELSE
                    MOVE 'N' TO SCREEN-HAS-TITLE,
                        SCREEN-SHOW-DATE, SCREEN-SHOW-TIME
                END-IF
                CALL "SD002020" USING 
                    NC-COBCURSES, BY REFERENCE SCREEN-IMAGE,
                    RCD-CHANGES, SCN-STRIP-CHARACTER
                MOVE 'P' TO NC-FIELD-ACTION
                PERFORM 1040-DRAW-SCREEN
                PERFORM 9100-OPEN-FILES
            ELSE
                MOVE "You have no current screen to paint."
                    TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5800-ACTION-Q.
            IF DEFINED-RECORD THEN
                PERFORM 9200-CLOSE-FILES
                CALL "SD002040" USING
                    NC-COBCURSES, SCREEN-RECORD,
                    SCREEN-IMAGE, RCD-CHANGES
                MOVE 'Q' TO NC-FIELD-ACTION
                PERFORM 1040-DRAW-SCREEN
                PERFORM 9100-OPEN-FILES
            ELSE
                MOVE "You have no screen selected." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5850-ACTION-T.
            IF DEFINED-RECORD THEN
                PERFORM 9200-CLOSE-FILES
                CALL "SD002050" USING
                    NC-COBCURSES, SCREEN-IMAGE,
                    SCREEN-RECORD
                MOVE 'T' TO NC-FIELD-ACTION
                PERFORM 1040-DRAW-SCREEN
                PERFORM 9100-OPEN-FILES
            ELSE
                MOVE "You have no screen selected." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5875-ACTION-R.
            PERFORM 9200-CLOSE-FILES.
            CALL "SD002060" USING NC-COBCURSES.
            MOVE 'R' TO NC-FIELD-ACTION.
            PERFORM 1040-DRAW-SCREEN.
            PERFORM 9100-OPEN-FILES.
            EXIT.

        5900-ACTION-EDIT.
            IF NOT DEFINED-RECORD THEN
                MOVE "No current record to edit." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            ELSE
                IF NC-FIELD-EDIT-TARGET = 'N' THEN
                    MOVE "You cannot edit that field." TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                ELSE
                    MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
                    MOVE NC-FIELD-SEARCH
                        TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT)
                END-IF
            END-IF.
            EXIT.

        5950-ACTION-G.
            IF DEFINED-RECORD THEN
                PERFORM 9200-CLOSE-FILES
                CALL "SD002070" USING
                    NC-COBCURSES, FLD-SCREEN-NAME
                PERFORM 1040-DRAW-SCREEN
                PERFORM 9100-OPEN-FILES
            ELSE
                MOVE "No current screen to generate." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            MOVE 'G' TO NC-FIELD-ACTION.
            EXIT.

        5960-ACTION-M.
            PERFORM 9200-CLOSE-FILES.
            CALL "SD002080" USING
                NC-COBCURSES,
                SCREEN-LAST-LINE, SCREEN-LAST-COLUMN.
            PERFORM 1040-DRAW-SCREEN
            PERFORM 9100-OPEN-FILES
            EXIT.

        5970-ACTION-X.
            IF DEFINED-RECORD THEN
                PERFORM 9200-CLOSE-FILES
                CALL "SD002085" USING
                    NC-COBCURSES, SCN-NAME, SCN-DESCRIPTION
                PERFORM 1040-DRAW-SCREEN
                PERFORM 9100-OPEN-FILES
            ELSE
                MOVE "No current screen selected." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        6000-LOOKUP-RECORD.
            MOVE FLD-SCREEN-NAME TO SCN-NAME.
            READ SCREEN-FILE
                INVALID KEY
                    MOVE FLD-SCREEN-NAME TO SCN-NAME
                NOT INVALID KEY
                    PERFORM 4100-MOVE-FROM-RECORD
                    MOVE FNO-ACTION TO NC-FSEQ-NEXT
                    SET DEFINED-RECORD TO TRUE
                    MOVE 'N' TO RCD-NEW, RCD-CHANGES
                    PERFORM 6100-UPDATE-SCREEN
                    CALL "SD002033" USING
                        SCREEN-RECORD, SCREEN-IMAGE,
                        SCRNBG-FILE-NAME
            END-READ.
            EXIT.

        6100-UPDATE-SCREEN.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        7000-ACTION-EVENT.
            EVALUATE NC-FIELD-ACTION
                WHEN "C"
                    PERFORM 5100-ACTION-C
                WHEN "N"
                    PERFORM 5200-ACTION-N
                WHEN "E"
                    PERFORM 5300-ACTION-E
                WHEN "S"
                    PERFORM 5400-ACTION-S
                WHEN "D"
                    PERFORM 5500-ACTION-D
                WHEN "F"
                    PERFORM 5600-ACTION-F
                WHEN "O"
                    PERFORM 5150-ACTION-O
                WHEN "P"
                    PERFORM 5700-ACTION-P
                WHEN "Q"
                    PERFORM 5800-ACTION-Q
                WHEN "T"
                    PERFORM 5850-ACTION-T
                WHEN "R"
                    PERFORM 5875-ACTION-R
                WHEN "G"
                    PERFORM 5950-ACTION-G
                WHEN "M"
                    PERFORM 5960-ACTION-M
                WHEN "X"
                    PERFORM 5970-ACTION-X
                WHEN " "
                    PERFORM 5900-ACTION-EDIT
                WHEN OTHER
                    PERFORM 8000-ERROR-ACTION
            END-EVALUATE.
            EXIT.

        7100-CHANGE-EVENT.
            IF NC-FIELD-NUMBER < FNO-ACTION
            AND NC-FSEQ-STATE NOT = FSEQ-FIND
                SET UNSAVED-CHANGES TO TRUE
            END-IF.
            EXIT.

        7200-VERIFY-EVENT.
            IF NC-FIELD-NUMBER = FNO-SCREEN-NAME
                IF NC-FSEQ-STATE = FNO-SCREEN-NAME
      *>            Input sequence requires non-blank name
                    IF FLD-SCREEN-NAME = SPACES
                        MOVE 'N' TO NC-FIELD-VERIFIED
                    ELSE
                        MOVE 'Y' TO NC-FIELD-VERIFIED
                    END-IF
                ELSE
      *>            Blank name allowed in Find mode
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                END-IF
            END-IF.
            EXIT.

        7500-FIELD-EVENT.
            IF NC-FIELD-NUMBER = FNO-ACTION
                PERFORM 7000-ACTION-EVENT
            END-IF.

            IF NC-FSEQ-STATE = FNO-SCREEN-NAME
                PERFORM 6000-LOOKUP-RECORD
            END-IF.

            IF NC-FSEQ-STATE = FNO-PROC-DIV-COPYBOOK
                SET DEFINED-RECORD TO TRUE
                SET NEW-RECORD TO TRUE
                SET UNSAVED-CHANGES TO TRUE
            END-IF.

            IF NC-FSEQ-STATE = FSEQ-FIND
                PERFORM 5610-ACTION-F-CONTD
                IF NC-FIELD-EXIT-CD OR NC-FIELD-EXIT-CU THEN
                    MOVE FSEQ-FIND TO NC-FSEQ-NEXT
                END-IF
            END-IF.
            EXIT.

        7600-MOUSE-EVENT.
            PERFORM 5900-ACTION-EDIT.
            EXIT.

        7700-STATE-CHANGE-EVENT.
            EXIT.

        8000-ERROR-ACTION.
            SET NC-MSG-TEXT TO ADDRESS OF INF-ACTION.
            MOVE LENGTH OF INF-ACTION TO NC-MSG-LENGTH.
            PERFORM NC-ERROR-MESSAGE-OVERRIDE.
            EXIT.

        8010-WARNING-UNSAVED-CHANGES.
            MOVE "Warning: You have unsaved changes (Save or Cancel)."
                TO NC-MSGBUF.
            PERFORM NC-PUT-ERROR-OVERRIDE.
            EXIT.

        9000-FINALIZE.
            PERFORM 9200-CLOSE-FILES.
            PERFORM NC-CLEAR.
            PERFORM NC-FINALIZE.
            EXIT.

        9100-OPEN-FILES.
            OPEN I-O SCREEN-FILE, SCRNBG-FILE, SCRFDEF-FILE,
                SCRFSTA-FILE.
            EXIT.

        9200-CLOSE-FILES.
            CLOSE SCREEN-FILE, SCRNBG-FILE, SCRFDEF-FILE,
                SCRFSTA-FILE.
            EXIT.

        9300-FLUSH-FILES.
            PERFORM 9200-CLOSE-FILES.
            PERFORM 9100-OPEN-FILES.
            EXIT.

        NC-VERIFY-EVENT.
            PERFORM 7200-VERIFY-EVENT.
            EXIT.

        NC-CHANGE-EVENT.
            PERFORM 7100-CHANGE-EVENT.
            EXIT.

        NC-FIELD-EVENT.
            PERFORM 7500-FIELD-EVENT.
            EXIT.

        NC-MOUSE-EVENT.
            PERFORM 7600-MOUSE-EVENT.
            EXIT.

        NC-STATE-CHANGE-EVENT.
            PERFORM 7700-STATE-CHANGE-EVENT.
            EXIT.

        NC-FKEY-EVENT.
            EXIT.

            COPY COBCURSQ.

        END PROGRAM SD002010.
