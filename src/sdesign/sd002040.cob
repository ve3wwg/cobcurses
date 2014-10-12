        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002040.
      *>
      *> THIS IS THE FIELD MAINTENANCE SCREEN. IN THIS SCREEN THE USER DEFINES
      *> OR ALTERS THE DEFINITION OF FIELDS RELATED TO A SCREEN.
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
            SELECT SCRFDEF-FILE
                ASSIGN TO SCRFDEF-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FDEF-KEY.

            SELECT CHARSET-FILE
                ASSIGN TO CHARSET-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS CHARSET-NAME.

            SELECT MENU-FILE
                ASSIGN TO MENU-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS MNU-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  SCRFDEF-FILE.
        01  SCRFDEF-RECORD.
            COPY SCREEN-FD.

        FD  CHARSET-FILE.
        01  CHARSET-RECORD.
            COPY SCREEN-CS.

        FD  MENU-FILE.
        01  MENU-RECORD.
            COPY MENURECD.

        WORKING-STORAGE SECTION.

            COPY COBCRETC.
            COPY COBCATTR.
            COPY COBCCOLOUR.
            COPY COBCURSL.

            COPY SD002040-WS.

        01  FILE-NAMES.
            10  FILE-NAME-LENGTH            PIC 9999.
            10  SCRFDEF-FILE-NAME           PIC X(256)
                VALUE "${COBCURSES_DATADIR}/SCRFDEF.X".
            10  CHARSET-FILE-NAME           PIC X(256)
                VALUE "${COBCURSES_DATADIR}/SCRCHRSET.X".
            10  MENU-FILE-NAME              PIC X(256)
                VALUE "${COBCURSES_DATADIR}/MENUS.X".

        01  RECORD-STATE.
            10  RCD-DEFINED                 PIC X.
                88  DEFINED-RECORD          VALUE 'Y'.
            10  RCD-NEW                     PIC X.
                88  NEW-RECORD              VALUE 'Y'.
            10  RCD-CHANGES                 PIC X.
                88 UNSAVED-CHANGES          VALUE 'Y'.

        01  CHARSET-STATE.
            10  CHARSET-FLAG                PIC X.
                88  DEFINED-CHARSET         VALUE 'Y'.
                88  UNDEFINED-CHARSET       VALUE 'N'.

        01  WORK-AREAS.
            10  NUM-LINE                    PIC 999.
            10  NUM-COLUMN                  PIC 999.
            10  NUM-FIELD-LENGTH            PIC 9999.
            10  NUM-WINDOW-LENGTH           PIC 999.
            10  NUM-DIGITS                  PIC 999.
            10  NUM-DECIMALS                PIC 999.
            10  NUM-TOTAL                   PIC 999.
            10  NUM-COMP                    PIC 99.
            10  NUM-USE-DIGITS              PIC 999.
            10  NUM-FIELD                   PIC 999.

        01  WS-FLAGS.
            10  WS-SCREEN-TOO-SMALL-FLAG    PIC X VALUE 'N'.
                88  WS-SCREEN-TOO-SMALL     VALUE 'Y'.
            10  WS-REFRESH-NEEDED-FLAG      PIC X VALUE 'N'.
                88  WS-REFRESH-NEEDED       VALUE 'Y'
                    FALSE IS                'N'.
            10  WS-NUM-RO-FLAG              PIC X VALUE 'N'.
                88  WS-NUM-RO               VALUE 'Y'
                    FALSE IS                'N'.
            10  WS-VALID-MENU-FLAG          PIC X.
                88  VALID-MENU              VALUE 'Y'
                    FALSE IS                'N'.

        LINKAGE SECTION.

            COPY COBCURSG.

        01  SCREEN-RECORD.
            COPY SCREEN-01.

        01  SCREEN-IMAGE.
            COPY SCREEN-SI.

        01  SCN-IMAGE-CHANGED               PIC X.

        PROCEDURE DIVISION USING
            NC-COBCURSES,
            SCREEN-RECORD, SCREEN-IMAGE, SCN-IMAGE-CHANGED.

        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZATION.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5000-PROCESS
            END-IF.
            PERFORM 9000-FINALIZE.
            EXIT PROGRAM.

        1000-INITIALIZATION.
            MOVE LENGTH OF SCRFDEF-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                SCRFDEF-FILE-NAME, FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                CHARSET-FILE-NAME, FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                MENU-FILE-NAME, FILE-NAME-LENGTH.

            PERFORM 9010-OPEN-FILES.

      *>
      *>     PRIME THE DYNAMIC MENU WITH THE SCREEN NAME :
      *>
            CALL "COBCURSES-MENU-SD-FIELDS" USING "X", SCN-NAME.

            MOVE FNO-ACTION TO NC-FSEQ-STATE.
            PERFORM 1010-RECORD-INIT.
            PERFORM 1020-COBCURSES-INIT.
            MOVE FNO-ACTION TO NC-FSEQ-STATE.
            EXIT.

        1010-RECORD-INIT.
            INITIALIZE SCRFDEF-RECORD.
            INITIALIZE MENU-RECORD.
            SET VALID-MENU TO FALSE.
            PERFORM 4200-MOVE-TO-KEY.
            MOVE 'N' TO RCD-DEFINED, RCD-NEW, RCD-CHANGES.
            EXIT.

        1020-COBCURSES-INIT.
            PERFORM NC-INIT.
            PERFORM NC-CLEAR.
            PERFORM 1030-SCREEN-INIT.
            PERFORM 1040-DRAW-SCREEN.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                SET WS-SCREEN-TOO-SMALL TO TRUE
            END-IF.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        1030-SCREEN-INIT.
            COPY SD002040-PD.
            PERFORM 1040-DRAW-SCREEN.
            EXIT.

        1040-DRAW-SCREEN.
            PERFORM NC-DRAW-SCREEN.
            PERFORM 1050-DRAW-FIELDS.
            EXIT.

        1050-DRAW-FIELDS.
            MOVE SCN-NAME TO FLD-DSP-SCREEN-NAME.
            MOVE SCN-DESCRIPTION TO FLD-DSP-SCREEN-DESCRIPTION.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        4000-MOVE-TO-RECORD.
            MOVE FLD-COBOL-NAME TO SCR-FDEF-COBOL-NAME.
            MOVE FLD-FIELD-DESCRIPTION TO SCR-FDEF-DESCRIPTION.
            MOVE FLD-LINE-NO TO SCR-FDEF-LINE.
            MOVE FLD-COLUMN-NO TO SCR-FDEF-COLUMN.
            MOVE FLD-FIELD-LENGTH TO SCR-FDEF-BUFFER-LENGTH.
            MOVE FLD-WINDOW-LENGTH TO SCR-FDEF-WINDOW-LENGTH.
            MOVE FLD-CLEAR-OPTION TO SCR-FDEF-CLEAR.
            MOVE FLD-OPTION-UPPERCASE TO SCR-FDEF-UPPERCASE.
            MOVE FLD-MASK-OPTION TO SCR-FDEF-PASSWORD.
            MOVE FLD-NOT-BLANK-OPTION TO SCR-FDEF-NOT-BLANK.
            MOVE FLD-YN-FIELD TO SCR-FDEF-YN.
            MOVE FLD-USE-CHARSET TO SCR-FDEF-RES-CHARSET.
            MOVE FLD-SIGNED-OPTION TO SCR-FDEF-SIGNED.
            MOVE FLD-NUMERIC-DIGITS TO SCR-FDEF-DIGITS.
            MOVE FLD-NUMERIC-DECIMALS TO SCR-FDEF-DECIMALS.
            MOVE FLD-VERIFY-PROCEDURE TO SCR-FDEF-VERIFY.
            MOVE FLD-VISIBLE-OPTION TO SCR-FDEF-VISIBLE.
            MOVE FLD-IGNORE-CHANGES TO SCR-FDEF-IGNORE-CHANGES.
            MOVE FLD-INPUT-SEQUENCE TO SCR-FDEF-INPUT-SEQ.
            MOVE FLD-ACTION-OPTION TO SCR-FDEF-ACTION.
            MOVE FLD-HELP TO SCR-FDEF-HELP.
            MOVE FLD-READ-ONLY-OPTION TO SCR-FDEF-READ-ONLY.
            MOVE FLD-COMP-TYPE TO SCR-FDEF-COMP-TYPE.
            MOVE FLD-MENU-NAME TO SCR-FDEF-MENU-REF.
            MOVE FLD-ACTION-EDIT TO SCR-FDEF-ACTION-EDIT.
            EXIT.

        4100-MOVE-FROM-RECORD.
            MOVE SCR-FDEF-NO TO FLD-FIELD-NO.
            MOVE SCR-FDEF-COBOL-NAME TO FLD-COBOL-NAME.
            MOVE SCR-FDEF-DESCRIPTION TO FLD-FIELD-DESCRIPTION.
            MOVE SCR-FDEF-LINE TO FLD-LINE-NO.
            MOVE SCR-FDEF-COLUMN TO FLD-COLUMN-NO.
            MOVE SCR-FDEF-BUFFER-LENGTH TO FLD-FIELD-LENGTH.
            MOVE SCR-FDEF-WINDOW-LENGTH TO FLD-WINDOW-LENGTH.
            MOVE SCR-FDEF-CLEAR TO FLD-CLEAR-OPTION.
            MOVE SCR-FDEF-UPPERCASE TO FLD-OPTION-UPPERCASE.
            MOVE SCR-FDEF-PASSWORD TO FLD-MASK-OPTION.
            MOVE SCR-FDEF-NOT-BLANK TO FLD-NOT-BLANK-OPTION.
            MOVE SCR-FDEF-YN TO FLD-YN-FIELD.
            MOVE SCR-FDEF-RES-CHARSET TO FLD-USE-CHARSET.
            MOVE SCR-FDEF-SIGNED TO FLD-SIGNED-OPTION.
            MOVE SCR-FDEF-DIGITS TO FLD-NUMERIC-DIGITS.
            MOVE SCR-FDEF-DECIMALS TO FLD-NUMERIC-DECIMALS.
            MOVE SCR-FDEF-VERIFY TO FLD-VERIFY-PROCEDURE.
            MOVE SCR-FDEF-VISIBLE TO FLD-VISIBLE-OPTION.
            MOVE SCR-FDEF-IGNORE-CHANGES TO FLD-IGNORE-CHANGES.
            MOVE SCR-FDEF-INPUT-SEQ TO FLD-INPUT-SEQUENCE.
            MOVE SCR-FDEF-ACTION TO FLD-ACTION-OPTION.
            MOVE SCR-FDEF-HELP TO FLD-HELP.
            MOVE SCR-FDEF-READ-ONLY TO FLD-READ-ONLY-OPTION.
      *>     MIGRATE FROM PRIOR VERSIONS OF DB :
            IF NOT SCR-FDEF-COMP-TYPE NUMERIC THEN
                MOVE ZERO TO SCR-FDEF-COMP-TYPE
            END-IF.
            MOVE SCR-FDEF-COMP-TYPE TO FLD-COMP-TYPE.
            MOVE SCR-FDEF-MENU-REF TO FLD-MENU-NAME.
            MOVE SCR-FDEF-ACTION-EDIT TO FLD-ACTION-EDIT.
            EXIT.

        4200-MOVE-TO-KEY.
            INITIALIZE SCR-FDEF-KEY.
            MOVE SCN-NAME TO SCR-FDEF-SCREEN-NAME.
            MOVE FLD-FIELD-NO TO SCR-FDEF-NO.
            EXIT.

        4300-FIELD-DEFAULTS.
            MOVE 'Y' TO FLD-OPTION-UPPERCASE.
            MOVE 'N' TO FLD-MASK-OPTION.
            MOVE 'N' TO FLD-NOT-BLANK-OPTION.
            MOVE 'N' TO FLD-YN-FIELD.
            MOVE 'N' TO FLD-CLEAR-OPTION.
            MOVE 'N' TO FLD-ACTION-OPTION.
            MOVE 'Y' TO FLD-VISIBLE-OPTION.
            MOVE '0' TO FLD-WINDOW-LENGTH.
            MOVE 'N' TO FLD-SIGNED-OPTION.
            MOVE '0' TO FLD-NUMERIC-DIGITS, FLD-NUMERIC-DECIMALS.
            MOVE 'Y' TO FLD-INPUT-SEQUENCE.
            MOVE 'N' TO FLD-IGNORE-CHANGES.
            MOVE 'N' TO FLD-VERIFY-PROCEDURE.
            MOVE 'N' TO FLD-READ-ONLY-OPTION.
            MOVE ZERO TO FLD-COMP-TYPE.
            MOVE 'Y' TO FLD-ACTION-EDIT.
            EXIT.

        5000-PROCESS.
            PERFORM NC-FIELD-STATE-MACHINE.
            EXIT.

        5100-ACTION-C.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM NC-RESET-CHANGES.
            PERFORM 1010-RECORD-INIT.
            IF NC-FIELD-ACTION = 'N' THEN
                PERFORM 4300-FIELD-DEFAULTS
            END-IF.
            SET UNDEFINED-CHARSET TO TRUE.
            EXIT.

        5200-ACTION-N.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                PERFORM 5100-ACTION-C
                MOVE 1 TO NC-FSEQ-NEXT
            END-IF.
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
                IF FLD-LINE-NO = SPACES
                    MOVE '0' TO FLD-LINE-NO
                END-IF
                IF FLD-COLUMN-NO = SPACES
                    MOVE '0' TO FLD-COLUMN-NO
                END-IF
                IF FLD-SIGNED-OPTION = 'Y'
                    MOVE FLD-NUMERIC-DIGITS TO NUM-DIGITS
                    MOVE FLD-NUMERIC-DECIMALS TO NUM-DECIMALS
                    ADD NUM-DIGITS TO NUM-DECIMALS GIVING NUM-TOTAL
                    IF NUM-TOTAL < 1 THEN
                        MOVE 'N' TO FLD-SIGNED-OPTION
                    END-IF
                END-IF
                IF NEW-RECORD THEN
                    PERFORM 5410-ADD-RECORD
                ELSE
                    PERFORM 5420-UPDATE-RECORD
                END-IF
                PERFORM 4100-MOVE-FROM-RECORD
                PERFORM 6110-LOOKUP-MENU-REF
                IF SCR-FDEF-LINE < 1 OR SCR-FDEF-COLUMN < 1 THEN
                    MOVE "Record saved, but LINE/COLUMN undefined."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
            ELSE
                MOVE "No record to save." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5410-ADD-RECORD.
            PERFORM 4200-MOVE-TO-KEY.
            PERFORM 4000-MOVE-TO-RECORD.
            WRITE SCRFDEF-RECORD
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
            PERFORM 9030-FLUSH-FILES.
            EXIT.

        5420-UPDATE-RECORD.
            PERFORM 4200-MOVE-TO-KEY.
            PERFORM 4000-MOVE-TO-RECORD.
            REWRITE SCRFDEF-RECORD
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
            PERFORM 9030-FLUSH-FILES.
            EXIT.

        5500-ACTION-D.
            IF DEFINED-RECORD AND NOT NEW-RECORD THEN
                PERFORM 4200-MOVE-TO-KEY
                DELETE SCRFDEF-FILE
                    INVALID KEY
                        MOVE "Error: Unable to delete this record."
                            TO NC-MSGBUF
                        PERFORM NC-PUT-ERROR-OVERRIDE
                    NOT INVALID KEY
                        PERFORM 5100-ACTION-C
                        MOVE "The record was deleted." TO NC-MSGBUF
                        PERFORM NC-PUT-MESSAGE-OVERRIDE
                END-DELETE
                PERFORM 9030-FLUSH-FILES
            ELSE
                STRING "The current record has not been saved, "
                    "and thus cannot be deleted. Do a Clear."
                    INTO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5600-ACTION-F.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                STRING "Enter search value + CR, or cursor up/down",
                    "to browse." INTO NC-MSGBUF
                PERFORM NC-PUT-MESSAGE-OVERRIDE
                MOVE FSEQ-FIND TO NC-FSEQ-NEXT
                MOVE FNO-FIELD-NO TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT)
            END-IF.
            EXIT.

        5610-ACTION-F-CONTD.
            PERFORM 1010-RECORD-INIT
            PERFORM 4200-MOVE-TO-KEY
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
            START SCRFDEF-FILE KEY IS < SCR-FDEF-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

         5630-GE-SEARCH.
            START SCRFDEF-FILE KEY IS >= SCR-FDEF-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

         5640-GT-SEARCH.
            START SCRFDEF-FILE KEY IS > SCR-FDEF-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

         5650-NEXT-RECORD.
            IF DEFINED-RECORD THEN
                READ SCRFDEF-FILE NEXT RECORD
                    AT END 
                        PERFORM 5100-ACTION-C
                    NOT AT END 
                        IF SCR-FDEF-SCREEN-NAME = SCN-NAME THEN
                            PERFORM 5670-READ-CHARSET
                            PERFORM 4100-MOVE-FROM-RECORD
                            SET DEFINED-RECORD TO TRUE
                            MOVE 'N' TO RCD-NEW, RCD-CHANGES
                            PERFORM 6110-LOOKUP-MENU-REF
                        ELSE
                            PERFORM 5100-ACTION-C
                        END-IF
                END-READ
            ELSE
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        5670-READ-CHARSET.
            MOVE SCR-FDEF-RES-CHARSET TO CHARSET-NAME.
            IF CHARSET-NAME NOT = SPACES THEN
                PERFORM 6350-READ-CHARSET
                IF DEFINED-CHARSET THEN
                    MOVE CHARSET-DATA TO FLD-DSP-CHARSET
                ELSE
                    MOVE "*** BAD CHARSET ***" TO FLD-DSP-CHARSET
                END-IF
            ELSE
                MOVE SPACES TO FLD-DSP-CHARSET
                SET UNDEFINED-CHARSET TO TRUE
            END-IF.
            EXIT.

        5700-ACTION-P.
            PERFORM NC-CLEAR
            MOVE SCR-FDEF-LINE TO SCREEN-LAST-LINE
            MOVE SCR-FDEF-COLUMN TO SCREEN-LAST-COLUMN
            CALL "SD002020" USING
                NC-COBCURSES, SCREEN-IMAGE,
                SCN-IMAGE-CHANGED, SCN-STRIP-CHARACTER
            MOVE FLD-LINE-NO TO NUM-LINE
            MOVE FLD-COLUMN-NO TO NUM-COLUMN
            IF SCREEN-LAST-LINE NOT = NUM-LINE
                MOVE SCREEN-LAST-LINE TO FLD-LINE-NO
                MOVE 'Y' TO RCD-CHANGES
            END-IF
            IF SCREEN-LAST-COLUMN NOT = NUM-COLUMN
                MOVE SCREEN-LAST-COLUMN TO FLD-COLUMN-NO
                MOVE 'Y' TO RCD-CHANGES
            END-IF
            MOVE 'P' TO NC-FIELD-ACTION
            PERFORM 1040-DRAW-SCREEN
            EXIT.

        5900-ACTION-EDIT.
            IF NOT DEFINED-RECORD THEN
                MOVE "No current record to edit." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            ELSE
                IF NC-FIELD-EDIT-TARGET NOT = 'Y' THEN
                    MOVE "You cannot edit that field." TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                ELSE
                    MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
                    MOVE NC-FIELD-SEARCH
                        TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT)
                END-IF
            END-IF.
            EXIT.

        6000-LOOKUP-RECORD.
            PERFORM 4200-MOVE-TO-KEY
            READ SCRFDEF-FILE
                INVALID KEY
                    MOVE SPACES TO SCR-FDEF-SCREEN-NAME
                NOT INVALID KEY
                    PERFORM 5670-READ-CHARSET
                    PERFORM 4100-MOVE-FROM-RECORD
                    MOVE FNO-ACTION TO NC-FSEQ-NEXT
                    SET DEFINED-RECORD TO TRUE
                    MOVE 'N' TO RCD-NEW, RCD-CHANGES
                    PERFORM 6110-LOOKUP-MENU-REF
            END-READ.
            EXIT.

        6110-LOOKUP-MENU-REF.
            IF FLD-MENU-NAME NOT = SPACES THEN
                INITIALIZE MENU-RECORD
                MOVE FLD-MENU-NAME TO MNU-MENU-NAME
                READ MENU-FILE
                    INVALID KEY
                        SET VALID-MENU TO FALSE
                        MOVE "*** UNKNOWN MENU ***"
                            TO FLD-DSP-MENU-TITLE
                    NOT INVALID KEY
                        SET VALID-MENU TO TRUE
                        MOVE MNU-TITLE TO FLD-DSP-MENU-TITLE
                END-READ
            ELSE
                SET VALID-MENU TO FALSE
                MOVE SPACES TO FLD-DSP-MENU-TITLE
            END-IF.
            EXIT.

        6100-UPDATE-SCREEN.
            PERFORM 1050-DRAW-FIELDS.
            SET WS-REFRESH-NEEDED TO FALSE.
            EXIT.

        6200-VERIFY-CHARSET.
            IF NC-FIELD-EXIT-CD THEN
                PERFORM 6400-FIND-NEXT
                MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
            END-IF.
            IF NC-FIELD-EXIT-CU THEN
                PERFORM 6500-FIND-PREV
                MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
            END-IF.
            IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB
                PERFORM 6300-LOOKUP-CHARSET
                IF DEFINED-CHARSET THEN
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                    MOVE CHARSET-DATA TO FLD-DSP-CHARSET
                    MOVE FNO-ACTION TO NC-FSEQ-NEXT
                ELSE
                    MOVE SPACES TO FLD-DSP-CHARSET
                END-IF
            END-IF.
            IF NC-FIELD-EXIT-SLASH OR FLD-USE-CHARSET = SPACES
                MOVE SPACES TO FLD-USE-CHARSET, FLD-DSP-CHARSET
                MOVE 'Y' TO NC-FIELD-VERIFIED
                MOVE FNO-ACTION TO NC-FSEQ-NEXT
            END-IF.
            IF NC-FIELD-EXIT-DOT
                MOVE 'Y' TO NC-FIELD-VERIFIED
                MOVE FNO-ACTION TO NC-FSEQ-NEXT
            END-IF.
            EXIT.

        6300-LOOKUP-CHARSET.
            MOVE FLD-USE-CHARSET TO CHARSET-NAME.
            PERFORM 6350-READ-CHARSET.
            EXIT.

        6350-READ-CHARSET.
            READ CHARSET-FILE
                INVALID KEY
                    SET UNDEFINED-CHARSET TO TRUE
                NOT INVALID KEY
                    SET DEFINED-CHARSET TO TRUE
            END-READ.
            EXIT.

        6400-FIND-NEXT.
            MOVE FLD-USE-CHARSET TO CHARSET-NAME.
            START CHARSET-FILE KEY IS > CHARSET-NAME
                INVALID KEY
                    SET UNDEFINED-CHARSET TO TRUE
                NOT INVALID KEY
                    SET DEFINED-CHARSET TO TRUE
                    PERFORM 6600-READ-NEXT
                    MOVE 'Y' TO NC-FIELD-VERIFIED
            END-START.
            EXIT.

        6500-FIND-PREV.
            MOVE FLD-USE-CHARSET TO CHARSET-NAME.
            START CHARSET-FILE KEY IS < CHARSET-NAME
                INVALID KEY
                    SET UNDEFINED-CHARSET TO TRUE
                NOT INVALID KEY
                    SET DEFINED-CHARSET TO TRUE
                    PERFORM 6600-READ-NEXT
                    MOVE 'Y' TO NC-FIELD-VERIFIED
            END-START.
            EXIT.

        6600-READ-NEXT.
            READ CHARSET-FILE NEXT RECORD
                AT END
                    SET UNDEFINED-CHARSET TO TRUE
                    MOVE SPACES TO FLD-USE-CHARSET, FLD-DSP-CHARSET
                NOT AT END
                    SET DEFINED-CHARSET TO TRUE
                    MOVE CHARSET-NAME TO FLD-USE-CHARSET
                    MOVE CHARSET-DATA TO FLD-DSP-CHARSET
            END-READ.
            EXIT.

        6700-VERIFY-COMP-TYPE.
            MOVE 99 TO NUM-COMP.
            MOVE FLD-COMP-TYPE TO NUM-COMP.
            EVALUATE NUM-COMP
                WHEN 00
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                WHEN 01
                    PERFORM 6800-COMP-ADJUST
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                WHEN 02
                    PERFORM 6800-COMP-ADJUST
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                WHEN OTHER
                    MOVE "00" TO FLD-COMP-TYPE
            END-EVALUATE.
            EXIT.

        6705-SET-RO-FIELDS.
            MOVE 99 TO NUM-COMP.
            MOVE FLD-COMP-TYPE TO NUM-COMP.
            EVALUATE NUM-COMP
                WHEN 00
                    SET WS-NUM-RO TO FALSE
                WHEN 01
                    SET WS-NUM-RO TO TRUE
                WHEN 02
                    SET WS-NUM-RO TO TRUE
                WHEN OTHER
                    SET WS-NUM-RO TO FALSE
            END-EVALUATE.
            PERFORM 6710-SET-RO.

            IF DEFINED-RECORD THEN
                IF FLD-READ-ONLY-OPTION = 'Y' THEN
                    IF FLD-INPUT-SEQUENCE NOT = 'N' THEN
                        MOVE 'N' TO FLD-INPUT-SEQUENCE
                        SET WS-REFRESH-NEEDED TO TRUE
                        SET UNSAVED-CHANGES TO TRUE
                    END-IF
                END-IF
                IF FLD-ACTION-OPTION = 'Y' OR FLD-READ-ONLY-OPTION = 'Y'
                    IF FLD-ACTION-EDIT NOT = 'N' THEN
                        MOVE 'N' TO FLD-ACTION-EDIT
                        SET WS-REFRESH-NEEDED TO TRUE
                        SET UNSAVED-CHANGES TO TRUE
                    END-IF
                END-IF
            END-IF.

            PERFORM 6100-UPDATE-SCREEN.
            EXIT.

        6710-SET-RO.
            MOVE FNO-SIGNED-OPTION TO NUM-FIELD
            PERFORM 6900-SET-READ-ONLY.
            MOVE FNO-NUMERIC-DECIMALS TO NUM-FIELD
            PERFORM 6900-SET-READ-ONLY.
            EXIT.

        6800-COMP-ADJUST.
            MOVE FLD-NUMERIC-DIGITS TO NUM-DIGITS.
            EVALUATE NUM-COMP
                WHEN 01
                    MOVE 6 TO NUM-DIGITS
                WHEN 02
                    MOVE 15 TO NUM-DIGITS
            END-EVALUATE
            MOVE NUM-DIGITS TO FLD-NUMERIC-DIGITS
            MOVE "000" TO FLD-NUMERIC-DECIMALS
            MOVE "Y" TO FLD-SIGNED-OPTION
            EXIT.

        6900-SET-READ-ONLY.
            IF WS-NUM-RO THEN
                MOVE 'Y' TO NC-FDESC-READ-ONLY(NUM-FIELD)
                MOVE 'Y' TO NC-FDESC-COLOUR-FLAG(NUM-FIELD)
                MOVE NC-TITLE-PAIR TO NC-FDESC-COLOUR-PAIR(NUM-FIELD)
            ELSE
                MOVE 'N' TO NC-FDESC-READ-ONLY(NUM-FIELD)
                MOVE 'N' TO NC-FDESC-COLOUR-FLAG(NUM-FIELD)
            END-IF.
            EXIT.

        7000-ACTION-EVENT.
            IF NC-FIELD-ACTION = 'C' THEN
                PERFORM 5100-ACTION-C
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'N' THEN
                PERFORM 5200-ACTION-N
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'E' THEN
                PERFORM 5300-ACTION-E
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'S' THEN
                PERFORM 5400-ACTION-S
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'D' THEN
                PERFORM 5500-ACTION-D
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'F' THEN
                PERFORM 5600-ACTION-F
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'P' THEN
                PERFORM 5700-ACTION-P
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = SPACES
                PERFORM 5900-ACTION-EDIT
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF FLD-ACTION NOT = SPACES THEN
                PERFORM 8000-ERROR-ACTION
            END-IF.
            EXIT.

        7100-CHANGE-EVENT.
            IF NC-FIELD-NUMBER < FNO-ACTION
            AND NC-FSEQ-STATE NOT = FSEQ-FIND
                SET UNSAVED-CHANGES TO TRUE
            END-IF.
      *>
      *>     IF USER SPECIFIES FIELD IS A Y/N FIELD, SET THE LENGTH
      *>     AND WINDOW OF THE FIELD TO 1
      *>
            IF NC-FIELD-NUMBER = FNO-YN-FIELD AND FLD-YN-FIELD = 'Y'
                MOVE '1' TO FLD-FIELD-LENGTH, FLD-WINDOW-LENGTH
                MOVE 'N' TO FLD-SIGNED-OPTION
                MOVE '0' TO FLD-NUMERIC-DIGITS, FLD-NUMERIC-DECIMALS
            END-IF.
            EXIT.

        7150-ENFORCE-NUM-PARAMS.
            IF FLD-SIGNED-OPTION NOT = 'Y' THEN
                MOVE 'Y' TO FLD-SIGNED-OPTION
                SET WS-REFRESH-NEEDED TO TRUE
            END-IF.

            MOVE ZERO TO NUM-DIGITS.
            MOVE FLD-NUMERIC-DIGITS TO NUM-DIGITS
            IF NUM-DIGITS < 1 OR NUM-DIGITS > NUM-USE-DIGITS THEN
                MOVE NUM-USE-DIGITS TO FLD-NUMERIC-DIGITS
                SET WS-REFRESH-NEEDED TO TRUE
            END-IF

            MOVE ZERO TO NUM-DECIMALS
            MOVE FLD-NUMERIC-DECIMALS TO NUM-DECIMALS
            IF NUM-DECIMALS > 0 THEN
                MOVE "0" TO FLD-NUMERIC-DECIMALS
                SET WS-REFRESH-NEEDED TO TRUE
            END-IF.
            EXIT.

        7160-VERIFY-NUMERIC-PARMS.
            MOVE 99 TO NUM-COMP.
            MOVE FLD-COMP-TYPE TO NUM-COMP.
            EVALUATE NUM-COMP
                WHEN 01
                    PERFORM 7170-COMP-X
                WHEN 02
                    PERFORM 7170-COMP-X
                WHEN OTHER
                    PERFORM 7170-NON-COMP
            END-EVALUATE.
            EXIT.

        7170-COMP-X.
            MOVE 99 TO NUM-COMP.
            MOVE FLD-COMP-TYPE TO NUM-COMP.
            EVALUATE NUM-COMP
                WHEN 01
                    MOVE 6 TO NUM-USE-DIGITS
                    PERFORM 7150-ENFORCE-NUM-PARAMS
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                WHEN 02
                    MOVE 16 TO NUM-USE-DIGITS
                    PERFORM 7150-ENFORCE-NUM-PARAMS
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                WHEN OTHER
                    CONTINUE
            END-EVALUATE.
            EXIT.

        7170-NON-COMP.
            MOVE ZERO TO NUM-DIGITS, NUM-DECIMALS.
            MOVE FLD-NUMERIC-DIGITS TO NUM-DIGITS.
            MOVE FLD-NUMERIC-DECIMALS TO NUM-DECIMALS.
            ADD NUM-DIGITS TO NUM-DECIMALS GIVING NUM-TOTAL.
            IF NUM-TOTAL > 18 THEN
                MOVE "Digits + Decimals cannot exceed 18."
                    TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            ELSE
                IF FLD-YN-FIELD = 'Y'
      *>
      *>             A FIELD CANNOT BE BOTH A Y/N AND A NUMERIC
      *>
                    MOVE 'N' TO FLD-YN-FIELD
                    SET WS-REFRESH-NEEDED TO TRUE
                END-IF
                MOVE 'Y' TO NC-FIELD-VERIFIED
            END-IF.
            EXIT.

        7200-VERIFY-EVENT.
            IF NC-FIELD-NUMBER = FNO-FIELD-NO
                IF NC-FSEQ-STATE = FNO-FIELD-NO
      *>             Input sequence requires non-blank name
                    IF FLD-FIELD-NO = SPACES
                        MOVE 'N' TO NC-FIELD-VERIFIED
                    ELSE
                        MOVE 'Y' TO NC-FIELD-VERIFIED
                    END-IF
                ELSE
      *>             Blank name allowed in Find mode
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                END-IF
            END-IF.

            IF NC-FIELD-NUMBER = FNO-COBOL-NAME
      *>        FIX ME
                MOVE 'Y' TO NC-FIELD-VERIFIED
            END-IF.

            IF NC-FIELD-NUMBER = FNO-FIELD-LENGTH THEN
                MOVE FLD-FIELD-LENGTH TO NUM-FIELD-LENGTH
                IF NUM-FIELD-LENGTH >= 1 AND NUM-FIELD-LENGTH <= 9999
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                END-IF
            END-IF.

            IF NC-FIELD-NUMBER = FNO-LINE-NO THEN
                MOVE FLD-LINE-NO TO NUM-LINE
                IF NUM-LINE < SCN-LINES-MIN
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                ELSE
                    MOVE "Line # too large." TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
            END-IF.

            IF NC-FIELD-NUMBER = FNO-COLUMN-NO THEN
                MOVE FLD-COLUMN-NO TO NUM-COLUMN
                IF NUM-COLUMN < SCN-COLUMNS-MIN
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                ELSE
                    MOVE "Column # too large." TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
            END-IF.

            IF NC-FIELD-NUMBER = FNO-WINDOW-LENGTH THEN
                MOVE FLD-FIELD-LENGTH TO NUM-FIELD-LENGTH
                MOVE FLD-WINDOW-LENGTH TO NUM-WINDOW-LENGTH
                IF NUM-WINDOW-LENGTH <= NUM-FIELD-LENGTH
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                ELSE
                    STRING "Window size cannot be greater than the ",
                        "field's buffer length." INTO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
            END-IF.

            IF NC-FIELD-NUMBER = FNO-NUMERIC-DIGITS
            OR NC-FIELD-NUMBER = FNO-NUMERIC-DECIMALS
            OR NC-FIELD-NUMBER = FNO-SIGNED-OPTION THEN
                PERFORM 7160-VERIFY-NUMERIC-PARMS
            END-IF.

            IF NC-FIELD-NUMBER = FNO-USE-CHARSET
                PERFORM 6200-VERIFY-CHARSET
            END-IF.

            IF NC-FIELD-NUMBER = FNO-READ-ONLY-OPTION THEN
                IF FLD-READ-ONLY-OPTION = 'Y' THEN
                    MOVE 'N' TO FLD-INPUT-SEQUENCE, FLD-ACTION-OPTION,
                        FLD-NOT-BLANK-OPTION, FLD-YN-FIELD,
                        FLD-CLEAR-OPTION, FLD-SIGNED-OPTION,
                        FLD-MASK-OPTION
                    MOVE ZERO TO FLD-NUMERIC-DIGITS, 
                        FLD-NUMERIC-DECIMALS
                    MOVE SPACES TO FLD-USE-CHARSET, FLD-DSP-CHARSET
                END-IF
                MOVE 'Y' TO NC-FIELD-VERIFIED
            END-IF.

            IF NC-FIELD-NUMBER = FNO-COMP-TYPE THEN
                PERFORM 6700-VERIFY-COMP-TYPE
            END-IF.

            IF NC-FIELD-NUMBER = FNO-MENU-NAME THEN
                PERFORM 8100-VERIFY-MENU-NAME
            END-IF.
            EXIT.

        7500-FIELD-EVENT.
            IF NC-FIELD-NUMBER = FNO-ACTION
                PERFORM 7000-ACTION-EVENT
            END-IF.

            IF NC-FSEQ-STATE = FNO-FIELD-NO
                PERFORM 6000-LOOKUP-RECORD
            END-IF.

            IF NC-FSEQ-STATE = FNO-HELP
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
            PERFORM 6705-SET-RO-FIELDS.
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

        8100-VERIFY-MENU-NAME.
            IF NC-FIELD-EXIT-CD THEN
                PERFORM 8400-FIND-NEXT
                MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
            END-IF.
            IF NC-FIELD-EXIT-CU THEN
                PERFORM 8500-FIND-PREV
                MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
            END-IF.
            IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB
                PERFORM 6110-LOOKUP-MENU-REF
                IF VALID-MENU OR FLD-MENU-NAME = SPACES THEN
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                    MOVE FNO-ACTION TO NC-FSEQ-NEXT
                END-IF
            END-IF.
            IF NC-FIELD-EXIT-SLASH OR NC-FIELD-EXIT-DOT THEN
                MOVE SCR-FDEF-MENU-REF TO FLD-MENU-NAME
                PERFORM 6110-LOOKUP-MENU-REF
                MOVE 'Y' TO NC-FIELD-VERIFIED
                MOVE FNO-ACTION TO NC-FSEQ-NEXT
            END-IF.
            EXIT.

        8300-READ-NEXT.
            READ MENU-FILE NEXT RECORD
                AT END
                    SET VALID-MENU TO FALSE
                    MOVE SPACES TO FLD-MENU-NAME, FLD-DSP-MENU-TITLE
                NOT AT END
                    SET VALID-MENU TO TRUE
                    MOVE MNU-MENU-NAME TO FLD-MENU-NAME
                    MOVE MNU-TITLE TO FLD-DSP-MENU-TITLE
            END-READ.
            EXIT.

        8400-FIND-NEXT.
            INITIALIZE MENU-RECORD.
            MOVE FLD-MENU-NAME TO MNU-MENU-NAME.
            START MENU-FILE KEY IS > MNU-KEY
                INVALID KEY
                    SET VALID-MENU TO FALSE
                    MOVE SPACES TO FLD-MENU-NAME, FLD-DSP-MENU-TITLE
                NOT INVALID KEY
                    SET VALID-MENU TO TRUE
                    PERFORM 8300-READ-NEXT
                    MOVE 'Y' TO NC-FIELD-VERIFIED
            END-START.
            EXIT.

        8500-FIND-PREV.
            INITIALIZE MENU-RECORD.
            MOVE FLD-MENU-NAME TO MNU-MENU-NAME.
            IF MNU-MENU-NAME = SPACES THEN
                MOVE HIGH-VALUES TO MNU-MENU-NAME
            END-IF.
            START MENU-FILE KEY IS < MNU-KEY
                INVALID KEY
                    SET VALID-MENU TO FALSE
                    MOVE SPACES TO FLD-MENU-NAME, FLD-DSP-MENU-TITLE
                NOT INVALID KEY
                    SET VALID-MENU TO TRUE
                    PERFORM 8300-READ-NEXT
                    MOVE 'Y' TO NC-FIELD-VERIFIED
            END-START.
            EXIT.

        9000-FINALIZE.
            PERFORM 9020-CLOSE-FILES.
            PERFORM NC-CLEAR.
            PERFORM NC-FINALIZE.
            EXIT.

        9010-OPEN-FILES.
            OPEN I-O SCRFDEF-FILE.
            OPEN INPUT CHARSET-FILE, MENU-FILE.
            EXIT.

        9020-CLOSE-FILES.
            CLOSE SCRFDEF-FILE, CHARSET-FILE, MENU-FILE.
            EXIT.

        9030-FLUSH-FILES.
            PERFORM 9020-CLOSE-FILES.
            PERFORM 9010-OPEN-FILES.
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

        END PROGRAM SD002040.
