        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002080.
      *>
      *> THIS IS THE MENU HEADER MAINTENANCE SCREEN, WHERE MENUS CAN
      *> BE ADDED, UPDATED, OR DELETED.
      *>
      *> INPUTS :
      *> 
      *>     NONE
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
            SELECT MENU-FILE
                ASSIGN TO MENU-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS MNU-MENU-NAME.

        DATA DIVISION.
        FILE SECTION.

        FD  MENU-FILE.
        01  MENU-RECORD.
            COPY MENURECD.

        WORKING-STORAGE SECTION.

            COPY COBCRETC.
            COPY COBCATTR.
            COPY COBCCOLOUR.
            COPY COBCURSL.

            COPY SD002080-WS.

        01  FILE-NAMES.
            10  FILE-NAME-LENGTH            PIC 9999.
            10  MENU-FILE-NAME              PIC X(256)
                VALUE "${COBCURSES_DATADIR}/MENUS.X".

        01  FLAGS.
            10  WS-SCREEN-TOO-SMALL-FLAG    PIC X VALUE 'N'.
                88  WS-SCREEN-TOO-SMALL     VALUE 'Y'.

        01  WORK-AREA.
            10  WS-NUMBER                   PIC 999.

        01  RECORD-STATE.
            10  RCD-DEFINED-FLAG            PIC X.
                88  DEFINED-RECORD          VALUE 'Y'
                    FALSE IS                'N'.
            10  RCD-NEW-FLAG                PIC X.
                88  NEW-RECORD              VALUE 'Y'
                    FALSE IS                'N'.
            10  RCD-CHANGES-FLAG            PIC X.
                88  UNSAVED-CHANGES         VALUE 'Y'
                    FALSE IS                'N'.

        LINKAGE SECTION.
        COPY COBCURSG.

        01  LS-LINE                         PIC 999 COMP.
        01  LS-COLUMN                       PIC 999 COMP.

        PROCEDURE DIVISION
            USING NC-COBCURSES, LS-LINE, LS-COLUMN.

        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZATION.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5000-PROCESS
            END-IF.            
            PERFORM 9000-FINALIZE.
            GOBACK.

        1000-INITIALIZATION.
            MOVE LENGTH OF MENU-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING MENU-FILE-NAME, FILE-NAME-LENGTH.

            PERFORM 9010-OPEN-FILES.

            PERFORM 1020-COBCURSES-INIT.
            PERFORM 1010-RECORD-INIT.
            MOVE FNO-ACTION TO NC-FSEQ-STATE.
            EXIT.

        1010-RECORD-INIT.
            SET DEFINED-RECORD TO FALSE.
            SET NEW-RECORD TO FALSE.
            SET UNSAVED-CHANGES TO FALSE.
            EXIT.

        1020-COBCURSES-INIT.
            PERFORM NC-INIT.
            PERFORM NC-CLEAR.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM 1030-SCREEN-INIT.
            EXIT.

        1030-SCREEN-INIT.
            COPY SD002080-PD.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM 1035-UPDATE-POINT.
            PERFORM 1040-DRAW-SCREEN.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                SET WS-SCREEN-TOO-SMALL TO TRUE
            END-IF.
            EXIT.

        1035-UPDATE-POINT.
            MOVE LS-LINE TO WS-NUMBER.
            MOVE WS-NUMBER TO FLD-POINT-LINE.
            MOVE LS-COLUMN TO WS-NUMBER.
            MOVE WS-NUMBER TO FLD-POINT-COLUMN.
            EXIT.

        1040-DRAW-SCREEN.
            PERFORM NC-DRAW-SCREEN.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        4000-MOVE-TO-RECORD.
            MOVE FLD-MENU-NAME TO MNU-MENU-NAME.
            MOVE FLD-MENU-TYPE TO MNU-MENU-TYPE.
            MOVE FLD-MENU-TITLE TO MNU-TITLE.
            MOVE FLD-TOP-LEFT-LINE-NO TO MNU-TOP-LEFT-LINE-NO.
            MOVE FLD-TOP-LEFT-COLUMN-NO TO MNU-TOP-LEFT-COLUMN-NO.
            MOVE FLD-OPT-ONEVALUE TO MNU-OPT-ONEVALUE.
            MOVE FLD-OPT-ROWMAJOR TO MNU-OPT-ROWMAJOR.
            MOVE FLD-OPT-IGNORECASE TO MNU-OPT-IGNORECASE.
            MOVE FLD-OPT-SHOWDESC TO MNU-OPT-SHOWDESC.
            MOVE FLD-FMT-ROWS TO MNU-OPT-ROWS.
            MOVE FLD-FMT-COLS TO MNU-OPT-COLS.
            MOVE FLD-MODULE-NAME TO MNU-MODULE-NAME.
            MOVE FLD-ITEM-LIMIT TO MNU-ITEM-LIMIT.
            EXIT.

        4100-MOVE-FROM-RECORD.
            MOVE MNU-MENU-NAME TO FLD-MENU-NAME.
            MOVE MNU-MENU-TYPE TO FLD-MENU-TYPE.
            MOVE MNU-TITLE TO FLD-MENU-TITLE.
            MOVE MNU-TOP-LEFT-LINE-NO TO FLD-TOP-LEFT-LINE-NO.
            MOVE MNU-TOP-LEFT-COLUMN-NO TO FLD-TOP-LEFT-COLUMN-NO.
            MOVE MNU-OPT-ONEVALUE TO FLD-OPT-ONEVALUE.
            MOVE MNU-OPT-ROWMAJOR TO FLD-OPT-ROWMAJOR.
            MOVE MNU-OPT-IGNORECASE TO FLD-OPT-IGNORECASE.
            MOVE MNU-OPT-SHOWDESC TO FLD-OPT-SHOWDESC.
            MOVE MNU-OPT-ROWS TO FLD-FMT-ROWS.
            MOVE MNU-OPT-COLS TO FLD-FMT-COLS.
            MOVE MNU-MODULE-NAME TO FLD-MODULE-NAME.
            MOVE MNU-ITEM-LIMIT TO FLD-ITEM-LIMIT.
            EXIT.

        5000-PROCESS.
            PERFORM NC-FIELD-STATE-MACHINE.
            EXIT.

        5100-ACTION-C.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM NC-RESET-CHANGES.
            PERFORM 1010-RECORD-INIT.
            PERFORM 1035-UPDATE-POINT.
            EXIT.

        5200-ACTION-N.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                PERFORM 5100-ACTION-C
                MOVE 1 TO NC-FSEQ-NEXT
                MOVE 'Y' TO NC-FDESC-NOT-BLANK(FNO-MENU-NAME)
            END-IF.
            EXIT.

        5300-ACTION-E.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                PERFORM 5100-ACTION-C
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
            ELSE
                MOVE "No record to save." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5410-ADD-RECORD.
            PERFORM 4000-MOVE-TO-RECORD.
            WRITE MENU-RECORD
                INVALID KEY
                    MOVE "Error: Adding a new menu record."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                NOT INVALID KEY
                    SET NEW-RECORD TO FALSE
                    SET UNSAVED-CHANGES TO FALSE
                    MOVE "A new record was added (saved)."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-MESSAGE-OVERRIDE
            END-WRITE.
            PERFORM 9030-FLUSH-FILES.
            EXIT.

        5420-UPDATE-RECORD.
            PERFORM 4000-MOVE-TO-RECORD.
            REWRITE MENU-RECORD
                INVALID KEY
                    MOVE "Error: Updating the menu record."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                NOT INVALID KEY
                    SET UNSAVED-CHANGES TO FALSE
                    MOVE "Your record was updated (saved)."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-MESSAGE-OVERRIDE
            END-REWRITE.
            PERFORM 9030-FLUSH-FILES.
            EXIT.

        5500-ACTION-D.
            IF DEFINED-RECORD AND NOT NEW-RECORD THEN
                INITIALIZE MENU-RECORD
                MOVE FLD-MENU-NAME TO MNU-MENU-NAME
                DELETE MENU-FILE
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
                IF NC-FIELD-SEARCH <= FNO-MENU-NAME THEN
                    PERFORM 5605-ACTION-F-START
                ELSE
                    MOVE "You can only search from the Menu Name field"
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
            END-IF.
            EXIT.

        5605-ACTION-F-START.
            STRING "Enter search value + CR, or cursor up/down",
                "to browse." INTO NC-MSGBUF
            PERFORM NC-PUT-MESSAGE-OVERRIDE
            MOVE FSEQ-FIND TO NC-FSEQ-NEXT
            MOVE FNO-MENU-NAME TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT)
            MOVE 'N' TO NC-FDESC-NOT-BLANK(FNO-MENU-NAME)
            EXIT.

        5610-ACTION-F-CONTD.
            PERFORM 1010-RECORD-INIT.
            INITIALIZE MENU-RECORD.
            MOVE FLD-MENU-NAME TO MNU-MENU-NAME.
            IF NC-FIELD-EXIT-CU THEN
                PERFORM 5620-LT-SEARCH
            END-IF.
            IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB THEN
                PERFORM 5630-GE-SEARCH
            END-IF.
            IF NC-FIELD-EXIT-CD THEN
                PERFORM 5640-GT-SEARCH
            END-IF.
            EXIT.

        5620-LT-SEARCH.
            START MENU-FILE KEY IS < MNU-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5630-GE-SEARCH.
            START MENU-FILE KEY IS >= MNU-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5640-GT-SEARCH.
            START MENU-FILE KEY IS > MNU-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5650-NEXT-RECORD.
            IF DEFINED-RECORD THEN
                READ MENU-FILE NEXT RECORD
                    AT END 
                        PERFORM 5100-ACTION-C
                    NOT AT END 
                        PERFORM 5670-LOAD-RECORD
                END-READ
            ELSE
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        5670-LOAD-RECORD.
            PERFORM 4100-MOVE-FROM-RECORD.
            SET DEFINED-RECORD TO TRUE.
            SET UNSAVED-CHANGES TO FALSE.
            SET NEW-RECORD TO FALSE.
            EXIT.

        5700-ACTION-P.
            IF LS-LINE > 0 AND LS-COLUMN > 0 THEN
                IF DEFINED-RECORD THEN
                    MOVE LS-LINE TO WS-NUMBER
                    MOVE WS-NUMBER TO FLD-TOP-LEFT-LINE-NO
                    MOVE LS-COLUMN TO WS-NUMBER
                    MOVE WS-NUMBER TO FLD-TOP-LEFT-COLUMN-NO
                    SET UNSAVED-CHANGES TO TRUE
                ELSE
                    MOVE "Error: You have no current record defined."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
            ELSE
                INITIALIZE NC-MSGBUF
                STRING "Error: You'll need to visit the 'Paint'"
                    " function in the main screen first."
                    INTO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5800-ACTION-I.
            IF UNSAVED-CHANGES THEN
                PERFORM 8020-MUST-SAVE
            ELSE
                IF NOT DEFINED-RECORD THEN
                    MOVE "You must pick a defined Menu Name first."
                        TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                ELSE
                    PERFORM 5810-EDIT-ITEMS
                END-IF
            END-IF.
            EXIT.

        5810-EDIT-ITEMS.
            PERFORM 9020-CLOSE-FILES.
            CALL "SD002090" USING NC-COBCURSES, MENU-RECORD.
            PERFORM 9010-OPEN-FILES.
            PERFORM NC-CLEAR.
            PERFORM 1040-DRAW-SCREEN.
            EXIT.

        5900-ACTION-EDIT.
            IF NOT DEFINED-RECORD THEN
                MOVE "There is no current record to edit" TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            ELSE
                PERFORM 5910-ACTION-EDIT
            END-IF.
            EXIT.
    
        5910-ACTION-EDIT.
            EVALUATE NC-FIELD-SEARCH
            WHEN 9
                MOVE FSEQ-MENU-FORMAT TO NC-FSEQ-NEXT
            WHEN 20
                MOVE FSEQ-MENU-TYPE TO NC-FSEQ-NEXT
            WHEN OTHER
                PERFORM 5920-ACTION-EDIT-NORMAL
            END-EVALUATE.
            EXIT.

        5920-ACTION-EDIT-NORMAL.
            IF NC-FIELD-EDIT-TARGET NOT = 'Y' THEN
                MOVE "You cannot edit that field." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            ELSE
                MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
                MOVE NC-FIELD-SEARCH
                    TO NC-FSEQ-FIELD-NO(FSEQ-EDIT)
            END-IF.
            EXIT.

        6100-UPDATE-SCREEN.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        6150-VERIFY-MENU-NAME.
            INITIALIZE MENU-RECORD.
            MOVE FLD-MENU-NAME TO MNU-MENU-NAME.
            READ MENU-FILE
                INVALID KEY
                    CONTINUE                    
                NOT INVALID KEY
                    PERFORM 5670-LOAD-RECORD
            END-READ.
            IF NC-FSEQ-STATE = FSEQ-MENU-NAME AND DEFINED-RECORD THEN
                MOVE FSEQ-EDIT TO NC-FSEQ-NEXT
            END-IF.
            MOVE 'Y' TO NC-FIELD-VERIFIED.
            EXIT.

        6200-VERIFY-LINE-NO.
            MOVE FLD-TOP-LEFT-LINE-NO TO WS-NUMBER.
            IF WS-NUMBER > 0 AND WS-NUMBER <= 50 THEN
                MOVE 'Y' TO NC-FIELD-VERIFIED
            END-IF.
            EXIT.

        6300-VERIFY-COL-NO.
            MOVE FLD-TOP-LEFT-COLUMN-NO TO WS-NUMBER.
            IF WS-NUMBER > 0 AND WS-NUMBER <= 150 THEN
                MOVE 'Y' TO NC-FIELD-VERIFIED
            END-IF.
            IF NC-FSEQ-STATE = FNO-TOP-LEFT-COLUMN-NO THEN
                SET UNSAVED-CHANGES TO TRUE
                SET NEW-RECORD TO TRUE
                SET DEFINED-RECORD TO TRUE
            END-IF.
            EXIT.

        7000-ACTION-EVENT.
            IF NC-FIELD-ACTION = SPACES
                PERFORM 5900-ACTION-EDIT
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'C' THEN
                PERFORM 5100-ACTION-C
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'N' THEN
                PERFORM 5200-ACTION-N
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

            IF NC-FIELD-ACTION = 'I' THEN
                PERFORM 5800-ACTION-I
                MOVE SPACES TO FLD-ACTION, NC-FIELD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'E' THEN
                PERFORM 5300-ACTION-E
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF FLD-ACTION NOT = SPACES THEN
                PERFORM 8000-ERROR-ACTION
            END-IF.
            EXIT.

        7500-FIELD-EVENT.
            IF NC-FSEQ-STATE = FNO-MENU-TITLE THEN
                MOVE 'Y' TO FLD-OPT-ONEVALUE, FLD-OPT-ROWMAJOR,
                    FLD-OPT-IGNORECASE, FLD-OPT-SHOWDESC
                MOVE "01" TO FLD-MENU-TYPE
                MOVE ZERO TO FLD-ITEM-LIMIT, FLD-FMT-ROWS, FLD-FMT-COLS
                SET DEFINED-RECORD TO TRUE
                SET NEW-RECORD TO TRUE
                SET UNSAVED-CHANGES TO TRUE
            END-IF

            IF NC-FSEQ-STATE = FSEQ-FIND
                PERFORM 5610-ACTION-F-CONTD
                IF NC-FIELD-EXIT-CD OR NC-FIELD-EXIT-CU THEN
                    MOVE FSEQ-FIND TO NC-FSEQ-NEXT
                END-IF
            END-IF.

            IF NC-FIELD-NUMBER = FNO-ACTION
                PERFORM 7000-ACTION-EVENT
            END-IF.
            EXIT.

        7600-MOUSE-EVENT.
            PERFORM 5900-ACTION-EDIT.
            EXIT.

        7700-CHANGE-EVENT.
            IF NC-FIELD-NUMBER >= FNO-MENU-TITLE
            AND NC-FIELD-NUMBER <= FNO-TOP-LEFT-COLUMN-NO THEN
                SET UNSAVED-CHANGES TO TRUE
            END-IF.
            EXIT.

        7800-STATE-CHANGE-EVENT.
            PERFORM 6100-UPDATE-SCREEN.
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

        8020-MUST-SAVE.
            MOVE "You must save your changes before using I=Items."
                TO NC-MSGBUF.
            PERFORM NC-PUT-ERROR-OVERRIDE.
            EXIT.

        9000-FINALIZE.
            PERFORM 9020-CLOSE-FILES.
            PERFORM NC-CLEAR.
            PERFORM NC-FINALIZE.
            EXIT.

        9010-OPEN-FILES.
            OPEN I-O MENU-FILE.
            EXIT.

        9020-CLOSE-FILES.
            CLOSE MENU-FILE.
            EXIT.

        9030-FLUSH-FILES.
            PERFORM 9020-CLOSE-FILES.
            PERFORM 9010-OPEN-FILES.
            EXIT.

        NC-FIELD-EVENT.
            PERFORM 7500-FIELD-EVENT.
            EXIT.

        NC-VERIFY-EVENT.
            IF NC-FIELD-NUMBER = FNO-MENU-NAME THEN
                PERFORM 6150-VERIFY-MENU-NAME
            END-IF.

            IF NC-FIELD-NUMBER = FNO-TOP-LEFT-LINE-NO THEN
                PERFORM 6200-VERIFY-LINE-NO
            END-IF.

            IF NC-FIELD-NUMBER = FNO-TOP-LEFT-COLUMN-NO THEN
                PERFORM 6300-VERIFY-COL-NO
            END-IF.
            EXIT.

        NC-CHANGE-EVENT.
            PERFORM 7700-CHANGE-EVENT.
            EXIT.

        NC-MOUSE-EVENT.
            PERFORM 7600-MOUSE-EVENT.
            EXIT.

        NC-STATE-CHANGE-EVENT.
            PERFORM 7800-STATE-CHANGE-EVENT.
            EXIT.

        NC-FKEY-EVENT.
            EXIT.

        COPY COBCURSQ.

        END PROGRAM SD002080.
