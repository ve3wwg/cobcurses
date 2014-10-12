        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002090.
      *>
      *> THIS IS THE MENU ITEM MAINTENANCE SCREEN, WHERE ITEMS CAN
      *> BE ADDED, UPDATED, OR DELETED.
      *>
      *> INPUTS :
      *> 
      *>     THE MENU HEADER RECORD
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
            SELECT ITEM-FILE
                ASSIGN TO ITEM-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS ITM-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  ITEM-FILE.
        01  ITEM-RECORD.
            COPY ITEMRECD.

        WORKING-STORAGE SECTION.

            COPY COBCRETC.
            COPY COBCATTR.
            COPY COBCCOLOUR.
            COPY COBCURSL.

            COPY SD002090-WS.

        01  FILE-NAMES.
            10  FILE-NAME-LENGTH            PIC 9999.
            10  ITEM-FILE-NAME              PIC X(256)
                VALUE "${COBCURSES_DATADIR}/ITEMS.X".

        01  FLAGS.
            10  WS-SCREEN-TOO-SMALL-FLAG    PIC X VALUE 'N'.
                88  WS-SCREEN-TOO-SMALL     VALUE 'Y'.

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

        01  MENU-RECORD.
            COPY MENURECD.

        PROCEDURE DIVISION
            USING NC-COBCURSES, MENU-RECORD.

        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZATION.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5000-PROCESS
            END-IF.            
            PERFORM 9000-FINALIZE.
            GOBACK.

        1000-INITIALIZATION.
            MOVE LENGTH OF ITEM-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING ITEM-FILE-NAME, FILE-NAME-LENGTH.

            PERFORM 9010-OPEN-FILES.

            CALL "COBCURSES-MENU-SD-MENU-ITEMS" USING
                "X", MNU-MENU-NAME.

            PERFORM 1020-COBCURSES-INIT.
            PERFORM 1010-RECORD-INIT.
            MOVE FNO-ACTION TO NC-FSEQ-STATE.
            EXIT.

        1010-RECORD-INIT.
            INITIALIZE ITEM-RECORD.
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
            COPY SD002090-PD.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM 1035-UPDATE-RO-FIELDS.
            PERFORM 1040-DRAW-SCREEN.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                SET WS-SCREEN-TOO-SMALL TO TRUE
            END-IF.
            EXIT.

        1035-UPDATE-RO-FIELDS.
            MOVE MNU-MENU-NAME TO FLD-MENU-NAME.
            MOVE MNU-TITLE TO FLD-MENU-TITLE.
            EXIT.

        1040-DRAW-SCREEN.
            PERFORM NC-DRAW-SCREEN.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        4000-MOVE-TO-RECORD.
            INITIALIZE ITEM-RECORD.
            MOVE MNU-MENU-NAME TO ITM-MENU-NAME.
            MOVE FLD-ITEM-NUMBER TO ITM-NUMBER.
            MOVE FLD-ITEM-NAME TO ITM-ITEM-NAME.
            MOVE FLD-TEXT TO ITM-TEXT.
            MOVE FLD-ENABLED-FLAG TO ITM-SELECTABLE.
            EXIT.

        4100-MOVE-FROM-RECORD.
            MOVE ITM-NUMBER TO FLD-ITEM-NUMBER.
            MOVE ITM-ITEM-NAME TO FLD-ITEM-NAME.
            MOVE ITM-TEXT TO FLD-TEXT.
            MOVE ITM-SELECTABLE TO FLD-ENABLED-FLAG.
            EXIT.

        5000-PROCESS.
            PERFORM NC-FIELD-STATE-MACHINE.
            EXIT.

        5100-ACTION-C.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM NC-RESET-CHANGES.
            PERFORM 1010-RECORD-INIT.
            PERFORM 1035-UPDATE-RO-FIELDS.
            EXIT.

        5200-ACTION-N.
            IF UNSAVED-CHANGES THEN
                PERFORM 8010-WARNING-UNSAVED-CHANGES
            ELSE
                PERFORM 5100-ACTION-C
                MOVE 1 TO NC-FSEQ-NEXT
                MOVE 'Y' TO NC-FDESC-NOT-BLANK(FNO-ITEM-NUMBER)
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
            WRITE ITEM-RECORD
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
            REWRITE ITEM-RECORD
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
                INITIALIZE ITEM-RECORD
                MOVE MNU-MENU-NAME TO ITM-MENU-NAME
                MOVE FLD-ITEM-NUMBER TO ITM-NUMBER
                DELETE ITEM-FILE
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
                IF NC-FIELD-SEARCH <= FNO-ITEM-NUMBER THEN
                    PERFORM 5605-ACTION-F-START
                ELSE
                    INITIALIZE NC-MSGBUF
                    STRING "You can only search from the ",
                        "Item Number field" INTO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                END-IF
            END-IF.
            EXIT.

        5605-ACTION-F-START.
            STRING "Enter search value + CR, or cursor up/down",
                "to browse." INTO NC-MSGBUF
            PERFORM NC-PUT-MESSAGE-OVERRIDE
            MOVE FSEQ-FIND TO NC-FSEQ-NEXT
            MOVE FNO-ITEM-NUMBER TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT)
            MOVE 'N' TO NC-FDESC-NOT-BLANK(FNO-ITEM-NUMBER)
            EXIT.

        5610-ACTION-F-CONTD.
            PERFORM 1010-RECORD-INIT.
            INITIALIZE ITEM-RECORD.
            MOVE MNU-MENU-NAME TO ITM-MENU-NAME.
            MOVE FLD-ITEM-NUMBER TO ITM-NUMBER.
            IF NC-FIELD-EXIT-CU THEN
                PERFORM 5620-LT-SEARCH
                MOVE FSEQ-FIND TO NC-FSEQ-NEXT
            END-IF.
            IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB THEN
                PERFORM 5630-GE-SEARCH
                MOVE FSEQ-ACTION TO NC-FSEQ-NEXT
                MOVE 'Y' TO NC-FIELD-VERIFIED
            END-IF.
            IF NC-FIELD-EXIT-CD THEN
                PERFORM 5640-GT-SEARCH
                MOVE FSEQ-FIND TO NC-FSEQ-NEXT
            END-IF.
            PERFORM 6100-UPDATE-SCREEN.
            EXIT.

        5620-LT-SEARCH.
            IF ITM-NUMBER = ZERO 
                MOVE 9999 TO ITM-NUMBER
            END-IF.
            START ITEM-FILE KEY IS < ITM-KEY
                INVALID KEY
                    PERFORM 5100-ACTION-C
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            IF DEFINED-RECORD THEN
                PERFORM 5650-NEXT-RECORD
            END-IF.
            EXIT.

        5630-GE-SEARCH.
            START ITEM-FILE KEY IS >= ITM-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    IF ITM-MENU-NAME = MNU-MENU-NAME THEN
                        SET DEFINED-RECORD TO TRUE
                    ELSE
                        PERFORM 1010-RECORD-INIT
                    END-IF
            END-START.
            IF DEFINED-RECORD THEN
                PERFORM 5650-NEXT-RECORD
            END-IF.
            EXIT.

        5640-GT-SEARCH.
            START ITEM-FILE KEY IS > ITM-KEY
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    IF ITM-MENU-NAME = MNU-MENU-NAME THEN
                        SET DEFINED-RECORD TO TRUE
                    ELSE
                        PERFORM 1010-RECORD-INIT
                    END-IF
            END-START.
            IF DEFINED-RECORD THEN
                PERFORM 5650-NEXT-RECORD
            END-IF
            EXIT.

        5650-NEXT-RECORD.
            IF DEFINED-RECORD THEN
                READ ITEM-FILE NEXT RECORD
                    AT END 
                        PERFORM 5100-ACTION-C
                    NOT AT END 
                        IF ITM-MENU-NAME = MNU-MENU-NAME THEN
                            PERFORM 4100-MOVE-FROM-RECORD
                            SET DEFINED-RECORD TO TRUE
                            SET UNSAVED-CHANGES TO FALSE
                            SET NEW-RECORD TO FALSE
                        ELSE
                            PERFORM 5100-ACTION-C
                        END-IF
                END-READ
            ELSE
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        5900-ACTION-EDIT.
            IF NOT DEFINED-RECORD
                MOVE "You have no current record to edit." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            ELSE
                IF NC-FIELD-EDIT-TARGET NOT = 'Y' THEN
                    MOVE "You cannot edit that field." TO NC-MSGBUF
                    PERFORM NC-PUT-ERROR-OVERRIDE
                ELSE
                    MOVE FSEQ-EDIT-FIELD TO NC-FSEQ-NEXT
                    MOVE NC-FIELD-SEARCH
                        TO NC-FSEQ-FIELD-NO(FSEQ-EDIT-FIELD)
                END-IF
            END-IF.
            EXIT.

        6100-UPDATE-SCREEN.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        6200-VERIFY-ITEM-NO.
            INITIALIZE ITEM-RECORD.
            MOVE MNU-MENU-NAME TO ITM-MENU-NAME.
            MOVE FLD-ITEM-NUMBER TO ITM-NUMBER.
            READ ITEM-FILE
                INVALID KEY
                    MOVE FLD-ITEM-NUMBER TO ITM-NUMBER
                    MOVE ITM-NUMBER TO FLD-ITEM-NUMBER
                NOT INVALID KEY
                    IF NC-FSEQ-STATE = FNO-ITEM-NUMBER THEN
                        MOVE FSEQ-EDIT-FIELD TO NC-FSEQ-NEXT
                    END-IF
                    PERFORM 4100-MOVE-FROM-RECORD
                    SET DEFINED-RECORD TO TRUE
                    SET NEW-RECORD TO FALSE
                    SET UNSAVED-CHANGES TO FALSE
            END-READ.
            MOVE 'Y' TO NC-FIELD-VERIFIED.
            EXIT.

        6300-VERIFY-ITEM-NAME.
            MOVE 'Y' TO NC-FIELD-VERIFIED
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

            IF NC-FIELD-ACTION = 'E' THEN
                PERFORM 5300-ACTION-E
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF FLD-ACTION NOT = SPACES THEN
                PERFORM 8000-ERROR-ACTION
            END-IF.
            EXIT.

        7500-FIELD-EVENT.
            IF NC-FSEQ-STATE = FNO-TEXT THEN
      *>
      *>         DEFAULT TO 'Y' FOR ENABLED FLAG ON INPUT
      *>
                MOVE 'Y' TO FLD-ENABLED-FLAG
            END-IF

            IF NC-FSEQ-STATE = FNO-ENABLED-FLAG THEN
                SET DEFINED-RECORD TO TRUE
                SET NEW-RECORD TO TRUE
                SET UNSAVED-CHANGES TO TRUE
            END-IF

            IF NC-FSEQ-STATE = FSEQ-FIND
                PERFORM 5610-ACTION-F-CONTD
            END-IF.

            IF NC-FIELD-NUMBER = FNO-ACTION
                PERFORM 7000-ACTION-EVENT
            END-IF.
            EXIT.

        7600-MOUSE-EVENT.
            PERFORM 5900-ACTION-EDIT.
            EXIT.

        7700-CHANGE-EVENT.
            IF NC-FIELD-NUMBER > FNO-ITEM-NUMBER
            AND NC-FIELD-NUMBER <= FNO-ENABLED-FLAG THEN
                SET UNSAVED-CHANGES TO TRUE
            END-IF.
            EXIT.

        7800-STATE-CHANGE-EVENT.
            PERFORM 1035-UPDATE-RO-FIELDS.
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

        9000-FINALIZE.
            PERFORM 9020-CLOSE-FILES.
            PERFORM NC-CLEAR.
            PERFORM NC-FINALIZE.
            EXIT.

        9010-OPEN-FILES.
            OPEN I-O ITEM-FILE.
            EXIT.

        9020-CLOSE-FILES.
            CLOSE ITEM-FILE.
            EXIT.

        9030-FLUSH-FILES.
            PERFORM 9020-CLOSE-FILES.
            PERFORM 9010-OPEN-FILES.
            EXIT.

        NC-FIELD-EVENT.
            PERFORM 7500-FIELD-EVENT.
            EXIT.

        NC-VERIFY-EVENT.
            IF NC-FSEQ-STATE = FNO-ITEM-NUMBER THEN
                PERFORM 6200-VERIFY-ITEM-NO
            END-IF.

            IF NC-FIELD-NUMBER = FNO-ITEM-NAME THEN
                PERFORM 6300-VERIFY-ITEM-NAME
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

        END PROGRAM SD002090.
