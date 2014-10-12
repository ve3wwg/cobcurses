        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002060.
      *>
      *> THIS MODULE IS THE CHARACTER SET MAINTENANCE SCREEN. THIS SCREEN 
      *> ALLOWS THE USER TO DEFINE AND MAINTAIN CHARACTER SETS PERTAINING
      *> TO FIELD CHARACTER SETS.
      *>
      *> INPUTS:
      *>
      *>     NC-COBCURSES        COBCURSES GLOBAL & WORK AREAS
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
            SELECT CHARSET-FILE
                ASSIGN TO CHARSET-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS CHARSET-NAME.

        DATA DIVISION.
        FILE SECTION.

        FD  CHARSET-FILE.
        01  CHARSET-RECORD.
            COPY SCREEN-CS.

        WORKING-STORAGE SECTION.

            COPY COBCRETC.
            COPY COBCATTR.
            COPY COBCCOLOUR.
            COPY COBCURSL.

            COPY SD002060-WS.

        77  FILE-NAME-LENGTH                PIC 9999.
        77  CHARSET-FILE-NAME               PIC X(256)
            VALUE "${COBCURSES_DATADIR}/SCRCHRSET.X".

        01  RECORD-STATE.
            10  RCD-DEFINED                 PIC X.
                88  DEFINED-RECORD          VALUE 'Y'.
            10  RCD-NEW                     PIC X.
                88  NEW-RECORD              VALUE 'Y'.
            10  RCD-CHANGES                 PIC X.
                88 UNSAVED-CHANGES          VALUE 'Y'.

        01  WS-FLAGS.
            10  WS-SCREEN-TOO-SMALL-FLAG    PIC X VALUE 'N'.
                88  WS-SCREEN-TOO-SMALL     VALUE 'Y'.

        01  WORK-AREAS.
            10  SV-CHRSET                   PIC 99.

        01  WS-COUNT-OPTIONS                PIC 99.
        01  WS-COUNT-UNITS                  PIC 99.
        01  WS-ERROR-MSG-BUF                PIC X(80).
        01  WS-ERROR-MSG-BUFLEN             PIC 99.
        01  WS-ERROR-OFFSET                 PIC 99.
        01  WS-FLD-UNITS-VALUE-LENGTH       PIC 99.

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
            INITIALIZE CHARSET-RECORD.
            MOVE FNO-ACTION TO NC-FSEQ-STATE.

            MOVE LENGTH OF CHARSET-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                BY REFERENCE CHARSET-FILE-NAME, FILE-NAME-LENGTH.

            OPEN I-O CHARSET-FILE.

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
            PERFORM 4200-UPDATE-UNITS-SPEC.
            PERFORM 1030-SCREEN-INIT.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        1030-SCREEN-INIT.
            COPY SD002060-PD.
            PERFORM 1040-DRAW-SCREEN.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                SET WS-SCREEN-TOO-SMALL TO TRUE
            END-IF.
            EXIT.

        1040-DRAW-SCREEN.
            PERFORM NC-DRAW-SCREEN.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        4000-MOVE-TO-RECORD.
            MOVE FLD-CHARSET-NAME TO CHARSET-NAME.
            MOVE FLD-CHARSET-DATA TO CHARSET-DATA.
            PERFORM 4200-UPDATE-UNITS-SPEC.
            EXIT.

        4100-MOVE-FROM-RECORD.
            MOVE CHARSET-NAME TO FLD-CHARSET-NAME.
            MOVE CHARSET-DATA TO FLD-CHARSET-DATA.
            PERFORM 4200-UPDATE-UNITS-SPEC.
            EXIT.

        4200-UPDATE-UNITS-SPEC.
      *>
      *>     WE MUST ALSO PROVIDE THE CHARSET DATA TO FIELD 4
      *>     SO THAT THE USER CAN TEST THE UNITS SPECIFICATION
      *>
            MOVE CHARSET-DATA TO NC-RESTRICT-CHARSET(20).
            MOVE 20 TO NC-FDESC-RESTRICT(4).
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
            WRITE CHARSET-RECORD
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
            EXIT.

        5420-UPDATE-RECORD.
            PERFORM 4000-MOVE-TO-RECORD.
            REWRITE CHARSET-RECORD
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
            EXIT.

        5500-ACTION-D.
            IF DEFINED-RECORD AND NOT NEW-RECORD THEN
                INITIALIZE CHARSET-RECORD
                MOVE FLD-CHARSET-NAME TO CHARSET-NAME
                DELETE CHARSET-FILE
                    INVALID KEY
                        MOVE "Error: Unable to delete this record."
                            TO NC-MSGBUF
                        PERFORM NC-PUT-ERROR-OVERRIDE
                    NOT INVALID KEY
                        PERFORM 5100-ACTION-C
                        MOVE "The record was deleted." TO NC-MSGBUF
                        PERFORM NC-PUT-MESSAGE-OVERRIDE
                END-DELETE
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
                MOVE NC-FDESC-RESTRICT(FNO-CHARSET-NAME) TO SV-CHRSET
                MOVE ZERO TO NC-FDESC-RESTRICT(FNO-CHARSET-NAME)
                MOVE FSEQ-FIND TO NC-FSEQ-NEXT
                MOVE FNO-CHARSET-NAME TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT)

                STRING "Enter search value + CR, or cursor up/down",
                    "to browse." INTO NC-MSGBUF
                PERFORM NC-PUT-MESSAGE-OVERRIDE
            END-IF.
            EXIT.

        5610-ACTION-F-CONTD.
            PERFORM 1010-RECORD-INIT.
            INITIALIZE CHARSET-RECORD.
            MOVE FLD-CHARSET-NAME TO CHARSET-NAME.
            IF NC-FIELD-EXIT-CU THEN
                PERFORM 5620-LT-SEARCH
            END-IF.
            IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB THEN
                PERFORM 5630-GE-SEARCH
                MOVE SV-CHRSET TO NC-FDESC-RESTRICT(FNO-CHARSET-NAME)
            END-IF.
            IF NC-FIELD-EXIT-CD THEN
                PERFORM 5640-GT-SEARCH
            END-IF.
            EXIT.

        5620-LT-SEARCH.
            START CHARSET-FILE KEY IS < CHARSET-NAME
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5630-GE-SEARCH.
            START CHARSET-FILE KEY IS >= CHARSET-NAME
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5640-GT-SEARCH.
            START CHARSET-FILE KEY IS > CHARSET-NAME
                INVALID KEY
                    PERFORM 1010-RECORD-INIT
                NOT INVALID KEY
                    SET DEFINED-RECORD TO TRUE
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5650-NEXT-RECORD.
            IF DEFINED-RECORD THEN
                READ CHARSET-FILE NEXT RECORD
                    AT END 
                        PERFORM 5100-ACTION-C
                    NOT AT END 
                        PERFORM 4100-MOVE-FROM-RECORD
                        SET DEFINED-RECORD TO TRUE
                        MOVE 'N' TO RCD-NEW, RCD-CHANGES
                        PERFORM 6100-UPDATE-SCREEN
                END-READ
            ELSE
                PERFORM 5100-ACTION-C
            END-IF.
            EXIT.

        5800-ACTION-U.
            IF DEFINED-RECORD THEN
                PERFORM 5810-TEST-UNIT-SPEC
            ELSE
                MOVE "No current record to test." to NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5810-TEST-UNIT-SPEC.
            MOVE LENGTH OF WS-ERROR-MSG-BUF
                TO WS-ERROR-MSG-BUFLEN.
            CALL "COBCURSES-TEST-UNITS" USING
                FLD-CHARSET-DATA,
                WS-COUNT-OPTIONS, WS-COUNT-UNITS,
                WS-ERROR-OFFSET, WS-ERROR-MSG-BUF,
                WS-ERROR-MSG-BUFLEN.
            INITIALIZE NC-MSGBUF
            IF RETURN-CODE NOT = NC-RET-OK THEN
                STRING "Error in specification near column ",
                    WS-ERROR-OFFSET INTO NC-MSGBUF
            ELSE
                STRING WS-COUNT-OPTIONS, " options, ",
                    WS-COUNT-UNITS, " units, parsed OK."
                    INTO NC-MSGBUF
            END-IF
            PERFORM NC-PUT-ERROR-OVERRIDE
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
            MOVE FLD-CHARSET-NAME TO CHARSET-NAME.
            READ CHARSET-FILE
                INVALID KEY
                    MOVE FLD-CHARSET-NAME TO CHARSET-NAME
                NOT INVALID KEY
                    PERFORM 4100-MOVE-FROM-RECORD
                    MOVE FNO-ACTION TO NC-FSEQ-NEXT
                    SET DEFINED-RECORD TO TRUE
                    MOVE 'N' TO RCD-NEW, RCD-CHANGES
                    PERFORM 6100-UPDATE-SCREEN
            END-READ.
            EXIT.

        6100-UPDATE-SCREEN.
            PERFORM NC-DRAW-FIELDS.
            EXIT.

        6200-UPDATE-UNIT-SPEC.
            PERFORM 5810-TEST-UNIT-SPEC.
            MOVE LENGTH OF FLD-UNITS-VALUE
                TO WS-FLD-UNITS-VALUE-LENGTH.
            CALL "COBCURSES-SCI-NOTATION-COMP-2" USING
                CMP-TEST-UNIT-SPEC,
                FLD-UNITS-VALUE,
                WS-FLD-UNITS-VALUE-LENGTH,
                "Y",
                OMITTED,
                OMITTED.
            PERFORM 6100-UPDATE-SCREEN.
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

            IF NC-FIELD-ACTION = 'U' THEN
                PERFORM 5800-ACTION-U
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
            IF NC-FIELD-NUMBER < FNO-TEST-UNIT-SPEC
            AND NC-FSEQ-STATE NOT = FSEQ-FIND THEN
                SET UNSAVED-CHANGES TO TRUE
            END-IF.
            IF NC-FIELD-NUMBER = FNO-CHARSET-DATA THEN
                PERFORM 4200-UPDATE-UNITS-SPEC
            END-IF.
            IF NC-FIELD-NUMBER = FNO-TEST-UNIT-SPEC THEN
                PERFORM 6200-UPDATE-UNIT-SPEC
            END-IF.
            EXIT.

        7200-VERIFY-EVENT.
            EXIT.

        7500-FIELD-EVENT.
            IF NC-FIELD-NUMBER = FNO-ACTION
                PERFORM 7000-ACTION-EVENT
            END-IF.

            IF NC-FSEQ-STATE = FNO-CHARSET-NAME
                PERFORM 6000-LOOKUP-RECORD
            END-IF.

            IF NC-FSEQ-STATE = FNO-CHARSET-DATA
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
            CLOSE CHARSET-FILE.
            PERFORM NC-CLEAR.
            PERFORM NC-FINALIZE.
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

        END PROGRAM SD002060.
