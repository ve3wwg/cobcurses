        IDENTIFICATION DIVISION.
        PROGRAM-ID. SD002050.
      *>
      *> THIS MODULE IS THE FIELD STATE MAINTENANCE SCREEN. THIS ALLOWS THE
      *> USER TO DEFINE AND EDIT FIELD STATES, PERTAINING TO ACTION MODE
      *> SCREENS.
      *>
      *> INPUTS:
      *>     NC-COBCURSES        COBCURSES STATE & WORK AREAS
      *>     SCREEN-IMAGE        SCREEN IMAGE & DIMENSION INFO
      *>     SCREEN-RECORD       SCREEN MAIN RECORD 
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
            SELECT SCRFSTA-FILE
                ASSIGN TO SCRFSTA-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FST-KEY.

            SELECT SCRFDEF-FILE
                ASSIGN TO SCRFDEF-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FDEF-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  SCRFSTA-FILE.
        01  SCRFSTA-RECORD.
            COPY SCREEN-FS.

        FD  SCRFDEF-FILE.
        01  SCRFDEF-RECORD.
            COPY SCREEN-FD.

        WORKING-STORAGE SECTION.

            COPY COBCRETC.
            COPY COBCATTR.
            COPY COBCCOLOUR.
            COPY COBCURSL.

            COPY SD002050-WS.

        01  FILE-NAMES.
            10  FILE-NAME-LENGTH            PIC 9999.
            10  SCRFSTA-FILE-NAME           PIC X(512)
                VALUE "${COBCURSES_DATADIR}/SCRFSTA.X".
            10  SCRFDEF-FILE-NAME           PIC X(512)
                VALUE "${COBCURSES_DATADIR}/SCRFDEF.X".

        01  RECORD-STATE.
            10  RCD-DEFINED                 PIC X.
            10  RCD-NEW                     PIC X.
            10  RCD-CHANGES                 PIC X.

        01  WORK-AREAS.
            10  FX                          PIC 999 COMP.
            10  NEED-SCREEN-UPDATE          PIC X.
            10  WS-SCREEN-TOO-SMALL-FLAG    PIC X VALUE 'N'.
                88  WS-SCREEN-TOO-SMALL     VALUE 'Y'.

        LINKAGE SECTION.

            COPY COBCURSG.

        01  SCREEN-IMAGE.
            COPY SCREEN-SI.

        01  SCREEN-RECORD.
            COPY SCREEN-01.

        PROCEDURE DIVISION USING
            NC-COBCURSES, SCREEN-IMAGE, SCREEN-RECORD.

        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZATION.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5000-PROCESS
            END-IF.
            PERFORM 9000-FINALIZE.
            EXIT PROGRAM.

        1000-INITIALIZATION.
            MOVE FNO-ACTION TO FSEQ-EDIT, FSEQ-FIND.
            ADD 1 TO FSEQ-EDIT.
            ADD 2 TO FSEQ-FIND.

            MOVE LENGTH OF SCRFSTA-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                BY REFERENCE SCRFSTA-FILE-NAME, FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                BY REFERENCE SCRFDEF-FILE-NAME, FILE-NAME-LENGTH.

            PERFORM 9010-OPEN-FILES.

      *>
      *>     SET CONTEXT FOR DYNAMIC MENU LOADER FOR STATES :
      *>
            CALL "COBCURSES-MENU-SD-STATES" USING "X", SCN-NAME.
            CALL "COBCURSES-MENU-SD-FIELDS" USING "X", SCN-NAME.

            CALL "LOOKUP-FIELD" USING "X", SCN-NAME, OMITTED.
            CALL "LOOKUP-FIELD" USING "O", OMITTED, OMITTED.

            PERFORM 1200-COBCURSES-INIT.
            PERFORM 1100-INIT-RECORD-STATE.
            MOVE FNO-ACTION TO NC-FSEQ-STATE.
            EXIT.

        1100-INIT-RECORD-STATE.
            MOVE 'N' TO RCD-DEFINED, RCD-NEW, RCD-CHANGES.
            EXIT.

        1200-COBCURSES-INIT.
            PERFORM NC-INIT.
            PERFORM NC-CLEAR.
            PERFORM 1300-SCREEN-INIT.
            PERFORM NC-DRAW-SCREEN.
            IF RETURN-CODE NOT = NC-RET-OK THEN
                SET WS-SCREEN-TOO-SMALL TO TRUE
            END-IF.
            IF NOT WS-SCREEN-TOO-SMALL THEN
                PERFORM 5400-ACTION-C
                PERFORM 7000-UPDATE-SCREEN
            END-IF.
            EXIT.

        1300-SCREEN-INIT.
            COPY SD002050-PD.
            EXIT.

        5000-PROCESS.
            PERFORM NC-FIELD-STATE-MACHINE.
            EXIT.

        5100-ACTION-E.
            MOVE 9999 TO NC-FSEQ-NEXT.
            EXIT.

        5200-ACTION-S.
            IF RCD-DEFINED = 'Y'
                IF RCD-NEW = 'Y' THEN
                    PERFORM 5210-ADD-RECORD
                ELSE
                    PERFORM 5220-UPDATE-RECORD
                END-IF
            ELSE
                MOVE "No record to save." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5210-ADD-RECORD.
            PERFORM 6100-MOVE-TO-RECORD.
            WRITE SCRFSTA-RECORD
                INVALID KEY
                    MOVE "Error: Adding a new field state record."
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

        5220-UPDATE-RECORD.
            PERFORM 6100-MOVE-TO-RECORD.
            REWRITE SCRFSTA-RECORD
                INVALID KEY
                    MOVE "Error: Updating the field state record."
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

        5300-ACTION-N.
            PERFORM 5400-ACTION-C.
            MOVE 1 TO NC-FSEQ-NEXT.
            EXIT.

        5400-ACTION-C.
            PERFORM NC-CLEAR-FIELDS.
            PERFORM NC-RESET-CHANGES.
            PERFORM 1100-INIT-RECORD-STATE.
            PERFORM 7000-UPDATE-SCREEN.
            EXIT.

        5500-ACTION-D.
            IF RCD-DEFINED = 'Y' AND RCD-NEW NOT = 'Y'
                INITIALIZE SCRFSTA-RECORD
                MOVE SCN-NAME TO SCR-FST-SCREEN-NAME
                MOVE FLD-STATE-NUMBER TO SCR-FST-STATE-NO
                DELETE SCRFSTA-FILE
                    INVALID KEY
                        MOVE "Error: Unable to delete this record."
                            TO NC-MSGBUF
                        PERFORM NC-PUT-ERROR-OVERRIDE
                    NOT INVALID KEY
                        PERFORM 5400-ACTION-C
                        MOVE "The record was deleted." TO NC-MSGBUF
                        PERFORM NC-PUT-MESSAGE-OVERRIDE
                END-DELETE
                PERFORM 9030-FLUSH-FILES
            ELSE
                MOVE "No record to delete." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            END-IF.
            EXIT.

        5600-ACTION-F.
            STRING "Enter search value + CR, or cursor up/down",
                "to browse." INTO NC-MSGBUF.
            PERFORM NC-PUT-MESSAGE-OVERRIDE.
            MOVE FSEQ-FIND TO NC-FSEQ-NEXT.
            MOVE FNO-STATE-NUMBER TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT).
            EXIT.

        5610-ACTION-F-CONTD.
            PERFORM 1100-INIT-RECORD-STATE.
            INITIALIZE SCRFSTA-RECORD.
            MOVE SCN-NAME TO SCR-FST-SCREEN-NAME.
            MOVE FLD-STATE-NUMBER TO SCR-FST-STATE-NO.
            IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB THEN
                PERFORM 5620-GE-SEARCH
            END-IF.
            IF NC-FIELD-EXIT-CD THEN
                PERFORM 5630-GT-SEARCH
            END-IF.
            IF NC-FIELD-EXIT-CU THEN
                PERFORM 5640-LT-SEARCH
            END-IF.
            EXIT.

        5620-GE-SEARCH.
            START SCRFSTA-FILE KEY IS >= SCR-FST-KEY
                INVALID KEY
                    PERFORM 1100-INIT-RECORD-STATE
                NOT INVALID KEY
                    MOVE 'Y' TO RCD-DEFINED
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5630-GT-SEARCH.
            START SCRFSTA-FILE KEY IS > SCR-FST-KEY
                INVALID KEY
                    PERFORM 1100-INIT-RECORD-STATE
                NOT INVALID KEY
                    MOVE 'Y' TO RCD-DEFINED
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5640-LT-SEARCH.
            START SCRFSTA-FILE KEY IS < SCR-FST-KEY
                INVALID KEY
                    PERFORM 1100-INIT-RECORD-STATE
                NOT INVALID KEY
                    MOVE 'Y' TO RCD-DEFINED
            END-START.
            PERFORM 5650-NEXT-RECORD.
            EXIT.

        5650-NEXT-RECORD.
            IF RCD-DEFINED = 'Y' THEN
                READ SCRFSTA-FILE NEXT RECORD
                    AT END 
                        PERFORM 5400-ACTION-C
                    NOT AT END 
                        IF SCR-FST-SCREEN-NAME = SCN-NAME
                            PERFORM 6200-MOVE-FROM-RECORD
                            MOVE 'Y' TO RCD-DEFINED
                            MOVE 'N' TO RCD-NEW, RCD-CHANGES
                            PERFORM 7100-UPDATE-FIELDS
                            PERFORM 7000-UPDATE-SCREEN
                        ELSE
                            PERFORM 5400-ACTION-C
                        END-IF
                END-READ
            ELSE
                PERFORM 5400-ACTION-C
            END-IF.
            EXIT.

        5900-ACTION-EDIT.
            IF RCD-DEFINED NOT = 'Y' THEN
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
            MOVE SCN-NAME TO SCR-FST-SCREEN-NAME.
            MOVE FLD-STATE-NUMBER TO SCR-FST-STATE-NO.
            READ SCRFSTA-FILE
                INVALID KEY
                    MOVE FLD-STATE-NUMBER TO SCR-FST-STATE-NO
                NOT INVALID KEY
                    PERFORM 6200-MOVE-FROM-RECORD
                    MOVE FNO-ACTION TO NC-FSEQ-NEXT
                    MOVE 'Y' TO RCD-DEFINED
                    MOVE 'N' TO RCD-NEW, RCD-CHANGES
                    PERFORM 7200-UPDATE-FIELD-NUMBER
                    PERFORM 7210-UPDATE-BACK-TO
                    PERFORM 7220-UPDATE-FORWARD-TO
                    PERFORM 7230-UPDATE-ESCAPE-TO
                    PERFORM 7240-UPDATE-SLASH-TO
                    PERFORM 7000-UPDATE-SCREEN
            END-READ.
            EXIT.

        6100-MOVE-TO-RECORD.
            MOVE SCN-NAME TO SCR-FST-SCREEN-NAME.
            MOVE FLD-STATE-NUMBER TO SCR-FST-STATE-NO.
            MOVE FLD-FIELD-NUMBER TO SCR-FST-FIELD-NO.
            MOVE FLD-BACK-TO TO SCR-FST-BACK-TO.
            MOVE FLD-FORWARD-TO TO SCR-FST-FORWARD-TO.
            MOVE FLD-ESCAPE-TO TO SCR-FST-ESCAPE-TO.
            MOVE FLD-SLASH-TO TO SCR-FST-SLASH-TO.
            MOVE FLD-GROUP-HEADER TO SCR-FST-GROUP-HEADER.
            MOVE FLD-STATE-COBOL-NAME TO SCR-FST-STATE-COBOL-NAME.
            EXIT.

        6200-MOVE-FROM-RECORD.
            MOVE SCR-FST-SCREEN-NAME TO SCN-NAME.
            MOVE SCR-FST-STATE-NO TO FLD-STATE-NUMBER.
            MOVE SCR-FST-FIELD-NO TO FLD-FIELD-NUMBER.
            MOVE SCR-FST-BACK-TO TO FLD-BACK-TO.
            MOVE SCR-FST-FORWARD-TO TO FLD-FORWARD-TO.
            MOVE SCR-FST-ESCAPE-TO TO FLD-ESCAPE-TO.
            MOVE SCR-FST-SLASH-TO TO FLD-SLASH-TO.
            MOVE SCR-FST-GROUP-HEADER TO FLD-GROUP-HEADER.
            MOVE SCR-FST-STATE-COBOL-NAME TO FLD-STATE-COBOL-NAME.
            EXIT.

        7000-UPDATE-SCREEN.
            MOVE SCN-NAME TO FLD-SCREEN-NAME.
            MOVE SCN-DESCRIPTION TO FLD-SCREEN-DESCRIPTION.
            MOVE SCN-TITLE TO FLD-SCREEN-TITLE.
            PERFORM NC-DRAW-FIELDS.
            EXIT.
        
        7100-UPDATE-FIELDS.
            PERFORM 7200-UPDATE-FIELD-NUMBER.
            PERFORM 7210-UPDATE-BACK-TO.
            PERFORM 7220-UPDATE-FORWARD-TO.
            PERFORM 7230-UPDATE-ESCAPE-TO.
            PERFORM 7240-UPDATE-SLASH-TO.
            EXIT.

        7200-UPDATE-FIELD-NUMBER.
            MOVE SCN-NAME TO SCR-FDEF-SCREEN-NAME.
            MOVE FLD-FIELD-NUMBER TO SCR-FDEF-NO.
            READ SCRFDEF-FILE
                INVALID KEY
                    MOVE SPACES TO FLD-FIELD-NAME
                NOT INVALID KEY
                    MOVE SCR-FDEF-COBOL-NAME TO FLD-FIELD-NAME
            END-READ.
            EXIT.

        7210-UPDATE-BACK-TO.
            MOVE SCN-NAME TO SCR-FDEF-SCREEN-NAME.
            MOVE FLD-BACK-TO TO SCR-FDEF-NO.
            CALL "LOOKUP-FIELD" USING
                "R", SCR-FDEF-NO,
                FLD-BACK-TO-NAME, FLD-BACK-TO-FNO.
            EXIT.

        7220-UPDATE-FORWARD-TO.
            MOVE SCN-NAME TO SCR-FDEF-SCREEN-NAME.
            MOVE FLD-FORWARD-TO TO SCR-FDEF-NO.
            CALL "LOOKUP-FIELD" USING
                "R", SCR-FDEF-NO,
                FLD-FORWARD-TO-NAME, FLD-FORWARD-TO-FNO.
            EXIT.

        7230-UPDATE-ESCAPE-TO.
            MOVE SCN-NAME TO SCR-FDEF-SCREEN-NAME.
            MOVE FLD-ESCAPE-TO TO SCR-FDEF-NO.
            CALL "LOOKUP-FIELD" USING
                "R", SCR-FDEF-NO,
                FLD-ESCAPE-TO-NAME, FLD-ESCAPE-TO-FNO.
            EXIT.

        7240-UPDATE-SLASH-TO.
            MOVE SCN-NAME TO SCR-FDEF-SCREEN-NAME.
            MOVE FLD-SLASH-TO TO SCR-FDEF-NO.
            CALL "LOOKUP-FIELD" USING
                "R", SCR-FDEF-NO,
                FLD-SLASH-TO-NAME, FLD-SLASH-TO-FNO.
            EXIT.

        7600-DISPATCH-ACTION.
            IF NC-FIELD-ACTION = 'E' THEN
                PERFORM 5100-ACTION-E
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'S' THEN
                PERFORM 5200-ACTION-S
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'N' THEN
                PERFORM 5300-ACTION-N
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF NC-FIELD-ACTION = 'C' THEN
                PERFORM 5400-ACTION-C
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

            IF NC-FIELD-ACTION = SPACES
                PERFORM 5900-ACTION-EDIT
                MOVE SPACES TO FLD-ACTION
            END-IF.

            IF FLD-ACTION NOT = SPACES THEN
                PERFORM 8000-ERROR-ACTION
            END-IF.
            EXIT.

        7700-CHANGE-EVENT.
            MOVE 'N' TO NEED-SCREEN-UPDATE.
            IF NC-FSEQ-STATE = FNO-FIELD-NUMBER
                PERFORM 7200-UPDATE-FIELD-NUMBER
                MOVE 'Y' TO NEED-SCREEN-UPDATE
            END-IF.
            IF NC-FSEQ-STATE = FNO-BACK-TO
                PERFORM 7210-UPDATE-BACK-TO
                MOVE 'Y' TO NEED-SCREEN-UPDATE
            END-IF.
            IF NC-FSEQ-STATE = FNO-FORWARD-TO
                PERFORM 7220-UPDATE-FORWARD-TO
                MOVE 'Y' TO NEED-SCREEN-UPDATE
            END-IF.
            IF NC-FSEQ-STATE = FNO-ESCAPE-TO
                PERFORM 7230-UPDATE-ESCAPE-TO
                MOVE 'Y' TO NEED-SCREEN-UPDATE
            END-IF.
            IF NC-FSEQ-STATE = FNO-SLASH-TO
                PERFORM 7240-UPDATE-SLASH-TO
                MOVE 'Y' TO NEED-SCREEN-UPDATE
            END-IF.
            IF NEED-SCREEN-UPDATE = 'Y'
                PERFORM 7000-UPDATE-SCREEN
            END-IF.
            EXIT.

        7800-VERIFY-EVENT.
            EXIT.

        7900-FIELD-DISPATCH.
            IF NC-FIELD-NUMBER = FNO-ACTION
                PERFORM 7600-DISPATCH-ACTION
                MOVE 'Y' TO NC-FIELD-VERIFIED
            END-IF.

            IF NC-FSEQ-STATE = FNO-GROUP-HEADER
                MOVE 'Y' TO RCD-NEW, RCD-CHANGES, RCD-DEFINED
            END-IF.

            IF NC-FSEQ-STATE = FNO-STATE-NUMBER
                PERFORM 6000-LOOKUP-RECORD
            END-IF.

            IF NC-FSEQ-STATE = FSEQ-FIND
                PERFORM 5610-ACTION-F-CONTD
                IF NC-FIELD-EXIT-CD OR NC-FIELD-EXIT-CU THEN
                    MOVE FSEQ-FIND TO NC-FSEQ-NEXT
                END-IF
            END-IF.

            IF NC-FSEQ-STATE = FSEQ-EDIT
                PERFORM 7100-UPDATE-FIELDS
                PERFORM 7000-UPDATE-SCREEN
            END-IF.
            EXIT.

        7900-MOUSE-EVENT.
            PERFORM 5900-ACTION-EDIT.
            EXIT.

        7900-STATE-CHANGE-EVENT.
            EXIT.

        8000-ERROR-ACTION.
            SET NC-MSG-TEXT TO ADDRESS OF INF-ACTION.
            MOVE LENGTH OF INF-ACTION TO NC-MSG-LENGTH.
            PERFORM NC-ERROR-MESSAGE-OVERRIDE.
            EXIT.

        9000-FINALIZE.
            PERFORM 9020-CLOSE-FILES.
            CALL "LOOKUP-FIELD" USING "C".
            PERFORM NC-CLEAR.
            PERFORM NC-FINALIZE.
            EXIT.

        9010-OPEN-FILES.
            OPEN INPUT SCRFDEF-FILE.
            OPEN I-O SCRFSTA-FILE.
            EXIT.

        9020-CLOSE-FILES.
            CLOSE SCRFSTA-FILE, SCRFDEF-FILE.
            EXIT.

        9030-FLUSH-FILES.
            PERFORM 9020-CLOSE-FILES.
            PERFORM 9010-OPEN-FILES.
            EXIT.

        NC-CHANGE-EVENT.
            PERFORM 7700-CHANGE-EVENT.
            EXIT.

        NC-VERIFY-EVENT.
            PERFORM 7800-VERIFY-EVENT.
            EXIT.

        NC-FIELD-EVENT.
            PERFORM 7900-FIELD-DISPATCH.
            EXIT.

        NC-MOUSE-EVENT.
            PERFORM 7900-MOUSE-EVENT.
            EXIT.

        NC-STATE-CHANGE-EVENT.
            PERFORM 7900-STATE-CHANGE-EVENT.
            EXIT.

        NC-FKEY-EVENT.
            EXIT.

        COPY COBCURSQ.

        END PROGRAM SD002050.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. LOOKUP-FIELD.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
            SELECT SCRFSTA-FILE
                ASSIGN TO SCRFSTA-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FST-KEY.

            SELECT SCRFDEF-FILE
                ASSIGN TO SCRFDEF-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FDEF-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  SCRFSTA-FILE.
        01  SCRFSTA-RECORD.
            COPY SCREEN-FS.

        FD  SCRFDEF-FILE.
        01  SCRFDEF-RECORD.
            COPY SCREEN-FD.

        WORKING-STORAGE SECTION.

        01  WS-SCREEN-NAME                  PIC X(32).

        01  FILE-NAMES.
            10  FILE-NAME-LENGTH            PIC 9999.
            10  SCRFSTA-FILE-NAME           PIC X(512)
                VALUE "${COBCURSES_DATADIR}/SCRFSTA.X".
            10  SCRFDEF-FILE-NAME           PIC X(512)
                VALUE "${COBCURSES_DATADIR}/SCRFDEF.X".

        LINKAGE SECTION.

        01  LS-REQUEST-TYPE                 PIC X.
        01  LS-ARG-1.
            10  LS-SCREEN-NAME              PIC X(16).
            10  LS-STATE-NO       REDEFINES LS-SCREEN-NAME
                                            PIC 999.
        01  LS-FIELD-NAME                   PIC X(40).
        01  LS-FIELD-NUMBER                 PIC X(3).

        PROCEDURE DIVISION USING
            LS-REQUEST-TYPE, LS-ARG-1,
            LS-FIELD-NAME, LS-FIELD-NUMBER.
            

        100-MAIN-DISPATCH.
            MOVE ZERO TO RETURN-CODE.

            EVALUATE LS-REQUEST-TYPE
            WHEN 'X'
                PERFORM 150-CONTEXT
            WHEN 'O'
                PERFORM 200-OPEN
            WHEN 'R'
                PERFORM 500-READ
            WHEN 'C'
                PERFORM 900-CLOSE
            WHEN OTHER
                MOVE 1 TO RETURN-CODE
            END-EVALUATE.
            GOBACK.

        150-CONTEXT.
            MOVE LS-SCREEN-NAME TO WS-SCREEN-NAME.
            EXIT.

        200-OPEN.
            MOVE LENGTH OF SCRFSTA-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                BY REFERENCE SCRFSTA-FILE-NAME, FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                BY REFERENCE SCRFDEF-FILE-NAME, FILE-NAME-LENGTH.
            OPEN INPUT SCRFSTA-FILE, SCRFDEF-FILE.
            EXIT.

        500-READ.
            INITIALIZE SCRFSTA-RECORD.
            MOVE WS-SCREEN-NAME TO SCR-FST-SCREEN-NAME.
            MOVE LS-STATE-NO TO SCR-FST-STATE-NO.
            READ SCRFSTA-FILE
                INVALID KEY
                    INITIALIZE LS-FIELD-NAME
                    MOVE 1 TO RETURN-CODE
                NOT INVALID KEY
                    PERFORM 510-READ-FD
            END-READ.
            EXIT.

        510-READ-FD.
            INITIALIZE SCRFDEF-RECORD
            MOVE WS-SCREEN-NAME TO SCR-FDEF-SCREEN-NAME
            MOVE SCR-FST-FIELD-NO TO SCR-FDEF-NO
            READ SCRFDEF-FILE
                INVALID KEY
                    INITIALIZE LS-FIELD-NAME
                    MOVE 2 TO RETURN-CODE
                NOT INVALID KEY
                    MOVE SCR-FDEF-COBOL-NAME
                        TO LS-FIELD-NAME
                    MOVE SCR-FDEF-NO TO LS-FIELD-NUMBER
                    MOVE ZERO TO RETURN-CODE
            END-READ.
            EXIT.

        900-CLOSE.
            CLOSE SCRFSTA-FILE, SCRFDEF-FILE.
            EXIT.            

        END PROGRAM LOOKUP-FIELD.
