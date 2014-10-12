        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-MENU-SD-STATES.
      *>
      *>     THIS MODULE IS A DYNAMIC MENU LOAD MODULE FOR FIELD STATES.
      *>
      *>     LS-REQUEST-TYPE :
      *>         'X' - SET CONTEXT (SCREEN NAME)
      *>         'O' - OPEN FILE (DOES NOT USE OTHER ARGS)
      *>         'R' - READ NEXT (MENU ITEM)
      *>         'C' - CLOSE
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT SCRFSTA-FILE
                ASSIGN TO WS-SCRFSTA-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FST-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  SCRFSTA-FILE.
        01  SCRFSTA-RECORD.
            COPY SCREEN-FS.

        WORKING-STORAGE SECTION.

        01  WS-SCREEN-NAME              PIC X(16) VALUE SPACES.
        01  WS-FILE-NAME-LENGTH         PIC 9999.
        01  WS-SCRFSTA-FILE-NAME        PIC X(256)
            VALUE "${COBCURSES_DATADIR}/SCRFSTA.X".

        LINKAGE SECTION.

        01  LS-REQUEST-TYPE             PIC X.
        01  LS-ITEM-NAME                PIC X(32).
        01  LS-ITEM-DESCRIPTION         PIC X(64).

        PROCEDURE DIVISION
            USING LS-REQUEST-TYPE, LS-ITEM-NAME, LS-ITEM-DESCRIPTION.

        100-MAIN-DISPATCH-ENTRY.
      *>
      *>     THIS DISPATCHES ACCORDING TO LS-REQUEST-TYPE :
      *>
            MOVE ZERO TO RETURN-CODE

            EVALUATE LS-REQUEST-TYPE
            WHEN 'X'
                MOVE LS-ITEM-NAME TO WS-SCREEN-NAME
            WHEN 'O'
                PERFORM 200-OPEN-FILE
            WHEN 'R'
                PERFORM 500-READ-FILE
            WHEN 'C'
                PERFORM 900-CLOSE-FILE
            WHEN OTHER
                MOVE 1 TO RETURN-CODE
            END-EVALUATE.
            GOBACK.

        200-OPEN-FILE.
      *>
      *>     'O' - OPEN FILE REQUEST
      *>
            MOVE LENGTH OF WS-SCRFSTA-FILE-NAME
                TO WS-FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING WS-SCRFSTA-FILE-NAME, WS-FILE-NAME-LENGTH. 
            OPEN INPUT SCRFSTA-FILE.
      *>
      *>     PREPARE FOR THE 'R' READ NEXT RECORD REQUEST :
      *>
            INITIALIZE SCRFSTA-RECORD.
            MOVE WS-SCREEN-NAME TO SCR-FST-SCREEN-NAME.
            START SCRFSTA-FILE KEY IS >= SCR-FST-KEY
                INVALID KEY
                    PERFORM 900-CLOSE-FILE
                    MOVE 1 TO RETURN-CODE
            END-START.
            EXIT.

        500-READ-FILE.
      *>
      *>     'R' - READ NEXT RECORD
      *>
            READ SCRFSTA-FILE NEXT RECORD
                AT END
                    PERFORM 510-END-FILE
                NOT AT END
                    IF SCR-FST-SCREEN-NAME = WS-SCREEN-NAME THEN
                        MOVE SCR-FST-STATE-NO TO LS-ITEM-NAME
                        INITIALIZE LS-ITEM-DESCRIPTION
                        IF SCR-FST-STATE-COBOL-NAME = SPACES THEN
                            STRING "FIELD # ", SCR-FST-FIELD-NO
                                INTO LS-ITEM-DESCRIPTION
                        ELSE
                            STRING "FSEQ-", SCR-FST-STATE-COBOL-NAME
                                INTO LS-ITEM-DESCRIPTION
                        END-IF
                    ELSE
                        PERFORM 510-END-FILE
                    END-IF
            END-READ
            EXIT.

        510-END-FILE.
            MOVE 1 TO RETURN-CODE
            INITIALIZE LS-ITEM-NAME, LS-ITEM-DESCRIPTION
            EXIT.

        900-CLOSE-FILE.
      *>
      *>     'C' - CLOSE FILE
      *>
            CLOSE SCRFSTA-FILE.
            EXIT.

        END PROGRAM COBCURSES-MENU-SD-STATES.
