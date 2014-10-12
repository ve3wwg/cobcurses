        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-MENU-SD-FIELDS.
      *>
      *>     THIS MODULE IS A DYNAMIC MENU LOAD MODULE FOR SCREEN FIELDS.
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

            SELECT SCRFDEF-FILE
                ASSIGN TO WS-SCRFDEF-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCR-FDEF-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  SCRFDEF-FILE.
        01  SCRFDEF-RECORD.
            COPY SCREEN-FD.

        WORKING-STORAGE SECTION.

        01  WS-SCREEN-NAME              PIC X(16) VALUE SPACES.

        01  WS-FILE-NAME-LENGTH         PIC 9999.
        01  WS-SCRFDEF-FILE-NAME        PIC X(512)
            VALUE "${COBCURSES_DATADIR}/SCRFDEF.X".

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
            MOVE LENGTH OF WS-SCRFDEF-FILE-NAME
                TO WS-FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING WS-SCRFDEF-FILE-NAME, WS-FILE-NAME-LENGTH. 
            OPEN INPUT SCRFDEF-FILE.
      *>
      *>     PREPARE FOR THE 'R' READ NEXT RECORD REQUEST :
      *>
            INITIALIZE SCRFDEF-RECORD.
            MOVE WS-SCREEN-NAME TO SCR-FDEF-SCREEN-NAME
            START SCRFDEF-FILE KEY IS >= SCR-FDEF-KEY
                INVALID KEY
                    PERFORM 900-CLOSE-FILE
                    MOVE 1 TO RETURN-CODE
            END-START.
            EXIT.

        500-READ-FILE.
      *>
      *>     'R' - READ NEXT RECORD
      *>
            READ SCRFDEF-FILE NEXT RECORD
                AT END
                    PERFORM 510-END-FILE
                NOT AT END
                    IF SCR-FDEF-SCREEN-NAME = WS-SCREEN-NAME THEN
                        MOVE SCR-FDEF-NO TO LS-ITEM-NAME
                        INITIALIZE LS-ITEM-DESCRIPTION
                        STRING "FLD-", SCR-FDEF-COBOL-NAME
                            INTO LS-ITEM-DESCRIPTION
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
            CLOSE SCRFDEF-FILE.
            EXIT.

        END PROGRAM COBCURSES-MENU-SD-FIELDS.
