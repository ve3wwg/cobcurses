        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-MENU-SD-SCREENS.
      *>
      *>     THIS MODULE IS A DYNAMIC MENU LOAD MODULE FOR SCREEN NAMES.
      *>
      *>     LS-REQUEST-TYPE :
      *>         'O' - OPEN FILE (DOES NOT USE OTHER ARGS)
      *>         'R' - READ NEXT (MENU ITEM)
      *>         'C' - CLOSE
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT SCREEN-FILE
                ASSIGN TO WS-SCREEN-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS SCN-NAME.

        DATA DIVISION.
        FILE SECTION.

        FD  SCREEN-FILE.
        01  SCREEN-RECORD.
            COPY SCREEN-01.

        WORKING-STORAGE SECTION.

        01  WS-FILE-NAME-LENGTH         PIC 9999.
        01  WS-SCREEN-FILE-NAME         PIC X(256)
            VALUE "${COBCURSES_DATADIR}/SCREENS.X".

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
            MOVE LENGTH OF WS-SCREEN-FILE-NAME
                TO WS-FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING WS-SCREEN-FILE-NAME, WS-FILE-NAME-LENGTH.
            OPEN INPUT SCREEN-FILE.
      *>
      *>     PREPARE FOR THE 'R' READ NEXT RECORD REQUEST :
      *>
            INITIALIZE SCREEN-RECORD.
            START SCREEN-FILE KEY IS >= SCN-NAME
                INVALID KEY
                    PERFORM 900-CLOSE-FILE
                    MOVE 1 TO RETURN-CODE
            END-START.
            EXIT.

        500-READ-FILE.
      *>
      *>     'R' - READ NEXT RECORD
      *>
            READ SCREEN-FILE NEXT RECORD
                AT END
                    MOVE 1 TO RETURN-CODE
                    INITIALIZE LS-ITEM-NAME, LS-ITEM-DESCRIPTION
                NOT AT END
                    MOVE SCN-NAME TO LS-ITEM-NAME
                    MOVE SCN-DESCRIPTION TO LS-ITEM-DESCRIPTION
            END-READ
            EXIT.

        900-CLOSE-FILE.
      *>
      *>     'C' - CLOSE FILE
      *>
            CLOSE SCREEN-FILE.
            EXIT.

        END PROGRAM COBCURSES-MENU-SD-SCREENS.
