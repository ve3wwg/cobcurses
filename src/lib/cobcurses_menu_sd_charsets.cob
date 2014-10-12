        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-MENU-SD-CHARSETS.
      *>
      *>     THIS MODULE IS A DYNAMIC MENU LOAD MODULE FOR CHARSETS.
      *>
      *>     LS-REQUEST-TYPE :
      *>         'O' - OPEN FILE (DOES NOT USE OTHER ARGS)
      *>         'R' - READ NEXT (MENU ITEM)
      *>         'C' - CLOSE
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT CHARSET-FILE
                ASSIGN TO WS-CHARSET-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS CHARSET-NAME.

        DATA DIVISION.
        FILE SECTION.

        FD  CHARSET-FILE.
        01  CHARSET-RECORD.
            COPY SCREEN-CS.

        WORKING-STORAGE SECTION.

        01  WS-FILE-NAME-LENGTH         PIC 9999.
        01  WS-CHARSET-FILE-NAME        PIC X(512)
            VALUE "${COBCURSES_DATADIR}/SCRCHRSET.X".

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
            MOVE LENGTH OF WS-CHARSET-FILE-NAME
                TO WS-FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING WS-CHARSET-FILE-NAME, WS-FILE-NAME-LENGTH.
            OPEN INPUT CHARSET-FILE.
      *>
      *>     PREPARE FOR THE 'R' READ NEXT RECORD REQUEST :
      *>
            INITIALIZE CHARSET-RECORD.
            START CHARSET-FILE KEY IS >= CHARSET-NAME
                INVALID KEY
                    PERFORM 900-CLOSE-FILE
                    MOVE 1 TO RETURN-CODE
            END-START.
            EXIT.

        500-READ-FILE.
      *>
      *>     'R' - READ NEXT RECORD
      *>
            READ CHARSET-FILE NEXT RECORD
                AT END
                    MOVE 1 TO RETURN-CODE
                    INITIALIZE LS-ITEM-NAME, LS-ITEM-DESCRIPTION
                NOT AT END
                    MOVE CHARSET-NAME TO LS-ITEM-NAME
                    MOVE CHARSET-DATA TO LS-ITEM-DESCRIPTION
            END-READ
            EXIT.

        900-CLOSE-FILE.
      *>
      *>     'C' - CLOSE FILE
      *>
            CLOSE CHARSET-FILE.
            EXIT.

        END PROGRAM COBCURSES-MENU-SD-CHARSETS.
