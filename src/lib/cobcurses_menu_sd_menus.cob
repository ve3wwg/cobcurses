        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-MENU-SD-MENUS.
      *>
      *>     THIS MODULE IS A DYNAMIC MENU LOAD MODULE FOR MENUS.
      *>
      *>     LS-REQUEST-TYPE :
      *>         'O' - OPEN FILE (DOES NOT USE OTHER ARGS)
      *>         'R' - READ NEXT (MENU ITEM)
      *>         'C' - CLOSE
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT MENU-FILE
                ASSIGN TO WS-MENU-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS MNU-MENU-NAME.

        DATA DIVISION.
        FILE SECTION.

        FD  MENU-FILE.
        01  MENU-RECORD.
            COPY MENURECD.

        WORKING-STORAGE SECTION.

        01  WS-FILE-NAME-LENGTH         PIC 9999.
        01  WS-MENU-FILE-NAME           PIC X(512)
            VALUE "${COBCURSES_DATADIR}/MENUS.X".

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
            MOVE LENGTH OF WS-MENU-FILE-NAME
                TO WS-FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING WS-MENU-FILE-NAME, WS-FILE-NAME-LENGTH.
            OPEN INPUT MENU-FILE.
      *>
      *>     PREPARE FOR THE 'R' READ NEXT RECORD REQUEST :
      *>
            INITIALIZE MENU-RECORD.
            START MENU-FILE KEY IS >= MNU-KEY
                INVALID KEY
                    PERFORM 900-CLOSE-FILE
                    MOVE 1 TO RETURN-CODE
            END-START.
            EXIT.

        500-READ-FILE.
      *>
      *>     'R' - READ NEXT RECORD
      *>
            READ MENU-FILE NEXT RECORD
                AT END
                    MOVE 1 TO RETURN-CODE
                    INITIALIZE LS-ITEM-NAME, LS-ITEM-DESCRIPTION
                NOT AT END
                    MOVE MNU-MENU-NAME TO LS-ITEM-NAME
                    INITIALIZE LS-ITEM-DESCRIPTION
                    STRING MNU-TITLE, " (", MNU-MENU-TYPE, ")"
                        INTO LS-ITEM-DESCRIPTION
            END-READ
            EXIT.

        900-CLOSE-FILE.
      *>
      *>     'C' - CLOSE FILE
      *>
            CLOSE MENU-FILE.
            EXIT.

        END PROGRAM COBCURSES-MENU-SD-MENUS.
