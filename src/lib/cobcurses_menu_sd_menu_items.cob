        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-MENU-SD-MENU-ITEMS.
      *>
      *>     THIS MODULE IS A DYNAMIC MENU LOAD MODULE FOR MENU ITEMS.
      *>
      *>     LS-REQUEST-TYPE :
      *>         'X' - SET CONTEXT (MENU NAME)
      *>         'O' - OPEN FILE (DOES NOT USE OTHER ARGS)
      *>         'R' - READ NEXT (MENU ITEM)
      *>         'C' - CLOSE
      *>
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT ITEM-FILE
                ASSIGN TO WS-ITEM-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS ITM-KEY.

        DATA DIVISION.
        FILE SECTION.

        FD  ITEM-FILE.
        01  ITEM-RECORD.
            COPY ITEMRECD.

        WORKING-STORAGE SECTION.

        01  WS-MENU-NAME                PIC X(16) VALUE SPACES.

        01  WS-FILE-NAME-LENGTH         PIC 9999.
        01  WS-ITEM-FILE-NAME           PIC X(512)
            VALUE "${COBCURSES_DATADIR}/ITEMS.X".
        01  WS-X                        PIC 9999 COMP-5.

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
                MOVE LS-ITEM-NAME TO WS-MENU-NAME
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
            MOVE LENGTH OF WS-ITEM-FILE-NAME
                TO WS-FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING WS-ITEM-FILE-NAME, WS-FILE-NAME-LENGTH. 
            OPEN INPUT ITEM-FILE.
      *>
      *>     PREPARE FOR THE 'R' READ NEXT RECORD REQUEST :
      *>
            INITIALIZE ITEM-RECORD.
            MOVE WS-MENU-NAME TO ITM-MENU-NAME.
            START ITEM-FILE KEY IS >= ITM-KEY
                INVALID KEY
                    PERFORM 900-CLOSE-FILE
                    MOVE 1 TO RETURN-CODE
            END-START.
            EXIT.

        500-READ-FILE.
      *>
      *>     'R' - READ NEXT RECORD
      *>
            READ ITEM-FILE NEXT RECORD
                AT END
                    PERFORM 510-END-FILE
                NOT AT END
                    IF ITM-MENU-NAME = WS-MENU-NAME THEN
                        MOVE ITM-NUMBER TO LS-ITEM-NAME
                        INITIALIZE LS-ITEM-DESCRIPTION
                        MOVE ZERO TO WS-X
                        INSPECT ITM-ITEM-NAME TALLYING WS-X
                            FOR CHARACTERS BEFORE ' '
                        STRING "(", ITM-ITEM-NAME(1:WS-X), ") ", 
                            ITM-TEXT
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
            CLOSE ITEM-FILE.
            EXIT.

        END PROGRAM COBCURSES-MENU-SD-MENU-ITEMS.
