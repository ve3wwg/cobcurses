        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-EDIT-COMP-1.

        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

            01  WS-OUT-COMP-1-ITEM          COMP-1.
            01  WS-IN-TEXT-PTR              POINTER VALUE NULL.
            01  WS-IN-TEXT-LENGTH           PIC 9999 COMP-5 VALUE 0.
            01  WS-IN-UNITS-CONFIG-PTR      POINTER VALUE NULL.

        LINKAGE SECTION.

            77  LS-IN-TEXT                  PIC X(40).
            77  LS-IN-TEXT-LENGTH           PIC 9999.
            77  LS-IN-UNITS-CONFIG          PIC X(80).
            77  LS-OUT-COMP-1-ITEM          COMP-1.

        PROCEDURE DIVISION
          USING
            LS-IN-TEXT,
            LS-IN-TEXT-LENGTH,
            LS-IN-UNITS-CONFIG,
            LS-OUT-COMP-1-ITEM.

            IF LS-IN-TEXT OMITTED
            OR LS-OUT-COMP-1-ITEM OMITTED THEN
                MOVE NC-RET-BADPARM TO RETURN-CODE
            ELSE
                PERFORM 1000-INITIALIZE
                PERFORM 5000-PROCESS
                PERFORM 9000-FINALIZE
            END-IF.
            GOBACK.

        1000-INITIALIZE.
            SET WS-IN-TEXT-PTR TO ADDRESS OF LS-IN-TEXT.
            IF NOT LS-IN-TEXT-LENGTH OMITTED THEN
                MOVE LS-IN-TEXT-LENGTH TO WS-IN-TEXT-LENGTH
            END-IF.            
            IF NOT LS-IN-UNITS-CONFIG OMITTED THEN
                SET WS-IN-UNITS-CONFIG-PTR TO
                    ADDRESS OF LS-IN-UNITS-CONFIG
            ELSE
                SET WS-IN-UNITS-CONFIG-PTR TO NULL
            END-IF.
            EXIT.

        5000-PROCESS.
            CALL "NC_EDIT_COMP1" USING
                WS-OUT-COMP-1-ITEM,
                WS-IN-TEXT-PTR,
                WS-IN-TEXT-LENGTH,
                WS-IN-UNITS-CONFIG-PTR.
            EXIT.

        9000-FINALIZE.
            IF RETURN-CODE = NC-RET-OK THEN
                MOVE WS-OUT-COMP-1-ITEM TO LS-OUT-COMP-1-ITEM
            ELSE
                MOVE ZERO TO LS-OUT-COMP-1-ITEM
            END-IF.
            EXIT.

        END PROGRAM COBCURSES-EDIT-COMP-1.
