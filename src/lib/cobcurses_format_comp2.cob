        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-FORMAT-COMP-2.
      *>
      *> FORMAT A COMP-2 INTO A USER READABLE FORMAT, POSSIBLY
      *> IN EXPONENTIAL NOTATION, OPTIONALLY ENGINEERING EXPONENTS,
      *> OPTIONALLY WITH UNITS.
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

            01  WS-IN-COMP-2-ITEM           COMP-2.
            01  WS-IN-ENG-FORMAT-FLAG       PIC X.
            01  WS-IN-DIGITS                PIC 9999 COMP-5.
            01  WS-IN-UNITS-CONFIG-PTR      POINTER.
            01  WS-OUT-BUFFER-PTR           POINTER.
            01  WS-IN-BUFFER-LENGTH         PIC 9999 COMP-5 VALUE 0.

        LINKAGE SECTION.

            77  LS-IN-COMP-2-ITEM           COMP-2.
            77  LS-IN-ENG-FORMAT-FLAG       PIC X.
            77  LS-IN-DIGITS                PIC 99.
            77  LS-IN-UNITS-CONFIG          PIC X(80).
            77  LS-OUT-BUFFER               PIC X(40).
            77  LS-IN-BUFFER-LENGTH         PIC 99.

        PROCEDURE DIVISION
          USING
            LS-IN-COMP-2-ITEM,
            LS-IN-ENG-FORMAT-FLAG,
            LS-IN-DIGITS,
            LS-IN-UNITS-CONFIG,
            LS-OUT-BUFFER,
            LS-IN-BUFFER-LENGTH.

            IF LS-IN-COMP-2-ITEM OMITTED
            OR LS-OUT-BUFFER OMITTED
            OR LS-IN-DIGITS OMITTED THEN
                MOVE NC-RET-BADPARM TO RETURN-CODE
            ELSE
                PERFORM 1000-INITIALIZE
                PERFORM 5000-PROCESS
                PERFORM 9000-FINALIZE
            END-IF.
            GOBACK.

        1000-INITIALIZE.
            MOVE LS-IN-COMP-2-ITEM TO WS-IN-COMP-2-ITEM.
            IF LS-IN-ENG-FORMAT-FLAG OMITTED THEN
                MOVE 'Y' TO WS-IN-ENG-FORMAT-FLAG
            ELSE
                MOVE LS-IN-ENG-FORMAT-FLAG TO WS-IN-ENG-FORMAT-FLAG
            END-IF.
            MOVE LS-IN-DIGITS TO WS-IN-DIGITS.
            IF LS-IN-UNITS-CONFIG OMITTED THEN
                SET WS-IN-UNITS-CONFIG-PTR TO NULL
            ELSE
                SET WS-IN-UNITS-CONFIG-PTR TO
                    ADDRESS OF LS-IN-UNITS-CONFIG
            END-IF.
            SET WS-OUT-BUFFER-PTR TO ADDRESS OF LS-OUT-BUFFER.
            IF NOT LS-IN-BUFFER-LENGTH OMITTED THEN
                MOVE LS-IN-BUFFER-LENGTH TO WS-IN-BUFFER-LENGTH
            END-IF.
            EXIT.

        5000-PROCESS.
            CALL "NC_FORMAT_COMP2" USING
                WS-IN-COMP-2-ITEM,
                WS-IN-ENG-FORMAT-FLAG,
                WS-IN-DIGITS,
                WS-IN-UNITS-CONFIG-PTR,
                WS-OUT-BUFFER-PTR,
                WS-IN-BUFFER-LENGTH.
            EXIT.

        9000-FINALIZE.
            EXIT.

        END PROGRAM COBCURSES-FORMAT-COMP-2.
