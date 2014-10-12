        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-FORMAT-COMP-1.
      *>
      *> FORMAT A COMP-1 INTO A USER READABLE FORMAT, POSSIBLY
      *> IN EXPONENTIAL NOTATION, OPTIONALLY ENGINEERING EXPONENTS,
      *> OPTIONALLY WITH UNITS.
      *>
      *> NOTE :
      *>     THIS ROUTINE PASSES THE BUCK TO COBCURSES-FORMAT-COMP-2.
      *>     THE IMPLICATION OF THAT IS THAT THE LAST LENGTH USED
      *>     FOR OMITTED LENGTH ARGUMENT IS SHARED BETWEEN BOTH
      *>     OF THESE ROUTINES.
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

            01  WS-IN-COMP-2-ITEM           COMP-2.

        LINKAGE SECTION.

            77  LS-IN-COMP-1-ITEM           COMP-1.
            77  LS-IN-ENG-FORMAT-FLAG       PIC X.
            77  LS-IN-DIGITS                PIC 99.
            77  LS-IN-UNITS-CONFIG          PIC X(80).
            77  LS-OUT-BUFFER               PIC X(40).
            77  LS-IN-BUFFER-LENGTH         PIC 99.

        PROCEDURE DIVISION
          USING
            LS-IN-COMP-1-ITEM,
            LS-IN-ENG-FORMAT-FLAG,
            LS-IN-DIGITS,
            LS-IN-UNITS-CONFIG,
            LS-OUT-BUFFER,
            LS-IN-BUFFER-LENGTH.

            IF LS-IN-COMP-1-ITEM OMITTED
                MOVE NC-RET-BADPARM TO RETURN-CODE
            ELSE
                MOVE LS-IN-COMP-1-ITEM TO WS-IN-COMP-2-ITEM
                CALL "COBCURSES-FORMAT-COMP-2" USING
                    WS-IN-COMP-2-ITEM,
                    LS-IN-ENG-FORMAT-FLAG,
                    LS-IN-DIGITS,
                    LS-IN-UNITS-CONFIG,
                    LS-OUT-BUFFER,
                    LS-IN-BUFFER-LENGTH
            END-IF.
            GOBACK.

        END PROGRAM COBCURSES-FORMAT-COMP-1.
