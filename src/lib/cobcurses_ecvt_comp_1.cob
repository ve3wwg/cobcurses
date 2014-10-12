        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-ECVT-COMP-1.
      *>
      *> CONVERT A COMP-1 ITEM INTO DISPLAYABLE TEXT :
      *>
      *>     LS-BUFFER RECEIVES LS-BUFLEN DIGITS.
      *>     LS-EXPONENT INDICATES THE EXPONENT'S VALUE.
      *>
      *> FOR EXAMPLE:
      *>
      *>     RETURNED BUFFER RESULTS ARE '+2300000'
      *>     RETURNED EXPONENT IS:       +001        
      *>     THIS MEANS THE VALUE IS +.23 X 10**+1
      *>
      *> INPUTS: 
      *>     LS-COMP-1       COMP-1.     (MANDATORY)
      *>                     THIS IS THE INPUT VALUE TO CONVERT.
      *>     LS-BUFFER       PIC X(*).   (MANDATORY)
      *>                     THIS IS THE RECEIVING BUFFER FOR 
      *>                     THE FORMATTED RESULTS.
      *>     LS-BUFLEN       PIC 99.     (OPTIONAL)
      *>                     THIS IS THE LENGTH OF LS-BUFFER.
      *> OUTPUTS:
      *>     LS-EXPONENT     PIC S999.   (OPTIONAL)
      *>                     RETURNED VALUE REPRESENTING THE
      *>                     EXPONENT'S VALUE.
      *>
      *> OPTIONAL PARAMETERS :
      *>     WHEN LS-BUFFER IS OMITTED, THE LAST VALUE USED
      *>         IS ASSUMED.
      *>     WHEN LS-EXPONENT IS OMITTED, NO EXPONENT VALUE
      *>         IS RETURNED.
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

        01  WS-COMP-1                       COMP-1.
        01  WS-BUFFER-PTR                   POINTER.
        01  WS-BUFLEN                       PIC 9999 COMP-5
                                            VALUE 0.
        01  WS-EXPONENT                     PIC S999 COMP-5.

        LINKAGE SECTION.

        01  LS-COMP-1                       COMP-1.
        01  LS-BUFFER                       PIC X(1024).
        01  LS-BUFLEN                       PIC 99.
        01  LS-EXPONENT                     PIC S999.

        PROCEDURE DIVISION
            USING LS-COMP-1, LS-BUFFER, LS-BUFLEN, LS-EXPONENT.

        MAIN-PROG.
            PERFORM 1000-INITIALIZE.
            IF RETURN-CODE = NC-RET-OK THEN
                PERFORM 5000-PROCESS
            END-IF.
            PERFORM 9000-FINALIZE.
            GOBACK.

        1000-INITIALIZE.
            IF ( LS-COMP-1 OMITTED ) OR ( LS-BUFFER OMITTED ) THEN
                MOVE NC-RET-BADPARM TO RETURN-CODE
            ELSE
                PERFORM 1100-INITIALIZE
            END-IF.
            EXIT.

        1100-INITIALIZE.
            MOVE ZERO TO RETURN-CODE.
            MOVE LS-COMP-1 TO WS-COMP-1.
            SET WS-BUFFER-PTR TO ADDRESS OF LS-BUFFER.
            IF NOT LS-BUFLEN OMITTED THEN
                MOVE LS-BUFLEN TO WS-BUFLEN
            END-IF.
            MOVE ZERO TO WS-EXPONENT.
            EXIT.

        5000-PROCESS.
            CALL "NC_ECVT_COMP1" USING
                WS-COMP-1, WS-BUFFER-PTR, WS-BUFLEN, WS-EXPONENT.
            EXIT.

        9000-FINALIZE.
            IF NOT LS-EXPONENT OMITTED THEN
                IF RETURN-CODE = NC-RET-OK THEN
                    MOVE WS-EXPONENT TO LS-EXPONENT
                END-IF
            END-IF.
            EXIT.

        END PROGRAM COBCURSES-ECVT-COMP-1.
