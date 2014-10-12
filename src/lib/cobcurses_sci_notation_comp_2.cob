        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSES-SCI-NOTATION-COMP-2.
      *>
      *> CONVERT A COMP-2 ITEM INTO DISPLAYABLE EXPONENTIAL FORMAT :
      *>
      *> INPUTS: 
      *>     LS-COMP-2       COMP-2.     (MANDATORY)
      *>                     THIS IS THE INPUT VALUE TO CONVERT.
      *>     LS-BUFFER       PIC X(*).   (MANDATORY)
      *>                     THIS IS THE RECEIVING BUFFER FOR 
      *>                     THE FORMATTED RESULTS.
      *>     LS-BUFLEN       PIC 99.     (OPTIONAL)
      *>                     THIS IS THE LENGTH OF LS-BUFFER.
      *>     LS-FORMAT       PIC X.      (OPTIONAL)
      *>                     'N' - PLAIN EXPONENTIAL FORMAT
      *>                     'Y' - ADJUST EXPONENT TO ENGINEERING
      *>                           MULTIPLE OF 3 (DEFAULT)
      *> OUTPUTS:
      *>     LS-EXPONENT     PIC S999.   (OPTIONAL)
      *>                     RETURNED VALUE REPRESENTING THE
      *>                     EXPONENT'S VALUE.
      *>     LS-E-OFFSET     PIC 99.     (OPTIONAL)
      *>                     RETURNED 1 BASED OFFSET TO 'E' CHAR.
      *>                 
      *> OPTIONAL PARAMETERS :
      *>     WHEN LS-BUFFER IS OMITTED, THE LAST VALUE USED
      *>         IS ASSUMED.
      *>     WHEN LS-FORMAT IS OMITTED, 'Y' IS ASSUMED.
      *>     WHEN LS-EXPONENT IS OMITTED, NO EXPONENT VALUE
      *>         IS RETURNED.
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

        01  WS-COMP-2                       COMP-2 SYNCHRONIZED.
        01  WS-BUFFER-PTR                   POINTER.
        01  WS-BUFLEN                       PIC 9999 COMP-5
                                            VALUE 0.
        01  WS-FORMAT                       PIC X VALUE 'Y'.
            88  WS-ENGINEERING-FORMAT       VALUE 'Y'
                FALSE IS                    'N'.
        01  WS-EXPONENT                     PIC S999 COMP-5.
        01  WS-E-OFFSET                     PIC 9999 COMP-5.

        LINKAGE SECTION.

        01  LS-COMP-2                       COMP-2.
        01  LS-BUFFER                       PIC X(1024).
        01  LS-BUFLEN                       PIC 99.
        01  LS-FORMAT                       PIC X.
        01  LS-EXPONENT                     PIC S999.
        01  LS-E-OFFSET                     PIC 99.

        PROCEDURE DIVISION
            USING LS-COMP-2, LS-BUFFER, LS-BUFLEN,
                LS-FORMAT, LS-EXPONENT, LS-E-OFFSET.

        MAIN-PROG.
            PERFORM 1000-INITIALIZE.
            IF RETURN-CODE = NC-RET-OK THEN
                PERFORM 5000-PROCESS
            END-IF.
            PERFORM 9000-FINALIZE.
            GOBACK.

        1000-INITIALIZE.
            IF ( LS-COMP-2 OMITTED ) OR ( LS-BUFFER OMITTED ) THEN
                MOVE NC-RET-BADPARM TO RETURN-CODE
            ELSE
                PERFORM 1100-INITIALIZE
            END-IF.
            EXIT.

        1100-INITIALIZE.
            MOVE ZERO TO RETURN-CODE.
            MOVE LS-COMP-2 TO WS-COMP-2.
            SET WS-BUFFER-PTR TO ADDRESS OF LS-BUFFER.
            IF NOT LS-BUFLEN OMITTED THEN
                MOVE LS-BUFLEN TO WS-BUFLEN
            END-IF.
            IF NOT LS-FORMAT OMITTED THEN
                MOVE LS-FORMAT TO WS-FORMAT
            ELSE
                MOVE "Y" TO WS-FORMAT
            END-IF.
            MOVE ZERO TO WS-EXPONENT.
            EXIT.

        5000-PROCESS.
            CALL "NC_SCINOTATION_COMP2" USING
                WS-COMP-2, WS-BUFFER-PTR, WS-BUFLEN,
                WS-FORMAT, WS-EXPONENT, WS-E-OFFSET.
            EXIT.

        9000-FINALIZE.
            IF NOT LS-EXPONENT OMITTED THEN
                IF RETURN-CODE = NC-RET-OK THEN
                    MOVE WS-EXPONENT TO LS-EXPONENT
                END-IF
            END-IF.
            IF NOT LS-E-OFFSET OMITTED THEN
                MOVE WS-E-OFFSET TO LS-E-OFFSET
            END-IF.
            EXIT.

        END PROGRAM COBCURSES-SCI-NOTATION-COMP-2.
