        PROGRAM-ID. COBCURSES-INIT-PATHNAME.
      *>
      *>     SUBSTITUTE ANY OCCURRENCES OF ${VARNAME} IN THE PATHNAME
      *>     WITH THE CONTENTS OF AN ENVIRONMENT VARIABLE
      *>
      *> INPUTS:
      *>     IO-PATHNAME                 MANDATORY
      *>     IN-LENGTH                   OPTIONAL
      *>
      *> OMITTED PARAMETERS:
      *>     WHEN IN-LENGTH IS OMITTED, A LENGTH PROVIDED IN
      *>     A FORMER CALL IS ASSUMED. IF THERE IS NO FORMER
      *>     LENGTH, THEN NC-RET-BADPARM IS RETURNED.
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

            COPY COBCRETC.

        01  WS-PATHNAME-PTR             POINTER.
        01  WS-PATH-LENGTH-PTR          POINTER.
        01  WS-PATH-LENGTH              PIC 9999 COMP-5 VALUE 0.

        LINKAGE SECTION.

        01  IO-PATHNAME                 PIC X(2048).
        01  IN-LENGTH                   PIC 9999.

        PROCEDURE DIVISION 
            USING IO-PATHNAME, IN-LENGTH.

            IF IO-PATHNAME OMITTED THEN
                MOVE NC-RET-BADPARM TO RETURN-CODE
            ELSE
                MOVE ZERO TO RETURN-CODE
            END-IF

            IF IN-LENGTH OMITTED THEN
                IF WS-PATH-LENGTH < 1 THEN
                    MOVE NC-RET-BADPARM TO RETURN-CODE
                END-IF
            ELSE
                MOVE IN-LENGTH TO WS-PATH-LENGTH
            END-IF.

            IF RETURN-CODE = ZERO THEN
                SET WS-PATHNAME-PTR
                    TO ADDRESS OF IO-PATHNAME
                SET WS-PATH-LENGTH-PTR
                    TO ADDRESS OF WS-PATH-LENGTH
                CALL "NC_PATHNAME" USING
                    WS-PATHNAME-PTR, WS-PATH-LENGTH-PTR
            END-IF.
            GOBACK.

        END PROGRAM COBCURSES-INIT-PATHNAME.
