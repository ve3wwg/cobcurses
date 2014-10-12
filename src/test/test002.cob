        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST002.
      *> 
      *> THIS PROGRAM IS DESIGNED TO READ IN FILE test002.csv AS
      *> A NORMAL EXCELL *>.CSV COMMA DELIMITED FILE. EACH COLUMN
      *> IN THIS TEST IS REGISTERED BY ITS COLUMN HEADING NAME.
      *> THE COLUMN NAMES ARE FROM THE FIRST RECORD IN THE FILE.
      *> 
      *> THE FILE IS READ, COLUMNS EXTRACTED AND PRINTED TO
      *> STANDARD OUTPUT. THE CAPTURE OUTPUT IS THEN COMPARED
      *> WITH THE EXPECTED RESULTS.
      *> 
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT CSV-FILE
                ASSIGN TO "test002.csv"
                ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.

            FD  CSV-FILE.
            01  CSV-RECORD                      PIC X(4096).

        WORKING-STORAGE SECTION.

            COPY COBCRETC.

            01  FILLER.
                10  WS-CSV-EOF-FLAG             PIC X VALUE 'N'.
                    88  WS-CSV-EOF              VALUE 'Y'.
                10  WS-RECORD-NO                PIC 9999 VALUE 0.
                10  WS-COUNT                    PIC 9999.

            01  CSV-VALUES.
                10  CSV-ONE                     PIC X(6).
                10  CSV-TWO                     PIC X(22).
                10  CSV-THREE                   PIC X(10).
                10  CSV-FOUR                    PIC X(14).

            COPY COBCEXTRA.

        PROCEDURE DIVISION.
      *> 
      *> MAIN PROGRAM
      *> 
        MAIN-PROG.
            PERFORM 1000-INITIALIZE.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            STOP RUN.

      *> 
      *> INITIALIZATION
      *> 
        1000-INITIALIZE.
            OPEN INPUT CSV-FILE.
      *> 
      *> CLEAR ALL PRIOR ASSOCIATIONS, IF ANY.
      *> 
            PERFORM NC-CLEAR-CSV-HEADINGS.
      *> 
      *> SET ALL CSV OPTIONS (THESE ARE DEFAULTS)
      *> 
            MOVE ',' TO NC-CSV-DELIMITER.
            SET NC-SINGLE-DELIMTER TO TRUE.
            SET NC-CSV-QUOTE TO TRUE.
      *> 
      *> TELL COBCURSES WHERE THE INPUT IS COMING FROM
      *> 
            SET NC-CSV-TEXT TO ADDRESS OF CSV-RECORD.
            MOVE LENGTH OF CSV-RECORD TO NC-CSV-LENGTH.
      *> 
      *> NOW WE MUST READ THE FIRST RECORD OF THE CSV FILE
      *> TO GET THE COLUMN HEADINGS :
      *> 
            PERFORM 5100-READ-CSV
            IF NOT WS-CSV-EOF THEN
                DISPLAY "READ FIRST CSV RECORD"
                PERFORM NC-LOAD-CSV-HEADINGS
                DISPLAY "LOADED CSV HEADINGS, RC = ",
                    RETURN-CODE
            END-IF.
      *> 
      *> REGISTER COLUMN FOUR'S BUFFER AND LENGTH
      *> (REGISTER FOURTH COL AS A TEST, AS ORDER SHOULD
      *> BE UNIMPORTANT)
      *> 
            MOVE "FOURTH COLUMN" TO NC-CSV-HEADING.
            SET NC-CSV-COL-BUFFER TO ADDRESS OF CSV-FOUR.
            MOVE LENGTH OF CSV-FOUR TO NC-CSV-COL-BUFLEN.
            PERFORM NC-REGISTER-CSV-COLUMN.
      *> 
      *> REGISTER COLUMN ONE'S BUFFER AND LENGTH
      *> 
            MOVE "COLUMN 1" TO NC-CSV-HEADING.
            SET NC-CSV-COL-BUFFER TO ADDRESS OF CSV-ONE.
            MOVE LENGTH OF CSV-ONE TO NC-CSV-COL-BUFLEN.
            PERFORM NC-REGISTER-CSV-COLUMN.
      *> 
      *> REGISTER COLUMN TWO'S BUFFER AND LENGTH
      *> 
            MOVE "COL TWO" TO NC-CSV-HEADING.
            SET NC-CSV-COL-BUFFER TO ADDRESS OF CSV-TWO.
            MOVE LENGTH OF CSV-TWO TO NC-CSV-COL-BUFLEN.
            PERFORM NC-REGISTER-CSV-COLUMN.
      *> 
      *> REGISTER COLUMN THREE'S BUFFER AND LENGTH
      *> 
            MOVE "COLUMN-003" TO NC-CSV-HEADING.
            SET NC-CSV-COL-BUFFER TO ADDRESS OF CSV-THREE.
            MOVE LENGTH OF CSV-THREE TO NC-CSV-COL-BUFLEN.
            PERFORM NC-REGISTER-CSV-COLUMN.
            EXIT.

      *> 
      *> MAIN PROCESSING LOOP
      *> 
        5000-PROCESS.
            PERFORM UNTIL WS-CSV-EOF
                PERFORM 5100-READ-CSV
                IF NOT WS-CSV-EOF THEN
                    PERFORM 5000-TEST
                END-IF
            END-PERFORM.
            EXIT.

      *> 
      *> PROCESS ONE *.CSV RECORD
      *> 
        5000-TEST.
            PERFORM NC-EXTRACT-CSV-RECORD
            EVALUATE RETURN-CODE
            WHEN NC-RET-OK
                DISPLAY "CSV RECORD LOADED SUCCESSFULLY :"
            WHEN NC-RET-TRUNCATED
                DISPLAY "CSV RECORD LOADED WITH TRUNCATED FIELDS :"
            WHEN OTHER
                DISPLAY "*** UNEXPECTED RETURN-CODE = ",
                    RETURN-CODE, " :"
            END-EVALUATE.

            PERFORM NC-COUNT-CSV-COLUMNS.
            MOVE NC-CSV-COLUMNS TO WS-COUNT.
            DISPLAY "FOUND ", WS-COUNT, " COLUMNS.".

            DISPLAY "COLUMN 1 PIC X(6)  : '", CSV-ONE, "'".
            DISPLAY "COLUMN 2 PIC X(22) : '", CSV-TWO, "'".
            DISPLAY "COLUMN 3 PIC X(10) : '", CSV-THREE, "'".
            DISPLAY "COLUMN 4 PIC X(14) : '", CSV-FOUR, "'".
            DISPLAY "END RECORD         : ", WS-RECORD-NO.
            DISPLAY " ".
            EXIT.

      *> 
      *> READ ONE *.CSV RECORD
      *> 
        5100-READ-CSV.
            READ CSV-FILE
                AT END
                    SET WS-CSV-EOF TO TRUE
                NOT AT END
                    ADD 1 TO WS-RECORD-NO
            END-READ.
            EXIT.

      *> 
      *> PROGRAM CLEANUP
      *> 
        9000-FINALIZE.
            CLOSE CSV-FILE.
      *>
      *> CLEARING THE HEADINGS IS NOT STRICTLY REQUIRED,
      *> BUT IS DONE HERE AS PART OF THE TEST (DOES IT ABORT?)
      *>
            PERFORM NC-CLEAR-CSV-HEADINGS.
            EXIT.

      *> 
      *> SUPPORT ROUTINES
      *> 
            COPY COBCURSX.

        END PROGRAM TEST002.
