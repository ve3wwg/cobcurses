        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST003.
      *> 
      *> THIS PROGRAM IS DESIGNED TO READ IN FILE test003.txt AS
      *> A FILE CONTAINING COMMANDS AND COMMAND OUTPUT DATA TO
      *> BE PARSED BY BLANK DELIMITED FIELDS, USING THE BACKSLASH
      *> ESCAPE CONVENTION.
      *> 
      *> THE FILE IS READ, COLUMNS EXTRACTED AND PRINTED TO
      *> STANDARD OUTPUT. THE CAPTURE OUTPUT IS THEN COMPARED
      *> WITH THE EXPECTED RESULTS.
      *> 
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT CSV-FILE
                ASSIGN TO "test003.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.

            FD  CSV-FILE.
            01  CSV-RECORD                      PIC X(4096).

        WORKING-STORAGE SECTION.

            01  FILLER.
                10  WS-CSV-EOF-FLAG             PIC X VALUE 'N'.
                    88  WS-CSV-EOF              VALUE 'Y'.
                10  WS-RECORD-NO                PIC 9999 VALUE 0.
                10  WS-COUNT                    PIC 999.

            01  WS-FIELD-TEXT                   PIC X(60).
            01  WS-FIELD-NO                     PIC 99.

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
      *> SET ALL CSV OPTIONS - THESE ARE SET SO THAT
      *>     1. BLANK IS THE DELIMITER
      *>     2. MULTIPLE BLANKS IS TREATED AS 1 DELIMITER
      *>     3. ESCAPING IS DONE WITH THE BACKSLASH
      *> 
            MOVE SPACE TO NC-CSV-DELIMITER.
            SET NC-MULT-DEL-AS-ONE TO TRUE.
            SET NC-CSV-BACKSLASH TO TRUE.
      *> 
      *> TELL COBCURSES WHERE THE INPUT IS COMING FROM
      *> 
            SET NC-CSV-TEXT TO ADDRESS OF CSV-RECORD.
            MOVE LENGTH OF CSV-RECORD TO NC-CSV-LENGTH.
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
            DISPLAY "READ RECORD ", WS-RECORD-NO.
            PERFORM NC-COUNT-CSV-COLUMNS.
            MOVE NC-CSV-COLUMNS TO WS-COUNT.
            DISPLAY WS-COUNT, " COLUMNS FOUND."

            PERFORM
              VARYING WS-FIELD-NO FROM 1 BY 1
              UNTIL WS-FIELD-NO > NC-CSV-COLUMNS
                PERFORM 5200-DUMP-FIELD
            END-PERFORM.

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
      *> DUMP THE EXTRACTED FIELD CONTENTS :
      *>
        5200-DUMP-FIELD.
            DISPLAY " * FIELD # ", WS-FIELD-NO.
            MOVE WS-FIELD-NO TO NC-EXTRACT-FIELD.
            SET NC-EXTRACT-BUFFER TO ADDRESS OF WS-FIELD-TEXT.
            MOVE LENGTH OF WS-FIELD-TEXT TO NC-EXTRACT-BUFLEN.
            PERFORM NC-EXTRACT-CSV-FIELD.
            EVALUATE RETURN-CODE
                WHEN 0
                    DISPLAY "      DATA IS <", WS-FIELD-TEXT, ">"
                WHEN 4
                    DISPLAY "     TRUNC IS <", WS-FIELD-TEXT, ">"
                WHEN OTHER
                    DISPLAY "     RETURN-CODE IS ", RETURN-CODE, " !"
            END-EVALUATE.
            EXIT.

      *> 
      *> PROGRAM CLEANUP
      *> 
        9000-FINALIZE.
            CLOSE CSV-FILE.
      *>
      *> CLEARING THE HEADINGS IS NOT REQUIRED,
      *> BUT IS DONE HERE AS PART OF THE TEST (DOES IT ABORT?)
      *>
            PERFORM NC-CLEAR-CSV-HEADINGS.
            EXIT.

      *> 
      *> SUPPORT ROUTINES
      *> 
            COPY COBCURSX.

        END PROGRAM TEST003.
