        IDENTIFICATION DIVISION.
        PROGRAM-ID. LOADTM.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT U-FILE ASSIGN TO WS-INPUT-FILENAME
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT DATA-FILE
                ASSIGN TO DATA-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS RANDOM
                RECORD KEY IS DTA-KEY OF DATA-RECORD.

        DATA DIVISION.
        FILE SECTION.

        FD  U-FILE.
        01  U-RECD.
            COPY TEMPDATA.

        FD  DATA-FILE.
        01  DATA-RECORD.
            COPY TEMPDATA.

        WORKING-STORAGE SECTION.

        01  WS-ENV-VAR                          PIC X(512).

        01  WS-INPUT-FILENAME                   PIC X(512)
            VALUE "data_tm.txt".

        01  EOF-FLAG                            PIC X VALUE 'N'.
            88  HAVE-DATA                       VALUE 'N'.
            88  END-OF-FILE                     VALUE 'Y'.

        01  MISC.
            10  FILE-NAME-LENGTH                PIC 9999.
            10  DATA-FILE-NAME                  PIC X(256)
                VALUE "${COBCURSES_DATADIR}/TEMPLATE.X".
            10  RECORD-COUNT                    PIC 9999 VALUE 0.

        PROCEDURE DIVISION.

        MAIN-PROGRAM.
            MOVE LENGTH OF DATA-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                BY REFERENCE DATA-FILE-NAME, FILE-NAME-LENGTH.

            ACCEPT WS-ENV-VAR
                FROM ENVIRONMENT "COBCURSES_SDLOAD_FILE".
            IF WS-ENV-VAR NOT = SPACES THEN
                MOVE WS-ENV-VAR TO WS-INPUT-FILENAME
            END-IF.

            OPEN INPUT U-FILE.
            OPEN OUTPUT DATA-FILE.

            INITIALIZE DATA-RECORD.

            PERFORM UNTIL END-OF-FILE
                READ U-FILE 
                    AT END
                        SET END-OF-FILE TO TRUE
                    NOT AT END
                        MOVE U-RECD TO DATA-RECORD
                        WRITE DATA-RECORD
                            INVALID KEY
                                DISPLAY "DUP KEY: ",
                                    DTA-KEY OF DATA-RECORD,
                                    "|", DTA-DATA OF DATA-RECORD
                            NOT INVALID KEY
                                ADD 1 TO RECORD-COUNT
                        END-WRITE
                END-READ
            END-PERFORM.

            CLOSE U-FILE.
            CLOSE DATA-FILE.

            DISPLAY "LOADED ", RECORD-COUNT, " RECORDS TO TEMPLATE.X".
            STOP RUN.
            
        END PROGRAM LOADTM.
