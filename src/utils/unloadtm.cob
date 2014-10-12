        IDENTIFICATION DIVISION.
        PROGRAM-ID. UNLOADTM.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT DATA-FILE
                ASSIGN TO DATA-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC
                RECORD KEY IS DTA-KEY OF DATA-RECORD.

            SELECT U-FILE ASSIGN TO "data_tm.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.

        FD  U-FILE.
        01  U-RECD.
            COPY TEMPDATA.

        FD  DATA-FILE.
        01  DATA-RECORD.
            COPY TEMPDATA.

        WORKING-STORAGE SECTION.

        01  EOF-FLAG                            PIC X.
            88  HAVE-DATA                       VALUE 'N'.
            88  END-OF-FILE                     VALUE 'Y'.

        01  MISC.
            10  FILE-NAME-LENGTH                PIC 9999.
            10  DATA-FILE-NAME                  PIC X(256)
                VALUE "${COBCURSES_DATADIR}/TEMPLATE.X".
            10  RECORD-COUNT                    PIC 9999 VALUE 0.

        COPY COBCATTR.
        COPY COBCURSG.
        COPY COBCURSL.

        PROCEDURE DIVISION.

            MOVE LENGTH OF DATA-FILE-NAME TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME" USING
                BY REFERENCE DATA-FILE-NAME, FILE-NAME-LENGTH.

            OPEN INPUT DATA-FILE.
            OPEN OUTPUT U-FILE.

            INITIALIZE DATA-RECORD.

            START DATA-FILE KEY IS >= DTA-KEY OF DATA-RECORD
                INVALID KEY
                    SET END-OF-FILE TO TRUE
                NOT INVALID KEY
                    SET HAVE-DATA TO TRUE
            END-START.

            PERFORM UNTIL END-OF-FILE
                READ DATA-FILE NEXT RECORD
                    AT END
                        SET END-OF-FILE TO TRUE
                    NOT AT END
                        PERFORM MOVE-DATA
                        WRITE U-RECD
                        ADD 1 TO RECORD-COUNT
                END-READ
            END-PERFORM.

            CLOSE U-FILE.
            CLOSE DATA-FILE.

            DISPLAY "UNLOADED ", RECORD-COUNT, " RECDS FOR TEMPLATE.X".
            STOP RUN.
            
        MOVE-DATA.
            MOVE DTA-KEY OF DATA-RECORD
                TO DTA-KEY OF U-RECD.
            MOVE DTA-DATA OF DATA-RECORD
                TO DTA-DATA OF U-RECD.
            MOVE DTA-DESC OF DATA-RECORD
                TO DTA-DESC OF U-RECD.
            MOVE DTA-COMP-2 OF DATA-RECORD
                TO DTA-COMP-2 OF U-RECD.
            EXIT.

            COPY NULLEVENTS.
            COPY COBCURSQ.

        END PROGRAM UNLOADTM.
