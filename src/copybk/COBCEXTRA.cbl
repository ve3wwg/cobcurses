      *>
      *> THIS DECLARES VALUES USED BY EXTRA COBCURSES
      *> LIBRARY SUPPORT FEATURES.
      *>
        01  COBCURSES-EXTRAS.
            10  NC-EXTRA-STATE.
                15  NC-SHLIB-EXTRA-FLAG     PIC X VALUE 'N'.
                    88  NC-SHLIB-EXTRA-NI   VALUE 'N'.
            10  NC-EXTRA-CONFIG.
                15  NC-CSV-DELIMITER        PIC X VALUE ','.
                15  NC-CSV-MULT-DEL-FLAG    PIC X VALUE 'N'.
                    88  NC-SINGLE-DELIMTER  VALUE 'N'.
                    88  NC-MULT-DEL-AS-ONE  VALUE 'Y'.
                15  NC-CSV-QUOTE-CONVENTION PIC X VALUE '"'.
                    88  NC-CSV-QUOTE        VALUE '"'.
                    88  NC-CSV-BACKSLASH    VALUE '\'.
            10  COBCURSES-EXTRA-CONSTANTS.
                15  NC-NULL-HANDLE          PIC 9(9) VALUE 999999999.
            10  COBCURSES-CSV-TEXT.
                15  NC-CSV-TEXT             POINTER VALUE NULL
                                            SYNCHRONIZED.
                15  NC-CSV-LENGTH           PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                15  NC-CSV-COLUMNS          PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
            10  COBCURSES-EXTRACT.
                15  NC-EXTRACT-FIELD        PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                15  NC-EXTRACT-BUFFER       POINTER VALUE NULL
                                            SYNCHRONIZED.
                15  NC-EXTRACT-BUFLEN       PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
            10  COBCURSES-REGISTER-COLUMN.
                15  NC-CSV-HEADING          PIC X(64).
                15  NC-CSV-COLUMN-NO        PIC 9999 COMP-5
                                            SYNCHRONIZED.
                15  NC-CSV-COL-BUFFER       POINTER VALUE NULL
                                            SYNCHRONIZED.
                15  NC-CSV-COL-BUFLEN       PIC 9999 COMP-5
                                            SYNCHRONIZED.
      *>
      *>    END COBCEXTRA
      *>

