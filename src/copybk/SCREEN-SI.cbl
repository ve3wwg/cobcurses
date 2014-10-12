            10  SCREEN-COLUMNS                  PIC 999 COMP 
                                                SYNCHRONIZED.
            10  SCREEN-LINES                    PIC 999 COMP
                                                SYNCHRONIZED.
            10  SCREEN-LAST-LINE                PIC 999 COMP
                                                SYNCHRONIZED.
            10  SCREEN-LAST-COLUMN              PIC 999 COMP
                                                SYNCHRONIZED.
            10  SCREEN-HAS-TITLE                PIC X.
            10  SCREEN-SHOW-DATE                PIC X.
            10  SCREEN-SHOW-TIME                PIC X.
            10  SCREEN-LINE                     PIC X(300)
                OCCURS 1 TO 50 TIMES.

