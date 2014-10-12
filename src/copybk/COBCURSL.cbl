      *>***********************************************
      *>     COBCURSES       DEC 22, 2006
      *>     NCURSES - COBOL INTERFACE
      *>     WARREN W. GAY   ve3wwg@cogeco.ca
      *>
      *>     LOCAL STORAGE FOR A SCREEN
      *>***********************************************

       01   NC-LOCALS-77.
            10  NC-FIELD-INDEX              PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-FKEY-INDEX               PIC 99 COMP-5
                                            SYNCHRONIZED.
            10  NC-THIS-FIELD               PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-BEST-FIELD               PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-DIFF-Y                   PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-DIFF-X                   PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-DIFF                     PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-TEMP-END-COLUMN          PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-FIELD-CHANGES            PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-TEMP-COLOUR-PAIR         PIC 999 COMP-5
                                            SYNCHRONIZED.
            10  NC-SAVED-EXIT               PIC X.
            10  NC-SAVED-FKEY               PIC 99.

        01  NC-SCREEN-CONFIG.
            10  NC-SCREEN-COLUMNS-REQ       PIC 999.
            10  NC-SCREEN-LINES-REQ         PIC 999.
            10  NC-SCREEN-PAIRS-REQ         PIC 999.

      *>
      *> NOTES:
      *>     NC-FKEY-EXEMPT DEFAULTS TO 'N'. WHEN IT
      *>     IS SET TO 'Y', THE 3270 FORM VALIDATION
      *>     WILL ALLOW AN EXIT EVEN WHEN SOME FIELDS
      *>     DO NOT VALIDATE. THIS IS HANDY FOR A
      *>     F12-CANCEL TYPE OF FUNCTION KEY.
      *>
       01   NC-FKEY-EXEMPTIONS.
            10  NC-FKEY-DEFN OCCURS 1 TO 12 TIMES.
                20  NC-FKEY-EXEMPT          PIC X.

       01   NC-FIELD-DESCRIPTORS.
            10  NC-FIELD-NUMBER             PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-FDESC OCCURS 1 TO 80 TIMES.
                20  NC-FDESC-LINE           PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-COLUMN         PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-ADDRESS        POINTER
                                            SYNCHRONIZED.
                20  NC-FDESC-LENGTH         PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-WINLENGTH      PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-CLEAR          PIC X.
                20  NC-FDESC-UPPERCASE      PIC X.
                20  NC-FDESC-MASK           PIC X.
                20  NC-FDESC-NOT-BLANK      PIC X.
                20  NC-FDESC-YN             PIC X.
                20  NC-FDESC-RESTRICT       PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-SIGNED         PIC X.
                20  NC-FDESC-DIGITS         PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-DECPLACES      PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-VERIFY         PIC X.
                20  NC-FDESC-VISIBLE        PIC X.
                20  NC-FDESC-IGNORE-CHGS    PIC X.
                20  NC-FDESC-INFO           POINTER
                                            SYNCHRONIZED.
                20  NC-FDESC-INFOLEN        PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-CHG-FLAG       PIC X.
                    88  NC-FDESC-CHANGED    VALUE 'Y'.
                20  NC-FDESC-COLOUR-FLAG    PIC X.
                20  NC-FDESC-COLOUR-PAIR    PIC 999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FDESC-READ-ONLY      PIC X.
                20  NC-FDESC-ACTION-EDIT    PIC X.
                20  NC-FDESC-COMP-TYPE      PIC 99.
                20  NC-FDESC-COMP-PTR       POINTER SYNCHRONIZED.
                20  NC-FDESC-MENU-PTR       POINTER SYNCHRONIZED.

      *>     REFERENCED FROM NC-FIELD-DESCRIPTORS.

       01   NC-RESTRICT-MAPS.
            10  NC-RESTRICTX                PIC 99 COMP-5
                                            SYNCHRONIZED.
            10  NC-RESTRICT-CHARSET         PIC X(80) OCCURS 20 TIMES.

      *>     NC-FIELD-STATE-MACHINE

       01   NC-FIELD-SEQUENCES.
            10  NC-FSEQ-LAST                PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
            10  NC-FSEQ-STATE               PIC 9999 COMP-5 VALUE 1
                                            SYNCHRONIZED.
            10  NC-FSEQ-NEXT                PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
            10  NC-FSEQUENCE OCCURS 1 TO 80 TIMES.
                20  NC-FSEQ-FIELD-NO        PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FSEQ-BCK-TO          PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FSEQ-FWD-TO          PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FSEQ-ESC-TO          PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FSEQ-SLASH-TO        PIC 9999 COMP-5
                                            SYNCHRONIZED.

      *>     NC-DRAW-SCREEN

       01   NC-SCREEN-DEFINITION.
            10  NC-SCREENX                  PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-SCREEN-COUNT             PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-SCREEN-DATE              PIC 9(8) COMP-5
                                            SYNCHRONIZED.
            10  NC-SCREEN-TIME              PIC 9(8) COMP-5
                                            SYNCHRONIZED.
            10  NC-SCREEN-TITLEX            PIC 9999 COMP-5
                                            SYNCHRONIZED.
            10  NC-BG-SCREEN OCCURS 80 TIMES.
                20  NC-BG-TEXT              POINTER
                                            SYNCHRONIZED.
                20  NC-BG-LENGTH            PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BG-Y                 PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BG-X                 PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BG-COLOUR-PAIR       PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BG-BOLD              PIC X.
                20  NC-BG-UNDERLINE         PIC X.
                20  NC-BG-REVERSE           PIC X.
                20  NC-BG-TITLE.
                    30  NC-BG-TITLE-FLAG    PIC X.
                    30  NC-BG-DATE          PIC X.
                    30  NC-BG-TIME          PIC X.

      *>***********************************************
      *>     END NCURSESL.cbl
      *>***********************************************
