      *>***********************************************
      *>     COBCURSES       DEC 22, 2006
      *>     NCURSES - COBOL INTERFACE
      *>     WARREN W. GAY   ve3wwg@cogeco.ca
      *>
      *>     NCURSES SUPPORT GLOBALS
      *>***********************************************

       01   NC-COBCURSES.

            10  NC-CURSES.
                20  NC-COLUMNS              PIC 999 COMP-5
                                            SYNCHRONIZED.
                20  NC-LINES                PIC 999 COMP-5
                                            SYNCHRONIZED.
                20  NC-CAP-COLOUR           PIC X.
                    88 NC-HAS-COLOUR        VALUE 'Y'
                       FALSE IS             'N'.
                20  NC-CAP-COLOUR-SAVED     PIC X.
                20  NC-CHG-COLOUR           PIC X.
                    88 NC-CAN-CHANGE-COLOUR VALUE 'Y'.
                20  NC-COLOUR-PAIRS         PIC 999 COMP-5
                                            SYNCHRONIZED.
                20  NC-MOUSE-SUPPORT        PIC X.
                    88 NC-MOUSE-SUPPORTED   VALUE 'Y'.
                20  NC-MOUSE-CLICK-MS       PIC 9(9) COMP-5
                                            SYNCHRONIZED.
                20  NC-FIELD-MODE           PIC X.
                    88 NC-FMODE-ACTION      VALUE 'A'.
                    88 NC-FMODE-3270        VALUE 'F'.
                20  NC-HAS-UNDERLINE-FLAG   PIC X.
                    88  NC-HAS-UNDERLINE    VALUE 'Y'
                        FALSE IS            'N'.

            10  NC-CONFIGURATION.
                20  NC-ALERT-MSG-PAIR       PIC 999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-INFO-MSG-PAIR        PIC 999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-EDIT-ATTR            PIC 9(9) COMP-5
                                            SYNCHRONIZED.
                20  NC-EDIT-PAIR            PIC 999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-TITLE-PAIR           PIC 999 COMP-5 VALUE 1
                                            SYNCHRONIZED.
                20  NC-TITLE-ATTR           PIC 9(9) COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-BACKGROUND-PAIR      PIC 999 COMP-5 VALUE 2
                                            SYNCHRONIZED.
                20  NC-MENU-PAIR            PIC 999 COMP-5 VALUE 3
                                            SYNCHRONIZED.
                20  NC-YN                   PIC XXX VALUE 'YN '.
                20  NC-NO-RECOVERY          PIC X VALUE 'N'.
                20  NC-MAX-FIELDS           PIC 999 COMP-5 VALUE 80.
                20  NC-MAX-STATES           PIC 999 COMP-5 VALUE 80.
                20  NC-MAX-SCRSEGS          PIC 999 COMP-5 VALUE 80.

            10  NC-INFO-MESSAGES.
                20  NC-MSG-TEXT             POINTER VALUE NULL
                                            SYNCHRONIZED.
                20  NC-MSG-LENGTH           PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-MSG-PAUSE            PIC X VALUE 'N'.
                20  NC-MSGBUF               PIC X(300).

            10  NC-POSITION-DATA.
                20  NC-POS-LINE             PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-POS-COLUMN           PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.

            10  NC-STRING-DATA.
                20  NC-STR-DATA             POINTER VALUE NULL
                                            SYNCHRONIZED.
                20  NC-STR-LENGTH           PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.

            10  FILLER                      SYNCHRONIZED.
                20  NC-PATHNAME             PIC X(1024).
                20  NC-PATHNAME-LENGTH      PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.

            10  NC-MISC.
                20  NC-CHAR                 PIC X(1).

            10  NC-KEY-DATA.
                20  NC-KEY-CODE-FLAG        PIC X(1).
                    88  IS-KEY-CODE         VALUE 'Y'.
                20  NC-KEY-CODE             PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-KEY-CHAR             PIC X.

            10  NC-MOUSE-DATA.
                20  NC-MOUSE-ID             PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-MOUSE-X              PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-MOUSE-Y              PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-MOUSE-Z              PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BUTTON-1.
                    25  NC-B1-PRESSED-FLG   PIC X.
                        88  NC-B1-PRESSED   VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B1-RELEASED-FLG  PIC X.
                        88  NC-B1-RELEASED  VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B1-CLICKED-FLG   PIC X.
                        88  NC-B1-CLICKED   VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B1-D-CLICKED-FLG PIC X.
                        88  NC-B1-D-CLICKED VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B1-T-CLICKED-FLG PIC X.
                        88  NC-B1-T-CLICKED VALUE 'Y'
                            FALSE IS        'N'.
                20  NC-BUTTON-2.
                    25  NC-B2-PRESSED-FLG   PIC X.
                        88  NC-B2-PRESSED   VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B2-RELEASED-FLG  PIC X.
                        88  NC-B2-RELEASED  VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B2-CLICKED-FLG   PIC X.
                        88  NC-B2-CLICKED   VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B2-D-CLICKED-FLG PIC X.
                        88  NC-B2-D-CLICKED VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B2-T-CLICKED-FLG PIC X.
                        88  NC-B2-T-CLICKED VALUE 'Y'
                            FALSE IS        'N'.
                20  NC-BUTTON-3.
                    25  NC-B3-PRESSED-FLG   PIC X.
                        88  NC-B3-PRESSED   VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B3-RELEASED-FLG  PIC X.
                        88  NC-B3-RELEASED  VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B3-CLICKED-FLG   PIC X.
                        88  NC-B3-CLICKED   VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B3-D-CLICKED-FLG PIC X.
                        88  NC-B3-D-CLICKED VALUE 'Y'
                            FALSE IS        'N'.
                    25  NC-B3-T-CLICKED-FLG PIC X.
                        88  NC-B3-T-CLICKED VALUE 'Y'
                            FALSE IS        'N'.

            10  NC-MOUSE-MASKS.
                20  NC-B1-MASK.
                    25  NC-B1-PRESSED-MSK   PIC X.
                    25  NC-B1-RELEASED-MSK  PIC X.
                    25  NC-B1-CLICKED-MSK   PIC X.
                    25  NC-B1-D-CLICKED-MSK PIC X.
                    25  NC-B1-T-CLICKED-MSK PIC X.
                20  NC-B2-MASK.
                    25  NC-B2-PRESSED-MSK   PIC X.
                    25  NC-B2-RELEASED-MSK  PIC X.
                    25  NC-B2-CLICKED-MSK   PIC X.
                    25  NC-B2-D-CLICKED-MSK PIC X.
                    25  NC-B2-T-CLICKED-MSK PIC X.
                20  NC-B3-MASK.
                    25  NC-B3-PRESSED-MSK   PIC X.
                    25  NC-B3-RELEASED-MSK  PIC X.
                    25  NC-B3-CLICKED-MSK   PIC X.
                    25  NC-B3-D-CLICKED-MSK PIC X.
                    25  NC-B3-T-CLICKED-MSK PIC X.

            10  NC-FIELD-WAIVE-INFO         PIC X VALUE 'N'.

            10  NC-FIELD.
                20  NC-FIELD-Y              PIC 9999 COMP-5 VALUE 1
                                            SYNCHRONIZED.
                20  NC-FIELD-X              PIC 9999 COMP-5 VALUE 1
                                            SYNCHRONIZED.
                20  NC-FIELD-LENGTH         PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-FIELD-WINLEN         PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-FIELD-BUFFER         POINTER VALUE NULL
                                            SYNCHRONIZED.
                20  NC-FIELD-MENU           POINTER VALUE NULL
                                            SYNCHRONIZED.
                20  NC-FIELD-CLEAR          PIC X VALUE 'Y'.
                20  NC-FIELD-UPPERCASE      PIC X VALUE 'Y'.
                20  NC-FIELD-MASK           PIC X VALUE 'N'.
                20  NC-FIELD-NOT-BLANK      PIC X VALUE 'N'.
                20  NC-FIELD-RESTRICT       POINTER VALUE NULL
                                            SYNCHRONIZED.
                20  NC-FIELD-SIGNED         PIC X VALUE 'N'.
                20  NC-FIELD-DIGITS         PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-FIELD-DECPLACES      PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-FIELD-X-POS          PIC 9999 COMP-5 VALUE 0
                                            SYNCHRONIZED.
                20  NC-FIELD-EXIT           PIC X VALUE ' '.
                    88  NC-FIELD-EXIT-CR    VALUE '0'.
                    88  NC-FIELD-EXIT-CD    VALUE '2'.
                    88  NC-FIELD-EXIT-TAB   VALUE '3'.
                    88  NC-FIELD-EXIT-CU    VALUE '1'.
                    88  NC-FIELD-EXIT-BTAB  VALUE '4'.
                    88  NC-FIELD-EXIT-ESC   VALUE '5'.
                    88  NC-FIELD-EXIT-DOT   VALUE '.'.
                    88  NC-FIELD-EXIT-SLASH VALUE '/'.
                    88  NC-FIELD-EXIT-FKEY  VALUE 'F'.
                20  NC-FIELD-ACTION         PIC X VALUE ' '.
                20  NC-FIELD-SEARCH         PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FIELD-FKEY REDEFINES NC-FIELD-SEARCH.
                 25 NC-FIELD-FKEY-NO        PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FIELD-FB             PIC X VALUE ' '.
                    88  NC-FIELD-FORWARD    VALUE 'F'.
                    88  NC-FIELD-BACKWARD   VALUE 'B'.
                20  NC-FIELD-CHG-FLAG       PIC X VALUE ' '.
                    88  NC-FIELD-CHANGED    VALUE 'Y'.
                20  NC-FIELD-MOUSE-FLAG     PIC X VALUE 'N'.
                    88  NC-FIELD-MOUSE-EVENT VALUE 'Y'.
                20  NC-FIELD-VERIFIED       PIC X VALUE 'N'.
                20  NC-FIELD-COMP-TYPE      PIC 99 VALUE 0.
                20  NC-FIELD-COMP-PTR       POINTER SYNCHRONIZED.
                20  NC-FIELD-EDIT-TARGET    PIC X.

            10  NC-COLOUR-PAIR.
                20  NC-PAIR-NUMBER          PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-FOREGROUND-COLOUR    PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BACKGROUND-COLOUR    PIC 9999 COMP-5
                                            SYNCHRONIZED.

            10  NC-TERMINAL-ATTRIUBTES.
                20  NC-ATTRIBUTE            PIC 9(9) COMP-5
                                            SYNCHRONIZED.

            10  NC-BOX-DATA.
                20  NC-BOX-TOP-LINE         PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BOX-LEFT-COLUMN      PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BOX-BOTTOM-LINE      PIC 9999 COMP-5
                                            SYNCHRONIZED.
                20  NC-BOX-BOTTOM-COLUMN    PIC 9999 COMP-5
                                            SYNCHRONIZED.

      *>***********************************************
      *>     END NCURSESG.cbl
      *>***********************************************
