IDENTIFICATION DIVISION.
PROGRAM-ID. libcobcurses_codegen.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

    SELECT SCREEN-FILE
        ASSIGN TO SCREEN-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS RANDOM
        RECORD KEY IS SCN-NAME.

    SELECT SCRNBG-FILE
        ASSIGN TO SCRNBG-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS SCRBG-KEY.

    SELECT SCRNFDEF-FILE
        ASSIGN TO SCRNFDEF-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS SCR-FDEF-KEY.

    SELECT SCRFSTA-FILE
        ASSIGN TO SCRFSTA-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS SCR-FST-KEY.

    SELECT CHARSET-FILE
        ASSIGN TO CHARSET-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS CHARSET-NAME.

    SELECT MENU-FILE
        ASSIGN TO MENU-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS MNU-KEY.

    SELECT ITEM-FILE
        ASSIGN TO ITEM-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS ITM-KEY.

    SELECT MREF-FILE
        ASSIGN TO MREF-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS MREF-KEY.

    SELECT GENWS-FILE
        ASSIGN TO PATH-WS
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT GENPD-FILE
        ASSIGN TO PATH-PD
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT GENSI-FILE
        ASSIGN TO PATH-SI
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.

    FD  SCREEN-FILE.
    01  SCREEN-RECORD.
        COPY SCREEN-01.

    FD  SCRNBG-FILE.
    01  SCRNBG-RECORD.
        COPY SCREEN-BG.

    FD  SCRNFDEF-FILE.
    01  SCRNFDEF-RECORD.
        COPY SCREEN-FD.

    FD  SCRFSTA-FILE.
    01  SCRFSTA-RECORD.
        COPY SCREEN-FS.

    FD  MENU-FILE.
    01  MENU-RECORD.
        COPY MENURECD.

    FD  ITEM-FILE.
    01  ITEM-RECORD.
        COPY ITEMRECD.

    FD  MREF-FILE.
    01  MREF-RECORD.
        COPY MENUREF.

    FD  GENWS-FILE.
    01  GENWS-LINE                          PIC X(80).

    FD  GENPD-FILE.
    01  GENPD-LINE                          PIC X(80).

    FD  GENSI-FILE.
    01  GENSI-LINE                          PIC X(300).

    FD  CHARSET-FILE.
    01  CHARSET-RECORD.
        COPY SCREEN-CS.

WORKING-STORAGE SECTION.

    01  SCREEN-IMAGE.
        COPY SCREEN-SI.

    01  FILE-NAMES.
        10  FILE-NAME-LENGTH                PIC 9999.
        10  FILE-NAME-LEN5                  PIC 9999 COMP-5.
        10  SCREEN-FILE-NAME                PIC X(512)        VALUE "${COBCURSES_DATADIR}/SCREENS.X".
        10  SCRNBG-FILE-NAME                PIC X(512)        VALUE "${COBCURSES_DATADIR}/SCRNBG.X".
        10  SCRNFDEF-FILE-NAME              PIC X(512)        VALUE "${COBCURSES_DATADIR}/SCRFDEF.X".
        10  SCRFSTA-FILE-NAME               PIC X(512)        VALUE "${COBCURSES_DATADIR}/SCRFSTA.X".
        10  CHARSET-FILE-NAME               PIC X(512)        VALUE "${COBCURSES_DATADIR}/SCRCHRSET.X".
        10  MENU-FILE-NAME                  PIC X(512)        VALUE "${COBCURSES_DATADIR}/MENUS.X".
        10  ITEM-FILE-NAME                  PIC X(512)        VALUE "${COBCURSES_DATADIR}/ITEMS.X".
        10  MREF-FILE-NAME                  PIC X(512)        VALUE "${COBCURSES_DATADIR}/MENUREFS.X".
        10  PATH-WS                         PIC X(512).
        10  PATH-PD                         PIC X(512).
        10  PATH-SI                         PIC X(512).

    01  WS-FLAGS.
        10  WS-EOF-FLAG                     PIC X.
            88  WS-EOF                      VALUE IS 'Y' FALSE IS 'N'.

    01  WORK-AREAS.
        10  FLD-SCREEN-NAME                 PIC X(16).
        10  SEGMENTS-REMAINING              PIC X.
        10  LINEX                           PIC 999 COMP.
        10  NUM-COLUMN                      PIC 999 COMP.
        10  NUM-OTLEN                       PIC 999 COMP-5 SYNCHRONIZED.
        10  PIC-OTLEN                       PIC 999.
        10  X                               PIC 999 COMP.
        10  X2                              PIC 999 COMP.
        10  ACTION-NO                       PIC 999.
        10  ADJ-FACTOR                      PIC 999.
        10  USE-SEGNO                       PIC 9999.
        10  SCREEN-FOUND-FLAG               PIC X VALUE 'N'.
            88  SCREEN-FOUND                VALUE 'Y'.
        10  COPYBK-SUFFIX                   PIC X(4) VALUE ".cbl".
        10  SI-SUFFIX                       PIC X(4) VALUE ".txt".
        10  WS-INSTANCE-ID                  PIC 9(9).
        10  WS-HEXBUF                       PIC X(64).              *> THIS MUST BE TWICE THE SIZE OF SCRBG-SEGMENT
        10  WS-HEXBUF-LENGTH                PIC 999 COMP-5 SYNCHRONIZED.
        10  WS-HEXBUF-OFFSET                PIC 999 COMP-5.
        10  WS-SCREEN-LINE-LENGTH           PIC 9999 COMP-5 SYNCHRONIZED.

    01  WS-ASSOC-KEY-PARAM.
        05  WS-KEY-POINTER                  POINTER.
        05  WS-KEY-LENGTH                   PIC 9999.

    01  WS-MENU-NAME                        PIC X(16).
    01  WS-DATA-LENGTH                      PIC 9999.

    01  WS-TEXT-AREA.
        10  WS-TEXT                         PIC X(80).
        10  WS-TEXT-MAX                     PIC 99 COMP.
        10  WS-TEXT-X                       PIC 99 COMP.
        10  WS-TEXT-LENGTH                  PIC 99 COMP.

    01  WS-TEXT-MAX-SEGLEN                  PIC 99 COMP VALUE 32.
    01  WS-EXTRACT.
        10  WS-TEXT-SEG                     PIC X(80).
        10  WS-TEXT-LEN                     PIC 99 COMP.

    01  WS-MENU-FLAG                        PIC X.
        88  VALID-MENU                      VALUE 'Y' FALSE IS 'N'.
    01  WS-ITEM-FLAG                        PIC X.
        88  END-OF-ITEMS                    VALUE 'Y' FALSE IS 'N'.
    01  WS-INIT-FLAG                        PIC X.
        88  INIT-OK                         VALUE 'Y' FALSE IS 'N'.

    01  CHARSET-NAMES.
        10  RX                              PIC 99.
        10  FRX                             PIC 99.
        10  TRX                             PIC 99.
        10  CS-ENTRY                        OCCURS 20 TIMES.
            20  CS-NAME                     PIC X(8).

    01  LENGTH-OFFSET.
        10  LO-BUFFER                       PIC X(80).
        10  LO-BUFLEN                       PIC 9999.
        10  LO-LIMIT-TO                     PIC 9999.
        10  LO-OFFSET                       PIC 9999.
        10  LO-LENGTH                       PIC 9999.
        10  LO-TEMP                         PIC 9999.

    01  GENPD-LINE-T0.
        10  FILLER                          PIC X(28)   VALUE "      *>    TITLE SEGMENT # ".
        10  GENPD-T0-SEGNO                  PIC 99.

    01  GENPD-LINE-T1.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(20)   VALUE "MOVE ADDRESS OF SCR-".
        10  GENPD-T1-SEGNO                  PIC 9999.
        10  FILLER                          PIC X(15)   VALUE " TO NC-BG-TEXT(".
        10  GENPD-T1-LINE                   PIC 99.
        10  FILLER                          PIC X       VALUE ")".
        
    01  GENPD-LINE-T2.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(19)   VALUE "MOVE LENGTH OF SCR-".
        10  GENPD-T2-SEGNO                  PIC 9999.
        10  FILLER                          PIC X(17)   VALUE " TO NC-BG-LENGTH(".
        10  GENPD-T2-LINE                   PIC 99.
        10  FILLER                          PIC X       VALUE ")".
        
    01  GENPD-LINE-T3.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(18)   VALUE "MOVE 1 TO NC-BG-Y(".
        10  GENPD-T3-SEGNO                  PIC 99.
        10  FILLER                          PIC X(11)   VALUE "), NC-BG-X(".
        10  GENPD-T3-SEGNO2                 PIC 99.
        10  FILLER                          PIC X       VALUE ")".

    01  GENPD-LINE-T4.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(28)   VALUE "MOVE 1 TO NC-BG-COLOUR-PAIR(".
        10  GENPD-T4-SEGNO                  PIC 99.
        10  FILLER                          PIC X       VALUE ")".

    01  GENPD-LINE-T5.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(28)   VALUE "MOVE 'Y' TO NC-BG-UNDERLINE(".
        10  GENPD-T5-SEGNO                  PIC 99.
        10  FILLER                          PIC X(14)   VALUE "), NC-BG-BOLD(".
        10  GENPD-T5-SEGNO2                 PIC 99.
        10  FILLER                          PIC XX      VALUE "),".
            
    01  GENPD-LINE-T6.
        10  FILLER                          PIC X(16)   VALUE SPACES.
        10  FILLER                          PIC X(14)   VALUE "NC-BG-REVERSE(".
        10  GENPD-T6-SEGNO                  PIC 99.
        10  FILLER                          PIC X       VALUE ")".
    
    01  GENPD-LINE-T7.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(29)   VALUE "MOVE 'Y' TO NC-BG-TITLE-FLAG(".
        10  GENPD-T7-SEGNO                  PIC 99.
        10  FILLER                          PIC X(14)   VALUE "), NC-BG-DATE(".
        10  GENPD-T7-SEGNO2                 PIC 99.
        10  FILLER                          PIC XX      VALUE "),".
            
    01  GENPD-LINE-T8.
        10  FILLER                          PIC X(16)   VALUE SPACES.
        10  FILLER                          PIC X(11)   VALUE "NC-BG-TIME(".
        10  GENPD-T8-SEGNO                  PIC 99.
        10  FILLER                          PIC X       VALUE ")".
            
    01  GENPD-LINE-S0.
        10  FILLER                          PIC X(29)   VALUE "      *>    SCREEN SEGMENT # ".
        10  GENPD-S0-SEGNO                  PIC 99.
        
    01  GENPD-LINE-S1.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(20)   VALUE "MOVE ADDRESS OF SCR-".
        10  GENPD-S1-SEGNO                  PIC 9999.
        10  FILLER                          PIC X(15)   VALUE " TO NC-BG-TEXT(".
        10  GENPD-S1-SEGNO2                 PIC 99.
        10  FILLER                          PIC X       VALUE ")".

    01  GENPD-LINE-S2.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(19)   VALUE "MOVE LENGTH OF SCR-".
        10  GENPD-S2-SEGNO                  PIC 9999.
        10  FILLER                          PIC X(17)   VALUE " TO NC-BG-LENGTH(".
        10  GENPD-S2-SEGNO2                 PIC 99.
        10  FILLER                          PIC X       VALUE ")".

    01  GENPD-LINE-S3.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(5)    VALUE "MOVE ".
        10  GENPD-S3-LINE                   PIC 999.
        10  FILLER                          PIC X(12)   VALUE " TO NC-BG-Y(".
        10  GENPD-S3-SEGNO                  PIC 99.
        10  FILLER                          PIC X       VALUE ")".

    01  GENPD-LINE-S4.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(5)    VALUE "MOVE ".
        10  GENPD-S4-COLUMN                 PIC 999.
        10  FILLER                          PIC X(12)   VALUE " TO NC-BG-X(".
        10  GENPD-S4-SEGNO                  PIC 99.
        10  FILLER                          PIC X       VALUE ")".

    01  GENPD-LINE-S9.
        10  FILLER                          PIC X(12)   VALUE SPACES.
        10  FILLER                          PIC X(5)    VALUE "MOVE ".
        10  GENPD-S9-COUNT                  PIC 99.
        10  FILLER                          PIC X(19)   VALUE " TO NC-SCREEN-COUNT".

    01  GENPD-F0-A.
        10  PIC X(20)                                   VALUE "      *>    FIELD # ".
        10  GENPD-F0-NO                     PIC 999.
        10  PIC X(09)                                   VALUE "  -  FLD-".
        10  GENPD-F0-COBOL-NAME             PIC X(32).
    
    01  GENWS-FI-0.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(08)                                   VALUE "10  INF-".
        10  GENWS-FI-0-COBOL-NAME           PIC X(32).
        10  PIC X                                       VALUE '.'.

    01  GENWS-FI-1.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(32)                                   VALUE "20  FILLER".
        10  PIC X(06)                                   VALUE "PIC X(".
        10  GENWS-FI-1-LENGTH               PIC 99.
        10  PIC X                                       VALUE ")".

    01  GENWS-FI-2.
        10  PIC X(20)                                   VALUE SPACES.
        10  PIC X(07)                                   VALUE 'VALUE "'.
        10  GENWS-FI-2-TEXT                 PIC X(40).
                                                                       .
    01  GENWS-FNO-1.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(08)                                   VALUE "10  FNO-".
        10  GENWS-FNO-COBOL-NAME            PIC X(32).
        10  PIC X(12)                                   VALUE "PIC 999 COMP".

    01  GENWS-FNO-2.
        10  PIC X(52)                                   VALUE SPACES.
        10  PIC X(06)                                   VALUE "VALUE ".
        10  GENWS-FNO-VALUE                 PIC ZZ9.
        10  PIC X                                       VALUE ".".

    01  GENPD-F0-B.
        10  PIC X(12)                                   VALUE "      *>    ".
        10  GENPD-F0-COMMENT                PIC X(70).

    01  GENPD-F1.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(05)                                   VALUE "MOVE ".
        10  GENPD-F1-VALUE                  PIC 9999.
        10  PIC X(13)                                   VALUE " TO NC-FDESC-".
        10  GENPD-F1-FDESC-NAME             PIC X(12).
        10  PIC X                                       VALUE "(".
        10  GENPD-F1-SUBSCR                 PIC 999.
        10  PIC X                                       VALUE ")".

    01  GENPD-F2.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(13)                                   VALUE "SET NC-FDESC-".
        10  GENPD-F2-FDESC-NAME             PIC X(12).
        10  PIC X                                       VALUE "(".
        10  GENPD-F2-SUBSCR                 PIC 999.
        10  PIC X                                       VALUE ")".

    01  GENPD-F3.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(14)                                   VALUE "TO ADDRESS OF ".
        10  GENPD-F3-PREFIX                 PIC XXX.
        10  PIC X                                       VALUE "-".
        10  GENPD-F3-COBOL-NAME             PIC X(40).

    01  GENPD-F4.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(06)                                   VALUE "MOVE '".
        10  GENPD-F4-FLAG                   PIC X.
        10  PIC X(14)                                   VALUE "' TO NC-FDESC-".
        10  GENPD-F4-FDESC-NAME             PIC X(12).
        10  PIC X                                       VALUE "(".
        10  GENPD-F4-SUBSCR                 PIC 999.
        10  PIC X                                       VALUE ")".

    01  GENWS-F1.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(8)                                    VALUE "10  FLD-".
        10  GENWS-F1-COBOL-NAME             PIC X(32).
        10  PIC X(6)                                    VALUE "PIC X(".
        10  GENWS-F1-LENGTH                 PIC 9999.
        10  PIC XX                                      VALUE ").".

    01  GENWS-F1-COMP-X.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(8)                                    VALUE "10  CMP-".
        10  GENWS-F1-COMP-COBOL-NAME        PIC X(32).
        10  PIC X(5)                                    VALUE "COMP-".
        10  GENWS-F1-COMP-TYPE              PIC 9.
        10  PIC X(14)                                   VALUE " SYNCHRONIZED.".

    01  SRC-0.
        10  PIC X(6)                                    VALUE SPACES.
        10  SRC-COMMENT-0                   PIC XX.
        10  PIC X                                       VALUE SPACES.
        10  SRC-LINE-0                      PIC X(64).

    01  SRC.
        10  PIC X(6)                                    VALUE SPACES.
        10  SRC-COMMENT                     PIC XX.
        10  PIC X(4)                                    VALUE SPACES.
        10  SRC-LINE                        PIC X(60).

    01  SRC-2.
        10  PIC X(16)                                   VALUE SPACES.
        10  SRC-2-LINE                      PIC X(60).
    
    01  GENWS-MENU-05.
        10  PIC X(08)                                   VALUE SPACES.
        10  PIC X(09)                                   VALUE "01  MENU-".
        10  GENWS-MENU-05-MENU-NAME         PIC X(16).
        10  PIC X                                       VALUE '.'.
    
    01  GENWS-MENU-10.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(21)                                   VALUE '05  PIC X(16) VALUE "'.
        10  GENWS-MENU-10-MENU-NAME         PIC X(16).
        10  PIC XX                                      VALUE '".'.
    
    01  GENWS-MENU-10-TYPE.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(17)                                   VALUE '05  PIC 99 VALUE '.
        10  GENWS-MENU-TYPE                 PIC 99.
        10  PIC X                                       VALUE '.'.
    
    01  GENWS-MENU-10-MODULE-NAME.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(21)                                   VALUE '05  PIC X(16) VALUE "'.
        10  GENWS-MENU-MODULE-NAME          PIC X(16).
        10  PIC X(02)                                   VALUE '".'.
    
    01  GENWS-MENU-10-ITEM-LIMIT.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(19)                                   VALUE '05  PIC 9999 VALUE '.
        10  GENWS-MENU-ITEM-LIMIT           PIC 9999.
        10  PIC X(01)                                   VALUE '.'.
    
    01  GENWS-MENU-10-YY.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(20)                                   VALUE "05  PIC 9(03) VALUE ".
        10  GENWS-MENU-10-MENU-YY           PIC 999.
        10  PIC X                                       VALUE ".".
    
    01  GENWS-MENU-10-XX.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(20)                                   VALUE "05  PIC 9(03) VALUE ".
        10  GENWS-MENU-10-MENU-XX           PIC 999.
        10  PIC X                                       VALUE ".".
    
    01  GENWS-MENU-05-OPTION.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(09)                                   VALUE "05  MENU-".
        10  GENWS-MENU-OPTION-NAME      PIC X(26).
        10  PIC X(14)                                   VALUE ' PIC X VALUE "'.
        10  GENWS-MENU-OPTION-VALUE     PIC X.
        10  PIC X(02)                                   VALUE '".'.
    
    01  GENWS-MENU-05-FORMAT.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(09)                                   VALUE "05  MENU-".
        10  GENWS-MENU-FORMAT-NAME      PIC X(26).
        10  PIC X(14)                                   VALUE ' PIC 99 VALUE '.
        10  GENWS-MENU-FORMAT-VALUE     PIC 99.
        10  PIC X(01)                                   VALUE '.'.
    
    01  GENWS-MENU-10-TITLE.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(20)                                   VALUE "05  PIC 9(02) VALUE ".
        10  GENWS-MENU-10-TITLE-SEGLEN      PIC 99.            
        10  PIC X                                       VALUE ".".
    
    01  GENWS-MENU-10-TITLE-2.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(10)                                   VALUE "05  PIC X(".
        10  GENWS-MENU-10-TITLE2-SEGLEN     PIC 99.            
        10  PIC X(9)                                    VALUE ') VALUE "'.
        10  GENWS-MENU-10-TITLE-TEXT        PIC X(40).
    
    01  GENWS-ITEM-10-HEADER-1.
        10  PIC X(12)                                   VALUE SPACES.
        10  PIC X(11)                                   VALUE "05  FILLER.".
    
    01  GENWS-ITEM-10-FLAGS.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(10)                                   VALUE "10  MITEM-".
        10  GENWS-ITEM-10-SELECTABLE-NAME   PIC X(25).
        10  PIC X(14)                                   VALUE ' PIC X VALUE "'.
        10  GENWS-ITEM-10-SELECTABLE        PIC X.
        10  PIC X(02)                                   VALUE '".'.
    
    01  GENWS-ITEM-10-END.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(24)                                   VALUE "10  PIC X(01) VALUE 'X'.".
    
    01  GENWS-ITEM-10-NAME-LEN.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(17) VALUE '10  PIC 99 VALUE '.
        10  GENWS-ITEM-NAME-LEN-0           PIC 99.
        10  PIC X(01) VALUE '.'.
    
    01  GENWS-ITEM-10-NAME.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(10)                                   VALUE '10  PIC X('.
        10  GENWS-ITEM-10-NAME-ITEM-NAMELEN PIC 99.
        10  PIC X(09)                                   VALUE ') VALUE "'.
        10  GENWS-ITEM-10-NAME-ITEM-NAME    PIC X(34).
    
    01  GENWS-ITEM-10-TEXT-LENGTH.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(20)                                   VALUE "10  PIC 9(02) VALUE ".
        10  GENWS-ITEM-10-TEXT-SEGLEN       PIC 99.
        10  PIC X                                       VALUE ".".
    
    01  GENWS-ITEM-10-TEXT-CONTENT.
        10  PIC X(16)                                   VALUE SPACES.
        10  PIC X(10)                                   VALUE "10  PIC X(".
        10  GENWS-ITEM-10-TEXT-SEGLEN-2     PIC 99.
        10  PIC X(9)                                    VALUE ') VALUE "'.
        10  GENWS-ITEM-10-TEXT              PIC X(40).
    
    01  STATE-DISP.
        10  DSP-FST-STATE-NO                PIC 999.
        10  DSP-FST-FIELD-NO                PIC 999.
        10  ZZZ-FST-FIELD-NO                PIC ZZ9.
        10  DSP-FST-BACK-TO                 PIC 999.
        10  DSP-FST-FORWARD-TO              PIC 999.
        10  DSP-FST-ESCAPE-TO               PIC 999.
        10  DSP-FST-SLASH-TO                PIC 999.
    
    77  DEV-NULL                            PIC X(9)    VALUE "/dev/null".
    77  DEV-NULL-LEN9                       PIC 9(4)    VALUE 9.
    
    77  FINAL-WS-FILENAME                   PIC X(20).
    77  FINAL-PD-FILENAME                   PIC X(20).
    77  FINAL-SI-FILENAME                   PIC X(20).
    77  FINAL-XX-FILENAME-LEN5              PIC 9999 COMP-5 VALUE 20.

    COPY COBCATTR.
    COPY COBCURSG.
    COPY COBCURSL.
    COPY COBCRETC.
    COPY COBCEXTRA.

LINKAGE SECTION.

    01  LS-SCREEN-NAME                      PIC X(16).
    01  FILE-NAME-WS                        PIC X(16).
    01  FILE-NAME-PD                        PIC X(16).
    01  FILE-NAME-SI                        PIC X(16).
    01  COPY-BOOK-DIR                       PIC X(512).
    01  SCREEN-IMAGE-DIR                    PIC X(512).

    01  COUNT-SEGMENTS                      PIC 9999.
    01  COUNT-FIELDS                        PIC 9999.
    01  COUNT-STATES                        PIC 9999.
    01  COUNT-MENUS                         PIC 9999.
    01  COUNT-ITEMS                         PIC 9999.
    01  SCREEN-DESCRIPTION                  PIC X(50).

PROCEDURE DIVISION USING
    LS-SCREEN-NAME, FILE-NAME-WS, FILE-NAME-PD, FILE-NAME-SI,
    COPY-BOOK-DIR, SCREEN-IMAGE-DIR,
    COUNT-SEGMENTS, COUNT-FIELDS, COUNT-STATES,
    COUNT-MENUS, COUNT-ITEMS,
    SCREEN-DESCRIPTION.

MAIN-PROGRAM.
    PERFORM 1000-INITIALIZE.
    IF INIT-OK THEN
        PERFORM 5000-PROCESS
        PERFORM 9000-FINALIZE
        IF SCREEN-FOUND THEN
            MOVE NC-RET-OK TO RETURN-CODE
        ELSE
            MOVE NC-RET-NOTFOUND TO RETURN-CODE
        END-IF
    ELSE
        MOVE NC-RET-FAILED TO RETURN-CODE
    END-IF.
    GOBACK.

1000-INITIALIZE.
    PERFORM NC-EXTRA-INIT.                                      *> INITIALIZE ACCESS TO ASSOCIATIVE ARRAY MODULES
    MOVE LENGTH OF SCREEN-LINE(1) TO WS-SCREEN-LINE-LENGTH.

    SET INIT-OK TO TRUE.
    PERFORM 2000-INITIALIZE-PATHNAMES.

    CALL "COBCURSEX-ASSOC-INSTANCE" USING WS-INSTANCE-ID.
    MOVE LENGTH OF WS-MENU-NAME TO WS-KEY-LENGTH.
    SET WS-KEY-POINTER TO ADDRESS OF WS-MENU-NAME.

    OPEN INPUT SCREEN-FILE, SCRNBG-FILE, SCRNFDEF-FILE, SCRFSTA-FILE, CHARSET-FILE, MENU-FILE, ITEM-FILE, MREF-FILE.

    IF FILE-NAME-WS = SPACES
        MOVE DEV-NULL TO FINAL-WS-FILENAME
    END-IF.

    IF FILE-NAME-PD = SPACES THEN
        MOVE DEV-NULL TO FINAL-PD-FILENAME
    END-IF.

    IF FILE-NAME-SI = SPACES THEN
        MOVE DEV-NULL TO FINAL-SI-FILENAME
    END-IF.

    IF COPY-BOOK-DIR = SPACES THEN
        MOVE FINAL-WS-FILENAME TO PATH-WS
        MOVE FINAL-PD-FILENAME TO PATH-PD
    ELSE
        MOVE LENGTH OF PATH-WS TO FILE-NAME-LEN5
        CALL "NC_mk_path" USING COPY-BOOK-DIR, FILE-NAME-LEN5, FINAL-WS-FILENAME, FINAL-XX-FILENAME-LEN5, PATH-WS, FILE-NAME-LEN5
        CALL "NC_mk_path" USING COPY-BOOK-DIR, FILE-NAME-LEN5, FINAL-PD-FILENAME, FINAL-XX-FILENAME-LEN5, PATH-PD, FILE-NAME-LEN5
    END-IF.

    IF SCREEN-IMAGE-DIR = SPACES THEN
        MOVE FINAL-SI-FILENAME TO PATH-SI
    ELSE
        MOVE LENGTH OF PATH-SI TO FILE-NAME-LEN5
        CALL "NC_mk_path" USING SCREEN-IMAGE-DIR, FILE-NAME-LEN5, FINAL-SI-FILENAME, FINAL-XX-FILENAME-LEN5, PATH-SI, FILE-NAME-LEN5
    END-IF.

    OPEN OUTPUT GENWS-FILE, GENPD-FILE, GENSI-FILE.

    PERFORM VARYING LINEX FROM 1 BY 1 UNTIL LINEX > 50
        MOVE ' ' TO SCREEN-LINE(LINEX)
    END-PERFORM.

    MOVE 1 TO ACTION-NO.
    MOVE ZERO TO RX.
    MOVE ZERO TO COUNT-SEGMENTS, COUNT-FIELDS, COUNT-STATES, COUNT-MENUS, COUNT-ITEMS.
    EXIT.

2000-INITIALIZE-PATHNAMES.
*>
*>  THIS FIRST CALL IS NECESSARY FOR MinGW/MSYS 
*>  ENVIRONMENTS, WHERE /dev/null BECOMES "NUL".
*>
    CALL "COBCURSES-INIT-PATHNAME" USING DEV-NULL, DEV-NULL-LEN9.

    MOVE LENGTH OF SCREEN-FILE-NAME TO FILE-NAME-LENGTH.
            
    CALL "COBCURSES-INIT-PATHNAME" USING SCREEN-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING SCRNBG-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING SCRNFDEF-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING SCRFSTA-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING CHARSET-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING MENU-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING ITEM-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING MREF-FILE-NAME, FILE-NAME-LENGTH.

    CALL "COBCURSES-APPEND-SUFFIX-X16" USING FILE-NAME-WS, COPYBK-SUFFIX, FINAL-WS-FILENAME.
    CALL "COBCURSES-APPEND-SUFFIX-X16" USING FILE-NAME-PD, COPYBK-SUFFIX, FINAL-PD-FILENAME.
    CALL "COBCURSES-APPEND-SUFFIX-X16" USING FILE-NAME-SI, SI-SUFFIX, FINAL-SI-FILENAME.
    EXIT.

5000-PROCESS.
    MOVE LS-SCREEN-NAME TO FLD-SCREEN-NAME.
    PERFORM 5000-LOOKUP-SCREEN.
    IF SCREEN-FOUND THEN
        PERFORM 5600-COUNT-ITEMS
        IF COUNT-SEGMENTS > 0 THEN
            PERFORM 5100-LOAD-N-WRITE
            PERFORM 5900-WRITE-SCREEN-IMAGE
        END-IF
        IF COUNT-FIELDS > 0 THEN
            PERFORM 6000-WRITE-FINFO
            PERFORM 6020-WRITE-FDEFS
        END-IF
        IF COUNT-STATES > 0 THEN
            PERFORM 6030-WRITE-STATE
        END-IF
        PERFORM 6910-WRITE-SCREEN-DIMENSIONS
        PERFORM 8000-WRITE-MENU-DATA
    END-IF.
    EXIT.

5000-LOOKUP-SCREEN.
    MOVE FLD-SCREEN-NAME TO SCN-NAME.

    READ SCREEN-FILE
        INVALID KEY
            MOVE "ERROR: NOT ON FILE" TO SCREEN-DESCRIPTION
            MOVE 'N' TO SCREEN-FOUND-FLAG
        NOT INVALID KEY
            SET SCREEN-FOUND TO TRUE
            MOVE SCN-DESCRIPTION TO SCREEN-DESCRIPTION
    END-READ.
    EXIT.

5600-COUNT-ITEMS.
    PERFORM 5800-COUNT-SEGMENTS.
    PERFORM 5080-COUNT-FIELDS.
    PERFORM 5900-COUNT-STATES.
    EXIT.

5800-COUNT-SEGMENTS.
    PERFORM 5200-START-SEGMENT.
    PERFORM UNTIL SEGMENTS-REMAINING = 'N'
        READ SCRNBG-FILE NEXT RECORD
            AT END
                MOVE 'N' TO SEGMENTS-REMAINING
            NOT AT END
                IF SCRBG-NAME NOT = FLD-SCREEN-NAME
                    MOVE 'N' TO SEGMENTS-REMAINING
                ELSE
                    ADD 1 TO COUNT-SEGMENTS
                END-IF
        END-READ
    END-PERFORM.
    EXIT.

5080-COUNT-FIELDS.
    PERFORM 6010-START-FIELDS.
    PERFORM UNTIL SEGMENTS-REMAINING = 'N'
        PERFORM 5085-READ-FD
        IF SEGMENTS-REMAINING = 'Y' THEN
            ADD 1 TO COUNT-FIELDS
        END-IF
    END-PERFORM.
    EXIT.

5085-READ-FD.
    READ SCRNFDEF-FILE NEXT RECORD
        AT END
            MOVE 'N' TO SEGMENTS-REMAINING
        NOT AT END
            IF SCR-FDEF-SCREEN-NAME NOT = FLD-SCREEN-NAME
                MOVE 'N' TO SEGMENTS-REMAINING
            ELSE
                IF NOT SCR-FDEF-COMP-TYPE NUMERIC THEN
                    MOVE ZERO TO SCR-FDEF-COMP-TYPE
                END-IF
            END-IF
    END-READ.
    EXIT.

5900-COUNT-STATES.
    PERFORM 6040-START-STATES.
    PERFORM UNTIL SEGMENTS-REMAINING = 'N'
        READ SCRFSTA-FILE NEXT RECORD
            AT END
                MOVE 'N' TO SEGMENTS-REMAINING
            NOT AT END
                IF SCR-FST-SCREEN-NAME NOT = FLD-SCREEN-NAME
                    MOVE 'N' TO SEGMENTS-REMAINING
                ELSE
                    ADD 1 TO COUNT-STATES
                END-IF
        END-READ
    END-PERFORM.
    EXIT.

5100-LOAD-N-WRITE.
    WRITE GENWS-LINE FROM "        01  SCREEN-TEXT.".

    PERFORM 5200-START-SEGMENT.

    IF SCN-TITLE NOT = SPACES
        MOVE 1 TO ADJ-FACTOR
        MOVE 1 TO SCRBG-LINE, SCRBG-COLUMN
        MOVE SCN-TITLE TO SCRBG-SEGMENT
        MOVE LENGTH OF SCRBG-SEGMENT TO X2
        SUBTRACT 1 FROM X2

        PERFORM UNTIL X2 < 8 OR SCRBG-SEGMENT(X2:1) NOT = SPACE
            SUBTRACT 1 FROM X2
        END-PERFORM

        ADD 1 TO X2
        MOVE X2 TO SCRBG-LENGTH
        PERFORM 5300-LOAD-SEGMENT
    END-IF.

    PERFORM UNTIL SEGMENTS-REMAINING = 'N'

        READ SCRNBG-FILE NEXT RECORD
            AT END
                MOVE 'N' TO SEGMENTS-REMAINING
            NOT AT END
                IF SCRBG-NAME = FLD-SCREEN-NAME THEN
                    MOVE SCRBG-SEGMENT-NO TO USE-SEGNO
                    ADD ADJ-FACTOR TO USE-SEGNO
                ELSE
                    MOVE 'N' TO SEGMENTS-REMAINING
                END-IF
        END-READ

        IF SEGMENTS-REMAINING = 'Y' THEN
            PERFORM 5300-LOAD-SEGMENT
        END-IF

    END-PERFORM.

    PERFORM 5800-GENERATE-COUNT.
    EXIT.

5200-START-SEGMENT.
    MOVE FLD-SCREEN-NAME TO SCRBG-KEY.
    MOVE 1 TO SCRBG-SEGMENT-NO, USE-SEGNO.
    MOVE ZERO TO ADJ-FACTOR.
    START SCRNBG-FILE KEY IS >= SCRBG-KEY
        INVALID KEY
            MOVE 'N' TO SEGMENTS-REMAINING
        NOT INVALID KEY
            MOVE 'Y' TO SEGMENTS-REMAINING
    END-START.
    EXIT.

5300-LOAD-SEGMENT.
    MOVE SCRBG-LINE TO LINEX.               *> SCREEN Y (LINE NUMBER)
    MOVE SCRBG-COLUMN TO NUM-COLUMN.        *> SCREEN X (COLUMN)
    MOVE SCRBG-LENGTH TO NUM-OTLEN.         *> LENGTH OF THIS TEXTUAL SEGMENT

    MOVE SCRBG-SEGMENT TO SCREEN-LINE(LINEX)(NUM-COLUMN:NUM-OTLEN)  *> UPDATE SCREEN IMAGE TEXT

*>
*>  REPLACE ALL DOUBLE-QUOTES WITH SINGLE QUOTES TO AVOID COMPILE ERRORS :
*>
    INSPECT SCRBG-SEGMENT REPLACING ALL '"' BY "'".

*>
*>  IF WE HAVE A STRIP CHARACTER, THEN REPLACE OCCURRENCES WITH A BLANK :
*>
    IF SCN-STRIP-CHARACTER NOT = SPACE THEN
        PERFORM 7100-STRIP-SEGMENT
    END-IF.
*>
*>  IF WE STILL HAVE TEXT AFTER THE STRIP, THEN GENERATE WS CODE FOR IT :
*>
    IF NUM-OTLEN > ZERO THEN
        MOVE USE-SEGNO TO GENPD-S9-COUNT
        PERFORM 5400-GENERATE-WS
        PERFORM 5500-GENERATE-PD
    END-IF.
    EXIT.

5400-GENERATE-WS.
*>
*>  CHECK FOR SPECIAL CHARACTERS. IF SO, THEN CONVERT TO HEX FORMAT.
*>
    MOVE LENGTH OF WS-HEXBUF TO WS-HEXBUF-LENGTH.
    CALL "COBCURSES_SPECIAL_CHARS" USING SCRBG-SEGMENT, NUM-OTLEN, WS-HEXBUF, WS-HEXBUF-LENGTH.
    IF RETURN-CODE = ZERO THEN
        PERFORM 5410-GENERATE-PLAIN-SEGMENT
    ELSE
        MOVE RETURN-CODE TO WS-HEXBUF-LENGTH
        PERFORM 5420-GENERATE-HEX-SEGMENT
    END-IF.
    EXIT.

5410-GENERATE-PLAIN-SEGMENT.
    MOVE SPACES TO SRC-COMMENT, SRC-LINE.
    MOVE NUM-OTLEN TO PIC-OTLEN.
    STRING "10  SCR-", USE-SEGNO, " PIC X(", PIC-OTLEN, ")" INTO SRC-LINE.
    WRITE GENWS-LINE FROM SRC.

    MOVE SPACES TO SRC-2-LINE.
    STRING 'VALUE "', SCRBG-SEGMENT(1:NUM-OTLEN), '".' INTO SRC-2-LINE.
    WRITE GENWS-LINE FROM SRC-2.
    EXIT.

5420-GENERATE-HEX-SEGMENT.
    MOVE NUM-OTLEN TO PIC-OTLEN.

    MOVE SPACES TO SRC-COMMENT, SRC-LINE.
    STRING "10  SCR-", USE-SEGNO, "." INTO SRC-LINE.
    WRITE GENWS-LINE FROM SRC.

    IF WS-HEXBUF-LENGTH > 46 THEN
        MOVE 46 TO PIC-OTLEN                *> EMIT 46 CHARACTERS WORTH OF HEX DATA
        MOVE SPACES TO SRC-COMMENT, SRC-LINE
        STRING "    15  PIC X(23)" INTO SRC-LINE
        WRITE GENWS-LINE FROM SRC
        
        MOVE SPACES TO SRC-2-LINE
        STRING "VALUE X'", WS-HEXBUF(1:46), "'." INTO SRC-2-LINE
        WRITE GENWS-LINE FROM SRC-2

        SUBTRACT 46 FROM WS-HEXBUF-LENGTH

        SUBTRACT 23 FROM NUM-OTLEN
        MOVE NUM-OTLEN TO PIC-OTLEN

        MOVE 47 TO WS-HEXBUF-OFFSET         *> RESUME AT THIS POINT

        MOVE SPACES TO SRC-COMMENT, SRC-LINE
        STRING "    15 PIC X(", PIC-OTLEN, ")" INTO SRC-LINE
    ELSE        
        MOVE 1 TO WS-HEXBUF-OFFSET          *> START AT THE BEGINNING
        MOVE SPACES TO SRC-COMMENT, SRC-LINE
        STRING "    15  PIC X(", PIC-OTLEN, ")" INTO SRC-LINE
    END-IF.

    WRITE GENWS-LINE FROM SRC.
        
    MOVE SPACES TO SRC-2-LINE.
    STRING "VALUE X'", WS-HEXBUF(WS-HEXBUF-OFFSET:WS-HEXBUF-LENGTH), "'." INTO SRC-2-LINE.
    WRITE GENWS-LINE FROM SRC-2.
    EXIT.

5500-GENERATE-PD.
    IF LINEX = 1 THEN
        PERFORM 5600-GENERATE-TITLE
    ELSE
        PERFORM 5700-GENERATE-SEGMENT
    END-IF.
    EXIT.

5600-GENERATE-TITLE.
    MOVE USE-SEGNO TO GENPD-T0-SEGNO.
    WRITE GENPD-LINE FROM GENPD-LINE-T0.
    MOVE USE-SEGNO TO GENPD-T1-SEGNO, GENPD-T1-LINE.
    WRITE GENPD-LINE FROM GENPD-LINE-T1.
    MOVE USE-SEGNO TO GENPD-T2-SEGNO, GENPD-T2-LINE.
    WRITE GENPD-LINE FROM GENPD-LINE-T2.
    MOVE USE-SEGNO TO GENPD-T3-SEGNO, GENPD-T3-SEGNO2.
    WRITE GENPD-LINE FROM GENPD-LINE-T3.
    MOVE USE-SEGNO TO GENPD-T4-SEGNO.
    WRITE GENPD-LINE FROM GENPD-LINE-T4.
    MOVE USE-SEGNO TO GENPD-T5-SEGNO, GENPD-T5-SEGNO2.
    WRITE GENPD-LINE FROM GENPD-LINE-T5.
    MOVE USE-SEGNO TO GENPD-T6-SEGNO.
    WRITE GENPD-LINE FROM GENPD-LINE-T6.
    MOVE USE-SEGNO TO GENPD-T7-SEGNO, GENPD-T7-SEGNO2.
    WRITE GENPD-LINE FROM GENPD-LINE-T7.
    MOVE USE-SEGNO TO GENPD-T8-SEGNO.
    WRITE GENPD-LINE FROM GENPD-LINE-T8.
    EXIT.

5700-GENERATE-SEGMENT.
    MOVE USE-SEGNO TO GENPD-S0-SEGNO.
    WRITE GENPD-LINE FROM GENPD-LINE-S0.
    MOVE USE-SEGNO TO GENPD-S1-SEGNO, GENPD-S1-SEGNO2.
    WRITE GENPD-LINE FROM GENPD-LINE-S1.
    MOVE USE-SEGNO TO GENPD-S2-SEGNO, GENPD-S2-SEGNO2.
    WRITE GENPD-LINE FROM GENPD-LINE-S2.
    MOVE LINEX TO GENPD-S3-LINE.
    MOVE USE-SEGNO TO GENPD-S3-SEGNO.
    WRITE GENPD-LINE FROM GENPD-LINE-S3.
    MOVE NUM-COLUMN TO GENPD-S4-COLUMN.
    MOVE USE-SEGNO TO GENPD-S4-SEGNO.
    WRITE GENPD-LINE FROM GENPD-LINE-S4.
    EXIT.

5800-GENERATE-COUNT.
    WRITE GENPD-LINE FROM " ".
    WRITE GENPD-LINE FROM GENPD-LINE-S9.
    EXIT.

5900-WRITE-SCREEN-IMAGE.
    PERFORM VARYING LINEX FROM 1 BY 1 UNTIL LINEX > 50 OR LINEX > SCN-LINES-MIN
        CALL "COBCURSES_SPECIAL2ASCII" USING SCREEN-LINE(LINEX), WS-SCREEN-LINE-LENGTH  *> CVT GRAPHICS TO ASCII
        WRITE GENSI-LINE FROM SCREEN-LINE(LINEX)                                        *> WRITE ASCII LINE IMAGE
    END-PERFORM.
    EXIT.

6000-WRITE-FINFO.
    PERFORM 6010-START-FIELDS.
    PERFORM UNTIL SEGMENTS-REMAINING = 'N'
        READ SCRNFDEF-FILE NEXT RECORD
            AT END
                MOVE 'N' TO SEGMENTS-REMAINING
            NOT AT END
                IF SCR-FDEF-SCREEN-NAME NOT = FLD-SCREEN-NAME
                    MOVE 'N' TO SEGMENTS-REMAINING
                END-IF
        END-READ
        IF SEGMENTS-REMAINING = 'Y'
            PERFORM 6200-GENERATE-FINFO
        END-IF
    END-PERFORM.
*>
*>  FNO-* CONSTANTS
*>
    WRITE GENWS-LINE FROM " ".
    WRITE GENWS-LINE FROM "        01  FIELD-NUMBERS.".
    MOVE FLD-SCREEN-NAME TO SCR-FDEF-SCREEN-NAME.
    MOVE ZERO TO SCR-FDEF-NO.

    START SCRNFDEF-FILE KEY IS > SCR-FDEF-KEY
        INVALID KEY
            MOVE 'N' TO SEGMENTS-REMAINING
        NOT INVALID KEY
            MOVE 'Y' TO SEGMENTS-REMAINING
    END-START.

    PERFORM UNTIL SEGMENTS-REMAINING = 'N'
        READ SCRNFDEF-FILE NEXT RECORD
            AT END
                MOVE 'N' TO SEGMENTS-REMAINING
            NOT AT END
                IF SCR-FDEF-SCREEN-NAME NOT = FLD-SCREEN-NAME
                    MOVE 'N' TO SEGMENTS-REMAINING
                END-IF
        END-READ
        IF SEGMENTS-REMAINING = 'Y'
            PERFORM 6300-GENERATE-FNO
        END-IF
    END-PERFORM.
    EXIT.

6010-START-FIELDS.
    MOVE FLD-SCREEN-NAME TO SCR-FDEF-SCREEN-NAME.
    MOVE ZERO TO SCR-FDEF-NO.
    START SCRNFDEF-FILE KEY IS > SCR-FDEF-KEY
        INVALID KEY
            MOVE 'N' TO SEGMENTS-REMAINING
        NOT INVALID KEY
            MOVE 'Y' TO SEGMENTS-REMAINING
    END-START.
    EXIT.

6020-WRITE-FDEFS.
    WRITE GENWS-LINE FROM " ".
    WRITE GENWS-LINE FROM "        01  FLD-BUFFERS.".

    WRITE GENPD-LINE FROM " ".
    WRITE GENPD-LINE FROM "      *>".
    WRITE GENPD-LINE FROM "      *>    FIELD DEFINITIONS :".
    WRITE GENPD-LINE FROM "      *>".

    MOVE FLD-SCREEN-NAME TO SCR-FDEF-SCREEN-NAME.
    MOVE ZERO TO SCR-FDEF-NO.

    START SCRNFDEF-FILE KEY IS >= SCR-FDEF-KEY
        INVALID KEY
            MOVE 'N' TO SEGMENTS-REMAINING
        NOT INVALID KEY
            MOVE 'Y' TO SEGMENTS-REMAINING
    END-START.

    PERFORM UNTIL SEGMENTS-REMAINING = 'N'
        READ SCRNFDEF-FILE NEXT RECORD
            AT END
                MOVE 'N' TO SEGMENTS-REMAINING
            NOT AT END
                IF SCR-FDEF-SCREEN-NAME NOT = FLD-SCREEN-NAME
                    MOVE 'N' TO SEGMENTS-REMAINING
                END-IF
        END-READ
        IF SEGMENTS-REMAINING = 'Y'
            PERFORM 6400-GENERATE-FIELD
        END-IF
    END-PERFORM.
    EXIT.
            
6030-WRITE-STATE.
    WRITE GENWS-LINE FROM " ".
    WRITE GENPD-LINE FROM " ".

    INITIALIZE SRC.
    MOVE "*>" TO SRC-COMMENT.
    WRITE GENPD-LINE FROM SRC.

    MOVE "STATE TABLE DEFINITION :" TO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    MOVE SPACES TO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    PERFORM 6040-START-STATES.

    PERFORM UNTIL SEGMENTS-REMAINING = 'N'
        READ SCRFSTA-FILE NEXT RECORD
            AT END
                MOVE 'N' TO SEGMENTS-REMAINING
            NOT AT END
                IF SCR-FST-SCREEN-NAME NOT = FLD-SCREEN-NAME
                    MOVE 'N' TO SEGMENTS-REMAINING
                END-IF
        END-READ
        IF SEGMENTS-REMAINING = 'Y' THEN
            PERFORM 6700-GENERATE-STATE
        END-IF
    END-PERFORM.

    INITIALIZE SRC.
    MOVE "*>" TO SRC-COMMENT.
    WRITE GENPD-LINE FROM SRC.

    MOVE "STARTING STATE :" TO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    MOVE SPACES TO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "MOVE ", ACTION-NO, " TO NC-FSEQ-STATE" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.
    EXIT.

6040-START-STATES.
    INITIALIZE SCRFSTA-RECORD.
    MOVE FLD-SCREEN-NAME TO SCR-FST-SCREEN-NAME.
    MOVE 1 TO SCR-FST-STATE-NO.

    START SCRFSTA-FILE KEY IS >= SCR-FST-KEY
        INVALID KEY
            MOVE 'N' TO SEGMENTS-REMAINING
        NOT INVALID KEY
            MOVE 'Y' TO SEGMENTS-REMAINING
    END-START.
    EXIT.

6200-GENERATE-FINFO.
    MOVE SCR-FDEF-COBOL-NAME TO GENWS-FI-0-COBOL-NAME.
    MOVE SCR-FDEF-HELP TO LO-BUFFER..

    MOVE 38 TO LO-LIMIT-TO..
    MOVE 1 TO LO-OFFSET.

    IF LO-BUFFER NOT = SPACES THEN
        WRITE GENWS-LINE FROM GENWS-FI-0
        PERFORM 7000-GET-LENGTH-OFFSET

        PERFORM UNTIL LO-BUFFER = SPACES
            MOVE LO-LIMIT-TO TO LO-LENGTH, GENWS-FI-1-LENGTH
            ADD LO-OFFSET TO LO-LENGTH GIVING LO-TEMP
            IF LO-TEMP > LENGTH OF LO-BUFFER THEN
                MOVE LENGTH OF LO-BUFFER TO LO-TEMP
                ADD 1 TO LO-TEMP
                SUBTRACT LO-OFFSET FROM LO-TEMP GIVING LO-LENGTH
            END-IF
                    
            MOVE LO-BUFFER(LO-OFFSET:LO-LENGTH)
                TO GENWS-FI-2-TEXT
            INSPECT GENWS-FI-2-TEXT REPLACING ALL '"' BY "'"
            MOVE SPACES TO LO-BUFFER(LO-OFFSET:LO-LENGTH)
*>
*>    ONLY TRIM TRAILING BLANKS ON THE LAST SEGMENT :
*>
            IF LO-BUFFER = SPACES THEN
                PERFORM 6210-TRIM-TRAILING-BLANKS
            END-IF
            ADD LO-LENGTH TO LO-OFFSET
            ADD 1 TO LO-LENGTH
            MOVE '".' TO GENWS-FI-2-TEXT(LO-LENGTH:2)
            WRITE GENWS-LINE FROM GENWS-FI-1
            WRITE GENWS-LINE FROM GENWS-FI-2
        END-PERFORM
    END-IF.
    EXIT.

6210-TRIM-TRAILING-BLANKS.
    PERFORM UNTIL LO-LENGTH < 2 OR GENWS-FI-2-TEXT(LO-LENGTH:1) NOT = SPACE
        SUBTRACT 1 FROM LO-LENGTH
    END-PERFORM.
    MOVE LO-LENGTH TO GENWS-FI-1-LENGTH.
    EXIT.

6300-GENERATE-FNO.
    MOVE SCR-FDEF-COBOL-NAME TO GENWS-FNO-COBOL-NAME.
    MOVE SCR-FDEF-NO TO GENWS-FNO-VALUE.
    WRITE GENWS-LINE FROM GENWS-FNO-1.
    WRITE GENWS-LINE FROM GENWS-FNO-2.
    EXIT.

6400-GENERATE-FIELD.
    PERFORM 6500-GENERATE-FIELD-WS.

    IF SCR-FDEF-LINE >= 1 AND SCR-FDEF-COLUMN >= 1 THEN
        PERFORM 6600-GENERATE-FIELD-PD
    ELSE
        INITIALIZE SRC
        MOVE "*>" TO SRC-COMMENT
        STRING SCR-FDEF-COBOL-NAME, " SUPPRESSED (NO LINE/COL)" INTO SRC-LINE
        WRITE GENPD-LINE FROM SRC
    END-IF.

    IF SCR-FDEF-ACTION = 'Y' THEN
        MOVE SCR-FDEF-NO TO ACTION-NO
    END-IF.
    EXIT.

6500-GENERATE-FIELD-WS.
    MOVE SCR-FDEF-COBOL-NAME TO GENWS-F1-COBOL-NAME.
    MOVE SCR-FDEF-BUFFER-LENGTH TO GENWS-F1-LENGTH.
    WRITE GENWS-LINE FROM GENWS-F1.
*>
*>  WRITE OUT CMP-* DEFINITION, FOR CERTAIN TYPES OF FIELDS :
*>
    MOVE SCR-FDEF-COBOL-NAME TO GENWS-F1-COMP-COBOL-NAME.
    EVALUATE SCR-FDEF-COMP-TYPE
        WHEN 01
            MOVE 1 TO GENWS-F1-COMP-TYPE
            WRITE GENWS-LINE FROM GENWS-F1-COMP-X
        WHEN 02
            MOVE 2 TO GENWS-F1-COMP-TYPE
            WRITE GENWS-LINE FROM GENWS-F1-COMP-X
        WHEN OTHER
            CONTINUE
    END-EVALUATE.
    EXIT.

6600-GENERATE-FIELD-PD.
    MOVE SCR-FDEF-NO TO GENPD-F0-NO.
    MOVE SCR-FDEF-COBOL-NAME TO GENPD-F0-COBOL-NAME.
    MOVE SCR-FDEF-DESCRIPTION TO GENPD-F0-COMMENT.

    WRITE GENPD-LINE FROM "      *>".
    WRITE GENPD-LINE FROM GENPD-F0-A.
    WRITE GENPD-LINE FROM GENPD-F0-B.
    WRITE GENPD-LINE FROM "      *>".

    IF SCR-FDEF-RES-CHARSET = SPACES THEN
        INITIALIZE CHARSET-RECORD
        MOVE ZERO TO FRX
    ELSE
        MOVE SCR-FDEF-RES-CHARSET TO CHARSET-NAME
        READ CHARSET-FILE
            INVALID KEY
                MOVE SPACES TO CHARSET-NAME, CHARSET-DATA
        END-READ
        IF CHARSET-NAME NOT = SPACES
            PERFORM 6900-REGISTER-CHARSET
        ELSE
            MOVE ZERO TO FRX
        END-IF
    END-IF.

    MOVE SCR-FDEF-LINE TO GENPD-F1-VALUE.
    MOVE "LINE" TO GENPD-F1-FDESC-NAME.
    MOVE SCR-FDEF-NO TO GENPD-F1-SUBSCR.
    WRITE GENPD-LINE FROM GENPD-F1.

    MOVE SCR-FDEF-COLUMN TO GENPD-F1-VALUE.
    MOVE "COLUMN" TO GENPD-F1-FDESC-NAME.
    WRITE GENPD-LINE FROM GENPD-F1.

    MOVE "ADDRESS" TO GENPD-F2-FDESC-NAME.
    MOVE SCR-FDEF-NO TO GENPD-F2-SUBSCR.
    WRITE GENPD-LINE FROM GENPD-F2.

    MOVE "FLD" TO GENPD-F3-PREFIX.
    MOVE SCR-FDEF-COBOL-NAME TO GENPD-F3-COBOL-NAME.
    WRITE GENPD-LINE FROM GENPD-F3.

    MOVE SCR-FDEF-BUFFER-LENGTH TO GENPD-F1-VALUE.
    MOVE "LENGTH" TO GENPD-F1-FDESC-NAME.
    WRITE GENPD-LINE FROM GENPD-F1.
                        
    MOVE SCR-FDEF-WINDOW-LENGTH TO GENPD-F1-VALUE.
    MOVE "WINLENGTH" TO GENPD-F1-FDESC-NAME.
    WRITE GENPD-LINE FROM GENPD-F1.

    MOVE "CLEAR" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-NO TO GENPD-F4-SUBSCR.
    MOVE SCR-FDEF-CLEAR TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE "UPPERCASE" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-UPPERCASE TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE "MASK" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-PASSWORD TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE "NOT-BLANK" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-NOT-BLANK TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE "YN" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-YN TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    INITIALIZE SRC.
    STRING "MOVE ", FRX, " TO NC-FDESC-RESTRICT(", SCR-FDEF-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    MOVE "SIGNED" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-SIGNED TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE SCR-FDEF-DIGITS TO GENPD-F1-VALUE.
    MOVE "DIGITS" TO GENPD-F1-FDESC-NAME.
    WRITE GENPD-LINE FROM GENPD-F1.

    MOVE SCR-FDEF-DECIMALS TO GENPD-F1-VALUE.
    MOVE "DECPLACES" TO GENPD-F1-FDESC-NAME.
    WRITE GENPD-LINE FROM GENPD-F1.

    MOVE "VERIFY" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-VERIFY TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE "VISIBLE" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-VISIBLE TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE "IGNORE-CHGS" TO GENPD-F4-FDESC-NAME.
    MOVE SCR-FDEF-IGNORE-CHANGES TO GENPD-F4-FLAG.
    WRITE GENPD-LINE FROM GENPD-F4.

    MOVE "INFO" TO GENPD-F2-FDESC-NAME.
    MOVE SCR-FDEF-NO TO GENPD-F2-SUBSCR.
    WRITE GENPD-LINE FROM GENPD-F2.

    MOVE SCR-FDEF-HELP TO LO-BUFFER.
    PERFORM 7000-INIT-LENGTH-OFFSET.
    IF LO-BUFLEN > 0 THEN
        MOVE "INF" TO GENPD-F3-PREFIX
        WRITE GENPD-LINE FROM GENPD-F3
    ELSE
        WRITE GENPD-LINE FROM "                TO NULL"
    END-IF.

    MOVE LO-BUFLEN TO GENPD-F1-VALUE.
    MOVE "INFOLEN" TO GENPD-F1-FDESC-NAME.
    WRITE GENPD-LINE FROM GENPD-F1.

*>
*>  MOVE NC-TITLE-PAIR TO NC-FDESC-COLOUR-PAIR(99)
*>  MOVE 'Y' TO NC-FDESC-COLOUR-FLAG(99)
*>
    IF SCR-FDEF-READ-ONLY = 'Y' THEN
        INITIALIZE SRC
        STRING "MOVE NC-TITLE-PAIR TO NC-FDESC-COLOUR-PAIR(", SCR-FDEF-NO, ")." INTO SRC-LINE
        WRITE GENPD-LINE FROM SRC

        INITIALIZE SRC
        STRING "MOVE 'Y' TO NC-FDESC-COLOUR-FLAG(", SCR-FDEF-NO, ")." INTO SRC-LINE
        WRITE GENPD-LINE FROM SRC
    END-IF.

*>
*>    INITIALIZE THE NC-FDESC-ACTION-EDIT FLAG :
*>
*>    THIS FLAG INDICATES IF THE FIELD IS A VALID TARGET
*>    OF THE ACTION FIELD, WHEN A FIELD EDIT IS REQUESTED.
*>
    IF SCR-FDEF-ACTION-EDIT = 'Y' THEN
        INITIALIZE SRC
        STRING "MOVE 'Y' TO NC-FDESC-ACTION-EDIT(", SCR-FDEF-NO, ")." INTO SRC-LINE
        WRITE GENPD-LINE FROM SRC
    END-IF.

    INITIALIZE SRC.
    STRING "MOVE '", SCR-FDEF-READ-ONLY, "'", " TO NC-FDESC-READ-ONLY(", SCR-FDEF-NO, ")." INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "MOVE ", SCR-FDEF-COMP-TYPE, " TO NC-FDESC-COMP-TYPE(", SCR-FDEF-NO, ")." INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "SET NC-FDESC-COMP-PTR(", SCR-FDEF-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    EVALUATE SCR-FDEF-COMP-TYPE
        WHEN 01
            STRING "    TO ADDRESS OF CMP-", SCR-FDEF-COBOL-NAME INTO SRC-LINE
        WHEN 02
            STRING "    TO ADDRESS OF CMP-", SCR-FDEF-COBOL-NAME INTO SRC-LINE
        WHEN OTHER
            STRING "    TO NULL" INTO SRC-LINE
    END-EVALUATE.
    WRITE GENPD-LINE FROM SRC.

*>
*>    NC-FDEC-MENU-PTR(X) :
*>
    INITIALIZE SRC.
    STRING "SET NC-FDESC-MENU-PTR(", SCR-FDEF-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    IF SCR-FDEF-MENU-REF = SPACES THEN
        MOVE "    TO NULL." TO SRC-LINE
    ELSE
        INITIALIZE SRC
        STRING "    TO ADDRESS OF MENU-", SCR-FDEF-MENU-REF, "." INTO SRC-LINE
    END-IF.
    WRITE GENPD-LINE FROM SRC.
    EXIT.

6700-GENERATE-STATE.
    PERFORM 6800-LOOKUP-FIELD.

    MOVE SCR-FST-STATE-NO TO DSP-FST-STATE-NO.
    MOVE SCR-FST-FIELD-NO TO DSP-FST-FIELD-NO.
    MOVE SCR-FST-FIELD-NO TO ZZZ-FST-FIELD-NO.
    MOVE SCR-FST-BACK-TO TO DSP-FST-BACK-TO.
    MOVE SCR-FST-FORWARD-TO TO DSP-FST-FORWARD-TO.
    MOVE SCR-FST-ESCAPE-TO TO DSP-FST-ESCAPE-TO.
    MOVE SCR-FST-SLASH-TO TO DSP-FST-SLASH-TO.

    IF SCR-FST-STATE-COBOL-NAME NOT = SPACES
        INITIALIZE SRC-0
        STRING "77  FSEQ-", SCR-FST-STATE-COBOL-NAME, " PIC 999 COMP VALUE ", DSP-FST-STATE-NO, "." INTO SRC-LINE-0
        WRITE GENWS-LINE FROM SRC-0
    END-IF.

    INITIALIZE SRC.
    MOVE "*>" TO SRC-COMMENT.
    IF SCR-FST-GROUP-HEADER = 'Y'
        STRING "STATE # ", DSP-FST-STATE-NO, ", GROUP HEADER : ", SCR-FST-STATE-COBOL-NAME INTO SRC-LINE
    ELSE
        STRING "STATE # ", DSP-FST-STATE-NO, " : ", ZZZ-FST-FIELD-NO, ".", SCR-FDEF-COBOL-NAME INTO SRC-LINE
    END-IF.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "MOVE ", DSP-FST-FIELD-NO, " TO NC-FSEQ-FIELD-NO(", DSP-FST-STATE-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "MOVE ", DSP-FST-FORWARD-TO, " TO NC-FSEQ-FWD-TO(", DSP-FST-STATE-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "MOVE ", DSP-FST-BACK-TO, " TO NC-FSEQ-BCK-TO(", DSP-FST-STATE-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "MOVE ", DSP-FST-ESCAPE-TO, " TO NC-FSEQ-ESC-TO(", DSP-FST-STATE-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    INITIALIZE SRC.
    STRING "MOVE ", DSP-FST-SLASH-TO, " TO NC-FSEQ-SLASH-TO(", DSP-FST-STATE-NO, ")" INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.
    EXIT.

6800-LOOKUP-FIELD.
    INITIALIZE SCRNFDEF-RECORD.
    MOVE SCR-FST-SCREEN-NAME TO SCR-FDEF-SCREEN-NAME.
    MOVE SCR-FST-FIELD-NO TO SCR-FDEF-NO.
    READ SCRNFDEF-FILE
        INVALID KEY
            INITIALIZE SCRNFDEF-RECORD
    END-READ.
    EXIT.

6900-REGISTER-CHARSET.
    MOVE ZERO TO FRX.
    PERFORM VARYING TRX FROM 1 BY 1 UNTIL TRX > RX
        IF CS-NAME(TRX) = CHARSET-NAME
            MOVE TRX TO FRX
            MOVE HIGH-VALUES TO TRX
        END-IF
    END-PERFORM.

    IF FRX = ZERO THEN
        ADD 1 TO RX
        MOVE RX TO FRX

        INITIALIZE SRC
        STRING "STRING ", '"', CHARSET-DATA(1:40), '",' INTO SRC-LINE
        WRITE GENPD-LINE FROM SRC

        INITIALIZE SRC
        STRING '    "', CHARSET-DATA(41:40), '"' INTO SRC-LINE
        WRITE GENPD-LINE FROM SRC
        
        INITIALIZE SRC
        STRING "    INTO NC-RESTRICT-CHARSET(", FRX, ")" INTO SRC-LINE
        WRITE GENPD-LINE FROM SRC
    END-IF.
    EXIT.

6910-WRITE-SCREEN-DIMENSIONS.
    MOVE SPACES TO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    MOVE "*>" TO SRC-COMMENT-0.
    MOVE SPACES TO SRC-LINE-0.
    WRITE GENPD-LINE FROM SRC-0.

    MOVE "THESE VALUES SPECIFY MINIMUM SCREEN REQUIREMENTS" TO SRC-LINE-0.
    WRITE GENPD-LINE FROM SRC-0.
            
    MOVE SPACES TO SRC-LINE-0.
    WRITE GENPD-LINE FROM SRC-0.

    MOVE SPACES TO SRC-LINE.
    STRING "MOVE ", SCN-COLUMNS-MIN, " TO ", "NC-SCREEN-COLUMNS-REQ." INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    MOVE SPACES TO SRC-LINE.
    STRING "MOVE ", SCN-LINES-MIN, " TO ", "NC-SCREEN-LINES-REQ." INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.

    MOVE SPACES TO SRC-LINE.
    STRING "MOVE ", SCN-PAIRS, " TO " "NC-SCREEN-PAIRS-REQ." INTO SRC-LINE.
    WRITE GENPD-LINE FROM SRC.
    EXIT.

7000-INIT-LENGTH-OFFSET.
    MOVE 1 TO LO-OFFSET.
    MOVE LENGTH OF LO-BUFFER TO LO-BUFLEN.

    PERFORM UNTIL LO-BUFLEN < 1 OR LO-OFFSET = 0
        IF LO-BUFFER(LO-BUFLEN:1) NOT = ' ' THEN
            MOVE ZERO TO LO-OFFSET
        ELSE
            SUBTRACT 1 FROM LO-BUFLEN
        END-IF
    END-PERFORM.
    EXIT.

7000-GET-LENGTH-OFFSET.
    MOVE ZERO TO LO-OFFSET.
    INSPECT LO-BUFFER(1:LO-BUFLEN) TALLYING LO-OFFSET FOR LEADING ' '.
    MOVE LO-BUFLEN TO LO-LENGTH.
    SUBTRACT LO-OFFSET FROM LO-LENGTH.
    IF LO-LENGTH > LO-LIMIT-TO
        MOVE LO-LIMIT-TO TO LO-LENGTH
    END-IF.
    ADD 1 TO LO-OFFSET.
    EXIT.

7100-STRIP-SEGMENT.
    INSPECT SCRBG-SEGMENT REPLACING ALL SCN-STRIP-CHARACTER BY SPACE.

    MOVE SCRBG-SEGMENT TO WS-TEXT.
    PERFORM 8900-GET-TEXT-LENGTH.
    MOVE WS-TEXT-LENGTH TO SCRBG-LENGTH, NUM-OTLEN.
*>
*>  NOW ADJUST FOR LEADING BLANKS THAT MAY RESULT FROM THE STRIP OPERATION:
*>
    PERFORM UNTIL NUM-OTLEN < 1 OR SCRBG-SEGMENT(1:1) NOT = SPACE
        ADD 1 TO NUM-COLUMN
        SUBTRACT 1 FROM NUM-OTLEN
        MOVE SCRBG-SEGMENT(2:NUM-OTLEN) TO WS-TEXT
        MOVE WS-TEXT TO SCRBG-SEGMENT
    END-PERFORM.

    MOVE NUM-OTLEN TO SCRBG-LENGTH.
    EXIT.

8000-WRITE-MENU-DATA.
    PERFORM 6010-START-FIELDS.
    PERFORM UNTIL SEGMENTS-REMAINING NOT = 'Y'
        PERFORM 5085-READ-FD
        IF SEGMENTS-REMAINING = 'Y' THEN
            IF SCR-FDEF-MENU-REF NOT = SPACES THEN
                PERFORM 8100-READ-N-EMIT-MENU
            END-IF
        END-IF
    END-PERFORM.
*>
*>  NOW ADD ANY MENUS THAT ARE REGISTERED IN THE MENUREFS.X FILE :
*>
    INITIALIZE MREF-RECORD.
    START MREF-FILE KEY IS >= MREF-KEY
        INVALID KEY
            SET WS-EOF TO TRUE
        NOT INVALID KEY
            SET WS-EOF TO FALSE
    END-START.

    PERFORM UNTIL WS-EOF
        READ MREF-FILE NEXT RECORD
            AT END
                SET WS-EOF TO TRUE
            NOT AT END
                INITIALIZE MENU-RECORD
                MOVE MREF-MENU-NAME TO MNU-MENU-NAME    *> THIS IS THE MENU WE WANT
                PERFORM 8105-PROCESS-THIS-MENU          *> ADD THIS MENU TO THE CODE IF WE'VE NOT ALREADY DONE SO
        END-READ
    END-PERFORM.
    EXIT.
        
8100-READ-N-EMIT-MENU.
    INITIALIZE MENU-RECORD.
    MOVE SCR-FDEF-MENU-REF TO MNU-MENU-NAME.
    PERFORM 8105-PROCESS-THIS-MENU.
    EXIT.

8105-PROCESS-THIS-MENU.
    READ MENU-FILE
        INVALID KEY
            SET VALID-MENU TO FALSE
        NOT INVALID KEY
            SET VALID-MENU TO TRUE
    END-READ.

    IF VALID-MENU THEN
*>
*>      CHECK IF WE DID THIS ONE ALREADY :
*>
        MOVE MNU-MENU-NAME TO WS-MENU-NAME
        CALL "COBCURSEX-ASSOC-FETCH" USING WS-INSTANCE-ID, WS-ASSOC-KEY-PARAM, WS-ASSOC-KEY-PARAM WS-DATA-LENGTH
        IF RETURN-CODE NOT = NC-RET-OK THEN
            PERFORM 8200-EMIT-MENU
            CALL "COBCURSEX-ASSOC-ASSIGN" USING WS-INSTANCE-ID, WS-ASSOC-KEY-PARAM, WS-ASSOC-KEY-PARAM
        END-IF
    END-IF.
    EXIT.

8200-EMIT-MENU.
    ADD 1 TO COUNT-MENUS.
    WRITE GENWS-LINE FROM " ".

    MOVE MNU-MENU-NAME TO GENWS-MENU-05-MENU-NAME, GENWS-MENU-10-MENU-NAME.
    WRITE GENWS-LINE FROM GENWS-MENU-05.
    WRITE GENWS-LINE FROM GENWS-MENU-10.

    PERFORM 8350-EMIT-MENU-MODULE-NAME.

    MOVE MNU-TOP-LEFT-COLUMN-NO TO GENWS-MENU-10-MENU-YY.
    MOVE MNU-TOP-LEFT-LINE-NO TO GENWS-MENU-10-MENU-XX.
    WRITE GENWS-LINE FROM GENWS-MENU-10-YY.
    WRITE GENWS-LINE FROM GENWS-MENU-10-XX.
    PERFORM 8205-EMIT-MENU-OPTIONS.
    PERFORM 8210-EMIT-MENU-TITLE.
    PERFORM 8300-EMIT-MENU-ITEMS.
    EXIT.

8205-EMIT-MENU-OPTIONS.
    MOVE ZERO TO X.
    INSPECT MNU-MENU-NAME TALLYING X FOR CHARACTERS BEFORE " ".
*>
*>  MNU-OPT-ONEVALUE :
*>
    INITIALIZE GENWS-MENU-OPTION-NAME.
    STRING MNU-MENU-NAME(1:X), "-ONEVALUE" INTO GENWS-MENU-OPTION-NAME.
    MOVE MNU-OPT-ONEVALUE TO GENWS-MENU-OPTION-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-OPTION.
*>
*>  MNU-OPT-ROWMAJOR :
*>
    INITIALIZE GENWS-MENU-OPTION-NAME.
    STRING MNU-MENU-NAME(1:X), "-ROWMAJOR" INTO GENWS-MENU-OPTION-NAME.
    MOVE MNU-OPT-ROWMAJOR TO GENWS-MENU-OPTION-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-OPTION.
*>
*>  MNU-OPT-IGNORECASE :
*>
    INITIALIZE GENWS-MENU-OPTION-NAME.
    STRING MNU-MENU-NAME(1:X), "-IGNORECASE" INTO GENWS-MENU-OPTION-NAME.
    MOVE MNU-OPT-IGNORECASE TO GENWS-MENU-OPTION-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-OPTION.
*>
*>  MNU-OPT-SHOWDESC :
*>
    INITIALIZE GENWS-MENU-OPTION-NAME.
    STRING MNU-MENU-NAME(1:X), "-SHOWDESC" INTO GENWS-MENU-OPTION-NAME.
    MOVE MNU-OPT-SHOWDESC TO GENWS-MENU-OPTION-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-OPTION.
*>
*>  MNU-OPT-NONCYCLIC :
*>
    INITIALIZE GENWS-MENU-OPTION-NAME.
    STRING MNU-MENU-NAME(1:X), "-NONCYCLIC" INTO GENWS-MENU-OPTION-NAME.
    MOVE MNU-OPT-NONCYCLIC TO GENWS-MENU-OPTION-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-OPTION.
*>
*>  MNU-OPT-SHOWMATCH :
*>
    INITIALIZE GENWS-MENU-OPTION-NAME.
    STRING MNU-MENU-NAME(1:X), "-SHOWMATCH" INTO GENWS-MENU-OPTION-NAME.
    MOVE MNU-OPT-SHOWMATCH TO GENWS-MENU-OPTION-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-OPTION.
*>
*>  MENU-FMT-ROWS :
*>
    INITIALIZE GENWS-MENU-FORMAT-NAME.
    STRING MNU-MENU-NAME(1:X), "-FMT-ROWS" INTO GENWS-MENU-FORMAT-NAME.
    MOVE MNU-OPT-ROWS TO GENWS-MENU-FORMAT-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-FORMAT.
*>
*>  MENU-FMT-COLS :
*>
    INITIALIZE GENWS-MENU-FORMAT-NAME.
    STRING MNU-MENU-NAME(1:X), "-FMT-COLS" INTO GENWS-MENU-FORMAT-NAME.
    MOVE MNU-OPT-COLS TO GENWS-MENU-FORMAT-VALUE.
    WRITE GENWS-LINE FROM GENWS-MENU-05-FORMAT.
    EXIT.

8210-EMIT-MENU-TITLE.
    INITIALIZE WS-TEXT-AREA.
    MOVE WS-TEXT-MAX-SEGLEN TO WS-TEXT-MAX.
    MOVE MNU-TITLE TO WS-TEXT.

    INSPECT WS-TEXT REPLACING ALL '"' BY "'".

    PERFORM 8900-GET-TEXT-LENGTH.
    MOVE WS-TEXT-LENGTH TO GENWS-MENU-10-TITLE-SEGLEN.
    WRITE GENWS-LINE FROM GENWS-MENU-10-TITLE.

    PERFORM TEST AFTER, UNTIL WS-TEXT-LEN <= 0
        CALL "EXTRACT-TEXT" USING WS-TEXT-AREA, WS-TEXT-SEG, WS-TEXT-LEN
        PERFORM 8220-EMIT-MENU-TITLE-SEG
    END-PERFORM.
    EXIT.

8220-EMIT-MENU-TITLE-SEG.
    MOVE WS-TEXT-LEN TO GENWS-MENU-10-TITLE2-SEGLEN.
    INITIALIZE GENWS-MENU-10-TITLE-TEXT.
    IF WS-TEXT-LEN > 0 THEN
        STRING WS-TEXT-SEG(1:WS-TEXT-LEN), '".' INTO GENWS-MENU-10-TITLE-TEXT
        WRITE GENWS-LINE FROM GENWS-MENU-10-TITLE-2
    END-IF.
    EXIT.

8300-EMIT-MENU-ITEMS.
    INITIALIZE ITEM-RECORD.
    MOVE MNU-MENU-NAME TO ITM-MENU-NAME.
    START ITEM-FILE KEY IS >= ITM-KEY
        INVALID KEY
            SET END-OF-ITEMS TO TRUE
        NOT INVALID KEY
            SET END-OF-ITEMS TO FALSE
    END-START.

    IF NOT END-OF-ITEMS THEN
        PERFORM UNTIL END-OF-ITEMS
            PERFORM 8400-NEXT-ITEM
        END-PERFORM
    ELSE
        PERFORM 8600-WRITE-END-OF-ITEMS
    END-IF.
    EXIT.

8350-EMIT-MENU-MODULE-NAME.
    MOVE MNU-MENU-TYPE TO GENWS-MENU-TYPE.
    WRITE GENWS-LINE FROM GENWS-MENU-10-TYPE.

    IF MNU-MENU-TYPE = 02 THEN
        MOVE MNU-MODULE-NAME TO GENWS-MENU-MODULE-NAME
        MOVE MNU-ITEM-LIMIT TO GENWS-MENU-ITEM-LIMIT
        WRITE GENWS-LINE FROM GENWS-MENU-10-MODULE-NAME
        WRITE GENWS-LINE FROM GENWS-MENU-10-ITEM-LIMIT
    END-IF.
    EXIT.

8400-NEXT-ITEM.
    READ ITEM-FILE NEXT RECORD
        AT END
            SET END-OF-ITEMS TO TRUE
        NOT AT END
            IF ITM-MENU-NAME = MNU-MENU-NAME THEN
                SET END-OF-ITEMS TO FALSE
            ELSE
                SET END-OF-ITEMS TO TRUE
            END-IF
    END-READ.

    IF NOT END-OF-ITEMS THEN
        PERFORM 8450-EMIT-MENU-ITEM
    ELSE
        PERFORM 8600-WRITE-END-OF-ITEMS
    END-IF.
    EXIT.

8450-EMIT-MENU-ITEM.
    ADD 1 TO COUNT-ITEMS.
    WRITE GENWS-LINE FROM GENWS-ITEM-10-HEADER-1.

    MOVE ZERO TO X.
    INITIALIZE GENWS-ITEM-10-SELECTABLE-NAME.

    INSPECT MNU-MENU-NAME TALLYING X FOR CHARACTERS BEFORE " ".
    STRING MNU-MENU-NAME(1:X), "-", ITM-NUMBER, "-SEL" INTO GENWS-ITEM-10-SELECTABLE-NAME.
    MOVE ITM-SELECTABLE TO GENWS-ITEM-10-SELECTABLE.
    WRITE GENWS-LINE FROM GENWS-ITEM-10-FLAGS.

    MOVE ZERO TO X.
    INSPECT ITM-ITEM-NAME TALLYING X FOR CHARACTERS BEFORE " ".
    IF X < 1 THEN
        MOVE 1 TO X
    END-IF.
    MOVE X TO GENWS-ITEM-10-NAME-ITEM-NAMELEN, GENWS-ITEM-NAME-LEN-0.

    WRITE GENWS-LINE FROM GENWS-ITEM-10-NAME-LEN.

    INITIALIZE GENWS-ITEM-10-NAME-ITEM-NAME.
    STRING ITM-ITEM-NAME(1:X), '".' INTO GENWS-ITEM-10-NAME-ITEM-NAME.
    WRITE GENWS-LINE FROM GENWS-ITEM-10-NAME.

    PERFORM 8500-EMIT-MENU-ITEM-TEXT.
    EXIT.

8500-EMIT-MENU-ITEM-TEXT.
    INITIALIZE WS-TEXT-AREA.
    MOVE WS-TEXT-MAX-SEGLEN TO WS-TEXT-MAX.
    MOVE ITM-TEXT TO WS-TEXT.

    INSPECT WS-TEXT REPLACING ALL '"' BY "'".

    PERFORM 8900-GET-TEXT-LENGTH.
    MOVE WS-TEXT-LENGTH TO GENWS-ITEM-10-TEXT-SEGLEN.
    WRITE GENWS-LINE FROM GENWS-ITEM-10-TEXT-LENGTH.

    PERFORM TEST AFTER, UNTIL WS-TEXT-LEN <= 0
        CALL "EXTRACT-TEXT" USING WS-TEXT-AREA, WS-TEXT-SEG, WS-TEXT-LEN,
        PERFORM 8550-EMIT-ITEM-SEGMENT
    END-PERFORM.
    EXIT.

8550-EMIT-ITEM-SEGMENT.
    INITIALIZE GENWS-ITEM-10-TEXT.
    MOVE WS-TEXT-LEN TO GENWS-ITEM-10-TEXT-SEGLEN-2.
    IF WS-TEXT-LEN > 0 THEN
        STRING WS-TEXT-SEG(1:WS-TEXT-LEN), '".' INTO GENWS-ITEM-10-TEXT
        WRITE GENWS-LINE FROM GENWS-ITEM-10-TEXT-CONTENT
    END-IF.
    EXIT.

8600-WRITE-END-OF-ITEMS.
    WRITE GENWS-LINE FROM GENWS-ITEM-10-HEADER-1.
    WRITE GENWS-LINE FROM GENWS-ITEM-10-END.
    EXIT.

8900-GET-TEXT-LENGTH.
    MOVE LENGTH OF WS-TEXT TO WS-TEXT-X.
    PERFORM UNTIL WS-TEXT-X = 0 OR WS-TEXT(WS-TEXT-X:1) NOT = SPACE
        SUBTRACT 1 FROM WS-TEXT-X
    END-PERFORM.
    MOVE WS-TEXT-X TO WS-TEXT-LENGTH.            
    MOVE ZERO TO WS-TEXT-X.
    EXIT.

9000-FINALIZE.
    CLOSE GENWS-FILE, GENPD-FILE, GENSI-FILE.
    CLOSE SCREEN-FILE, SCRNBG-FILE, SCRFSTA-FILE, SCRNFDEF-FILE, CHARSET-FILE, MENU-FILE, ITEM-FILE, MREF-FILE.

    CALL "COBCURSEX-ASSOC-CLEAR" USING WS-INSTANCE-ID.
    EXIT.
            
    COPY NULLEVENTS.
    COPY COBCURSQ.
    COPY COBCURSX.

END PROGRAM libcobcurses_codegen.


IDENTIFICATION DIVISION.
PROGRAM-ID. COBCURSES-APPEND-SUFFIX-X16.

DATA DIVISION.
WORKING-STORAGE SECTION.

01  WS-X                        PIC 99 COMP.
01  WS-X2                       PIC 99 COMP.

LINKAGE SECTION.

01  INPUT-FILENAME              PIC X(16).
01  INPUT-SUFFIX                PIC X(4).
01  OUTPUT-FILENAME             PIC X(20).

PROCEDURE DIVISION USING
    INPUT-FILENAME, INPUT-SUFFIX, OUTPUT-FILENAME.

    MOVE INPUT-FILENAME TO OUTPUT-FILENAME.
    IF INPUT-FILENAME NOT = SPACES THEN
        PERFORM 5000-APPEND-SUFFIX
    END-IF.
    GOBACK.

5000-APPEND-SUFFIX.
    MOVE LENGTH OF OUTPUT-FILENAME TO WS-X.

    PERFORM UNTIL WS-X < 1 OR OUTPUT-FILENAME(WS-X:1) NOT = SPACE
        SUBTRACT 1 FROM WS-X
    END-PERFORM.

    IF WS-X > 0 THEN
        ADD 1 TO WS-X
        MOVE INPUT-SUFFIX TO OUTPUT-FILENAME(WS-X:4)
    END-IF.
    EXIT.

END PROGRAM COBCURSES-APPEND-SUFFIX-X16.

IDENTIFICATION DIVISION.
PROGRAM-ID. EXTRACT-TEXT.

DATA DIVISION.
WORKING-STORAGE SECTION.

01  WS-WORK-AREAS.
    10  WS-TEXT-END                     PIC 99 COMP.
    10  WS-TEXT-LEN                     PIC 99 COMP.
    10  WS-TEXT-BUFLEN                  PIC 99 COMP.

LINKAGE SECTION.

01  TEXT-AREA.
    10  WS-TEXT                         PIC X(80).
    10  WS-TEXT-MAX                     PIC 99 COMP.
    10  WS-TEXT-X                       PIC 99 COMP.

01  TEXT-RETURN                         PIC X(32).
01  TEXT-RETURN-LEN                     PIC 99 COMP.

PROCEDURE DIVISION USING
    TEXT-AREA, TEXT-RETURN, TEXT-RETURN-LEN.
*>
*>  INITIALIZE RETURN VALUES
*>
    MOVE SPACES TO TEXT-RETURN.
    MOVE ZERO TO TEXT-RETURN-LEN.
*>
*>  TEST FOR END-OF-SEGMENT
*>
    IF WS-TEXT = SPACES OR WS-TEXT-X > WS-TEXT-BUFLEN THEN
        PERFORM 9900-END-OF-SEGMENT
        GOBACK
    END-IF.
*>
*>  IF THIS IS THE START, DO INITIALIZATION :
*>
*>  CALCULATE BLANK TAIL TRIMMED LENGTH
*>
    IF WS-TEXT-X = 0 THEN
        MOVE LENGTH OF WS-TEXT TO WS-TEXT-X

        PERFORM UNTIL WS-TEXT(WS-TEXT-X:1) NOT = SPACE AND WS-TEXT-X > 1
            SUBTRACT 1 FROM WS-TEXT-X
        END-PERFORM

        MOVE WS-TEXT-X TO WS-TEXT-BUFLEN
        MOVE 1 TO WS-TEXT-X
    END-IF.
*>
*>  COMPUTE THE END OF THE CURRENT SEGMENT
*>
    COMPUTE WS-TEXT-END = WS-TEXT-X + WS-TEXT-MAX - 1.
    IF WS-TEXT-END > WS-TEXT-BUFLEN THEN
        MOVE WS-TEXT-BUFLEN TO WS-TEXT-END
    END-IF.
*>
*>  COMPUTE MAX LENGTH OF THIS SEGMENT
*>
    COMPUTE WS-TEXT-LEN = WS-TEXT-END - WS-TEXT-X + 1.
    MOVE WS-TEXT(WS-TEXT-X:WS-TEXT-LEN) TO TEXT-RETURN.
    MOVE WS-TEXT-LEN TO TEXT-RETURN-LEN.
    MOVE SPACES TO WS-TEXT(WS-TEXT-X:WS-TEXT-LEN).
*>
*>  ADVANCE TEXT POINTER WS-TEXT-X
*>
    ADD WS-TEXT-LEN TO WS-TEXT-X.
    GOBACK.

9900-END-OF-SEGMENT.
    MOVE ZERO TO WS-TEXT-X, WS-TEXT-LEN, WS-TEXT-END.
    EXIT.

END PROGRAM EXTRACT-TEXT.
