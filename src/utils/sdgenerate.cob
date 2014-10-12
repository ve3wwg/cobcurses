        IDENTIFICATION DIVISION.
        PROGRAM-ID. SDGENERATE.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT SCREEN-FILE
                ASSIGN TO SCREEN-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS RANDOM
                RECORD KEY IS SCN-NAME.

        DATA DIVISION.
        FILE SECTION.

        FD  SCREEN-FILE.
        01  SCREEN-RECORD.
            COPY SCREEN-01.

        WORKING-STORAGE SECTION.

        01  CMD-LINE.
            05  CMD-ARGC                    PIC 9(4).
            05  CMD-N                       PIC 9(4).
            05  CMD-EOF-FLAG                PIC X.
                88  CMD-EOF                 VALUE 'Y'
                    FALSE IS                'N'.
            05  CMD-ARG.
                10  CMD-OPTION-FLAG         PIC X.
                    88  CMD-OPTION          VALUE '-'.
                10  CMD-OPTION-NAME         PIC X(32).
                    88  CMD-OPT-D           VALUE 'd'.
                    88  CMD-OPT-I           VALUE 'i'.
                    88  CMD-OPT-UC-D        VALUE 'D'.
                    88  CMD-OPT-S           VALUE 's'.
                    88  CMD-OPT-H           VALUE 'h'.
                    88  CMD-OPT-HELP        VALUE '-help'.
                    88  CMD-OPT-EOF         VALUE '-'.
                10  FILLER                  PIC X(479).            
            05  CMD-ARG-2 REDEFINES CMD-ARG.
                10  CMD-ARGUMENT            PIC X(512).
            05  CMD-ARG-LENGTH              PIC 9(4).
            05  CMD-ARGLEN-5                PIC 9(4) COMP-5.

        01  CMD-RETURN-CODE                 PIC S9(9) VALUE 0.

        01  CMD-OPTIONS.
            05  OPT-VALID-FLAG              PIC X.
                88  OPT-VALID               VALUE 'Y'
                    FALSE IS                'N'.
                88  OPT-ARG-INVALID         VALUE 'X'.
            05  OPT-CURRENT                 PIC X(32).
            05  OPT-CURRENT-LENGTH          PIC 9(4).
            05  OPT-D-FLAG                  PIC X.
                88  OPT-D                   VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-D-ARG                   PIC X(512).
            05  OPT-I-FLAG                  PIC X.
                88  OPT-I                   VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-I-ARG                   PIC X(512).
            05  OPT-S-FLAG                  PIC X.
                88  OPT-S                   VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-UC-D-FLAG               PIC X.
                88  OPT-UC-D                VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-UC-D-ARG                PIC X(512).
            05  OPT-H-FLAG                  PIC X.
                88  OPT-H                   VALUE 'Y'
                    FALSE IS                'N'.

        01  FLD-SCREEN-NAME                 PIC X(16).
        01  SCREEN-FOUND-FLAG               PIC X(1) VALUE 'N'.
            88  SCREEN-FOUND                VALUE 'Y'.

        77  WS-ENV-NAME                     PIC X(32)
            VALUE "COBCURSES_DATADIR".
        77  WS-ENV-NAME-LEN5                PIC 9(4) COMP-5.

        01  WS-DATADIR                      PIC X(512)
            VALUE "${COBCURSES_DATADIR}".

        01  FILE-NAMES.
            10  SCREEN-FILE-NAME            PIC X(512)
                VALUE "SCREENS.X".
            01  FILE-NAME-WS                PIC X(16).
            01  FILE-NAME-PD                PIC X(16).
            01  FILE-NAME-SI                PIC X(16).

        77  COUNT-SEGMENTS                  PIC 9999.
        77  COUNT-FIELDS                    PIC 9999.
        77  COUNT-STATES                    PIC 9999.
        77  COUNT-MENUS                     PIC 9999.
        77  COUNT-ITEMS                     PIC 9999.
        77  SCREEN-DESCRIPTION              PIC X(50).
        77  FILE-NAME-LENGTH                PIC 9999.
        77  FILE-NAME-LEN5                  PIC 9(4) COMP-5.

        COPY COBCEXTRA.
        COPY COBCRETC.

        PROCEDURE DIVISION.
        MAIN-PROGRAM.
            PERFORM 1000-INITIALIZE.
            IF OPT-H THEN
                PERFORM 8900-DISPLAY-USAGE
            ELSE
                IF CMD-RETURN-CODE = ZERO THEN
                    PERFORM 5000-PROCESS
                END-IF
            END-IF.
            PERFORM 9000-FINALIZE.
            STOP RUN.

        1000-INITIALIZE.
            PERFORM NC-EXTRA-INIT.
            INITIALIZE FLD-SCREEN-NAME.
            MOVE ZERO TO COUNT-SEGMENTS, COUNT-FIELDS,
                COUNT-STATES, COUNT-MENUS, COUNT-ITEMS.

            MOVE LENGTH OF WS-DATADIR TO FILE-NAME-LENGTH.
            CALL "COBCURSES-INIT-PATHNAME"
                USING WS-DATADIR, FILE-NAME-LENGTH.

            PERFORM 7000-OPEN-CMDLINE.
            PERFORM 7100-GET-ARG.
            PERFORM
              UNTIL CMD-EOF OR CMD-OPT-EOF OR NOT CMD-OPTION
                PERFORM 1100-PROCESS-OPTION
                IF NOT OPT-VALID THEN
                    DISPLAY "ERROR: Unsupported command option -",
                        OPT-CURRENT(1:OPT-CURRENT-LENGTH)
                    MOVE 1 TO CMD-RETURN-CODE
                ELSE
                    IF OPT-ARG-INVALID THEN
                        DISPLAY "ERROR: Missing or ",
                            "invalid argument to -",
                            OPT-CURRENT(1:OPT-CURRENT-LENGTH),
                            " option."
                        MOVE 1 TO CMD-RETURN-CODE
                    END-IF
                END-IF
                PERFORM 7100-GET-ARG
            END-PERFORM.
            PERFORM 1200-SET-OPTIONS.
            EXIT.

        1100-PROCESS-OPTION.
            SET OPT-VALID TO FALSE.
            MOVE CMD-OPTION-NAME TO OPT-CURRENT.

            MOVE LENGTH OF OPT-CURRENT TO OPT-CURRENT-LENGTH.
            PERFORM UNTIL OPT-CURRENT-LENGTH = ZERO
              OR OPT-CURRENT(OPT-CURRENT-LENGTH:1) NOT = SPACE
                SUBTRACT 1 FROM OPT-CURRENT-LENGTH
            END-PERFORM.

            IF CMD-OPT-D THEN
                SET OPT-VALID TO TRUE
                SET OPT-D TO TRUE
                PERFORM 7100-GET-ARG
                IF CMD-EOF OR CMD-ARGUMENT = SPACES THEN
                    MOVE SPACES TO OPT-D-ARG
                    SET OPT-ARG-INVALID TO TRUE
                ELSE
                    MOVE CMD-ARGUMENT TO OPT-D-ARG
                END-IF
            END-IF.

            IF CMD-OPT-I THEN
                SET OPT-VALID TO TRUE
                SET OPT-I TO TRUE
                PERFORM 7100-GET-ARG
                IF CMD-EOF OR CMD-ARGUMENT = SPACES THEN
                    MOVE SPACES TO OPT-I-ARG
                    SET OPT-ARG-INVALID TO TRUE
                ELSE
                    MOVE CMD-ARGUMENT TO OPT-I-ARG
                END-IF
            END-IF.

            IF CMD-OPT-S THEN
                SET OPT-VALID TO TRUE
                SET OPT-S TO TRUE
            END-IF.

            IF CMD-OPT-UC-D THEN
                SET OPT-VALID TO TRUE
                SET OPT-UC-D TO TRUE
                PERFORM 7100-GET-ARG
                IF CMD-EOF OR CMD-ARGUMENT = SPACES THEN
                    MOVE SPACES TO OPT-UC-D-ARG
                    SET OPT-ARG-INVALID TO TRUE
                ELSE
                    MOVE CMD-ARGUMENT TO OPT-UC-D-ARG
                END-IF
            END-IF.

            IF CMD-OPT-H OR CMD-OPT-HELP THEN
                SET OPT-VALID TO TRUE
                SET OPT-H TO TRUE
            END-IF.
            EXIT.

        1200-SET-OPTIONS.
      *>
      *>     CREATE SCREEN-FILE-NAME WITH CORRECT DATA DIRECTORY :
      *>
            IF OPT-UC-D THEN
                MOVE OPT-UC-D-ARG TO WS-DATADIR
            END-IF.
            MOVE LENGTH OF SCREEN-FILE-NAME TO FILE-NAME-LEN5.
            CALL "NC_mk_path" USING
                WS-DATADIR, FILE-NAME-LEN5,
                SCREEN-FILE-NAME, FILE-NAME-LEN5,
                SCREEN-FILE-NAME, FILE-NAME-LEN5.
      *>
      *>     SET ENVIRONMENT VARIABLE $COBCURSES_DATADIR FOR
      *>     THE BENEFIT OF THE "libcobcurses_codegen" CALL.
      *>
            MOVE LENGTH OF WS-ENV-NAME TO WS-ENV-NAME-LEN5.
            CALL "NC_setenv" USING
                WS-ENV-NAME, WS-ENV-NAME-LEN5,
                WS-DATADIR, FILE-NAME-LEN5.
            EXIT.

        5000-PROCESS.
            PERFORM UNTIL CMD-EOF OR CMD-RETURN-CODE NOT = ZERO
                PERFORM 5100-PROCESS-ARG
                PERFORM 7100-GET-ARG
            END-PERFORM
            EXIT.

        5100-PROCESS-ARG.
            MOVE CMD-ARGUMENT TO FLD-SCREEN-NAME.
            PERFORM 6000-LOOKUP-SCREEN.
      *>
      *>     SUPPRESS SCREEN IMAGE FILE IF OPTION -s IS GIVEN
      *>
            IF OPT-S THEN
                MOVE SPACES TO FILE-NAME-SI
            END-IF.
            CALL "libcobcurses_codegen"
              USING
                FLD-SCREEN-NAME,
                FILE-NAME-WS,
                FILE-NAME-PD,
                FILE-NAME-SI,
                OPT-D-ARG,
                OPT-I-ARG,
                COUNT-SEGMENTS,
                COUNT-FIELDS,
                COUNT-STATES,
                COUNT-MENUS,
                COUNT-ITEMS,
                SCREEN-DESCRIPTION.
            
            IF RETURN-CODE = NC-RET-OK THEN
                DISPLAY FLD-SCREEN-NAME, "  : ", SCREEN-DESCRIPTION
                DISPLAY "  SCREEN SEGMENTS : ", COUNT-SEGMENTS
                DISPLAY "FIELD DEFINITIONS : ", COUNT-FIELDS
                DISPLAY "     FIELD STATES : ", COUNT-STATES
                DISPLAY " MENU DEFINITIONS : ", COUNT-MENUS
                DISPLAY "       MENU ITEMS : ", COUNT-ITEMS
            ELSE
                DISPLAY "SCREEN ", FLD-SCREEN-NAME,
                    " IS NOT ON FILE."
            END-IF.
            EXIT.

        6000-LOOKUP-SCREEN.
            OPEN INPUT SCREEN-FILE.
            MOVE FLD-SCREEN-NAME TO SCN-NAME
            READ SCREEN-FILE
                INVALID KEY
                    DISPLAY "ERROR: SCREEN NOT ON FILE: ",
                        SCN-NAME
                    MOVE SPACES TO FILE-NAME-WS, FILE-NAME-PD,
                        FILE-NAME-SI
                    MOVE 1 TO CMD-RETURN-CODE
                NOT INVALID KEY
                    SET SCREEN-FOUND TO TRUE
                    MOVE SCN-WS-SECTION TO FILE-NAME-WS
                    MOVE SCN-PROCEDURE-DIVISION TO FILE-NAME-PD
                    MOVE SCN-NAME TO FILE-NAME-SI
            END-READ
            CLOSE SCREEN-FILE.
            EXIT.

        7000-OPEN-CMDLINE.
            INITIALIZE CMD-LINE.
            ACCEPT CMD-ARGC FROM ARGUMENT-NUMBER.
            MOVE ZERO TO CMD-N.
            SET CMD-EOF TO FALSE.
            INITIALIZE CMD-OPTIONS.
            SET OPT-D TO FALSE.
            SET OPT-D-ARG TO SPACES.
            SET OPT-I TO FALSE.
            SET OPT-I-ARG TO SPACES.
            SET OPT-S TO FALSE.
            SET OPT-UC-D TO FALSE.
            SET OPT-UC-D-ARG TO SPACES.
            SET OPT-H TO FALSE.
            EXIT.

        7100-GET-ARG.
            IF NOT CMD-EOF THEN
                IF CMD-N >= CMD-ARGC THEN
                    INITIALIZE CMD-ARG
                    SET CMD-EOF TO TRUE
                ELSE
                    ACCEPT CMD-ARG FROM ARGUMENT-VALUE
                    ADD 1 TO CMD-N
                    SET CMD-EOF TO FALSE
                    PERFORM 7200-FIND-LENGTH
                END-IF
            END-IF.
            EXIT.

        7200-FIND-LENGTH.
            MOVE LENGTH OF CMD-ARG TO CMD-ARG-LENGTH.
            PERFORM UNTIL CMD-ARG-LENGTH = ZERO
              OR CMD-ARGUMENT(CMD-ARG-LENGTH:1) NOT = SPACE
                SUBTRACT 1 FROM CMD-ARG-LENGTH
            END-PERFORM
            EXIT.

        8900-DISPLAY-USAGE.
            DISPLAY "Usage:  sdgenerate [-d destdir] [-i imagedir] "
                "[-D datadir] [-s] screens..".
            DISPLAY "where".
            DISPLAY "        -d subdir       Specifies the destination"
                " for the generated".
            DISPLAY "                        copy books. Defaults "
                "to current directory.".
            DISPLAY "        -i imagedir     Specifies the destin"
                "ation for the generated".
            DISPLAY "                        screen image text file. "
                "Default is current".
            DISPLAY "                        directory.".
            DISPLAY "        -D datadir      Use this as the data "
                "directory for the ".
            DISPLAY "                        screens database".
            DISPLAY "        -s              Suppress the screen "
                "image file.".
            DISPLAY "        -h              Requests this info.".
            DISPLAY "                        other options and "
                "arguments.".
            DISPLAY "        screens         One or more screen "
                "names (as they exist".
            DISPLAY "                        on file).".
            EXIT.

        9000-FINALIZE.
            MOVE CMD-RETURN-CODE TO RETURN-CODE.        
            EXIT.
            
        COPY COBCURSX.

        END PROGRAM SDGENERATE.
