IDENTIFICATION DIVISION.
PROGRAM-ID. SD002085.
*>
*>  THIS SCREEN MAINTAINS A LIST OF MENUS THAT ARE TO BE GENERATED
*>  FOR A SCREEN, BUT ARE NOT REFERENCED BY A SCREEN FIELD. MENUS
*>  THAT ARE REFERENCED BY A SCREEN FIELD WILL AUTOMATICALLY BE
*>  GENERATED.
*>
*>  INPUTS :
*>      NC-COBCURSES            COBCURSES GLOBALS
*>      LS-SCREEN-NAME          THE SCREEN NAME THAT IS BEING CONSIDERED

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

    SELECT MENUREF-FILE
        ASSIGN TO MENUREF-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS MREF-KEY.

    SELECT MENU-FILE
        ASSIGN TO MENU-FILE-NAME
        ORGANIZATION IS INDEXED
        ACCESS IS DYNAMIC
        RECORD KEY IS MNU-KEY.

DATA DIVISION.
FILE SECTION.

    FD  MENUREF-FILE.
    01  MENUREF-RECORD.
        COPY MENUREF.

    FD  MENU-FILE.
    01  MENU-RECORD.
        COPY MENURECD.

WORKING-STORAGE SECTION.

    COPY COBCRETC.
    COPY COBCATTR.
    COPY COBCCOLOUR.
    COPY COBCURSL.
    COPY SD002085-WS.

    01  FILE-NAMES.
        10  FILE-NAME-LENGTH                PIC 9999.
        10  MENUREF-FILE-NAME               PIC X(512)      VALUE "${COBCURSES_DATADIR}/MENUREFS.X".
        10  MENU-FILE-NAME                  PIC X(512)      VALUE "${COBCURSES_DATADIR}/MENUS.X".

    01  FLAGS.
        10  WS-SCREEN-TOO-SMALL-FLAG        PIC X VALUE 'N'.
            88  WS-SCREEN-TOO-SMALL         VALUE 'Y' FALSE IS 'N'.
        10  WS-MENU-RECORD-FLAG             PIC X VALUE 'N'.
            88  WS-HAVE-MENU-RECORD         VALUE 'Y' FALSE IS 'N'.
        10  WS-EOF-FLAG                     PIC X VALUE 'N'.
            88  WS-EOF                      VALUE 'Y' FALSE IS 'N'.

    01  WORK-AREA.
        10  WS-REF-COUNT                    PIC 9999        VALUE 0.

    01  RECORD-STATE.
        10  RECORD-DEFINED-FLAG             PIC X.
            88  DEFINED-RECORD              VALUE 'Y' FALSE IS 'N'.
        10  RECORD-NEW-FLAG                 PIC X.
            88  NEW-RECORD                  VALUE 'Y' FALSE IS 'N'.
        10  RECORD-CHANGES-FLAG             PIC X.
            88  UNSAVED-CHANGES             VALUE 'Y' FALSE IS 'N'.

    01  MENU-MENUS.
        05  PIC X(16) VALUE "MENUS".
        05  PIC 99 VALUE 02.
        05  PIC X(16) VALUE "SD-MENUS".
        05  PIC 9999 VALUE 0000.
        05  PIC 9(03) VALUE 034.
        05  PIC 9(03) VALUE 004.
        05  MENU-MENUS-ONEVALUE             PIC X VALUE "Y".
        05  MENU-MENUS-ROWMAJOR             PIC X VALUE "Y".
        05  MENU-MENUS-IGNORECASE           PIC X VALUE "Y".
        05  MENU-MENUS-SHOWDESC             PIC X VALUE "Y".
        05  MENU-MENUS-NONCYCLIC            PIC X VALUE " ".
        05  MENU-MENUS-SHOWMATCH            PIC X VALUE " ".
        05  MENU-MENUS-FMT-ROWS             PIC 99 VALUE 00.
        05  MENU-MENUS-FMT-COLS             PIC 99 VALUE 00.
        05  PIC 9(02) VALUE 16.
        05  PIC X(16) VALUE "Menu Definitions".
        05  FILLER.
            10  PIC X(01) VALUE 'X'.

LINKAGE SECTION.
    COPY COBCURSG.                                      *> COBCURSES GLOBALS
    01  LS-SCREEN-NAME                  PIC X(16).      *> THE SCREEN NAME BEING MAINTAINED
    01  LS-SCREEN-DESC                  PIC X(50).      *> THE SCREEN DESCRIPTION

PROCEDURE DIVISION
    USING NC-COBCURSES, LS-SCREEN-NAME, LS-SCREEN-DESC.

MAIN-PROGRAM.
    PERFORM 1000-INITIALIZATION.
    IF NOT WS-SCREEN-TOO-SMALL THEN
        PERFORM 5000-PROCESS
    END-IF.            
    PERFORM 9000-FINALIZE.
    GOBACK.

1000-INITIALIZATION.
    MOVE LENGTH OF MENUREF-FILE-NAME TO FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING MENUREF-FILE-NAME, FILE-NAME-LENGTH.
    CALL "COBCURSES-INIT-PATHNAME" USING MENU-FILE-NAME, FILE-NAME-LENGTH.
    PERFORM 9010-OPEN-FILES.
    PERFORM 6610-DO-COUNT-REFS.                         *> GET A REF COUNT
    PERFORM 1020-COBCURSES-INIT.
    PERFORM 1010-RECORD-INIT.
    CALL "COBCURSES-MENU-SD-MENU-MREFS"                 *> THIS MENU NEEDS TO KNOW SCREEN NAME
        USING "X", LS-SCREEN-NAME, OMITTED.
    MOVE FSEQ-ACTION TO NC-FSEQ-STATE.
    EXIT.

1010-RECORD-INIT.
    SET DEFINED-RECORD TO FALSE.                        *> THERE IS NO DEFINED RECORD
    SET NEW-RECORD TO FALSE.                            *> NO NEW RECORD DEFINED THAT NEEDS TO BE SAVED
    SET UNSAVED-CHANGES TO FALSE.                       *> THERE ARE NO UNSAVED CHANGES
    EXIT.

1020-COBCURSES-INIT.
    PERFORM NC-INIT.                                    *> INITIALIZE SCREEN ARRAYS
    PERFORM NC-CLEAR.                                   *> CLEAR THE SCREEN
    PERFORM 1030-SCREEN-INIT.                           *> INITIALIZE AND DRAW THE SCREEN
    EXIT.

1030-SCREEN-INIT.
    COPY SD002085-PD.                                   *> PAINT THE SCREEN
    PERFORM NC-CLEAR-FIELDS.                            *> CLEAR ALL FIELD BUFFERS
    PERFORM 1040-DRAW-SCREEN.                           *> DRAW ALL SCREENS AND FIELDS
    EXIT.

1040-DRAW-SCREEN.
    PERFORM NC-DRAW-SCREEN.                             *> DRAW SCREEN BACKGROUND
    IF RETURN-CODE = NC-RET-OK THEN                     *> IS THE SCREEN SIZE TOO SMALL?
        PERFORM 1050-UPDATE-RO-FIELDS                   *> UPDATE DISPLAY FIELDS
        PERFORM NC-DRAW-FIELDS                          *> NO, DRAW ALL FIELDS
    ELSE
        SET WS-SCREEN-TOO-SMALL TO TRUE                 *> YES, FLAG THIS FOR THE MAIN PROGRAM
    END-IF.
    EXIT.

1050-UPDATE-RO-FIELDS.
    MOVE LS-SCREEN-NAME TO FLD-SCREEN-NAME.
    MOVE LS-SCREEN-DESC TO FLD-SCREEN-TITLE.
    MOVE WS-REF-COUNT TO FLD-REFERENCED-COUNT.          *> SHOW THIS ON SCREEN
    EXIT.

4000-MOVE-TO-RECORD.
    PERFORM 4000-MOVE-TO-KEY.                           *> INITIALIZE AND CREATE KEY FIELDS
    *> CURRENTLY NO DATA FIELDS
    EXIT.

4000-MOVE-TO-KEY.
    INITIALIZE MENUREF-RECORD.
    MOVE LS-SCREEN-NAME TO MREF-SCREEN-NAME.            *> SCREEN NAME
    MOVE FLD-MENU-NAME TO MREF-MENU-NAME.               *> REFERENCED MENU NAME
    EXIT.

4100-MOVE-FROM-RECORD.
    MOVE MREF-MENU-NAME TO FLD-MENU-NAME.               *> REFERENCED MENU NAME
    EXIT.

5000-PROCESS.
    PERFORM NC-FIELD-STATE-MACHINE.                     *> PERFORM USER INTERACTION
    EXIT.

5100-ACTION-C.
*>
*>  ACTION C-LEAR :
*>
    PERFORM NC-CLEAR-FIELDS.                            *> BLANK OUT ALL FIELDS
    PERFORM 1050-UPDATE-RO-FIELDS.                      *> LOAD DISPLAY FIELDS
    PERFORM NC-RESET-CHANGES.                           *> RESET ALL FIELD CHANGED FLAGS
    PERFORM 1010-RECORD-INIT.                           *> RESET RECORD STATUS FLAGS
    EXIT.

5200-ACTION-N.
*>
*>  ACTION N-NEW :
*>
    IF UNSAVED-CHANGES THEN                             *> ARE THERE UNSAVED CHANGES?
        PERFORM 8010-WARNING-UNSAVED-CHANGES            *> YES, WARN THE USER ABOUT THEM INSTEAD OF DOING A N-EW
    ELSE
        PERFORM 5100-ACTION-C                           *> NO, CLEAR ALL SCREEN FIELDS AND RESET RECORD STATUS
        MOVE 1 TO NC-FSEQ-NEXT                          *> START USER INPUT
        MOVE 'Y' TO NC-FDESC-NOT-BLANK(FNO-MENU-NAME)   *> DO NOT ALLOW A BLANK ENTRY IN THIS FIELD
    END-IF.
    EXIT.

5300-ACTION-E.
*>
*>  ACTION E-ND :
*>
    IF UNSAVED-CHANGES THEN                             *> ARE THERE UNSAVED CHANGES?
        PERFORM 8010-WARNING-UNSAVED-CHANGES            *> YES, WARN THE USER AND DO NOT END
    ELSE
        PERFORM 5100-ACTION-C                           *> NO, CLEAR THE RECORD AND STATUS FLAGS
        MOVE 9999 TO NC-FSEQ-NEXT                       *> TELL THE FSM TO EXIT BACK TO CALLER
    END-IF.
    EXIT.

5400-ACTION-S.
*>
*>  ACTION S-AVE :
*>
    IF DEFINED-RECORD THEN                              *> DO WE HAVE A RECORD?
        IF NEW-RECORD THEN                              *> YES, BUT IS IT A NEW RECORD (TO BE ADDED?)
            PERFORM 5410-ADD-RECORD                     *> YES, THEN GO ADD THIS RECORD
            MOVE "REF NOW ON FILE" TO FLD-REF-INFO      *> UPDATE REF INFO
        ELSE
            PERFORM 5420-UPDATE-RECORD                  *> NO, GO UPDATE EXISTING RECORD
        END-IF
    ELSE
        MOVE "No record to save." TO NC-MSGBUF          *> THERE IS NO RECORD TO S-AVE
        PERFORM NC-PUT-ERROR-OVERRIDE
    END-IF.
    EXIT.

5410-ADD-RECORD.
*>
*>  ADD A NEW RECORD
*>
    PERFORM 4000-MOVE-TO-RECORD.                        *> COPY FIELDS TO THE RECORD MENUREF-RECORD
    WRITE MENUREF-RECORD                                *> ADD IT
        INVALID KEY
            MOVE "Error: Adding a new menu record." TO NC-MSGBUF
            PERFORM NC-PUT-ERROR-OVERRIDE
        NOT INVALID KEY
            SET NEW-RECORD TO FALSE                     *> RECORD IS NO LONGER "NEW"
            SET UNSAVED-CHANGES TO FALSE                *> CHANGES HAVE BEEN SAVED
            ADD 1 TO WS-REF-COUNT                       *> INCREASE REF COUNT
            MOVE WS-REF-COUNT TO FLD-REFERENCED-COUNT   *> SHOW THIS ON SCREEN
            MOVE "A new record was added (saved)." TO NC-MSGBUF
            PERFORM NC-PUT-MESSAGE-OVERRIDE
            PERFORM 9030-FLUSH-FILES                    *> CLOSE AND REOPEN FILES TO FORCE WRITES (FOR BDB)
    END-WRITE.
    EXIT.

5420-UPDATE-RECORD.
*>
*>  UPDATE AN EXISTING RECORD :
*>
    PERFORM 4000-MOVE-TO-RECORD.                        *> COPY SCREEN FIELDS TO MENUREF-RECORD
    REWRITE MENUREF-RECORD                              *> UPDATE RECORD ON FILE
        INVALID KEY
            MOVE "Error: Updating the menu record." TO NC-MSGBUF
            PERFORM NC-PUT-ERROR-OVERRIDE
        NOT INVALID KEY
            SET UNSAVED-CHANGES TO FALSE                *> THERE ARE NO LONGER UNSAVED CHANGES
            MOVE "Your record was updated (saved)." TO NC-MSGBUF
            PERFORM NC-PUT-MESSAGE-OVERRIDE
            PERFORM 9030-FLUSH-FILES                    *> CLOSE AND REOPEN FILES TO FORCE WRITES (FOR BDB)
    END-REWRITE.
    EXIT.

5500-ACTION-D.
*>
*>  DELETE EXISTING RECORD :
*>
    IF DEFINED-RECORD AND NOT NEW-RECORD THEN           *> DO WE HAVE AN EXISTING RECORD?
        PERFORM 4000-MOVE-TO-KEY                        *> INITIALIZE AND CREATE KEY FIELDS
        DELETE MENUREF-FILE
            INVALID KEY
                MOVE "Unable to delete this record." TO NC-MSGBUF
                PERFORM NC-PUT-ERROR-OVERRIDE
            NOT INVALID KEY
                SUBTRACT 1 FROM WS-REF-COUNT            *> UPDATE REF'D COUNT
                PERFORM 5100-ACTION-C                   *> FORGET ALL WE KNOW ABOUT THIS RECORD, & CLEAR FIELDS
                MOVE "The record was deleted." TO NC-MSGBUF
                PERFORM NC-PUT-MESSAGE-OVERRIDE
                PERFORM 9030-FLUSH-FILES                *> FORCE WRITES TO DISK (FOR BDB)
        END-DELETE
    ELSE
        MOVE "The current record has not been saved." TO NC-MSGBUF
        PERFORM NC-PUT-ERROR-OVERRIDE
    END-IF.
    EXIT.

5600-ACTION-F.
*>
*>  ACTION F-IND :
*>
    IF UNSAVED-CHANGES THEN                             *> DO WE HAVE UNSAVED CHANGES?
        PERFORM 8010-WARNING-UNSAVED-CHANGES            *> YES, THEN GIVE WARNINGS INSTEAD
    ELSE
        PERFORM 5605-ACTION-F-START                     *> START THE SEARCH
    END-IF.
    EXIT.

5605-ACTION-F-START.
*>
*>  PREPARE FOR THE FSEQ-FIND STATE :
*>
    MOVE "Enter search value + CR, or cursor up/down to browse." TO NC-MSGBUF
    PERFORM NC-PUT-MESSAGE-OVERRIDE                     *> TELL USER WHAT TO DO
    MOVE FSEQ-FIND TO NC-FSEQ-NEXT                      *> TELL FSM TO GO TO THIS STATE
    MOVE FNO-MENU-NAME TO NC-FSEQ-FIELD-NO(NC-FSEQ-NEXT) *> GO TO THE FLD-MENU-NAME FIELD FOR SEARCH VALUES
    MOVE 'N' TO NC-FDESC-NOT-BLANK(FNO-MENU-NAME)       *> ALLOW A BLANK VALUE TO BE ENTERED
    EXIT.

5610-ACTION-F-CONTD.
*>
*>  CONTROL PASSES HERE WHEN IN THE FSEQ-FIND STATE UPON FIELD EXIT EVENTS :
*>
    PERFORM 1010-RECORD-INIT.                           *> CLEAR RECORD STATUS FLAGS
    PERFORM 4000-MOVE-TO-KEY.                           *> INITIALIZE A MENU REF KEY
    IF NC-FIELD-EXIT-CU THEN                            *> CURSOR UP?
        PERFORM 5620-LT-SEARCH                          *> YES, DO A LESS THAN SEARCH
    END-IF.
    IF NC-FIELD-EXIT-CR OR NC-FIELD-EXIT-TAB THEN       *> ENTER OR TAB?
        PERFORM 5630-GE-SEARCH                          *> YES, DO A >= SEARCH
    END-IF.
    IF NC-FIELD-EXIT-CD THEN                            *> CURSOR DOWN?
        PERFORM 5640-GT-SEARCH                          *> YES, DO A GREATER THAN SEARCH
    END-IF.
    EXIT.

5620-LT-SEARCH.
*>
*>  PERFORM A LESS THAN SEARCH :
*>
    START MENUREF-FILE KEY IS < MREF-KEY
        INVALID KEY
            CONTINUE                                    *> RECORD STATUS FLAGS ARE ALREADY RESET
        NOT INVALID KEY
            SET DEFINED-RECORD TO TRUE                  *> INDICATE POSSIBLE RECORD STATUS
    END-START.
    PERFORM 5650-NEXT-RECORD.                           *> PERFORM REMAINDER OF THIS TASK
    EXIT.

5630-GE-SEARCH.
*>
*>  PERFORM A GREATER THAN OR EQUAL SEARCH :
*>
    START MENUREF-FILE KEY IS >= MREF-KEY
        INVALID KEY
            CONTINUE                                    *> RECORD STATUS FLAGS REMAIN RESET
        NOT INVALID KEY
            SET DEFINED-RECORD TO TRUE                  *> INDICATE POSSIBLE RECORD STATUS
    END-START.
    PERFORM 5650-NEXT-RECORD.                           *> DO REST..
    EXIT.

5640-GT-SEARCH.
*>
*>  PERFORM A GREATER THAN SEARCH
*>
    START MENUREF-FILE KEY IS > MREF-KEY
        INVALID KEY
            CONTINUE                                    *> RECORD STATUS FLAGS REMAIN RESET
        NOT INVALID KEY
            SET DEFINED-RECORD TO TRUE                  *> INDICATE POSSIBLE RECORD STATUS
    END-START.
    PERFORM 5650-NEXT-RECORD.                           *> DO REST..
    EXIT.

5650-NEXT-RECORD.
*>
*>  COMPLETE THE RESULT OF A F-IND REQUEST
*>
    IF DEFINED-RECORD THEN                              *> WAS START SUCCESSUL?
        READ MENUREF-FILE NEXT RECORD                   *> YES, THEN RETREIVE THAT NEXT RECORD
            AT END 
                PERFORM 5100-ACTION-C                   *> NOPE, NO MORE RECORDS
            NOT AT END 
                PERFORM 5670-LOAD-RECORD                *> YES, DISPLAY AND SET STATUS FOR RECORD 
        END-READ
    ELSE
        PERFORM 5100-ACTION-C                           *> CLEAR ALL FIELDS AND RESET RECORD STATUS
    END-IF.
    EXIT.

5670-LOAD-RECORD.
*>
*>  A RECORD WAS READ. MOVE CONTENTS TO SCREEN FIELDS.
*>
    PERFORM 4100-MOVE-FROM-RECORD.                      *> COPY RECORD CONTENTS TO SCREEN FIELDS
    SET DEFINED-RECORD TO TRUE.                         *> NOTE THAT WE HAVE AN EXISTING RECORD
    SET UNSAVED-CHANGES TO FALSE.                       *> NO UNSAVED CHANGES AT THIS POINT
    SET NEW-RECORD TO FALSE.                            *> THIS IS NOT A NEW RECORD
    PERFORM 5700-LOOKUP-MENU.                           *> LOOKUP MATCHING MENU RECORD
    MOVE MNU-TITLE TO FLD-MENU-TITLE.                   *> UPDATE READ-ONLY FIELD
    MOVE "REF ON FILE" TO FLD-REF-INFO.                 *> UPDATE STATUS FIELD
    EXIT.

5700-LOOKUP-MENU.
*>
*>  GET MENU TITLE IF POSSIBLE
*>
    INITIALIZE MENU-RECORD.
    MOVE MREF-MENU-NAME TO MNU-MENU-NAME.
    READ MENU-FILE
        INVALID KEY
            INITIALIZE MENU-RECORD
            SET WS-HAVE-MENU-RECORD TO FALSE
        NOT INVALID KEY
            SET WS-HAVE-MENU-RECORD TO TRUE
    END-READ.
    EXIT.

5900-ACTION-EDIT.
*>
*>  A USER WANTS TO VISIT A FIELD FOR EDITING :
*>
    IF NOT DEFINED-RECORD THEN                          *> DO WE HAVE A RECORD TO EDIT?
        MOVE "There is no current record to edit" TO NC-MSGBUF
        PERFORM NC-PUT-ERROR-OVERRIDE                   *> NOPE, INFORM THE USER
    ELSE
        PERFORM 5910-ACTION-EDIT                        *> YES, LET'S DO IT
    END-IF.
    EXIT.

5910-ACTION-EDIT.
*>
*>  PREPARE THE FSM TO EDIT A FIELD (GO INTO STATE FSEQ-EDIT)
*>
    IF NC-FIELD-EDIT-TARGET NOT = 'Y' THEN              *> IS THIS AN EDITABLE FIELD?
        MOVE "You cannot edit that field." TO NC-MSGBUF
        PERFORM NC-PUT-ERROR-OVERRIDE
    ELSE
        MOVE FSEQ-EDIT TO NC-FSEQ-NEXT                          *> USE THIS STATE TO EDIT THE FIELD
        MOVE NC-FIELD-SEARCH TO NC-FSEQ-FIELD-NO(FSEQ-EDIT)     *> USE THIS FIELD IN THAT STATE
    END-IF.
    EXIT.

6100-UPDATE-SCREEN.
    PERFORM NC-DRAW-FIELDS.                             *> REDRAW THE CONTENTS OF ALL FIELDS
    EXIT.

6150-VERIFY-MENU-NAME.
*>
*>  VERIFY ROUTINE FOR THE FIELD FLD-MENU-NAME :
*>
    PERFORM 4000-MOVE-TO-KEY.                           *> LOAD KEY INTO MENUREF-RECORD
    READ MENUREF-FILE                                   *> SEARCH THE FILE FOR THIS KEY
        INVALID KEY
            PERFORM 5700-LOOKUP-MENU                    *> GET MATCHING MENU RECORD
            IF WS-HAVE-MENU-RECORD THEN
                MOVE 'Y' TO NC-FIELD-VERIFIED           *> THIS IS A VALID UNSAVED MENU REF
                MOVE MNU-TITLE TO FLD-MENU-TITLE        *> UPDATE READ-ONLY FIELD
                SET NEW-RECORD TO TRUE
                SET UNSAVED-CHANGES TO TRUE
                MOVE "NO REF YET" TO FLD-REF-INFO       *> UPDATE STATUS WINDOW
            ELSE
                MOVE SPACES TO FLD-MENU-TITLE
            END-IF
        NOT INVALID KEY
            PERFORM 5670-LOAD-RECORD                    *> LOAD THE RECORD FOUND ON FILE
            PERFORM 5700-LOOKUP-MENU                    *> GET MATCHING MENU RECORD
            MOVE MNU-TITLE TO FLD-MENU-TITLE            *> UPDATE READ-ONLY FIELD
            MOVE 'Y' TO NC-FIELD-VERIFIED               *> MARK THE VALUE AS VERIFIED OK
    END-READ.
    EXIT.

6600-COUNT-REFS.
    INITIALIZE MENUREF-RECORD.
    MOVE ZERO TO WS-REF-COUNT.
    START MENUREF-FILE KEY IS >= MREF-KEY
        INVALID KEY
            SET WS-EOF TO TRUE
        NOT INVALID KEY
            SET WS-EOF TO FALSE
            PERFORM 6610-DO-COUNT-REFS
    END-START.
    EXIT.

6610-DO-COUNT-REFS.
    PERFORM UNTIL WS-EOF
        READ MENUREF-FILE NEXT RECORD
            AT END
                SET WS-EOF TO TRUE
            NOT AT END
                ADD 1 TO WS-REF-COUNT
        END-READ
    END-PERFORM.
    MOVE WS-REF-COUNT TO FLD-REFERENCED-COUNT.
    EXIT.

7000-ACTION-EVENT.
*>
*>  ACTION COMMAND DISPATCHER :
*>
    EVALUATE NC-FIELD-ACTION
        WHEN SPACES
            PERFORM 5900-ACTION-EDIT                    *> REQUEST EDIT OF A FIELD #
        WHEN "S"
            PERFORM 5400-ACTION-S                       *> S-AVE
        WHEN "E"
            PERFORM 5300-ACTION-E                       *> E-ND
        WHEN "N"    
            PERFORM 5200-ACTION-N                       *> N-EW
        WHEN "D"
            PERFORM 5500-ACTION-D                       *> D-ELETE
        WHEN "C"
            PERFORM 5100-ACTION-C                       *> C-LEAR
        WHEN "F"
            PERFORM 5600-ACTION-F                       *> F-IND
        WHEN OTHER
            PERFORM 8000-ERROR-ACTION                   *> ERROR
    END-EVALUATE.
    MOVE SPACES TO NC-FIELD-ACTION.
    EXIT.

7500-FIELD-EVENT.
*>
*>  THIS IS INVOKED FOR EVERY FIELD EXIT EVENT :
*>
    EVALUATE NC-FSEQ-STATE
        WHEN FNO-MENU-NAME                              *> END OF DATA ENTRY?
            SET DEFINED-RECORD TO TRUE                  *> WE HAVE A DEFINED RECORD
            SET NEW-RECORD TO TRUE                      *> IT IS A NEW RECORD
            SET UNSAVED-CHANGES TO TRUE                 *> MARK HAS HAVING UNSAVED CHANGES
        WHEN FSEQ-FIND                                  *> IN THE FIND STATE?
            PERFORM 5610-ACTION-F-CONTD                 *> CONTINUE WITH FIND OPERATIONS
            IF NC-FIELD-EXIT-CD OR NC-FIELD-EXIT-CU THEN
                MOVE FSEQ-FIND TO NC-FSEQ-NEXT          *> STAY IN FIND STATE FOR CURSOR UP/DOWN EVENTS
            END-IF
        WHEN OTHER
            IF NC-FIELD-NUMBER = FNO-ACTION-FIELD       *> DOES IT INVOLVE THE ACTION FIELD?
                PERFORM 7000-ACTION-EVENT               *> YES, DISPATCH ACTIONS
            END-IF
    END-EVALUATE.
    EXIT.
    
7600-MOUSE-EVENT.
*>
*>  MOUSE EVENTS GO HERE
*>
    PERFORM 5900-ACTION-EDIT.                           *> MOUSE EVENT CHOOSES A FIELD TO EDIT
    EXIT.

7700-CHANGE-EVENT.
*>
*>  CHANGE EVENTS GO HERE :
*>
    EXIT.

7800-STATE-CHANGE-EVENT.
*>
*>  STATE CHANGE CALLBACK
*>
    IF NC-FSEQ-STATE = FNO-MENU-NAME THEN
        SET NC-FDESC-MENU-PTR(FNO-MENU-NAME) TO ADDRESS OF MENU-MENUS       *> LIST OF DEFINED MENUS
    ELSE    
        SET NC-FDESC-MENU-PTR(FNO-MENU-NAME) TO ADDRESS OF MENU-MENU-REFS   *> LIST OF REFERENCES WE HAVE SAVED
    END-IF.
    PERFORM 6100-UPDATE-SCREEN.                         *> THIS IS PROBABLY UNNECESSARY FOR THIS SCREEN
    EXIT.

8000-ERROR-ACTION.
*>
*>  DISPLAY VALID ACTION RESPONSES AS AN ERROR MESSAGE :
*>
    SET NC-MSG-TEXT TO ADDRESS OF INF-ACTION-FIELD.     *> POINT TO ACTION-FIELD'S INFO TEXT
    MOVE LENGTH OF INF-ACTION-FIELD TO NC-MSG-LENGTH.   *> SET THE LENGTH OF THE MESSSAGE
    PERFORM NC-ERROR-MESSAGE-OVERRIDE.                  *> ISSUE THE MESSAGE
    EXIT.

8010-WARNING-UNSAVED-CHANGES.
    MOVE "Warning: You have unsaved changes (Save or Cancel)." TO NC-MSGBUF.
    PERFORM NC-PUT-ERROR-OVERRIDE.
    EXIT.

8020-MUST-SAVE.
    MOVE "You must save your changes before using I=Items." TO NC-MSGBUF.
    PERFORM NC-PUT-ERROR-OVERRIDE.
    EXIT.

9000-FINALIZE.
    PERFORM 9020-CLOSE-FILES.                           *> CLOSE ALL FILES
    PERFORM NC-CLEAR.                                   *> CLEAR SCREEN
    PERFORM NC-FINALIZE.
    EXIT.

9010-OPEN-FILES.
    OPEN I-O MENUREF-FILE.                              *> OPEN FILE FOR READ/UPDATE
    OPEN INPUT MENU-FILE.                               *> MENU DEFINITIONS (NEED THIS FOR TITLE)
    EXIT.
    
9020-CLOSE-FILES.
    CLOSE MENUREF-FILE MENU-FILE.
    EXIT.

9030-FLUSH-FILES.
*>
*>  THE BERKELEY DB SEEMS TO KEEP UNSAVED CHANGES IN MEMORY. THIS CREATES A PROBLEM
*>  FOR OTHER FILE OPENS ON THE SAME FILE, SINCE THEY DO NOT SEE THE NEWLY WRITTEN
*>  CHANGES. TO FORCE A FLUSH TO DISK, WE CLOSE AND REOPEN THE FILES.
*>
    PERFORM 9020-CLOSE-FILES.
    PERFORM 9010-OPEN-FILES.
    EXIT.

NC-FIELD-EVENT.
    PERFORM 7500-FIELD-EVENT.
    EXIT.

NC-VERIFY-EVENT.
    IF NC-FIELD-NUMBER = FNO-MENU-NAME THEN
        PERFORM 6150-VERIFY-MENU-NAME                   *> VERIFY THE FLD-MENU-NAME CONTENTS
    END-IF.
    EXIT.
    
NC-CHANGE-EVENT.
    PERFORM 7700-CHANGE-EVENT.
    EXIT.

NC-MOUSE-EVENT.
    PERFORM 7600-MOUSE-EVENT.
    EXIT.

NC-STATE-CHANGE-EVENT.
    PERFORM 7800-STATE-CHANGE-EVENT.
    EXIT.

NC-FKEY-EVENT.
    EXIT.                                               *> NO FKEYS PROCESSED HERE

    COPY COBCURSQ.                                      *> COBCURSES SCREEN SUPPORT ROUTINES

END PROGRAM SD002085.
