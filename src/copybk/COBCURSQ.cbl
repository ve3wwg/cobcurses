      *>***********************************************
      *>     COBCURSES       COBOL NCURSES INTERFACE
      *>     WARREN W. GAY   ve3wwg@cogeco.ca
      *>
      *> SCREEN SUPPORT (EXCLUDING OPEN/CLOSE)
      *>***********************************************

      *>
      *> NC-INIT :
      *>     INITIALIZE STORAGE FOR NCURSES BINDING, IN
      *>     "ACTION" MODE :
      *>
        NC-INIT.
            CALL "COBCURSES-INIT" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-FINALIZE : 
      *>     RESTORE SOME TEMPORARILY CHANGED ITEMS FOR THE
      *>     PRESENT SCREEN.
      *>
        NC-FINALIZE.
            MOVE NC-CAP-COLOUR-SAVED TO NC-CAP-COLOUR.
            EXIT.

      *>
      *> NC-CLEAR :
      *>     CLEAR SCREEN.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-CLEAR.
            CALL "COBCURSES-CLEAR" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-CLEAR-TO-END-LINE :
      *>     CLEAR TO END OF LINE.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-CLEAR-TO-END-LINE.
            CALL "COBCURSES-COBCURSES-CLEAR-TO-END-LINE" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-CLEAR-TO-BOTTOM :
      *>     CLEAR TO BOTTOM OF SCREEN.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-CLEAR-TO-BOTTOM.
            CALL "COBCURSES-COBCURSES-CLEAR-TO-BOTTOM" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-COLOUR-PAIR :
      *>     CREATE A COLOUR ATTRIBUTE VALUE FROM TWO
      *>     CHOSEN COLOURS (FOREGROUND & BACKGROUND)
      *>
      *> INPUTS :
      *>     NC-PAIR-NUMBER (1 TO NC-COLOUR-PAIRS)
      *>     NC-FOREGROUND-COLOUR
      *>     NC-BACKGROUND-COLOUR
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-INIT-COLOUR-PAIR.
            CALL "COBCURSES-INIT-COLOOUR-PAIR" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-SET-COLOUR :
      *>     SET TO USE THE COLOUR PAIR SPECIFIED.
      *>
      *> INPUTS :
      *>     NC-PAIR-NUMBER
      *>
        NC-SET-COLOUR.
            CALL "COBCURSES-SET-COLOUR" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-SET-ATTRIBUTE :
      *>     SET NCURSES TO DRAW WITH THE NEW ATTRIBUTE.
      *>
      *> INPUTS :
      *>     NC-ATTRIBUTE
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> NOTES :
      *>     THIS API CALL ONLY CHANGES THE ATTRIBUTES
      *>     WITHOUT CHANGING THE COLOUR PAIR USED.
      *>
        NC-SET-ATTRIBUTE.
            CALL "COBCURSES-SET-ATTRIBUTE" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-SETATTR-* :
      *>     CONVENIENCE ROUTINES FOR SETTING ATTRIBUTES
      *>
      *> NO INPUTS -- ATTRIBUTE CHOSEN BY ROUTINE NAME.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-SETATTR-NORMAL.
            MOVE NC-ATTR-NORMAL TO NC-ATTRIBUTE
            PERFORM NC-SET-ATTRIBUTE.
            EXIT.
            
        NC-SETATTR-STANDOUT.
            MOVE NC-ATTR-STANDOUT TO NC-ATTRIBUTE
            PERFORM NC-SET-ATTRIBUTE.
            EXIT.

        NC-SETATTR-UNDERLINE.
            MOVE NC-ATTR-UNDERLINE TO NC-ATTRIBUTE
            PERFORM NC-SET-ATTRIBUTE.
            EXIT.

        NC-SETATTR-REVERSE.
            MOVE NC-ATTR-REVERSE TO NC-ATTRIBUTE
            PERFORM NC-SET-ATTRIBUTE.
            EXIT.

        NC-SETATTR-BLINK.
            MOVE NC-ATTR-BLINK TO NC-ATTRIBUTE
            PERFORM NC-SET-ATTRIBUTE.
            EXIT.

        NC-SETATTR-DIM.
            MOVE NC-ATTR-DIM TO NC-ATTRIBUTE
            PERFORM NC-SET-ATTRIBUTE.
            EXIT.

        NC-SETATTR-BOLD.
            MOVE NC-ATTR-BOLD TO NC-ATTRIBUTE
            PERFORM NC-SET-ATTRIBUTE.
            EXIT.

      *>
      *> NC-MOVE :
      *>     MOVE CURSOR.
      *>
      *> INPUT :
      *>     NC-POSITION-DATA.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-MOVE.
            CALL "COBCURSES-MOVE" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-ADDCH :
      *>     WRITE CHARACTER TO TERMINAL.
      *>
      *> INPUT:
      *>     NC-CHAR.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-ADDCH.
            CALL "COBCURSES-ADDCH" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-ADDSTR :
      *>     WRITE TEXT TO TERMINAL.
      *>
      *> INPUT :
      *>     NC-STRING-DATA :
      *>        NC-STR-DATA
      *>        NC-STR-LENGTH
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-ADDSTR.
            CALL "COBCURSES-ADDSTR" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-MVADDSTR :
      *>     MOVE TO Y, X AND WRITE TEXT TO TERMINAL
      *>
      *> INPUT :
      *>     NC-POSITION-DATA.
      *>     NC-STRING-DATA.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-MVADDSTR.
            PERFORM NC-MOVE.
            PERFORM NC-ADDSTR.
            EXIT.

      *>
      *> NC-GETCH :
      *>     GET KEY CODE FROM TERMINAL.
      *> RETURNS :
      *>     NC-KEY-DATA
      *> RETURN-CODE:
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-GETCH.
            CALL "COBCURSES-GETCH" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-DRAW-BOX :
      *>     DRAW A BOX ON THE SCREEN
      *>
      *> INPUTS :
      *>     NC-BOX-TOP-LINE
      *>     NC-BOX-LEFT-COLUMN
      *>     NC-BOX-BOTTOM-LINE
      *>     NC-BOX-BOTTOM-COLUMN
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-DRAW-BOX.
            CALL "COBCURSES-DRAW-BOX" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-GET-TEXT :
      *>     GET TEXT FROM A WINDOWED FIELD.
      *>
      *> INPUT :
      *>     NC-FIELD-NUMBER.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> EVENT CALLBACKS :
      *>
      *>		1. NC-GET-TEXT INTERNALS GET CALLED.
      *>		2. IF MOUSE EVENT, PERFORM NC-MOUSE-EVENT 
      *>	           ELSE NC-FIELD-EVENT
      *>         3. PERFORM NC-VERIFY-EVENT IF REQUIRED BY FIELD
      *>         4. PERFORM NC-CHANGE-EVENT IF FIELD HAS CHANGED
      *>         5. REPEAT STEPS 1-4 UNTIL NC-FIELD-VERIFIED = 'Y'
      *>            OR AN ESCAPE FROM THE FIELD WAS EXECUTED
      *>
        NC-GET-TEXT.
            PERFORM NC-UPDATE-TITLE.
            INITIALIZE NC-FIELD.
            PERFORM NC-SELECT-FIELD.
            MOVE NC-FDESC-CLEAR(NC-FIELD-NUMBER) TO NC-FIELD-CLEAR.
            MOVE NC-FDESC-UPPERCASE(NC-FIELD-NUMBER)
                TO NC-FIELD-UPPERCASE.
            MOVE NC-FDESC-MASK(NC-FIELD-NUMBER) TO NC-FIELD-MASK.
            MOVE NC-FDESC-NOT-BLANK(NC-FIELD-NUMBER) 
                TO NC-FIELD-NOT-BLANK.

            MOVE NC-FDESC-SIGNED(NC-FIELD-NUMBER) TO NC-FIELD-SIGNED.
            MOVE NC-FDESC-DIGITS(NC-FIELD-NUMBER) TO NC-FIELD-DIGITS.
            MOVE NC-FDESC-DECPLACES(NC-FIELD-NUMBER) 
                TO NC-FIELD-DECPLACES.

            MOVE NC-FDESC-RESTRICT(NC-FIELD-NUMBER) TO NC-RESTRICTX.
            IF NC-RESTRICTX > 0 
                MOVE ADDRESS OF NC-RESTRICT-CHARSET(NC-RESTRICTX)
                    TO NC-FIELD-RESTRICT
            ELSE
                MOVE NULL TO NC-FIELD-RESTRICT
            END-IF.

      *>
      *>     ISSUE FIELD INFO IF CONFIGURED FOR IT
      *>
            IF NC-FIELD-WAIVE-INFO = 'Y'
                MOVE 'N' TO NC-FIELD-WAIVE-INFO
            ELSE
                IF NOT NC-FDESC-INFO(NC-FIELD-NUMBER) = NULL
                AND NC-FDESC-INFOLEN(NC-FIELD-NUMBER) > 0
                    SET NC-MSG-TEXT TO NC-FDESC-INFO(NC-FIELD-NUMBER)
    		    MOVE NC-FDESC-INFOLEN(NC-FIELD-NUMBER) 
                        TO NC-MSG-LENGTH
		    PERFORM NC-INFO-MESSAGE
                END-IF
            END-IF.

            IF NC-FDESC-YN(NC-FIELD-NUMBER) = 'Y'
                SET NC-FIELD-RESTRICT TO ADDRESS OF NC-YN
                MOVE 'Y' TO NC-FIELD-UPPERCASE
            END-IF.

            IF NC-FDESC-VERIFY(NC-FIELD-NUMBER) = 'Y'
                MOVE 'N' TO NC-FIELD-VERIFIED
            END-IF.

            PERFORM
              TEST AFTER
              UNTIL NC-FIELD-VERIFIED = 'Y'
      *>
      *>         GO GET INPUT FROM THE USER FOR THIS FIELD
      *>
                MOVE 0 TO NC-FIELD-X-POS
                PERFORM TEST AFTER UNTIL NOT NC-FIELD-EXIT-FKEY
                    PERFORM NC-GET-TEXT-X

                    IF NC-FIELD-EXIT-FKEY THEN
                        PERFORM NC-FKEY-EVENT
                    END-IF
                END-PERFORM

                IF NC-FIELD-MOUSE-EVENT THEN
                    PERFORM NC-FIND-MOUSE-FIELD
                    CALL "NC_TRACE_MSG" USING
                        "EVENT: NC-MOUSE-EVENT.  "
                    PERFORM NC-MOUSE-EVENT
                ELSE
                    CALL "NC_TRACE_MSG" USING
                        "EVENT: NC-FIELD-EVENT.  "
                    PERFORM NC-FIELD-EVENT
                    PERFORM INTERNAL-TRACE-STATE
      *>
      *>         DO NC-VERIFY-EVENT IF FIELD REQUIRES IT
      *>
                    IF NC-FDESC-VERIFY(NC-FIELD-NUMBER) = 'Y'
                        IF NOT ( NC-FIELD-EXIT-ESC
                              OR NC-FIELD-EXIT-DOT
                              OR NC-FIELD-EXIT-SLASH ) THEN
                            CALL "NC_TRACE_MSG" USING
                                "EVENT: NC-VERIFY-EVENT.  "
                            PERFORM NC-VERIFY-EVENT
                            PERFORM INTERNAL-TRACE-STATE
                        END-IF
                    END-IF
                END-IF

      *>
      *>         DO NC-CHANGED-EVENT IF DATA VALUE HAS CHANGED
      *>
                IF NC-FIELD-CHANGED
                    CALL "NC_TRACE_MSG" USING
                        "EVENT: NC-CHANGE-EVENT.  "
                    PERFORM NC-CHANGE-EVENT
                    PERFORM INTERNAL-TRACE-STATE
                END-IF

                IF NC-FDESC-VERIFY(NC-FIELD-NUMBER) NOT = 'Y'
                OR NC-FIELD-EXIT-ESC OR NC-FIELD-EXIT-DOT 
                OR NC-FIELD-EXIT-SLASH THEN
                    MOVE 'Y' TO NC-FIELD-VERIFIED
                END-IF

                IF NC-FIELD-VERIFIED = 'Y' THEN
                    CALL "NC_TRACE_MSG" USING
                        "NC-FIELD-VERIFIED = 'Y'.  "
                ELSE
                    CALL "NC_TRACE_MSG" USING
                        "NC-FIELD-VERIFIED = 'N'.  "
                END-IF
            END-PERFORM.

            CALL "NC_TRACE_MSG" USING
                "EXITING NC-GET-TEXT LOOP.  "
            EXIT.

      *>
      *> NC-GET-TEXT-X :
      *>     GET TEXT FROM A WINDOWED FIELD.
      *>
      *> INPUT :
      *>     NC-FIELD-DATA.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> NOTES :
      *>     1.  THIS ROUTINE IS NOT NORMALLY CALLED BY THE
      *>         PROGRAMMER (NC-GET-TEXT CALLS THIS). THIS IS
      *>         USED BY PROGRAMS LIKE THE SCREEN DESIGNER.
      *>
        NC-GET-TEXT-X.
            CALL "COBCURSES-GET-TEXT-X" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-PUT-TEXT :
      *>     PUT REFORMATED TEXT BACK TO A WINDOWED FIELD.
      *>
      *> INPUT :
      *>     NC-FIELD-DATA.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> COMMENTS:
      *>     THIS ROUTINE IS IDENTICAL TO NC-GET-TEXT 
      *>     EXCEPT THAT IT DOES NOT WAIT FOR ANY
      *>     INPUT (IT JUST PUTS THE DATA OUT INTO
      *>     THE FIELD'S SPECIFIED WINDOW). THIS ALLOWS
      *>     THE CALLER TO REFORMAT THE TEXT, AND THEN
      *>     CALL THIS ROUTINE AGAIN TO PUT THE FINAL
      *>     VALUE OUT TO THE SCREEN.
      *>
        NC-PUT-TEXT.
            CALL "COBCURSES-PUT-TEXT" USING
                COPY COBCPARMS..
            EXIT.
           
      *>
      *> NC-REFRESH :
      *>     WRITE ANY PENDING CHANGES OUT TO TERMINAL.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-REFRESH.
            CALL "COBCURSES-REFRESH" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-PUT-MESSAGE          - PUT AN INFO MESSAGE
      *> NC-PUT-MESSAGE-CR       - PUT MSG AND WAIT FOR CR
      *> NC-PUT-MESSAGE-OVERRIDE - AS ABOVE OVERRIDING NC-GET-TEXT
      *> NC-PUT-ERROR            - PUT AN ERROR MESSAGE
      *> NC-PUT-ERROR-CR         - PUT MSG AND WAIT FOR CR
      *> NC-PUT-ERROR-OVERRIDE   - AS ABOVE OVERRIDING NC-GET-TEXT
      *>
      *> INPUT :
      *>     NC-MSGBUF
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-PUT-MESSAGE.
            CALL "COBCURSES-PUT-MESSAGE" USING
                COPY COBCPARMS..
            EXIT.

        NC-PUT-MESSAGE-CR.
            CALL "COBCURSES-PUT-MESSAGE-CR" USING
                COPY COBCPARMS..
            EXIT.

        NC-PUT-MESSAGE-OVERRIDE.
            CALL "COBCURSES-PUT-MESSAGE-OVERRIDE" USING
                COPY COBCPARMS..
            EXIT.
                
        NC-PUT-ERROR.
            CALL "COBCURSES-PUT-ERROR" USING
                COPY COBCPARMS..
            EXIT.

        NC-PUT-ERROR-CR.
            CALL "COBCURSES-PUT-ERROR-CR" USING
                COPY COBCPARMS..
            EXIT.

        NC-PUT-ERROR-OVERRIDE.
            CALL "COBCURSES-PUT-ERROR-OVERRIDE" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-ERROR-MESSAGE-OVERRIDE :
      *> NC-ERROR-MESSAGE :
      *>     WRITE ERROR MESSAGE AT BOTTOM OF SCREEN
      *>
      *> INPUTS :
      *>     NC-MSG-TEXT
      *>     NC-MSG-LENGTH
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> OVERRIDE PREVENTS GET-TEXT-MSG FROM ISSUING
      *> AN INFO MESSAGE (ONCE).
      *>
        NC-ERROR-MESSAGE-OVERRIDE.
            CALL "COBCURSES-ERROR-MSG-OVERRIDE" USING
                COPY COBCPARMS..
            EXIT.

        NC-ERROR-MESSAGE.
            CALL "COBCURSES-ERROR-MESSAGE" USING
                COPY COBCPARMS.
            EXIT.

      *>
      *> NC-INFO-MESSAGE-CR :
      *>     WRITE INFO MESSAGE AT BOTTOM OF SCREEN
      *>     AND THEN WAIT FOR ANY KEY.
      *>
      *> INPUTS :
      *>     NC-MSG-TEXT
      *>     NC-MSG-LENGTH
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-INFO-MESSAGE-CR.
            CALL "COBCURSES-INFO-MESSAGE-CR" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-FIELD-STATE-MACHINE :
      *>     (FOR ACTION MODE USE ONLY)
      *>     ASK FOR INPUT AT ALL OF THE DEFINED FIELD SEQUENCES
      *>     UNTIL THE NEXT FIELD STATE BECOMES ZERO.
      *>
        NC-FIELD-STATE-MACHINE.
            MOVE ZERO TO NC-FSEQ-LAST.
            MOVE ZERO TO NC-FSEQ-NEXT.
            CALL "NC_TRACE_MSG" USING
                "STARTING NC-FIELD-STATE-MACHINE.  ".
            PERFORM WITH TEST BEFORE UNTIL NC-FSEQ-STATE <= 0
                MOVE NC-FSEQ-FIELD-NO(NC-FSEQ-STATE)
                    TO NC-FIELD-NUMBER
      *>
      *>         PERFORM STATE CHANGE CALLBACK
      *>
                CALL "NC_TRACE_MSG" USING
                    "BEFORE NC-STATE-CHANGE-EVENT:  "
                PERFORM INTERNAL-TRACE-STATE

                PERFORM NC-STATE-CHANGE-EVENT
      *>
      *>		RE-ESTABLISH THE FIELD NUMBER IN CASE THE CALLBACK
      *>         CHANGED THE STATE NUMBER.
      *>
                MOVE NC-FSEQ-FIELD-NO(NC-FSEQ-STATE)
                    TO NC-FIELD-NUMBER

                CALL "NC_TRACE_MSG" USING
                    "AFTER NC-STATE-CHANGE-EVENT:  "
                PERFORM INTERNAL-TRACE-STATE

      *>         NC-GET-TEXT CAN CALL NC-VERIFY :

                PERFORM NC-GET-TEXT

      *>         DETERMINE NEXT FIELD STATE :

                IF NC-FSEQ-NEXT = 0 AND NC-FIELD-EXIT-SLASH
                    MOVE NC-FSEQ-SLASH-TO(NC-FSEQ-STATE)
                        TO NC-FSEQ-NEXT
                END-IF

                IF NC-FSEQ-NEXT = 0 AND NC-FIELD-EXIT-ESC
                    MOVE NC-FSEQ-ESC-TO(NC-FSEQ-STATE)
                        TO NC-FSEQ-NEXT
                END-IF

                IF NC-FSEQ-NEXT = 0
                    IF NC-FIELD-FORWARD
                        MOVE NC-FSEQ-FWD-TO(NC-FSEQ-STATE) 
                            TO NC-FSEQ-NEXT
                    ELSE
                        MOVE NC-FSEQ-BCK-TO(NC-FSEQ-STATE)
                            TO NC-FSEQ-NEXT
                    END-IF
                END-IF

                IF NC-FSEQ-NEXT >= 9999
                    MOVE 0 TO NC-FSEQ-NEXT
                END-IF

                MOVE NC-FSEQ-STATE TO NC-FSEQ-LAST
                MOVE NC-FSEQ-NEXT TO NC-FSEQ-STATE
                MOVE ZERO TO NC-FSEQ-NEXT

            END-PERFORM.

            CALL "NC_TRACE_MSG" USING
                "EXIT NC-FIELD-STATE-MACHINE.  ".
            EXIT.

      *>
      *> DRAW SCREEN :
      *>     DRAW SCREEN BASED UPON THE NC-SCREEN-DEFINITION.
      *>
        NC-DRAW-SCREEN.
            CALL "COBCURSES-DRAW-SCREEN" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-CLEAR-FIELD :
      *>     CLEAR A FIELD'S CONTENTS TO BLANKS
      *>
      *> INPUT :
      *>     NC-FIELD-NUMBER
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-CLEAR-FIELD.
            CALL "COBCURSES-CLEAR-FIELD" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-CLEAR-FIELDS.
      *>     CLEARS ALL FIELD BUFFERS DEFINED IN 
      *>     NC-FIELD-DESCRIPTORS.
      *>
      *> INPUT :
      *>     NC-FIELD-DESCRIPTORS
      *>
        NC-CLEAR-FIELDS.
            CALL "COBCURSES-CLEAR-FIELDS" USING 
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-COUNT-CHANGES :
      *>     COUNT HOW MANY FIELDS HAVE CHANGED VALUES
      *> OUTPUTS :
      *>     NC-FIELD-CHANGES CONTAINS THE COUNT
      *>
        NC-COUNT-CHANGES.
            CALL "COBCURSES-COUNT-CHANGES" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-DRAW-FIELDS :
      *>     DISPLAY ALL FIELD CONTENT ON THE SCREEN.
      *>
        NC-DRAW-FIELDS.
            CALL "COBCURSES-DRAW-FIELDS" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-UPDATE-TITLE :
      *>     UPDATE TITLE LINE WITH NEW DATE/TIME INFO.
      *>
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> NOTES :
      *>     NC-SCREEN-TITLEX MUST BE > 0 TO CAUSE UPDATE
      *>     NC-SCREEN-TITLEX IS RESET TO ZERO AFTER AN
      *>         UPDATE IF THERE IS NO REQUEST FOR DATE
      *>         OR TIME.
      *>     CALLED BY NC-DRAW-SCREEN.
      *>
        NC-UPDATE-TITLE.
            CALL "COBCURSES-UPDATE-TITLE" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-SET-MOUSE-CLICK :
      *>     SET THE MOUSE CLICK INTERVAL (MILLISECS)
      *>
      *> INPUTS :
      *>     NC-MOUSE-CLICK-MS
      *>
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>     3 - NOT SUPPORTED (NO MOUSE SUPPORT)
      *>
      *> NOTES :
      *>     CODE 3 MEANS THAT THERE IS NO MOUSE SUPPORT
      *>     COMPILED IN.
      *>
        NC-SET-MOUSE-CLICK.
            CALL "COBCURSES-SET-MOUSE-CLICK" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-FIND-MOUSE-FIELD :
      *>
      *>     LOCATES THE FIELD NUMBER FROM THE MOUSE COORDINATES
      *>
      *> INPUT :
      *>     NC-MOUSE-DATA
      *>     NC-FIELD-DESCRIPTIONS
      *>
      *> OUTPUT :
      *>     NC-FIELD-SEARCH
      *>         ZERO    - NO FIELD MATCHED
      *>         > 0     - FIELD NUMBER MATCHING MOUSE COORDS
      *>
        NC-FIND-MOUSE-FIELD.
            CALL "COBCURSES-FIND-MOUSE-FIELD" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-RESET-CHANGE :
      *>     RESET CHANGED INDICATOR FOR FIELD.
      *>
      *> INPUTS :
      *>     NC-FIELD-NUMBER
      *>
        NC-RESET-CHANGE.
            CALL "COBCURSES-RESET-CHANGE" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-RESET-CHANGES :
      *>     RESET CHANGED INDICATOR FOR ALL FIELDS.
      *>
        NC-RESET-CHANGES.
            CALL "COBCURSES-RESET-CHANGES" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-SET-MOUSE-MASK :
      *>     SET THE MOUSE EVENTS YOU WISH TO SUPPORT.
      *>
      *> INPUTS :
      *>     NC-MOUSE-MASKS.
      *>
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>     3 - NOT SUPPORTED (NO MOUSE SUPPORT)
      *>
      *> NOTES :
      *>     CODE 3 MEANS THAT THERE IS NO MOUSE SUPPORT
      *>     COMPILED IN.
      *>
        NC-SET-MOUSE-MASK.
            CALL "COBCURSES-MOUSE-MASK" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-GET-MOUSE-EVENT
      *>     GET MOUSE EVENT DATA
      *>
      *> INPUTS :
      *>     NONE.
      *>
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED (NO MOUSE DATA)
      *>     3 - NOT SUPPORTED (NO MOUSE SUPPORT)
      *>
      *> NOTES :
      *>     CODE 3 MEANS THAT THERE IS NO MOUSE SUPPORT
      *>     COMPILED IN.
      *>
        NC-GET-MOUSE-EVENT.
            CALL "COBCURSES-GET-MOUSE-EVENT" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-GET-TEXT-RAW :
      *>     GET TEXT FROM A WINDOWED FIELD.
      *>
      *> INPUT :
      *>     NC-FIELD-DATA.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> NOTES :
      *>     1.  THIS ROUTINE IS NOT NORMALLY CALLED BY THE
      *>         PROGRAMMER (NC-GET-TEXT/X CALLS THIS). THIS IS
      *>         USED BY PROGRAMS LIKE THE SCREEN DESIGNER.
      *>
        NC-GET-TEXT-RAW.
            CALL "COBCURSES-GET-TEXT-RAW" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-SELECT-FIELD :
      *>     SELECT A FIELD FOR OPERATION (INTERNAL)
      *>
      *> INPUT :
      *>     NC-FIELD-NUMBER
      *>
        NC-SELECT-FIELD.
            CALL "COBCURSES-SELECT-FIELD" USING
                COPY COBCPARMS..
            EXIT.

        NC-SELECT-FIELD-AND-OPTS.
            CALL "COBCURSES-SELECT-FIELD-AND-OPTS" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-PUT-TEXT-RAW :
      *>     LIKE NC-PUT-TEXT, BUT WITHOUT ANY CONNECTION
      *>     TO "DEFINED FIELDS".
      *>
      *> INPUTS :
      *>     NC-FIELD-Y
      *>     NC-FIELD-X
      *>     NC-TEMP-COLOUR-PAIR
      *>     NC-FIELD-LENGTH
      *>     NC-FIELD-WINLEN
      *>     NC-FIELD-BUFFER
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-PUT-TEXT-RAW.
            CALL "COBCURSES-PUT-TEXT-RAW" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-PUT-TEXT-RAW-NORMAL :
      *>     LIKE NC-PUT-TEXT-RAW EXCEPT THAT IT FALLS BACK
      *>     TO ATTRIBUTE A_NORMAL IF NO COLOUR PAIR.
      *>
      *> INPUTS :
      *>     NC-FIELD-Y
      *>     NC-FIELD-X
      *>     NC-EDIT-PAIR
      *>     NC-FIELD-LENGTH
      *>     NC-FIELD-WINLEN
      *>     NC-FIELD-BUFFER
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-PUT-TEXT-RAW-NORMAL.
            CALL "COBCURSES-PUT-TEXT-RAW-NORMAL" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-PAUSE :
      *>     WAIT FOR THE USER TO HIT ANY KEY.
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-PAUSE.
            CALL "COBCURSES-PAUSE" USING
                COPY COBCPARMS..
            EXIT.

      *>** INTERNAL *** DON'T USE IN APPLICATIONS ***

        NC-MSG-STRIP-BLANKS-INTERNAL.
            CALL "COBCURSES-MSG-STRIP-BLANKS-INT" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-ERROR-MESSAGE-CR :
      *>     WRITE ERROR MESSAGE AT BOTTOM OF SCREEN
      *>     AND WAIT FOR ANY KEY.
      *>
      *> INPUTS :
      *>     NC-MSG-TEXT
      *>     NC-MSG-LENGTH
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
        NC-ERROR-MESSAGE-CR.
            CALL "COBCURSES-ERROR-MESSAGE-CR" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-INFO-MESSAGE-OVERRIDE :
      *> NC-INFO-MESSAGE :
      *>     WRITE INFO MESSAGE AT BOTTOM OF SCREEN
      *>
      *> INPUTS :
      *>     NC-MSG-TEXT
      *>     NC-MSG-LENGTH
      *> RETURN-CODE :
      *>     0 - OK
      *>     1 - FAILED
      *>
      *> OVERRIDE PREVENTS NEXT GET-TEXT-MSG TO 
      *> REPLACE THE INFO MESSAGE (WORKS ONCE)
      *>
        NC-INFO-MESSAGE-OVERRIDE.
            CALL "COBCURSES-INFO-MESSAGE-OVERRIDE" USING
                COPY COBCPARMS..
            EXIT.

        NC-INFO-MESSAGE.
            CALL "COBCURSES-INFO-MESSAGE" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-EDIT-PATHNAME :
      *>     EDIT A PATHAME, SUBSITUTING ${...} WHERE FOUND,
      *>     WITH THE VALUES OF ENVIRONMENT VARIABLES.
      *>
      *> INPUTS :
      *>     NC-PATHNAME
      *> OUTPUTS :
      *>     NC-PATHNAME
      *>
        NC-EDIT-PATHNAME.
            CALL "libcobcurses" USING NC-COBCURSES.
            CALL "COBCURSES-EDIT-PATHNAME" USING NC-COBCURSES.
            EXIT.

      *>
      *> NC-SUSPEND :
      *>
      *>    SUSPEND CURSES TO ALLOW A SYSTEM CALL.
      *>
        NC-SUSPEND.
            CALL "NC_SUSPEND".
            EXIT.

      *>
      *> NC-RESUME :
      *>    
      *>    RESUME AFTER A NC-SUSPEND CALL.
      *>
        NC-RESUME.
            CALL "NC_RESUME".
            EXIT.

      *>
      *> INTERNAL- DO NOT CALL
      *>
        INTERNAL-TRACE-STATE.
            CALL "NC_TRACE_STATE" USING
                NC-FSEQ-STATE, NC-FSEQ-NEXT, NC-FIELD-NUMBER, 
                NC-FIELD-VERIFIED.
            EXIT.

      *>***********************************************
      *>     END COBCURSQ.cbl
      *>***********************************************
