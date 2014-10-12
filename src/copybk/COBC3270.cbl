      *>***********************************************
      *>     COBCURSES       COBOL NCURSES INTERFACE
      *>     WARREN W. GAY   ve3wwg@cogeco.ca
      *>
      *> 3270 MODE SUPPORT
      *>***********************************************

      *>
      *> NC-INIT-3270 :
      *>     INITIALIZE STORAGE FOR NCURSES BINDING
      *>     FOR 3270 MODE.
      *>
        NC-INIT-3270.
            CALL "COBCURSES-INIT-3270" USING
                COPY COBCPARMS..
            EXIT.

      *>
      *> NC-3270-STATE-MACHINE :
      *>     (FOR 3270 MODE USE ONLY)
      *>     ASK FOR INPUT FROM ANY/ALL FIELDS, UNTIL THE
      *>     USER PRESSES "ENTER" OR A PF KEY.
      *>
        NC-3270-STATE-MACHINE.
            CALL "COBCURSES-3270-STATE-MACHINE" USING
                COPY COBCPARMS..
            IF NOT NC-FIELD-MOUSE-EVENT THEN
                PERFORM NC-FIELD-EVENT
            END-IF.
            EXIT.

      *>
      *> NC-3270-VERIFY-FIELD :
      *>
      *> INPUTS :
      *>     NC-FIELD-NUMBER
      *>
      *> RETURNS :
      *>     RETURN-CODE = 0     WHEN FIELD VALIDATES OK
      *>     NC-FIELD-NUMBER = 0 WHEN FIELD VALIDATES OK
      *>
        NC-3270-VERIFY-FIELD.
            CALL "COBCURSES-3270-VERIFY-FIELD" USING
                COPY COBCPARMS..
            EXIT.

	COPY NULLEVENTS.

      *>***********************************************
      *>     END COBC3270
      *>***********************************************
