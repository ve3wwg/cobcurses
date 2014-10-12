      *>***********************************************
      *>     COBCURSES       COBOL NCURSES INTERFACE
      *>     WARREN W. GAY   ve3wwg@cogeco.ca
      *>***********************************************
      *>
      *> OPEN/CLOSE SUPPORT
      *>
      *>***********************************************

      *>
      *> NC-OPEN :
      *>     OPEN TERMINAL I/O
      *> RETURN-CODE:
      *>     0 = OK
      *>     1 = FAILED
      *>     2 = ALREADY OPEN
      *>
        NC-OPEN.
            CALL "libcobcurses" USING NC-COBCURSES.
            CALL "COBCURSES-OPEN" USING NC-COBCURSES.
            EXIT.

      *>
      *> NC-CLOSE :
      *>     CLOSE TERMINAL I/O
      *> RETURN-CODE:
      *>     0 = OK
      *>     1 = FAILED
      *>     2 = WAS NOT OPEN
      *>
        NC-CLOSE.
            CALL "COBCURSES-CLOSE".
            EXIT.

      *>***********************************************
      *>     END COBCURSP.cbl
      *>***********************************************

