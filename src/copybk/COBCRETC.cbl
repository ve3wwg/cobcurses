      *> RETURN CODES FOR THE COBCURSES PACKAGE :
      *> $Id: COBCRETC.cbl,v 1.4 2007/10/24 03:37:40 ve3wwg Exp $
      *>
      *> Warren W. Gay VE3WWG
      *> Wed Jul 18 10:36:19 2007
      *>
        01  NC-RETURN-CODES.
      *>        Successful :
            10  NC-RET-OK               PIC S9(9) VALUE 0.
      *>        Failed :
            10  NC-RET-FAILED           PIC S9(9) VALUE 1.
      *>        Was already open/was already not-open
            10  NC-RET-OPEN             PIC S9(9) VALUE 2.
      *>        Not supported :
            10  NC-RET-NSUPPORT         PIC S9(9) VALUE 3.
      *>        Results were truncated
            10  NC-RET-TRUNCATED        PIC S9(9) VALUE 4.
      *>        Not Found
            10  NC-RET-NOTFOUND         PIC S9(9) VALUE 5.
      *>        Bad parameter in call
            10  NC-RET-BADPARM          PIC S9(9) VALUE 6.
      *>        End was reached (no data returned)
            10  NC-RET-END              PIC S9(9) VALUE 7.
      *>        Resources exhausted for requested function
            10  NC-RET-RESOURCE         PIC S9(9) VALUE 8.

      *>
      *> END OF COBCRETC
      *>
