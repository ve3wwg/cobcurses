        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST004.
      *> 
      *> THIS PROGRAM IS DESIGNED TO TEST OUT THE BASIC FEATURES
      *> OF THE COBOL ASSOCIATIVE ARRAY ROUTINES.
      *> 
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        COPY COBCRETC.

        01  WS-MISC.
            05  WS-EXPECTED-RET-CODE    PIC S9(9).
            05  WS-EXPECTED-COUNT       PIC 9(9).
            05  WS-PASSED-COUNT         PIC 9999 VALUE 0.
            05  WS-FAILED-COUNT         PIC 9999 VALUE 0.
            05  WS-DATA-CMP-FLAG        PIC X.
                88  WS-DATA-CMP-FAILED  VALUE 'N'.
                88  WS-DATA-CMP-MATCHED VALUE 'Y'.

        77  WS-INSTANCE-X               PIC 9999.
        77  WS-INSTANCE                 PIC 9(9).
        77  WS-HANDLE                   PIC 9(9).
        77  WS-ITER-HANDLE              PIC 9(9).

        77  WS-OUT-COUNT                PIC 9(9).

        77  WS-OUT-KEY-LENGTH           PIC 9999.
        77  WS-OUT-DATA-LENGTH          PIC 9999.

        01  WS-ASSOC-KEY-PARAM.
            05  WS-ASGN-IN-KEY-POINTER  POINTER.
            05  WS-ASGN-IN-KEY-LENGTH   PIC 9999.

        01  WS-ASSOC-DATA-PARAM.
            05  WS-IN-DATA-POINTER      POINTER.
            05  WS-IN-DATA-LENGTH       PIC 9999.

        01  WS-ASSOC-KEYS.
            05  WS-KEY-COMPANY          PIC 9.
            05  WS-KEY-ITEM-NO          PIC X(12).

        01  WS-ASSOC-DATA.
            05  WS-ITEM-DESCRIPTION     PIC X(40).
            05  WS-ITEM-COST            PIC $Z,ZZ9.99.
            05  WS-ITEM-LIST            PIC $Z,ZZ9.99.

        01  WS-ASSOC-DATA-SAVED.
            05  WS-SV-ITEM-DESCRIPTION  PIC X(40).
            05  WS-SV-ITEM-COST         PIC $Z,ZZ9.99.
            05  WS-SV-ITEM-LIST         PIC $Z,ZZ9.99.

        01  WS-TEST-DATA-AREA.
            05  WS-TEST-X               PIC 999 COMP.
            05  WS-TEST-N               PIC 999 COMP VALUE 10.
            05  WS-TEST-X-SAVED         PIC 999 COMP.
            05  WS-LOOKUP-FLAG          PIC X VALUE 'N'.
                88  WS-LOOKUP-FOUND     VALUE 'Y'.
            05  WS-TEST-DATA-ENTRY OCCURS 1 TO 20 TIMES.
                10  WS-TEST-KEY.
                    15  WS-T-KEY-COMPANY    PIC 9.
                    15  WS-T-KEY-ITEM-NO    PIC X(12).
                10  WS-TEST-DATA.
                    15  WS-T-ITEM-DESCRIPTION  PIC X(40).
                    15  WS-T-ITEM-COST      PIC $Z,ZZ9.99.
                    15  WS-T-ITEM-LIST      PIC $Z,ZZ9.99.

        01  WS-INSTANCES-INFO.
            05  FILLER OCCURS 23 TO 60 TIMES.
                10  WS-INSTANCE-ID-NO   PIC 9(9).
                10  FILLER OCCURS 1 TO 10 TIMES.
                    15  WS-HANDLE-ID-NO PIC 9(9).

        77  WS-FIRST-INSTANCE           PIC 9(9) VALUE 23.
        77  WS-ALLOC-INSTANCE-START     PIC 9(9) VALUE 50.
        77  WS-LAST-INSTANCE            PIC 9(9) VALUE 60.
        77  WS-X                        PIC 999 COMP.
        77  WS-GOT-5-FLAG               PIC X VALUE 'N'.
            88  WS-GOT-5                VALUE 'Y'.
        77  WS-INSTANCE-COUNT           PIC 9999 COMP VALUE 0.

            COPY COBCEXTRA.

        PROCEDURE DIVISION.
      *> 
      *> MAIN PROGRAM
      *> 
        MAIN-PROG.
            PERFORM 1000-INITIALIZE.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            STOP RUN.

      *> 
      *> INITIALIZATION
      *> 
        1000-INITIALIZE.
            PERFORM NC-EXTRA-INIT.
            PERFORM 1100-TEST-DATA-INIT.
            PERFORM 1200-INSTANCE-INIT.
            EXIT.

        1100-TEST-DATA-INIT.
            PERFORM 9900-INIT-DATA.
            EXIT.

        1200-INSTANCE-INIT.
      *>
      *>     INITIALIZE INSTANCES
      *>
            PERFORM VARYING WS-INSTANCE
              FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE >= WS-ALLOC-INSTANCE-START
                MOVE WS-INSTANCE TO WS-INSTANCE-ID-NO(WS-INSTANCE)
            END-PERFORM.
            PERFORM VARYING WS-INSTANCE
              FROM WS-ALLOC-INSTANCE-START BY 1
              UNTIL WS-INSTANCE > WS-LAST-INSTANCE
                CALL "COBCURSEX-ASSOC-INSTANCE"
                    USING BY REFERENCE
                        WS-INSTANCE-ID-NO(WS-INSTANCE)
                MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                PERFORM 6000-RETURN-CODE
                IF RETURN-CODE = NC-RET-OK THEN
                    DISPLAY WS-INSTANCE, ": ALLOCATED INSTANCE ",
                        WS-INSTANCE-ID-NO(WS-INSTANCE)
                    IF WS-INSTANCE-ID-NO(WS-INSTANCE) >= 100000 THEN
                        PERFORM 6100-PASSED
                    ELSE
                        PERFORM 6100-FAILED
                    END-IF
                END-IF
            END-PERFORM.
            EXIT.

      *> 
      *> MAIN PROCESSING LOOP
      *> 
        5000-PROCESS.
            PERFORM 5100-TEST-1.
            PERFORM 5200-TEST-2.
            PERFORM 5300-TEST-3.
            PERFORM 5400-TEST-4.
            PERFORM 5500-TEST-5.
            PERFORM 5600-TEST-6.
            PERFORM 5700-TEST-7.
            PERFORM 5800-TEST-8.
            PERFORM 5900-TEST-9.
            PERFORM 5950-TEST-10.
            EXIT.

        5100-TEST-1.
            DISPLAY " ".
            DISPLAY "TEST-1: COBCURSEX-ASSOC-COUNT PRIOR TO INIT.".

            MOVE 9000 TO WS-INSTANCE.
            DISPLAY "  TESTING INSTANCE ", WS-INSTANCE.

            MOVE NC-RET-NOTFOUND TO WS-EXPECTED-RET-CODE.
            MOVE NC-RET-OK TO WS-EXPECTED-COUNT.
            PERFORM 7000-COUNT-TEST.
            EXIT.
            
        5200-TEST-2.
            DISPLAY " ".
            DISPLAY "TEST-2: COBCURSEX-ASSOC-CLEAR PRIOR TO USE.".

            MOVE 9000 TO WS-INSTANCE.
            DISPLAY "  TESTING INSTANCE ", WS-INSTANCE.
            CALL "COBCURSEX-ASSOC-CLEAR" 
                USING WS-INSTANCE.

            MOVE NC-RET-NOTFOUND TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            MOVE 9999 TO WS-INSTANCE.
            DISPLAY "  TESTING INSTANCE ", WS-INSTANCE.
            CALL "COBCURSEX-ASSOC-CLEAR" 
                USING WS-INSTANCE.

            MOVE NC-RET-NOTFOUND TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.
            EXIT.

        5300-TEST-3.
            DISPLAY " ".
            DISPLAY "TEST-3: COBCURSEX-ASSOC-ASSIGN".
      *>
      *>     FIRST ROUND OF ASSIGNMENT TESTS
      *>
            DISPLAY "((( 1ST TIME ASSIGNS - CREATES )))".
            PERFORM VARYING WS-INSTANCE-X
              FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE-X > WS-LAST-INSTANCE
                MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                    TO WS-INSTANCE
                DISPLAY "  TESTING INSTANCE ", WS-INSTANCE
                PERFORM VARYING WS-TEST-X FROM 1 BY 1
                  UNTIL WS-TEST-X > WS-TEST-N
                    DISPLAY "  SUBTEST # ", WS-TEST-X
                    PERFORM 7000-ASSGN-FETCH-TEST
                    MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                    MOVE WS-TEST-X TO WS-EXPECTED-COUNT
                    PERFORM 7000-COUNT-TEST
                END-PERFORM
            END-PERFORM.
      *>
      *>     OVERWRITE TESTS
      *>
            DISPLAY "((( 2ND TIME ASSIGNS - OVERWRITES )))".
            PERFORM VARYING WS-INSTANCE-X
              FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE-X > WS-LAST-INSTANCE
                MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                    TO WS-INSTANCE
                DISPLAY "  TESTING INSTANCE ", WS-INSTANCE
                PERFORM VARYING WS-TEST-X FROM 1 BY 1
                UNTIL WS-TEST-X > WS-TEST-N
                    DISPLAY "  SUBTEST # ", WS-TEST-X
                    PERFORM 7000-ASSGN-FETCH-TEST
                    MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                    MOVE WS-TEST-N TO WS-EXPECTED-COUNT
                    PERFORM 7000-COUNT-TEST
                END-PERFORM
            END-PERFORM.
            EXIT.

        5400-TEST-4.
            DISPLAY " ".
            DISPLAY "TEST-4: COBCURSES-ASSOC-DELETE".
      *> 
      *>     DELETE THE 5TH TEST ITEM BY KEY IN EACH
      *>     INSTANCE.
      *> 
            PERFORM VARYING WS-INSTANCE-X
              FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE-X > WS-LAST-INSTANCE
                MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                    TO WS-INSTANCE
                DISPLAY "  TESTING INSTANCE ", WS-INSTANCE
                MOVE 5 TO WS-TEST-X
                PERFORM 6300-SETUP-KEYS
                MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                PERFORM 7100-DELETE-TEST
                MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                MOVE WS-TEST-N TO WS-EXPECTED-COUNT
                SUBTRACT 1 FROM WS-EXPECTED-COUNT
                PERFORM 7000-COUNT-TEST
            END-PERFORM.
      *>
      *>     NOW TRY TO DELETE IT AGAIN (IT SHOULDN'T EXIST)
      *>
            PERFORM VARYING WS-INSTANCE-X
              FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE-X > WS-LAST-INSTANCE
                MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                    TO WS-INSTANCE
                DISPLAY "  DELETING AGAIN, SHOULD RC=5.."
                DISPLAY "  TESTING INSTANCE ", WS-INSTANCE
                MOVE NC-RET-NOTFOUND TO WS-EXPECTED-RET-CODE
                MOVE 7 TO WS-EXPECTED-COUNT
                PERFORM 6200-RPT-KEYS
                PERFORM 7100-DELETE-TEST
                MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                MOVE WS-TEST-N TO WS-EXPECTED-COUNT
                SUBTRACT 1 FROM WS-EXPECTED-COUNT
                PERFORM 7000-COUNT-TEST
            END-PERFORM.
      *>
      *>     VERIFY THAT ALL OTHERS STILL EXIST (EXCEPT FOR
      *>     WS-TEST-X = 5, WHICH SHOULD BE MISSING)
      *>
            PERFORM VARYING WS-INSTANCE-X
              FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE-X > WS-LAST-INSTANCE
                MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                    TO WS-INSTANCE
                DISPLAY "  VERIFYING OTHER ENTRIES STILL EXIST.."
                DISPLAY "  TESTING INSTANCE ", WS-INSTANCE
                MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                PERFORM VARYING WS-TEST-X FROM 1 BY 1
                  UNTIL WS-TEST-X > WS-TEST-N
                    DISPLAY "  SUBTEST # ", WS-TEST-X
                    IF WS-TEST-X NOT = 5 THEN
                        MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE
                    ELSE
                        MOVE NC-RET-NOTFOUND TO WS-EXPECTED-RET-CODE
                    END-IF
                    PERFORM 7300-FETCH-TEST
                END-PERFORM
            END-PERFORM.
            EXIT.

        5500-TEST-5.
            DISPLAY " ".
            DISPLAY "TEST-5: COBCURSES-ASSOC-CLEAR".
      *>
      *>     TEST OUT COBCURSEX-ASSOC-CLEAR
      *>
            INITIALIZE WS-OUT-COUNT.

            MOVE WS-LAST-INSTANCE TO WS-INSTANCE-X.
            MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                TO WS-INSTANCE.

            DISPLAY "  CLEARING INSTANCE ", WS-INSTANCE
            CALL "COBCURSEX-ASSOC-CLEAR"
                USING WS-INSTANCE.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            DISPLAY "  COUNT FROM INSTANCE ", WS-INSTANCE
            MOVE NC-RET-NOTFOUND TO WS-EXPECTED-RET-CODE.
            MOVE ZERO TO WS-EXPECTED-COUNT.
            PERFORM 7000-COUNT-TEST.
            EXIT.

        5600-TEST-6.
            DISPLAY " ".
            DISPLAY "TEST-6: ITERATION TESTS".

            PERFORM VARYING WS-INSTANCE-X
            FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE-X >= WS-LAST-INSTANCE
                MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                    TO WS-INSTANCE
                DISPLAY "  ITERATING INSTANCE ", WS-INSTANCE
                PERFORM 5610-ITERATION-TEST
            END-PERFORM.
            EXIT.

        5610-ITERATION-TEST.
            DISPLAY "  ITERATE FIRST..".
            MOVE 'N' TO WS-GOT-5-FLAG.

      *>     THE VALUE OF WS-ITER-HANDLE SHOULD BE REPLACED
            MOVE 9999 TO WS-ITER-HANDLE

            CALL "COBCURSEX-ASSOC-FIRST" 
              USING
                WS-INSTANCE, BY REFERENCE WS-ITER-HANDLE.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            PERFORM UNTIL RETURN-CODE NOT = NC-RET-OK
                PERFORM 5620-ITER-FETCH-X
                PERFORM 5630-ITER-FETCH-KEY-X
                PERFORM 5640-ITER-FETCH-DATA-X

                DISPLAY "  ITERATE NEXT.."
                CALL "COBCURSEX-ASSOC-NEXT"
                  USING
                    WS-INSTANCE, BY REFERENCE WS-ITER-HANDLE
            END-PERFORM.

            IF WS-GOT-5 THEN
                DISPLAY "  WE GOT DATA FROM X=5, WHICH IS INCORRECT"
                DISPLAY "    BECAUSE IT WAS DELETED."
                PERFORM 6100-FAILED
            ELSE
                DISPLAY "  DATA FOR X=5 NEVER SHOWED UP, WHICH IS"
                DISPLAY "    CORRECT, BECAUSE IT WAS DELETED."
                PERFORM 6100-PASSED
            END-IF
            EXIT.

        5620-ITER-FETCH-X.
            DISPLAY "    RETURNED HANDLE=" WS-ITER-HANDLE.
            
            INITIALIZE WS-ASSOC-KEYS.
            INITIALIZE WS-ASSOC-DATA.

            CALL "COBCURSEX-ASSOC-FETCH-X"
                USING WS-INSTANCE, WS-ITER-HANDLE,
                    WS-ASSOC-KEY-PARAM,
                    WS-ASSOC-DATA-PARAM,
                    BY REFERENCE WS-OUT-KEY-LENGTH,
                    BY REFERENCE WS-OUT-DATA-LENGTH.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.
            IF RETURN-CODE = NC-RET-OK THEN
                IF WS-OUT-KEY-LENGTH = LENGTH OF WS-ASSOC-KEYS
                    DISPLAY "  RETURNED KEY LENGTH IS CORRECT."
                    PERFORM 6100-PASSED
                ELSE
                    DISPLAY "  *** RETURNED KEY LENGTH IS INCORRECT ***"
                    DISPLAY "  GOT KEY LENGTH   ", WS-OUT-KEY-LENGTH
                    DISPLAY "  SHOULD HAVE BEEN ",
                        LENGTH OF WS-ASSOC-KEYS
                    PERFORM 6100-FAILED
                END-IF

                IF WS-OUT-DATA-LENGTH = LENGTH OF WS-ASSOC-DATA THEN
                    DISPLAY "  RETURNED DATA LENGTH IS CORRECT."
                    PERFORM 6100-PASSED
                ELSE
                    DISPLAY "  *** RETURNED DATA LENGTH INCORRECT ***"
                    DISPLAY "  GOT DATA LENGTH  ", WS-OUT-DATA-LENGTH
                    DISPLAY "  SHOULD HAVE BEEN ",
                        LENGTH OF WS-ASSOC-DATA
                    PERFORM 6100-FAILED
                END-IF

                PERFORM 6200-RPT-KEY-DATA
                PERFORM 6500-LOCATE-KEY
                IF WS-LOOKUP-FOUND THEN
                    MOVE WS-TEST-X TO WS-TEST-X-SAVED
                    PERFORM 6100-PASSED
                    PERFORM 6300-CMP-DATA-X
                    IF WS-TEST-X = 5 THEN
                        SET WS-GOT-5 TO TRUE
                    END-IF
                ELSE
                    PERFORM 6100-FAILED
                END-IF
            END-IF.
            EXIT.

        5630-ITER-FETCH-KEY-X.
            DISPLAY "  ((( TEST KEY FETCH ONLY )))".
            INITIALIZE WS-ASSOC-KEYS.

            CALL "COBCURSEX-ASSOC-FETCH-KEY-X"
                USING WS-INSTANCE, WS-ITER-HANDLE,
                    WS-ASSOC-KEY-PARAM,
                    BY REFERENCE WS-OUT-KEY-LENGTH.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            IF RETURN-CODE = NC-RET-OK THEN
                IF WS-OUT-KEY-LENGTH = LENGTH OF WS-ASSOC-KEYS
                    DISPLAY "  RETURNED KEY LENGTH IS CORRECT."
                    PERFORM 6100-PASSED
                ELSE
                    DISPLAY "  *** RETURNED KEY LENGTH IS INCORRECT ***"
                    DISPLAY "  GOT KEY LENGTH   ", WS-OUT-KEY-LENGTH
                    DISPLAY "  SHOULD HAVE BEEN ",
                        LENGTH OF WS-ASSOC-KEYS
                    PERFORM 6100-FAILED
                END-IF

                PERFORM 6200-RPT-KEYS

                PERFORM 6500-LOCATE-KEY
                IF WS-LOOKUP-FOUND THEN
                    PERFORM 6100-PASSED
                    PERFORM 6300-CMP-KEY-X
                    IF WS-TEST-X = 5 THEN
                        SET WS-GOT-5 TO TRUE
                    END-IF
                ELSE
                    PERFORM 6100-FAILED
                END-IF

                DISPLAY "  CHECKING HANDLE:"
                IF WS-ITER-HANDLE
                 = WS-HANDLE-ID-NO(WS-INSTANCE-X,WS-TEST-X) THEN
                    DISPLAY "    HANDLE ID MATCHED ASSIGNED HANDLE."
                    PERFORM 6100-PASSED
                ELSE
                    DISPLAY "    HANDLE ID DOES NOT MATCH ASSIGNED."
                    PERFORM 6100-FAILED
                END-IF
            END-IF.
            EXIT.

        5640-ITER-FETCH-DATA-X.
            DISPLAY "  ((( TEST DATA FETCH ONLY )))".

            DISPLAY "  FETCHING INSTANCE=", WS-INSTANCE,
                    ", HANDLE=", WS-ITER-HANDLE.

            INITIALIZE WS-ASSOC-DATA.

            CALL "COBCURSEX-ASSOC-FETCH-DATA-X"
                USING WS-INSTANCE, WS-ITER-HANDLE,
                WS-ASSOC-DATA-PARAM,
                WS-OUT-DATA-LENGTH.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            IF RETURN-CODE = NC-RET-OK THEN
                IF WS-OUT-DATA-LENGTH = LENGTH OF WS-ASSOC-DATA
                    DISPLAY "  RETURNED DATA LENGTH IS CORRECT."
                    PERFORM 6100-PASSED
                ELSE
                    DISPLAY "  *** RETURNED DATA LENGTH INCORRECT ***"
                    DISPLAY "  GOT DATA LENGTH  ", WS-OUT-DATA-LENGTH
                    DISPLAY "  SHOULD HAVE BEEN ",
                        LENGTH OF WS-ASSOC-DATA
                    PERFORM 6100-FAILED
                END-IF

                PERFORM 6200-RPT-DATA
                PERFORM 6300-CMP-DATA-X
            END-IF.
            EXIT.

        5700-TEST-7.
            DISPLAY " ".
            DISPLAY "TEST-7: CLEAR TESTS".

            PERFORM VARYING WS-INSTANCE-X
            FROM WS-FIRST-INSTANCE BY 1
              UNTIL WS-INSTANCE-X >= WS-LAST-INSTANCE
                MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                    TO WS-INSTANCE
                DISPLAY "  CLEARINGING INSTANCE ", WS-INSTANCE
                PERFORM 5710-TEST-7-CLEAR
            END-PERFORM.
            EXIT.

        5710-TEST-7-CLEAR.
            DISPLAY "  CLEARING INSTANCE=", WS-INSTANCE.
            CALL "COBCURSEX-ASSOC-CLEAR"
                USING WS-INSTANCE.
            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.
            EXIT.

        5800-TEST-8.
            DISPLAY " ".
            DISPLAY "TEST-8: TEST FOR NO REMAINING INSTANCES".
            MOVE ZERO TO WS-INSTANCE-COUNT.
	    PERFORM VARYING WS-INSTANCE-X FROM 0 BY 1
	      UNTIL WS-INSTANCE-X > 100

                IF WS-INSTANCE-X >= WS-FIRST-INSTANCE
                AND WS-INSTANCE-X <= WS-LAST-INSTANCE
                    MOVE WS-INSTANCE-ID-NO(WS-INSTANCE-X)
                        TO WS-INSTANCE
                ELSE
                    MOVE WS-INSTANCE-X TO WS-INSTANCE
                END-IF

                INITIALIZE WS-OUT-COUNT

                CALL "COBCURSEX-ASSOC-COUNT"
                  USING 
                    WS-INSTANCE,
                    BY REFERENCE WS-OUT-COUNT

                IF WS-OUT-COUNT > 0 THEN
                    DISPLAY "  *** INSTANCE=", WS-INSTANCE,
                        " SHOULD NOT EXIST ***"
                    PERFORM 6100-FAILED
                    ADD 1 TO WS-INSTANCE-COUNT
                END-IF
	    END-PERFORM.
            IF WS-INSTANCE-COUNT = 0 THEN
                DISPLAY "  ALL INSTANCES ARE GONE AS EXPECTED."
                PERFORM 6100-PASSED
            END-IF
            EXIT.

        5900-TEST-9.
            DISPLAY " ".
            DISPLAY "TEST-9: TEST FOR NO REMAINING INSTANCES".
            DISPLAY "  CREATING AN ASSOCIATION TO DELETE..".

            MOVE 23 TO WS-INSTANCE.
            MOVE 5 TO WS-TEST-X.

            PERFORM 6300-SETUP-KEY-DATA.

            DISPLAY "  USING INSTANCE ", WS-INSTANCE

            CALL "COBCURSEX-ASSOC-ASSIGN"
                USING WS-INSTANCE, WS-ASSOC-KEY-PARAM,
                    WS-ASSOC-DATA-PARAM.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            IF RETURN-CODE = NC-RET-OK THEN
                PERFORM 5910-DELETE-TEST-01
      *>
      *>     PUT SOME GARBAGE OUT THERE FOR THE NEXT TEST..
      *>
                DISPLAY "  CREATING GARBAGE FOR THE NEXT TEST.."
                CALL "COBCURSEX-ASSOC-ASSIGN"
                    USING WS-INSTANCE, WS-ASSOC-KEY-PARAM,
                        WS-ASSOC-DATA-PARAM
                IF RETURN-CODE = NC-RET-OK THEN
                    PERFORM 6100-PASSED
                ELSE
                    PERFORM 6100-FAILED
                END-IF
            END-IF.
            EXIT.

        5910-DELETE-TEST-01.
      *>
      *>     GET THE HANDLE TO THE LAST ASSIGNED VALUE
      *>
            CALL "COBCURSEX-ASSOC-HANDLE" USING
                WS-INSTANCE, BY REFERENCE WS-HANDLE.
            IF RETURN-CODE = NC-RET-OK THEN
                DISPLAY "    ASSIGNED HANDLE= ", WS-HANDLE
                PERFORM 5920-DELETE-TEST-02
            ELSE
                PERFORM 6100-FAILED
            END-IF.
            EXIT.

        5920-DELETE-TEST-02.
      *>
      *>     NOW DELETE WS-INSTANCE, WS-OUT-HANDLE
      *>
            DISPLAY "  NOW DELETING HANDLE= ", WS-HANDLE.
            CALL "COBCURSEX-ASSOC-DELETE-X" USING
                WS-INSTANCE, WS-HANDLE.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.
      *>
      *>     CHECK WITH A COUNT
      *>
            DISPLAY "  NOW CHECKING COUNT..".
            CALL "COBCURSEX-ASSOC-COUNT"
              USING WS-INSTANCE, BY REFERENCE WS-OUT-COUNT.
            
            PERFORM 6000-RETURN-CODE.
            IF WS-OUT-COUNT = ZERO THEN
                DISPLAY "    COUNT=0 CONFIRMS DELETION."
                PERFORM 6100-PASSED
            ELSE
                DISPLAY "    COUNT= ", WS-OUT-COUNT,
                    " IS INCORRECT. SB ZERO."
                PERFORM 6100-FAILED
            END-IF.
            EXIT.

        5950-TEST-10.
            DISPLAY " ".
            DISPLAY "TEST-10: CLEARING ALL REMAINING INSTANCES".

            CALL "COBCURSEX-ASSOC-CLEAR-ALL".
            IF RETURN-CODE = NC-RET-OK THEN
                PERFORM 6100-PASSED
            ELSE
                PERFORM 6100-FAILED
            END-IF.

            DISPLAY "  REPEATING CLEAR ALL - TEST FOR ABORT.."
            CALL "COBCURSEX-ASSOC-CLEAR-ALL".
            IF RETURN-CODE = NC-RET-OK THEN
                PERFORM 6100-PASSED
            ELSE
                PERFORM 6100-FAILED
            END-IF.
            EXIT.

        6000-RETURN-CODE.
            DISPLAY "  RETURN-CODE= ", RETURN-CODE.
            IF RETURN-CODE NOT = WS-EXPECTED-RET-CODE THEN
                DISPLAY "  *** INCORRECT RETURN CODE ***"
                PERFORM 6100-FAILED
            ELSE
                DISPLAY "  CORRECT RETURN CODE."
                PERFORM 6100-PASSED
            END-IF.
            EXIT.

        6100-FAILED.
            DISPLAY "  *** FAILED ***".
            ADD 1 TO WS-FAILED-COUNT.
            EXIT.

        6100-PASSED.
            DISPLAY "  --- PASSED ---".
            ADD 1 TO WS-PASSED-COUNT.
            EXIT.

        6200-RPT-KEYS.
            DISPLAY "          INSTANCE: ", WS-INSTANCE.
            DISPLAY "     KEY-1-COMPANY: ", WS-KEY-COMPANY.
            DISPLAY "     KEY-2-ITEM-NO: '", WS-KEY-ITEM-NO, "'".
            EXIT.            

        6200-RPT-DATA.
            DISPLAY "  ITEM-DESCRIPTION: '", WS-ITEM-DESCRIPTION, "'".
            DISPLAY "         ITEM-COST: ", WS-ITEM-COST.
            DISPLAY "         ITEM-LIST: ", WS-ITEM-LIST.
	    EXIT.

        6200-RPT-KEY-DATA.
            PERFORM 6200-RPT-KEYS.
            PERFORM 6200-RPT-DATA.
            EXIT.

        6300-SETUP-KEYS.
            INITIALIZE WS-ASSOC-KEYS.
            MOVE WS-T-KEY-COMPANY(WS-TEST-X) TO WS-KEY-COMPANY.
            MOVE WS-T-KEY-ITEM-NO(WS-TEST-X) TO WS-KEY-ITEM-NO.

            SET WS-ASGN-IN-KEY-POINTER TO ADDRESS OF WS-ASSOC-KEYS.
            MOVE LENGTH OF WS-ASSOC-KEYS TO WS-ASGN-IN-KEY-LENGTH.
            EXIT.

        6300-SETUP-DATA.
            INITIALIZE WS-ASSOC-DATA.
            MOVE WS-T-ITEM-DESCRIPTION(WS-TEST-X)
                TO WS-ITEM-DESCRIPTION.
            MOVE WS-T-ITEM-COST(WS-TEST-X) TO WS-ITEM-COST.
            MOVE WS-T-ITEM-LIST(WS-TEST-X) TO WS-ITEM-LIST.
            SET WS-IN-DATA-POINTER TO
                ADDRESS OF WS-ASSOC-DATA.
            MOVE LENGTH OF WS-ASSOC-DATA TO WS-IN-DATA-LENGTH.
            EXIT.

        6300-SETUP-KEY-DATA.
            PERFORM 6300-SETUP-KEYS.
            PERFORM 6300-SETUP-DATA.
            EXIT.

        6300-CMP-DATA.
            IF WS-ITEM-DESCRIPTION NOT = WS-SV-ITEM-DESCRIPTION
            OR WS-ITEM-COST NOT = WS-SV-ITEM-COST
            OR WS-ITEM-LIST NOT = WS-SV-ITEM-LIST THEN
                DISPLAY "  *** DATA DOES NOT MATCH ***"
                SET WS-DATA-CMP-FAILED TO TRUE
            ELSE
                DISPLAY "  --- DATA CONTENT MATCHES ---"
                SET WS-DATA-CMP-MATCHED TO TRUE
            END-IF.
            EXIT.

        6300-CMP-DATA-X.
            IF WS-ITEM-DESCRIPTION NOT =
                WS-T-ITEM-DESCRIPTION(WS-TEST-X)
            OR WS-ITEM-COST NOT = WS-T-ITEM-COST(WS-TEST-X)
            OR WS-ITEM-LIST NOT = WS-T-ITEM-LIST(WS-TEST-X) THEN
                DISPLAY "  *** DATA DOES NOT MATCH ***"
                DISPLAY "  INDEX=", WS-TEST-X
                SET WS-DATA-CMP-FAILED TO TRUE
                PERFORM 6100-FAILED
            ELSE
                DISPLAY "  --- DATA CONTENT MATCHES ---"
                SET WS-DATA-CMP-MATCHED TO TRUE
                PERFORM 6100-PASSED
            END-IF.
            EXIT.

        6300-CMP-KEY-X.
            IF WS-T-KEY-COMPANY(WS-TEST-X) = WS-KEY-COMPANY
            AND WS-T-KEY-ITEM-NO(WS-TEST-X) = WS-KEY-ITEM-NO THEN
                DISPLAY "  --- KEY CONTENT MATCHES ---"
                SET WS-DATA-CMP-MATCHED TO TRUE
                PERFORM 6100-PASSED
            ELSE
                DISPLAY "  *** KEY CONTENT DOES NOT MATCH ***"
                DISPLAY "  INDEX=", WS-TEST-X
                SET WS-DATA-CMP-FAILED TO TRUE
                PERFORM 6100-FAILED
            END-IF.
            EXIT.

        6500-LOCATE-KEY.
            MOVE 'N' TO WS-LOOKUP-FLAG.
            PERFORM VARYING WS-X FROM 1 BY 1
              UNTIL WS-X > WS-TEST-N OR WS-LOOKUP-FOUND
                IF WS-T-KEY-COMPANY(WS-X) = WS-KEY-COMPANY
                AND WS-T-KEY-ITEM-NO(WS-X) = WS-KEY-ITEM-NO THEN
                    SET WS-LOOKUP-FOUND TO TRUE
                    MOVE WS-X TO WS-TEST-X
                END-IF
            END-PERFORM.
            IF WS-LOOKUP-FOUND THEN
                DISPLAY "  KEY LOCATED AT X = ", WS-TEST-X
            ELSE
                DISPLAY "  KEY NOT LOCATED."
            END-IF.
            EXIT.

        7000-COUNT-TEST.
      *>
      *>     RETRIEVE A COUNT, 
      *>         EXPECTING RETURN-CODE = WS-EXPECTED-RET-CODE,
      *>     AND           COUNT       = WS-EXPECTED-COUNT
      *>
            DISPLAY "  == 7000-COUNT-TEST ==".

            MOVE 666 TO WS-OUT-COUNT.

            CALL "COBCURSEX-ASSOC-COUNT"
              USING
                WS-INSTANCE,
                BY REFERENCE WS-OUT-COUNT.

            PERFORM 6000-RETURN-CODE.

            IF RETURN-CODE = 0 THEN
                DISPLAY "        COUNT= ", WS-OUT-COUNT
                IF WS-OUT-COUNT NOT = WS-EXPECTED-COUNT THEN
                    PERFORM 6100-FAILED
                ELSE
                    PERFORM 6100-PASSED
                END-IF
            END-IF.
            EXIT.

        7000-ASSGN-FETCH-TEST.
      *>
      *>     PERFORM AN ASSIGN/FETCH TEST BASED UPON WS-TEST-X.
      *>
            DISPLAY "  == 7000-ASSGN-FETCH-TEST ==".
            PERFORM 6300-SETUP-KEY-DATA.
            PERFORM 6200-RPT-KEY-DATA.

            CALL "COBCURSEX-ASSOC-ASSIGN" 
                USING WS-INSTANCE, WS-ASSOC-KEY-PARAM,
                    WS-ASSOC-DATA-PARAM.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            MOVE WS-ASSOC-DATA TO WS-ASSOC-DATA-SAVED.
            INITIALIZE WS-ASSOC-DATA.

            DISPLAY "  (( FETCHING STORED DATA ITEM BY KEY ))".

            CALL "COBCURSEX-ASSOC-FETCH" 
              USING
                WS-INSTANCE,
                WS-ASSOC-KEY-PARAM,
                WS-ASSOC-DATA-PARAM,
                BY REFERENCE WS-OUT-DATA-LENGTH.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.

            IF RETURN-CODE = NC-RET-OK THEN
                DISPLAY "  RETURNED DATA IS.."
                PERFORM 6200-RPT-DATA
                PERFORM 6300-CMP-DATA
                IF WS-DATA-CMP-MATCHED THEN
                    PERFORM 6100-PASSED
                ELSE
                    PERFORM 6100-FAILED
                END-IF
                IF WS-OUT-DATA-LENGTH NOT = WS-IN-DATA-LENGTH THEN
                    DISPLAY "  RETURNED LENGTH ", WS-OUT-DATA-LENGTH,
                        " SHOULD BE ", WS-IN-DATA-LENGTH
                    PERFORM 6100-FAILED
                ELSE
                    DISPLAY "  RETURNED LENGTH ", WS-OUT-DATA-LENGTH,
                        " IS CORRECT."
                    PERFORM 6100-PASSED
                END-IF
            END-IF.
      *>
      *>     GET THE LAST ASSIGNED HANDLE 
      *>
            DISPLAY "  RETRIEVING LAST ASSIGNED HANDLE:".

            CALL "COBCURSEX-ASSOC-HANDLE" USING
                WS-INSTANCE
                BY REFERENCE WS-HANDLE.

            MOVE NC-RET-OK TO WS-EXPECTED-RET-CODE.
            PERFORM 6000-RETURN-CODE.
            
            IF RETURN-CODE = NC-RET-OK THEN
                DISPLAY "    HANDLE RETURNED WAS ", WS-HANDLE
                MOVE WS-HANDLE TO
                    WS-HANDLE-ID-NO(WS-INSTANCE-X,WS-TEST-X)
            ELSE
                MOVE NC-NULL-HANDLE TO
                    WS-HANDLE-ID-NO(WS-INSTANCE-X,WS-TEST-X)
            END-IF.
            EXIT.            

        7100-DELETE-TEST.
            MOVE 5 TO WS-TEST-X.
            DISPLAY "  == 7100-DELETE-TEST ==".

            PERFORM 6200-RPT-KEYS.

            CALL "COBCURSEX-ASSOC-DELETE"
                USING WS-INSTANCE, WS-ASSOC-KEY-PARAM.

            PERFORM 6000-RETURN-CODE.
            EXIT.

        7300-FETCH-TEST.
            INITIALIZE WS-ASSOC-KEYS.
            MOVE WS-T-KEY-COMPANY(WS-TEST-X) TO WS-KEY-COMPANY.
            MOVE WS-T-KEY-ITEM-NO(WS-TEST-X) TO WS-KEY-ITEM-NO.
        
            INITIALIZE WS-ASSOC-DATA.
            INITIALIZE WS-ASSOC-DATA-SAVED.

            MOVE WS-T-ITEM-DESCRIPTION(WS-TEST-X)
                TO WS-SV-ITEM-DESCRIPTION.
            MOVE WS-T-ITEM-COST(WS-TEST-X)
                TO WS-SV-ITEM-COST.
            MOVE WS-T-ITEM-LIST(WS-TEST-X)
                TO WS-SV-ITEM-LIST.

            PERFORM 6200-RPT-KEYS.

            CALL "COBCURSEX-ASSOC-FETCH"
              USING
                WS-INSTANCE,
                WS-ASSOC-KEY-PARAM,
                WS-ASSOC-DATA-PARAM,
                BY REFERENCE WS-OUT-DATA-LENGTH.

            PERFORM 6000-RETURN-CODE.

            IF RETURN-CODE = NC-RET-OK THEN
                PERFORM 6200-RPT-DATA
                IF WS-EXPECTED-RET-CODE = NC-RET-OK THEN
                    PERFORM 6300-CMP-DATA
                    IF WS-DATA-CMP-MATCHED THEN
                        PERFORM 6100-PASSED
                    ELSE
                        PERFORM 6100-FAILED
                    END-IF

                    IF WS-OUT-DATA-LENGTH NOT = WS-IN-DATA-LENGTH
                        DISPLAY "  RETURNED LENGTH ",
                            WS-OUT-DATA-LENGTH,
                            " SHOULD BE ", WS-IN-DATA-LENGTH
                        PERFORM 6100-FAILED
                    ELSE
                        DISPLAY "  RETURNED LENGTH ",
                            WS-OUT-DATA-LENGTH, " IS CORRECT."
                        PERFORM 6100-PASSED
                    END-IF
                END-IF
            END-IF.
            EXIT.

      *>
      *> PROGRAM CLEANUP
      *> 
        9000-FINALIZE.
            DISPLAY "---".
            DISPLAY "   FAILED TESTS: ", WS-FAILED-COUNT.
            DISPLAY "   PASSED TESTS: ", WS-PASSED-COUNT.
            IF WS-FAILED-COUNT = ZERO THEN
                MOVE ZERO TO RETURN-CODE
            ELSE
                MOVE 1 TO RETURN-CODE
            END-IF.
            EXIT.

        9900-INIT-DATA.
            MOVE 1 TO WS-TEST-X.
      *> 1
            MOVE 4      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "LAMP-01-176"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "CAMPING LAMP, WITH BATTERY"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 7.45   TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 12.99  TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 2
            MOVE 8      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "TENT-03-758"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "CAMPING TENT, 8' X 8'"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 124.06   TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 289.97   TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 3
            MOVE 5      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "WASHMORE-086"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "WASHMORE DISWASHER MODEL 86"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 601.34   TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 875.47   TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 4
            MOVE 8      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "DISHWIPE-X"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "DISHWIPE EXTREME SOAP"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 1.34   TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 3.61   TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 5
            MOVE 8      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "BEAUSPASH007"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "BEAU SPASH AFTER-SHAVE"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 2.01   TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 3.19   TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 6
            MOVE 8      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "HOUSEFRAUAPR"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "HOUSE FRAU APRON, PINK"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 4.07   TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 7.89   TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 7
            MOVE 6      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "SUPRSHINECWX"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "SUPER SHINE CAR WAX"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 6.77   TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 11.54  TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 8
            MOVE 6      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "WIN-DOZE-666"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "WINDOWS O/S INSTALL CDS VERSION 666"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 666.00 TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 777.00 TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 9
            MOVE 6      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "WIN-BUG-666"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "WINDOWS O/S DEBUGGER CDS FOR VERS 666"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 369.70 TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 401.66 TO WS-T-ITEM-LIST(WS-TEST-X).

            ADD 1 TO WS-TEST-X.
      *> 10
            MOVE 5      TO WS-T-KEY-COMPANY(WS-TEST-X).
            MOVE "SLIME-AWAY"
                TO WS-T-KEY-ITEM-NO(WS-TEST-X).
            MOVE "SLIME-AWAY KITCHEN CLEANER"
                TO WS-T-ITEM-DESCRIPTION(WS-TEST-X).
            MOVE 3.38 TO WS-T-ITEM-COST(WS-TEST-X).
            MOVE 5.69 TO WS-T-ITEM-LIST(WS-TEST-X).

            DISPLAY "== DUMP OF INITIAL TEST DATA ==".
            PERFORM VARYING WS-TEST-X FROM 1 BY 1
            UNTIL WS-TEST-X > WS-TEST-N
                DISPLAY WS-TEST-X ":"
                DISPLAY " WS-T-KEY-COMPANY....... '" 
                    WS-T-KEY-COMPANY(WS-TEST-X) "'"
                DISPLAY " WS-T-KEY-ITEM-NO....... '" 
                    WS-T-KEY-ITEM-NO(WS-TEST-X) "'"
                DISPLAY " WS-T-ITEM-DESCRIPTION.. '" 
                    WS-T-ITEM-DESCRIPTION(WS-TEST-X) "'"
                DISPLAY " WS-T-ITEM-COST......... " 
                    WS-T-ITEM-COST(WS-TEST-X)
                DISPLAY " WS-T-ITEM-LIST......... " 
                    WS-T-ITEM-LIST(WS-TEST-X)
            END-PERFORM.
            DISPLAY "== END OF INITIAL TEST DATA ==".
            EXIT.

      *> 
      *> SUPPORT ROUTINES
      *> 
            COPY COBCURSX.

        END PROGRAM TEST004.
