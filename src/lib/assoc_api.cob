        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-INSTANCE.
      *>
      *> ASSIGN A FREE ASSOCIATIVE ARRAY INSTANCE ID :
      *>
      *> NOTES :
      *>
      *>     UPON SUCCESSFUL CONCLUSION, WHEN AN INSTANCE ID
      *>     IS RETURNED, AN EMPTY ASSOCIATIVE ARRAY INSTANCE
      *>     IS ALSO CREATED (TO AVOID RE-ISSUING THE SAME
      *>     INSTANCE ID).
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL - INSTANCE CREATED AND ID RETURNED
      *>     8   RESOURCE   - NO FREE INSTANCES WERE FOUND
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  OUT-C-INSTANCE              PIC 9(9) COMP-5.

        LINKAGE SECTION.

        01  OUT-INSTANCE-ID             PIC 9(9).

        PROCEDURE DIVISION USING OUT-INSTANCE-ID.

            CALL "NC_ASSIGN_INSTANCE"
                USING OUT-C-INSTANCE.
            MOVE OUT-C-INSTANCE TO OUT-INSTANCE-ID.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-INSTANCE.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-ASSIGN.
      *>
      *> ASSIGN AN ASSOCIATION BY KEY, WITH DATA (BY INSTANCE)
      *>
      *> NOTES :
      *>
      *>     ARRAY INSTANCE IS CREATED IF IT DOES NOT YET EXIST.
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IN-C-KEY-POINTER            POINTER.
        01  IN-C-KEY-LENGTH             PIC 9999 COMP-5.
        01  IN-C-DATA-POINTER           POINTER.
        01  IN-C-DATA-LENGTH            PIC 9999 COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).

        01  ASSOC-KEY-PARAM.
            05  IN-KEY-POINTER          POINTER.
            05  IN-KEY-LENGTH           PIC 9999.

        01  ASSOC-DATA-PARAM.
            05  IN-DATA-POINTER         POINTER.
            05  IN-DATA-LENGTH          PIC 9999.

        PROCEDURE DIVISION USING
            IN-INSTANCE, ASSOC-KEY-PARAM, ASSOC-DATA-PARAM.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            MOVE IN-KEY-POINTER TO IN-C-KEY-POINTER.
            MOVE IN-KEY-LENGTH TO IN-C-KEY-LENGTH.
            MOVE IN-DATA-POINTER TO IN-C-DATA-POINTER.
            MOVE IN-DATA-LENGTH TO IN-C-DATA-LENGTH.

            CALL "NC_ASSOC_ASSIGN" 
                USING IN-C-INSTANCE,
                    IN-C-KEY-POINTER, IN-C-KEY-LENGTH,
                    IN-C-DATA-POINTER, IN-C-DATA-LENGTH.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-ASSIGN.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-HANDLE.
      *>
      *> RETRIEVE THE HANDLE FROM THE LAST ASSOCIATION MADE,
      *> IF ANY.
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   INSTANCE NOT FOUND
      *>     7   NO HANDLE HAS BEEN ASSIGNED (OR DELETED)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  OUT-C-HANDLE                PIC 9(9) COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  OUT-HANDLE                  PIC 9(9).

        PROCEDURE DIVISION
            USING IN-INSTANCE, OUT-HANDLE.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            CALL "NC_LAST_ASSOC" 
                USING IN-C-INSTANCE, OUT-C-HANDLE.
            MOVE OUT-C-HANDLE TO OUT-HANDLE.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-HANDLE.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-FETCH.
      *>
      *> FETCH KEY AND DATA INFO FOR ITERATOR HANDLE (BY INSTANCE)
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL (WHETHER OR NOT KEY EXISTED)
      *>     5   NOT FOUND (INSTANCE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IN-C-KEY-POINTER            POINTER.
        01  IN-C-KEY-LENGTH             PIC 9999 COMP-5.
        01  IN-C-DATA-POINTER           POINTER.
        01  IO-C-DATA-LENGTH            PIC 9999 COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).

        01  IN-ASSOC-KEY.
            05  IN-KEY-POINTER          POINTER.
            05  IN-KEY-LENGTH           PIC 9999.

        01  IN-ASSOC-DATA.
            05  IN-DATA-POINTER         POINTER.
            05  IN-DATA-LENGTH          PIC 9999.

        77  OUT-DATA-LENGTH             PIC 9999.

        PROCEDURE DIVISION 
            USING IN-INSTANCE, IN-ASSOC-KEY, IN-ASSOC-DATA,
                OUT-DATA-LENGTH.

            MOVE IN-INSTANCE        TO IN-C-INSTANCE.
            MOVE IN-KEY-POINTER     TO IN-C-KEY-POINTER.
            MOVE IN-KEY-LENGTH      TO IN-C-KEY-LENGTH.
            MOVE IN-DATA-POINTER    TO IN-C-DATA-POINTER.
            MOVE IN-DATA-LENGTH     TO IO-C-DATA-LENGTH.

            CALL "NC_ASSOC_FETCH" 
                USING IN-C-INSTANCE,
                    IN-C-KEY-POINTER, IN-C-KEY-LENGTH,
                    IN-C-DATA-POINTER, IO-C-DATA-LENGTH.

            MOVE IO-C-DATA-LENGTH TO OUT-DATA-LENGTH.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-FETCH.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-DELETE.
      *>
      *> DELETE AN ASSOCIATION BY KEY (AND INSTANCE)
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL (WHETHER OR NOT KEY EXISTED)
      *>     5   NOT FOUND (INSTANCE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IN-C-KEY-POINTER            POINTER.
        01  IN-C-KEY-LENGTH             PIC 9999 COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).

        01  ASSOC-KEY-PARAM.
            05  IN-KEY-POINTER          POINTER.
            05  IN-KEY-LENGTH           PIC 9999.

        PROCEDURE DIVISION
            USING IN-INSTANCE, ASSOC-KEY-PARAM.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            MOVE IN-KEY-POINTER TO IN-C-KEY-POINTER.
            MOVE IN-KEY-LENGTH TO IN-C-KEY-LENGTH.
            CALL "NC_ASSOC_DELETE" 
                USING IN-C-INSTANCE,
                    IN-C-KEY-POINTER, IN-C-KEY-LENGTH.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-DELETE.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-DELETE-X.
      *>
      *> DELETE AN ASSOCIATION BY HANDLE (AND INSTANCE)
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE/HANDLE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IN-C-HANDLE                 PIC 9(9) COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  IN-HANDLE                   PIC 9(9).

        PROCEDURE DIVISION
            USING IN-INSTANCE, IN-HANDLE.

            MOVE IN-INSTANCE    TO IN-C-INSTANCE.
            MOVE IN-HANDLE      TO IN-C-HANDLE.

            CALL "NC_ASSOC_DELETE_X" 
                USING IN-C-INSTANCE, IN-C-HANDLE.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-DELETE-X.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-CLEAR.
      *>
      *> CLEAR THE ASSOC ARRAY (BY INSTANCE)
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).

        PROCEDURE DIVISION USING IN-INSTANCE.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            CALL "NC_ASSOC_CLEAR" USING IN-C-INSTANCE.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-CLEAR.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-CLEAR-ALL.
      *>
      *> CLEAR ALL ASSOCIATIVE ARRAYS (LEAVES *CSV API
      *> UNAFFECTED.
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>
        PROCEDURE DIVISION.

            CALL "NC_ASSOC_CLEAR_ALL".
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-CLEAR-ALL.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-COUNT.
      *>
      *> RETURN THE COUNT OF ITEMS HELD BY ASSOC ARRAY (BY INSTANCE)
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  OUT-C-COUNT                 PIC 9(9) COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  OUT-COUNT                   PIC 9(9).

        PROCEDURE DIVISION USING IN-INSTANCE, OUT-COUNT.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            CALL "NC_ASSOC_COUNT"
                USING IN-C-INSTANCE, OUT-C-COUNT.
            MOVE OUT-C-COUNT TO OUT-COUNT.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-COUNT.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-FETCH-X.
      *>
      *> RETURN ASSOC KEY & DATA CONTENT FOR ITERATOR HANDLE
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE OR HANDLE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IN-C-HANDLE                 PIC 9(9) COMP-5.
        01  IN-C-KEY-POINTER            POINTER.
        01  IO-C-KEY-LENGTH             PIC 9999 COMP-5.
        01  IN-C-DATA-POINTER           POINTER.
        01  IO-C-DATA-LENGTH            PIC 9999 COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  IN-HANDLE                   PIC 9(9).

        01  OUT-ASSOC-KEY.
            05  IN-KEY-POINTER          POINTER.
            05  IN-KEY-LENGTH           PIC 9999.

        01  OUT-ASSOC-DATA.
            05  IN-DATA-POINTER         POINTER.
            05  IN-DATA-LENGTH          PIC 9999.

        77  OUT-KEY-LENGTH              PIC 9999.
        77  OUT-DATA-LENGTH             PIC 9999.

        PROCEDURE DIVISION
          USING IN-INSTANCE, IN-HANDLE,
            OUT-ASSOC-KEY, OUT-ASSOC-DATA,
            OUT-KEY-LENGTH, OUT-DATA-LENGTH.

            MOVE IN-INSTANCE        TO IN-C-INSTANCE.
            MOVE IN-HANDLE          TO IN-C-HANDLE.
            MOVE IN-KEY-POINTER     TO IN-C-KEY-POINTER.
            MOVE IN-KEY-LENGTH      TO IO-C-KEY-LENGTH.
            MOVE IN-DATA-POINTER    TO IN-C-DATA-POINTER.
            MOVE IN-DATA-LENGTH     TO IO-C-DATA-LENGTH.

            CALL "NC_ASSOC_FETCH_X"
                USING IN-C-INSTANCE, IN-C-HANDLE,
                    IN-C-KEY-POINTER,  IO-C-KEY-LENGTH,
                    IN-C-DATA-POINTER, IO-C-DATA-LENGTH.

            MOVE IO-C-KEY-LENGTH  TO OUT-KEY-LENGTH.
            MOVE IO-C-DATA-LENGTH TO OUT-DATA-LENGTH.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-FETCH-X.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-FETCH-KEY-X.
      *>
      *> RETURN ASSOC KEY INFO FOR AN ITERATOR HANDLE
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE OR HANDLE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IN-C-HANDLE                 PIC 9(9) COMP-5.
        01  IN-C-KEY-POINTER            POINTER.
        01  IO-C-KEY-LENGTH             PIC 9999 COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  IN-HANDLE                   PIC 9(9).

        01  IN-ASSOC-KEY.
            05  IN-KEY-POINTER          POINTER.
            05  IN-KEY-LENGTH           PIC 9999.

        77  OUT-KEY-LENGTH              PIC 9999.

        PROCEDURE DIVISION
            USING IN-INSTANCE, IN-HANDLE, IN-ASSOC-KEY,
                OUT-KEY-LENGTH.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            MOVE IN-HANDLE   TO IN-C-HANDLE.
            MOVE IN-KEY-POINTER TO IN-C-KEY-POINTER.
            MOVE IN-KEY-LENGTH TO IO-C-KEY-LENGTH.

            CALL "NC_ASSOC_FETCH_KEY_X" USING
                IN-C-INSTANCE, IN-C-HANDLE,
                IN-C-KEY-POINTER, IO-C-KEY-LENGTH.

            MOVE IO-C-KEY-LENGTH TO OUT-KEY-LENGTH.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-FETCH-KEY-X.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-FETCH-DATA-X.
      *>
      *> RETURN ASSOC DATA CONTENT FOR ITERATOR HANDLE
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE OR HANDLE)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IN-C-HANDLE                 PIC 9(9) COMP-5.
        01  IN-C-DATA-POINTER           POINTER.
        01  IO-C-DATA-LENGTH            PIC 9999 COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  IN-HANDLE                   PIC 9(9).

        01  OUT-ASSOC-DATA.
            05  IN-DATA-POINTER         POINTER.
            05  IN-DATA-LENGTH          PIC 9999.

        77  OUT-DATA-LENGTH             PIC 9999.

        PROCEDURE DIVISION
            USING IN-INSTANCE, IN-HANDLE,
                OUT-ASSOC-DATA, OUT-DATA-LENGTH.

            MOVE IN-INSTANCE     TO IN-C-INSTANCE.
            MOVE IN-HANDLE       TO IN-C-HANDLE.
            MOVE IN-DATA-POINTER TO IN-C-DATA-POINTER.
            MOVE IN-DATA-LENGTH  TO IO-C-DATA-LENGTH.

            CALL "NC_ASSOC_FETCH_DATA_X" USINg
                IN-C-INSTANCE, IN-C-HANDLE,
                IN-C-DATA-POINTER, IO-C-DATA-LENGTH.

            MOVE IO-C-DATA-LENGTH TO OUT-DATA-LENGTH.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-FETCH-DATA-X.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-FIRST.
      *>
      *> GET NEXT ITERATOR HANDLE FOR ASSOCIATION.
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE)
      *>     7   END OF LIST (NOTHING RETURNED)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  OUT-C-HANDLE                PIC 9(9) COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  OUT-HANDLE                  PIC 9(9).

        PROCEDURE DIVISION
            USING IN-INSTANCE, OUT-HANDLE.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            MOVE ZERO TO OUT-C-HANDLE.

            CALL "NC_ASSOC_FIRST"
                USING IN-C-INSTANCE, OUT-C-HANDLE.

            MOVE OUT-C-HANDLE TO OUT-HANDLE.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-FIRST.


        IDENTIFICATION DIVISION.
        PROGRAM-ID. COBCURSEX-ASSOC-NEXT.
      *>
      *> GET NEXT ITERATOR HANDLE FOR ASSOCIATION.
      *>
      *> RETURN-CODE :
      *>     0   SUCCESSFUL
      *>     5   NOT FOUND (INSTANCE)
      *>     7   END OF LIST (NOTHING RETURNED)
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  IN-C-INSTANCE               PIC 9(9) COMP-5.
        01  IO-C-HANDLE                 PIC 9(9) COMP-5.

        LINKAGE SECTION.

        77  IN-INSTANCE                 PIC 9(9).
        77  IO-HANDLE                   PIC 9(9).

        PROCEDURE DIVISION
            USING IN-INSTANCE, IO-HANDLE.

            MOVE IN-INSTANCE TO IN-C-INSTANCE.
            MOVE IO-HANDLE   TO IO-C-HANDLE.

            CALL "NC_ASSOC_NEXT"
                USING IN-C-INSTANCE, IO-C-HANDLE.

            MOVE IO-C-HANDLE TO IO-HANDLE.
            GOBACK.

        END PROGRAM COBCURSEX-ASSOC-NEXT.
