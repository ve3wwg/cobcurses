      *>
      *> NCURSES KEY DEFINITIONS :
      *>
        01  NC-KEY-DEFINITIONS.
      *>        MINIMUM KEY VALUE
            10  NC-KEY-MIN         PIC 9(4) COMP-5 VALUE 256
                                            SYNCHRONIZED.
      *>        BREAK KEY (UNRELIABLE)
            10  NC-KEY-BREAK       PIC 9(4) COMP-5 VALUE 257
                                            SYNCHRONIZED.
      *>        SOFT (PARTIAL) RESET (UNRELIABLE)
            10  NC-KEY-SRESET      PIC 9(4) COMP-5 VALUE 258
                                            SYNCHRONIZED.
      *>        RESET OR HARD RESET (UNRELIABLE)
            10  NC-KEY-RESET       PIC 9(4) COMP-5 VALUE 259
                                            SYNCHRONIZED.
      *>        DOWN-ARROW KEY
            10  NC-KEY-DOWN        PIC 9(4) COMP-5 VALUE 260
                                            SYNCHRONIZED.
      *>        UP-ARROW KEY
            10  NC-KEY-UP          PIC 9(4) COMP-5 VALUE 261
                                            SYNCHRONIZED.
      *>        LEFT-ARROW KEY
            10  NC-KEY-LEFT        PIC 9(4) COMP-5 VALUE 262
                                            SYNCHRONIZED.
      *>        RIGHT-ARROW KEY
            10  NC-KEY-RIGHT       PIC 9(4) COMP-5 VALUE 264
                                            SYNCHRONIZED.
      *>        HOME KEY
            10  NC-KEY-HOME        PIC 9(4) COMP-5 VALUE 266
                                            SYNCHRONIZED.
      *>        BACKSPACE KEY
            10  NC-KEY-BACKSPACE   PIC 9(4) COMP-5 VALUE 268
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 0
            10  NC-KEY-F0          PIC 9(4) COMP-5 VALUE 269
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 1
            10  NC-KEY-F1          PIC 9(4) COMP-5 VALUE 270
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 2
            10  NC-KEY-F2          PIC 9(4) COMP-5 VALUE 271
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 3
            10  NC-KEY-F3          PIC 9(4) COMP-5 VALUE 272
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 4
            10  NC-KEY-F4          PIC 9(4) COMP-5 VALUE 273
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 5
            10  NC-KEY-F5          PIC 9(4) COMP-5 VALUE 274
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 6
            10  NC-KEY-F6          PIC 9(4) COMP-5 VALUE 275
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 7
            10  NC-KEY-F7          PIC 9(4) COMP-5 VALUE 276
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 8
            10  NC-KEY-F8          PIC 9(4) COMP-5 VALUE 277
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 9
            10  NC-KEY-F9          PIC 9(4) COMP-5 VALUE 278
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 10
            10  NC-KEY-F10         PIC 9(4) COMP-5 VALUE 279
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 11
            10  NC-KEY-F11         PIC 9(4) COMP-5 VALUE 280
                                            SYNCHRONIZED.
      *>        FUNCTION KEY 12
            10  NC-KEY-F12         PIC 9(4) COMP-5 VALUE 281
                                            SYNCHRONIZED.
      *>        DELETE-LINE KEY
            10  NC-KEY-DL          PIC 9(4) COMP-5 VALUE 282
                                            SYNCHRONIZED.
      *>        INSERT-LINE KEY
            10  NC-KEY-IL          PIC 9(4) COMP-5 VALUE 284
                                            SYNCHRONIZED.
      *>        DELETE-CHARACTER KEY
            10  NC-KEY-DC          PIC 9(4) COMP-5 VALUE 285
                                            SYNCHRONIZED.
      *>        INSERT-CHARACTER KEY
            10  NC-KEY-IC          PIC 9(4) COMP-5 VALUE 287
                                            SYNCHRONIZED.
      *>        SENT BY RMIR OR SMIR IN INSERT MODE
            10  NC-KEY-EIC         PIC 9(4) COMP-5 VALUE 289
                                            SYNCHRONIZED.
      *>        CLEAR-SCREEN OR ERASE KEY
            10  NC-KEY-CLEAR       PIC 9(4) COMP-5 VALUE 290
                                            SYNCHRONIZED.
      *>        CLEAR-TO-END-OF-SCREEN KEY
            10  NC-KEY-EOS         PIC 9(4) COMP-5 VALUE 291
                                            SYNCHRONIZED.
      *>        CLEAR-TO-END-OF-LINE KEY
            10  NC-KEY-EOL         PIC 9(4) COMP-5 VALUE 292
                                            SYNCHRONIZED.
      *>        SCROLL-FORWARD KEY
            10  NC-KEY-SF          PIC 9(4) COMP-5 VALUE 294
                                            SYNCHRONIZED.
      *>        SCROLL-BACKWARD KEY
            10  NC-KEY-SR          PIC 9(4) COMP-5 VALUE 295
                                            SYNCHRONIZED.
      *>        NEXT-PAGE KEY
            10  NC-KEY-NPAGE       PIC 9(4) COMP-5 VALUE 296
                                            SYNCHRONIZED.
      *>        PREVIOUS-PAGE KEY
            10  NC-KEY-PPAGE       PIC 9(4) COMP-5 VALUE 297
                                            SYNCHRONIZED.
      *>        SET-TAB KEY
            10  NC-KEY-STAB        PIC 9(4) COMP-5 VALUE 298
                                            SYNCHRONIZED.
      *>        CLEAR-TAB KEY
            10  NC-KEY-CTAB        PIC 9(4) COMP-5 VALUE 299
                                            SYNCHRONIZED.
      *>        CLEAR-ALL-TABS KEY
            10  NC-KEY-CATAB       PIC 9(4) COMP-5 VALUE 300
                                            SYNCHRONIZED.
      *>        ENTER/SEND KEY
            10  NC-KEY-ENTER       PIC 9(4) COMP-5 VALUE 301
                                            SYNCHRONIZED.
      *>        PRINT KEY
            10  NC-KEY-PRINT       PIC 9(4) COMP-5 VALUE 302
                                            SYNCHRONIZED.
      *>        LOWER-LEFT KEY (HOME DOWN)
            10  NC-KEY-LL          PIC 9(4) COMP-5 VALUE 304
                                            SYNCHRONIZED.
      *>        UPPER LEFT OF KEYPAD
            10  NC-KEY-A1          PIC 9(4) COMP-5 VALUE 305
                                            SYNCHRONIZED.
      *>        UPPER RIGHT OF KEYPAD
            10  NC-KEY-A3          PIC 9(4) COMP-5 VALUE 306
                                            SYNCHRONIZED.
      *>        CENTER OF KEYPAD
            10  NC-KEY-B2          PIC 9(4) COMP-5 VALUE 307
                                            SYNCHRONIZED.
      *>        LOWER LEFT OF KEYPAD
            10  NC-KEY-C1          PIC 9(4) COMP-5 VALUE 308
                                            SYNCHRONIZED.
      *>        LOWER RIGHT OF KEYPAD
            10  NC-KEY-C3          PIC 9(4) COMP-5 VALUE 309
                                            SYNCHRONIZED.
      *>        BACK-TAB KEY
            10  NC-KEY-BTAB        PIC 9(4) COMP-5 VALUE 310
                                            SYNCHRONIZED.
      *>        BEGIN KEY
            10  NC-KEY-BEG         PIC 9(4) COMP-5 VALUE 311
                                            SYNCHRONIZED.
      *>        CANCEL KEY
            10  NC-KEY-CANCEL      PIC 9(4) COMP-5 VALUE 313
                                            SYNCHRONIZED.
      *>        CLOSE KEY
            10  NC-KEY-CLOSE       PIC 9(4) COMP-5 VALUE 315
                                            SYNCHRONIZED.
      *>        COMMAND KEY
            10  NC-KEY-COMMAND     PIC 9(4) COMP-5 VALUE 316
                                            SYNCHRONIZED.
      *>        COPY KEY
            10  NC-KEY-COPY        PIC 9(4) COMP-5 VALUE 318
                                            SYNCHRONIZED.
      *>        CREATE KEY
            10  NC-KEY-CREATE      PIC 9(4) COMP-5 VALUE 320
                                            SYNCHRONIZED.
      *>        END KEY
            10  NC-KEY-END         PIC 9(4) COMP-5 VALUE 322
                                            SYNCHRONIZED.
      *>        EXIT KEY
            10  NC-KEY-EXIT        PIC 9(4) COMP-5 VALUE 323
                                            SYNCHRONIZED.
      *>        FIND KEY
            10  NC-KEY-FIND        PIC 9(4) COMP-5 VALUE 325
                                            SYNCHRONIZED.
      *>        HELP KEY
            10  NC-KEY-HELP        PIC 9(4) COMP-5 VALUE 327
                                            SYNCHRONIZED.
      *>        MARK KEY
            10  NC-KEY-MARK        PIC 9(4) COMP-5 VALUE 329
                                            SYNCHRONIZED.
      *>        MESSAGE KEY
            10  NC-KEY-MESSAGE     PIC 9(4) COMP-5 VALUE 330
                                            SYNCHRONIZED.
      *>        MOVE KEY
            10  NC-KEY-MOVE        PIC 9(4) COMP-5 VALUE 332
                                            SYNCHRONIZED.
      *>        NEXT KEY
            10  NC-KEY-NEXT        PIC 9(4) COMP-5 VALUE 334
                                            SYNCHRONIZED.
      *>        OPEN KEY
            10  NC-KEY-OPEN        PIC 9(4) COMP-5 VALUE 336
                                            SYNCHRONIZED.
      *>        OPTIONS KEY
            10  NC-KEY-OPTIONS     PIC 9(4) COMP-5 VALUE 337
                                            SYNCHRONIZED.
      *>        PREVIOUS KEY
            10  NC-KEY-PREVIOUS    PIC 9(4) COMP-5 VALUE 339
                                            SYNCHRONIZED.
      *>        REDO KEY
            10  NC-KEY-REDO        PIC 9(4) COMP-5 VALUE 341
                                            SYNCHRONIZED.
      *>        REFERENCE KEY
            10  NC-KEY-REFERENCE   PIC 9(4) COMP-5 VALUE 343
                                            SYNCHRONIZED.
      *>        REFRESH KEY
            10  NC-KEY-REFRESH     PIC 9(4) COMP-5 VALUE 344
                                            SYNCHRONIZED.
      *>        REPLACE KEY
            10  NC-KEY-REPLACE     PIC 9(4) COMP-5 VALUE 345
                                            SYNCHRONIZED.
      *>        RESTART KEY
            10  NC-KEY-RESTART     PIC 9(4) COMP-5 VALUE 347
                                            SYNCHRONIZED.
      *>        RESUME KEY
            10  NC-KEY-RESUME      PIC 9(4) COMP-5 VALUE 348
                                            SYNCHRONIZED.
      *>        SAVE KEY
            10  NC-KEY-SAVE        PIC 9(4) COMP-5 VALUE 350
                                            SYNCHRONIZED.
      *>        SHIFTED BEGIN KEY
            10  NC-KEY-SBEG        PIC 9(4) COMP-5 VALUE 312
                                            SYNCHRONIZED.
      *>        SHIFTED CANCEL KEY
            10  NC-KEY-SCANCEL     PIC 9(4) COMP-5 VALUE 314
                                            SYNCHRONIZED.
      *>        SHIFTED COMMAND KEY
            10  NC-KEY-SCOMMAND    PIC 9(4) COMP-5 VALUE 317
                                            SYNCHRONIZED.
      *>        SHIFTED COPY KEY
            10  NC-KEY-SCOPY       PIC 9(4) COMP-5 VALUE 319
                                            SYNCHRONIZED.
      *>        SHIFTED CREATE KEY
            10  NC-KEY-SCREATE     PIC 9(4) COMP-5 VALUE 321
                                            SYNCHRONIZED.
      *>        SHIFTED DELETE-CHARACTER KEY
            10  NC-KEY-SDC         PIC 9(4) COMP-5 VALUE 286
                                            SYNCHRONIZED.
      *>        SHIFTED DELETE-LINE KEY
            10  NC-KEY-SDL         PIC 9(4) COMP-5 VALUE 283
                                            SYNCHRONIZED.
      *>        SELECT KEY
            10  NC-KEY-SELECT      PIC 9(4) COMP-5 VALUE 352
                                            SYNCHRONIZED.
      *>        SHIFTED END KEY
            10  NC-KEY-SEND        PIC 9(4) COMP-5 VALUE 353
                                            SYNCHRONIZED.
      *>        SHIFTED CLEAR-TO-END-OF-LINE KEY
            10  NC-KEY-SEOL        PIC 9(4) COMP-5 VALUE 293
                                            SYNCHRONIZED.
      *>        SHIFTED EXIT KEY
            10  NC-KEY-SEXIT       PIC 9(4) COMP-5 VALUE 324
                                            SYNCHRONIZED.
      *>        SHIFTED FIND KEY
            10  NC-KEY-SFIND       PIC 9(4) COMP-5 VALUE 326
                                            SYNCHRONIZED.
      *>        SHIFTED HELP KEY
            10  NC-KEY-SHELP       PIC 9(4) COMP-5 VALUE 328
                                            SYNCHRONIZED.
      *>        SHIFTED HOME KEY
            10  NC-KEY-SHOME       PIC 9(4) COMP-5 VALUE 267
                                            SYNCHRONIZED.
      *>        SHIFTED INSERT-CHARACTER KEY
            10  NC-KEY-SIC         PIC 9(4) COMP-5 VALUE 288
                                            SYNCHRONIZED.
      *>        SHIFTED LEFT-ARROW KEY
            10  NC-KEY-SLEFT       PIC 9(4) COMP-5 VALUE 263
                                            SYNCHRONIZED.
      *>        SHIFTED MESSAGE KEY
            10  NC-KEY-SMESSAGE    PIC 9(4) COMP-5 VALUE 331
                                            SYNCHRONIZED.
      *>        SHIFTED MOVE KEY
            10  NC-KEY-SMOVE       PIC 9(4) COMP-5 VALUE 333
                                            SYNCHRONIZED.
      *>        SHIFTED NEXT KEY
            10  NC-KEY-SNEXT       PIC 9(4) COMP-5 VALUE 335
                                            SYNCHRONIZED.
      *>        SHIFTED OPTIONS KEY
            10  NC-KEY-SOPTIONS    PIC 9(4) COMP-5 VALUE 338
                                            SYNCHRONIZED.
      *>        SHIFTED PREVIOUS KEY
            10  NC-KEY-SPREVIOUS   PIC 9(4) COMP-5 VALUE 340
                                            SYNCHRONIZED.
      *>        SHIFTED PRINT KEY
            10  NC-KEY-SPRINT      PIC 9(4) COMP-5 VALUE 303
                                            SYNCHRONIZED.
      *>        SHIFTED REDO KEY
            10  NC-KEY-SREDO       PIC 9(4) COMP-5 VALUE 342
                                            SYNCHRONIZED.
      *>        SHIFTED REPLACE KEY
            10  NC-KEY-SREPLACE    PIC 9(4) COMP-5 VALUE 346
                                            SYNCHRONIZED.
      *>        SHIFTED RIGHT-ARROW KEY
            10  NC-KEY-SRIGHT      PIC 9(4) COMP-5 VALUE 265
                                            SYNCHRONIZED.
      *>        SHIFTED RESUME KEY
            10  NC-KEY-SRSUME      PIC 9(4) COMP-5 VALUE 349
                                            SYNCHRONIZED.
      *>        SHIFTED SAVE KEY
            10  NC-KEY-SSAVE       PIC 9(4) COMP-5 VALUE 351
                                            SYNCHRONIZED.
      *>        SHIFTED SUSPEND KEY
            10  NC-KEY-SSUSPEND    PIC 9(4) COMP-5 VALUE 356
                                            SYNCHRONIZED.
      *>        SHIFTED UNDO KEY
            10  NC-KEY-SUNDO       PIC 9(4) COMP-5 VALUE 354
                                            SYNCHRONIZED.
      *>        SUSPEND KEY
            10  NC-KEY-SUSPEND     PIC 9(4) COMP-5 VALUE 355
                                            SYNCHRONIZED.
      *>        UNDO KEY
            10  NC-KEY-UNDO        PIC 9(4) COMP-5 VALUE 357
                                            SYNCHRONIZED.
      *>        MOUSE EVENT HAS OCCURRED
            10  NC-KEY-MOUSE       PIC 9(4) COMP-5 VALUE 358
                                            SYNCHRONIZED.
      *>        TERMINAL RESIZE EVENT
            10  NC-KEY-RESIZE      PIC 9(4) COMP-5 VALUE 359
                                            SYNCHRONIZED.
      *>        WE WERE INTERRUPTED BY AN EVENT
            10  NC-KEY-EVENT       PIC 9(4) COMP-5 VALUE 360
                                            SYNCHRONIZED.
      *>        IDLE/TIMEOUT EVENT
            10  NC-KEY-IDLE        PIC 9(4) COMP-5 VALUE 362
                                            SYNCHRONIZED.
      *>        MAXIMUM KEY VALUE
            10  NC-KEY-MAX         PIC 9(4) COMP-5 VALUE 3840
                                            SYNCHRONIZED.
      *>
      *> END NCURSES KEY DEFINITONS.
      *>

