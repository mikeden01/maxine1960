000100***************************************************************** DHELP01P
000200*                                                               * DHELP01P
000300*   Copyright (C) 1998-2004 Micro Focus. All Rights Reserved.   * DHELP01P
000400*   This demonstration program is provided for use by users     * DHELP01P
000500*   of Micro Focus products and may be used, modified and       * DHELP01P
000600*   distributed as part of your application provided that       * DHELP01P
000700*   you properly acknowledge the copyright of Micro Focus       * DHELP01P
000800*   in this material.                                           * DHELP01P
000900*                                                               * DHELP01P
001000***************************************************************** DHELP01P
001100                                                                  DHELP01P
001200***************************************************************** DHELP01P
001300* Program:     DHELP01P.CBL                                     * DHELP01P
001400* Function:    Obtain screen help information                   * DHELP01P
001500*              VSAM version                                     * DHELP01P
001600***************************************************************** DHELP01P
001700                                                                  DHELP01P
001800 IDENTIFICATION DIVISION.                                         DHELP01P
001900 PROGRAM-ID.                                                      DHELP01P
002000     DHELP01P.                                                    DHELP01P
002100 DATE-WRITTEN.                                                    DHELP01P
002200     September 2002.                                              DHELP01P
002300 DATE-COMPILED.                                                   DHELP01P
002400     Today.                                                       DHELP01P
002500                                                                  DHELP01P
002600 ENVIRONMENT DIVISION.                                            DHELP01P
002700                                                                  DHELP01P
002800 DATA DIVISION.                                                   DHELP01P
002900                                                                  DHELP01P
003000 WORKING-STORAGE SECTION.                                         DHELP01P
003100 01  WS-MISC-STORAGE.                                             DHELP01P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DHELP01P
003300       VALUE 'DHELP01P'.                                          DHELP01P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DHELP01P
003500   05  WS-SUB1                               PIC S9(4) COMP.      DHELP01P
003600   05  WS-RESP                               PIC S9(4) COMP.      DHELP01P
003700   05  WS-HELP-RID                           PIC X(8).            DHELP01P
003800   05  WS-LINE                               PIC X(2).            DHELP01P
003900   05  WS-LINE-N REDEFINES WS-LINE           PIC 9(2).            DHELP01P
004000                                                                  DHELP01P
004100 01  WS-COMMAREA.                                                 DHELP01P
004200 COPY CHELPD01.                                                   DHELP01P
004300                                                                  DHELP01P
004400 01  WS-HELP-REC.                                                 DHELP01P
004500 COPY CHELPVSM.                                                   DHELP01P
004600                                                                  DHELP01P
004700 COPY CABENDD.                                                    DHELP01P
004800                                                                  DHELP01P
004900 LINKAGE SECTION.                                                 DHELP01P
005000 01  DFHCOMMAREA.                                                 DHELP01P
005100   05  LK-COMMAREA                           PIC X(1)             DHELP01P
005200         OCCURS 1 TO 4096 TIMES                                   DHELP01P
005300           DEPENDING ON WS-COMMAREA-LENGTH.                       DHELP01P
005400                                                                  DHELP01P
005500 COPY CENTRY.                                                     DHELP01P
005600***************************************************************** DHELP01P
005700* Move the passed data to our area                              * DHELP01P
005800***************************************************************** DHELP01P
005900     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DHELP01P
006000     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DHELP01P
006100                                                                  DHELP01P
006200***************************************************************** DHELP01P
006300* Initialize our output area                                    * DHELP01P
006400***************************************************************** DHELP01P
006500     MOVE SPACES TO HELP01O-DATA.                                 DHELP01P
006600     MOVE HELP01I-SCRN TO HELP01O-SCRN.                           DHELP01P
006700     SET HELP-FOUND TO TRUE.                                      DHELP01P
006800                                                                  DHELP01P
006900***************************************************************** DHELP01P
007000* Set up start position for the browse                          * DHELP01P
007100***************************************************************** DHELP01P
007200     MOVE HELP01I-SCRN TO WS-HELP-RID (1:6).                      DHELP01P
007300     MOVE SPACES TO WS-HELP-RID (7:2).                            DHELP01P
007400     MOVE HELP01I-SCRN TO HELP01O-SCRN.                           DHELP01P
007500                                                                  DHELP01P
007600***************************************************************** DHELP01P
007700* Start browsing the file                                       * DHELP01P
007800***************************************************************** DHELP01P
007900     EXEC CICS STARTBR FILE('BNKHELP')                            DHELP01P
008000                      RIDFLD(WS-HELP-RID)                         DHELP01P
008100                      GTEQ                                        DHELP01P
008200     END-EXEC.                                                    DHELP01P
008300                                                                  DHELP01P
008400***************************************************************** DHELP01P
008500* Now browse the selected rows are move up to 19 into our area  * DHELP01P
008600***************************************************************** DHELP01P
008700     MOVE 0 TO WS-SUB1.                                           DHELP01P
008800 TEXT-FETCH-LOOP.                                                 DHELP01P
008900     ADD 1 TO WS-SUB1.                                            DHELP01P
009000     IF WS-SUB1 IS GREATER THAN 19                                DHELP01P
009100        GO TO TEXT-FETCH-LOOP-EXIT.                               DHELP01P
009200     EXEC CICS READNEXT FILE('BNKHELP')                           DHELP01P
009300                        INTO(WS-HELP-REC)                         DHELP01P
009400                        LENGTH(LENGTH OF WS-HELP-REC)             DHELP01P
009500                        RIDFLD(WS-HELP-RID)                       DHELP01P
009600                        RESP(WS-RESP)                             DHELP01P
009700     END-EXEC.                                                    DHELP01P
009800                                                                  DHELP01P
009900***************************************************************** DHELP01P
010000* Did we get the record OK                                      * DHELP01P
010100***************************************************************** DHELP01P
010200     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       DHELP01P
010300        IF HLP-SCRN IS EQUAL TO HELP01I-SCRN                      DHELP01P
010400           MOVE HLP-LINE TO WS-LINE                               DHELP01P
010500           IF WS-LINE IS NUMERIC                                  DHELP01P
010600              IF WS-LINE-N IS GREATER THAN 00 AND                 DHELP01P
010700                 WS-LINE-N IS NOT GREATER THAN 19                 DHELP01P
010800                 MOVE HLP-TEXT TO HELP01O-LINE (WS-LINE-N)        DHELP01P
010900              END-IF                                              DHELP01P
011000           END-IF                                                 DHELP01P
011100           GO TO TEXT-FETCH-LOOP                                  DHELP01P
011200        ELSE                                                      DHELP01P
011300           IF HELP01O-INDIVIDUAL-LINES IS EQUAL TO SPACES         DHELP01P
011400              SET HELP-NOT-FOUND TO TRUE                          DHELP01P
011500              MOVE 'No help available'                            DHELP01P
011600                TO HELP01O-LINE (10) (30:17)                      DHELP01P
011700           END-IF                                                 DHELP01P
011800     ELSE                                                         DHELP01P
011900        IF HELP01O-INDIVIDUAL-LINES IS EQUAL TO SPACES            DHELP01P
012000           SET HELP-NOT-FOUND TO TRUE                             DHELP01P
012100           MOVE 'No help available' TO HELP01O-LINE (10) (30:17)  DHELP01P
012200        END-IF                                                    DHELP01P
012300        GO TO TEXT-FETCH-LOOP-EXIT                                DHELP01P
012400     END-IF.                                                      DHELP01P
012500                                                                  DHELP01P
012600***************************************************************** DHELP01P
012700* We quit the loop for some reason                              * DHELP01P
012800***************************************************************** DHELP01P
012900 TEXT-FETCH-LOOP-EXIT.                                            DHELP01P
013000     EXEC CICS ENDBR FILE('BNKHELP')                              DHELP01P
013100     END-EXEC.                                                    DHELP01P
013200                                                                  DHELP01P
013300***************************************************************** DHELP01P
013400* Move the result back to the callers area                      * DHELP01P
013500***************************************************************** DHELP01P
013600     MOVE WS-COMMAREA TO DFHCOMMAREA (1:WS-COMMAREA-LENGTH).      DHELP01P
013700                                                                  DHELP01P
013800***************************************************************** DHELP01P
013900* Return to our caller                                          * DHELP01P
014000***************************************************************** DHELP01P
014100 COPY CRETURN.                                                    DHELP01P
014200                                                                  DHELP01P
