000100***************************************************************** DBANK04P
000200*                                                               * DBANK04P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK04P
000400*   This demonstration program is provided for use by users     * DBANK04P
000500*   of Micro Focus products and may be used, modified and       * DBANK04P
000600*   distributed as part of your application provided that       * DBANK04P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK04P
000800*   in this material.                                           * DBANK04P
000900*                                                               * DBANK04P
001000***************************************************************** DBANK04P
001100                                                                  DBANK04P
001200***************************************************************** DBANK04P
001300* Program:     DBANK04P.CBL                                     * DBANK04P
001400* Function:    Update account balances                          * DBANK04P
001500*              SQL version                                      * DBANK04P
001600***************************************************************** DBANK04P
001700                                                                  DBANK04P
001800 IDENTIFICATION DIVISION.                                         DBANK04P
001900 PROGRAM-ID.                                                      DBANK04P
002000     DBANK04P.                                                    DBANK04P
002100 DATE-WRITTEN.                                                    DBANK04P
002200     September 2002.                                              DBANK04P
002300 DATE-COMPILED.                                                   DBANK04P
002400     Today.                                                       DBANK04P
002500                                                                  DBANK04P
002600 ENVIRONMENT DIVISION.                                            DBANK04P
002700                                                                  DBANK04P
002800 DATA DIVISION.                                                   DBANK04P
002900                                                                  DBANK04P
003000 WORKING-STORAGE SECTION.                                         DBANK04P
003100 01  WS-MISC-STORAGE.                                             DBANK04P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK04P
003300       VALUE 'DBANK04P'.                                          DBANK04P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK04P
004100
           EXEC SQL                                                     DBANK04P
004200          BEGIN DECLARE SECTION                                   DBANK04P
004300     END-EXEC.                                                    DBANK04P
003500                                                                  DBANK04P
003600 01  WS-COMMAREA.                                                 DBANK04P
003700     EXEC SQL                                                     DBANK04P
003800          INCLUDE CBANKD04                                        DBANK04P
003900     END-EXEC.                                                    DBANK04P
004000                                                                  DBANK04P
004400     EXEC SQL                                                     DBANK04P
004500          INCLUDE CBANKSAC                                        DBANK04P
004600     END-EXEC.                                                    DBANK04P
004700     EXEC SQL                                                     DBANK04P
004800          INCLUDE SQLCA                                           DBANK04P
004900     END-EXEC.                                                    DBANK04P
005000                                                                  DBANK04P
005100 COPY CTSTAMPD.                                                   DBANK04P
005200                                                                  DBANK04P
005300 COPY CABENDD.                                                    DBANK04P
004100     EXEC SQL                                                     DBANK04P
004200          END DECLARE SECTION                                     DBANK04P
004300     END-EXEC.                                                    DBANK04P
005400                                                                  DBANK04P
005500 LINKAGE SECTION.                                                 DBANK04P
005600 01  DFHCOMMAREA.                                                 DBANK04P
005700   05  LK-COMMAREA                           PIC X(1)             DBANK04P
005800       OCCURS 1 TO 6144 TIMES                                     DBANK04P
005900         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK04P
006000                                                                  DBANK04P
006100 COPY CENTRY.                                                     DBANK04P
006200***************************************************************** DBANK04P
006300* Move the passed data to our area                              * DBANK04P
006400***************************************************************** DBANK04P
006500     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK04P
006600     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK04P
006700                                                                  DBANK04P
006800***************************************************************** DBANK04P
006900* Initialize our output area                                    * DBANK04P
007000***************************************************************** DBANK04P
007100     MOVE SPACES TO CD04O-DATA.                                   DBANK04P
007200     SET CD04O-UPDATE-FAIL TO TRUE.                               DBANK04P
007300     MOVE '0001-01-01-00.00.00.000000' TO CD04O-TIMESTAMP.        DBANK04P
007400                                                                  DBANK04P
007500***************************************************************** DBANK04P
007600* Try to update the 'from' balance                              * DBANK04P
007700***************************************************************** DBANK04P
007800     EXEC SQL                                                     DBANK04P
007900          UPDATE BNKACC                                           DBANK04P
008000          SET BAC_BALANCE = :CD04I-FROM-NEW-BAL                   DBANK04P
008100          WHERE (BAC_PID = :CD04I-PERSON-PID AND                  DBANK04P
008200                 BAC_ACCNO = :CD04I-FROM-ACC AND                  DBANK04P
008300                 BAC_BALANCE = :CD04I-FROM-OLD-BAL)               DBANK04P
008400     END-EXEC.                                                    DBANK04P
008500                                                                  DBANK04P
008600***************************************************************** DBANK04P
008700* Did we update the record OK                                   * DBANK04P
008800***************************************************************** DBANK04P
008900     IF SQLCODE IS NOT EQUAL TO ZERO                              DBANK04P
009000        MOVE 'Unable to update FROM account details'              DBANK04P
009100          TO CD04O-MSG                                            DBANK04P
009200     GO TO DBANK04P-EXIT                                          DBANK04P
009300     END-IF.                                                      DBANK04P
009400                                                                  DBANK04P
009500***************************************************************** DBANK04P
009600* Try to update the 'to' balance                                * DBANK04P
009700***************************************************************** DBANK04P
009800     EXEC SQL                                                     DBANK04P
009900          UPDATE BNKACC                                           DBANK04P
010000          SET BAC_BALANCE = :CD04I-TO-NEW-BAL                     DBANK04P
010100          WHERE (BAC_PID = :CD04I-PERSON-PID AND                  DBANK04P
010200                 BAC_ACCNO = :CD04I-TO-ACC AND                    DBANK04P
010300                 BAC_BALANCE = :CD04I-TO-OLD-BAL)                 DBANK04P
010400     END-EXEC.                                                    DBANK04P
010500                                                                  DBANK04P
010600***************************************************************** DBANK04P
010700* Did we update the record OK                                   * DBANK04P
010800***************************************************************** DBANK04P
010900     IF SQLCODE IS NOT EQUAL TO ZERO                              DBANK04P
011000        MOVE 'Unable to update TO account details'                DBANK04P
011100          TO CD04O-MSG                                            DBANK04P
011200     GO TO DBANK04P-EXIT                                          DBANK04P
011300     END-IF.                                                      DBANK04P
011400                                                                  DBANK04P
011500***************************************************************** DBANK04P
011600* If we got this far then the accounts should have been updated * DBANK04P
011700***************************************************************** DBANK04P
011800*COPY CTSTAMPP.                                                   DBANK04P
011900*    MOVE WS-TIMESTAMP TO CD04O-TIMESTAMP                         DBANK04P
012000     EXEC SQL                                                     DBANK04P
012100          SET :CD04O-TIMESTAMP = CURRENT TIMESTAMP                DBANK04P
012200     END-EXEC.                                                    DBANK04P
012300     SET CD04O-UPDATE-OK TO TRUE.                                 DBANK04P
012400                                                                  DBANK04P
012500 DBANK04P-EXIT.                                                   DBANK04P
012600***************************************************************** DBANK04P
012700* Move the result back to the callers area                      * DBANK04P
012800***************************************************************** DBANK04P
012900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK04P
013000                                                                  DBANK04P
013100***************************************************************** DBANK04P
013200* Return to our caller                                          * DBANK04P
013300***************************************************************** DBANK04P
013400 COPY CRETURN.                                                    DBANK04P
013500                                                                  DBANK04P
