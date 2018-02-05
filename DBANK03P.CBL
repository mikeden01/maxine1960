000100***************************************************************** DBANK03P
000200*                                                               * DBANK03P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK03P
000400*   This demonstration program is provided for use by users     * DBANK03P
000500*   of Micro Focus products and may be used, modified and       * DBANK03P
000600*   distributed as part of your application provided that       * DBANK03P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK03P
000800*   in this material.                                           * DBANK03P
000900*                                                               * DBANK03P
001000***************************************************************** DBANK03P
001100                                                                  DBANK03P
001200***************************************************************** DBANK03P
001300* Program:     DBANK03P.CBL                                     * DBANK03P
001400* Function:    Obtain Bank Account balances                     * DBANK03P
001500*              SQL version                                      * DBANK03P
001600***************************************************************** DBANK03P
001700                                                                  DBANK03P
001800 IDENTIFICATION DIVISION.                                         DBANK03P
001900 PROGRAM-ID.                                                      DBANK03P
002000     DBANK03P.                                                    DBANK03P
002100 DATE-WRITTEN.                                                    DBANK03P
002200     September 2002.                                              DBANK03P
002300 DATE-COMPILED.                                                   DBANK03P
002400     Today.                                                       DBANK03P
002500                                                                  DBANK03P
002600 ENVIRONMENT DIVISION.                                            DBANK03P
002700                                                                  DBANK03P
002800 DATA DIVISION.                                                   DBANK03P
002900                                                                  DBANK03P
003000 WORKING-STORAGE SECTION.                                         DBANK03P
004100     EXEC SQL                                                     DBANK02P
004200          BEGIN DECLARE SECTION                                   DBANK02P
004300     END-EXEC.                                                    DBANK02P
003100
       01  WS-MISC-STORAGE.                                             DBANK03P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK03P
003300       VALUE 'DBANK03P'.                                          DBANK03P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK03P
003500   05  WS-SUB1                               PIC S9(4) COMP.      DBANK03P
003600   05  WS-COUNT                              PIC S9(10) COMP-3.   DBANK03P
003700   05  WS-TXN-TYPE                           PIC X(1)             DBANK03P
003800       VALUE '1'.                                                 DBANK03P
003900                                                                  DBANK03P
004000 01  WS-COMMAREA.                                                 DBANK03P
004100     EXEC SQL                                                     DBANK03P
004200          INCLUDE CBANKD03                                        DBANK03P
004300     END-EXEC.                                                    DBANK03P
004400                                                                  DBANK03P
004800     EXEC SQL                                                     DBANK03P
004900          INCLUDE CBANKSCS                                        DBANK03P
005000     END-EXEC.                                                    DBANK03P
005100     EXEC SQL                                                     DBANK03P
005200          INCLUDE CBANKSAC                                        DBANK03P
005300     END-EXEC.                                                    DBANK03P
005400     EXEC SQL                                                     DBANK03P
005500          INCLUDE CBANKSAT                                        DBANK03P
005600     END-EXEC.                                                    DBANK03P
005700     EXEC SQL                                                     DBANK03P
005800          INCLUDE CBANKSTX                                        DBANK03P
005900     END-EXEC.                                                    DBANK03P
006000     EXEC SQL                                                     DBANK03P
006100          INCLUDE CBANKTXD                                        DBANK03P
006200     END-EXEC.                                                    DBANK03P
006300     EXEC SQL                                                     DBANK03P
006400          INCLUDE SQLCA                                           DBANK03P
006500     END-EXEC.                                                    DBANK03P
006600                                                                  DBANK03P
006700 COPY CABENDD.                                                    DBANK03P
004500     EXEC SQL                                                     DBANK03P
004600          END DECLARE SECTION                                     DBANK03P
004700     END-EXEC.                                                    DBANK03P
006800                                                                  DBANK03P
006900 LINKAGE SECTION.                                                 DBANK03P
007000 01  DFHCOMMAREA.                                                 DBANK03P
007100   05  LK-COMMAREA                           PIC X(1)             DBANK03P
007200       OCCURS 1 TO 6144 TIMES                                     DBANK03P
007300         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK03P
007400                                                                  DBANK03P
007500 COPY CENTRY.                                                     DBANK03P
007600***************************************************************** DBANK03P
007700* Move the passed data to our area                              * DBANK03P
007800***************************************************************** DBANK03P
007900     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK03P
008000     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK03P
008100                                                                  DBANK03P
008200***************************************************************** DBANK03P
008300* Initialize our output area                                    * DBANK03P
008400***************************************************************** DBANK03P
008500     MOVE SPACES TO CD03O-DATA.                                   DBANK03P
008600                                                                  DBANK03P
008700***************************************************************** DBANK03P
008800* Set up the cursor so we can get all our data                  * DBANK03P
008900***************************************************************** DBANK03P
009000     EXEC SQL                                                     DBANK03P
009100          DECLARE BAL_CSR CURSOR FOR                              DBANK03P
009200          SELECT BAC.BAC_ACCNO,                                   DBANK03P
009300                 BAT.BAT_DESC,                                    DBANK03P
009400                 BAC.BAC_BALANCE,                                 DBANK03P
009500                 BAC.BAC_LAST_STMT_DTE,                           DBANK03P
009600                 BAC.BAC_LAST_STMT_BAL                            DBANK03P
009700          FROM BNKACC BAC,                                        DBANK03P
009800               BNKATYPE BAT                                       DBANK03P
009900          WHERE ((BAC.BAC_ACCTYPE = BAT.BAT_TYPE) AND             DBANK03P
010000                 (BAC.BAC_PID = :CD03I-CONTACT-ID))               DBANK03P
010100          ORDER BY BAT.BAT_DESC ASC                               DBANK03P
010200          FOR FETCH ONLY                                          DBANK03P
010300     END-EXEC.                                                    DBANK03P
010400                                                                  DBANK03P
010500     EXEC SQL                                                     DBANK03P
010600          OPEN BAL_CSR                                            DBANK03P
010700     END-EXEC                                                     DBANK03P
010800                                                                  DBANK03P
010900***************************************************************** DBANK03P
011000* Now browse the selected rows and move up to 5 into our area   * DBANK03P
011100***************************************************************** DBANK03P
011200     MOVE 0 TO WS-SUB1.                                           DBANK03P
011300 ACCOUNT-FETCH-LOOP.                                              DBANK03P
011400     ADD 1 TO WS-SUB1.                                            DBANK03P
011500     IF WS-SUB1 IS GREATER THAN 5                                 DBANK03P
011600        GO TO ACCOUNT-FETCH-LOOP-EXIT                             DBANK03P
011700     END-IF.                                                      DBANK03P
011800     EXEC SQL                                                     DBANK03P
011900          FETCH BAL_CSR                                           DBANK03P
012000          INTO :DCL-BAC-ACCNO,                                    DBANK03P
012100               :DCL-BAT-DESC,                                     DBANK03P
012200               :DCL-BAC-BALANCE,                                  DBANK03P
012300               :DCL-BAC-LAST-STMT-DTE,                            DBANK03P
012400               :DCL-BAC-LAST-STMT-BAL                             DBANK03P
012500     END-EXEC.                                                    DBANK03P
012600                                                                  DBANK03P
012700***************************************************************** DBANK03P
012800* Did we get the record OK                                      * DBANK03P
012900***************************************************************** DBANK03P
013000     IF SQLCODE IS EQUAL TO ZERO                                  DBANK03P
013100        MOVE DCL-BAC-ACCNO TO CD03O-ACC-NO (WS-SUB1)              DBANK03P
013200        MOVE DCL-BAT-DESC TO CD03O-ACC-DESC (WS-SUB1)             DBANK03P
013300        MOVE DCL-BAC-BALANCE TO CD03O-ACC-BAL-N (WS-SUB1)         DBANK03P
013400        MOVE DCL-BAC-LAST-STMT-DTE TO CD03O-DTE (WS-SUB1)         DBANK03P
013500        EXEC SQL                                                  DBANK03P
013600             SELECT COUNT(*)                                      DBANK03P
013700             INTO :WS-COUNT                                       DBANK03P
013800             FROM BNKTXN                                          DBANK03P
013900             WHERE (BTX_ACCNO = :DCL-BAC-ACCNO) AND               DBANK03P
014000                   (BTX_TYPE = :WS-TXN-TYPE)                      DBANK03P
014100        END-EXEC                                                  DBANK03P
014200        IF WS-COUNT IS EQUAL TO ZERO                              DBANK03P
014300           MOVE SPACE TO CD03O-TXN (WS-SUB1)                      DBANK03P
014400        ELSE                                                      DBANK03P
014500           MOVE '*' TO CD03O-TXN (WS-SUB1)                        DBANK03P
014600        END-IF                                                    DBANK03P
014700        GO TO ACCOUNT-FETCH-LOOP                                  DBANK03P
014800     ELSE                                                         DBANK03P
014900        GO TO ACCOUNT-FETCH-LOOP-EXIT                             DBANK03P
015000     END-IF.                                                      DBANK03P
015100                                                                  DBANK03P
015200***************************************************************** DBANK03P
015300* We quit the loop for some reason                              * DBANK03P
015400***************************************************************** DBANK03P
015500 ACCOUNT-FETCH-LOOP-EXIT.                                         DBANK03P
015600     EXEC SQL                                                     DBANK03P
015700          CLOSE BAL_CSR                                           DBANK03P
015800     END-EXEC.                                                    DBANK03P
015900                                                                  DBANK03P
016000***************************************************************** DBANK03P
016100* Move the result back to the callers area                      * DBANK03P
016200***************************************************************** DBANK03P
016300     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK03P
016400                                                                  DBANK03P
016500***************************************************************** DBANK03P
016600* Return to our caller                                          * DBANK03P
016700***************************************************************** DBANK03P
016800 COPY CRETURN.                                                    DBANK03P
016900                                                                  DBANK03P
