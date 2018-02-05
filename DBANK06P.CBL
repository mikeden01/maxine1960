000100***************************************************************** DBANK06P
000200*                                                               * DBANK06P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK06P
000400*   This demonstration program is provided for use by users     * DBANK06P
000500*   of Micro Focus products and may be used, modified and       * DBANK06P
000600*   distributed as part of your application provided that       * DBANK06P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK06P
000800*   in this material.                                           * DBANK06P
000900*                                                               * DBANK06P
001000***************************************************************** DBANK06P
001100                                                                  DBANK06P
001200***************************************************************** DBANK06P
001300* Program:     DBANK06P.CBL                                     * DBANK06P
001400* Function:    Write transaction records for audit trail        * DBANK06P
001500*              SQL version                                      * DBANK06P
001600***************************************************************** DBANK06P
001700                                                                  DBANK06P
001800 IDENTIFICATION DIVISION.                                         DBANK06P
001900 PROGRAM-ID.                                                      DBANK06P
002000     DBANK06P.                                                    DBANK06P
002100 DATE-WRITTEN.                                                    DBANK06P
002200     September 2002.                                              DBANK06P
002300 DATE-COMPILED.                                                   DBANK06P
002400     Today.                                                       DBANK06P
002500                                                                  DBANK06P
002600 ENVIRONMENT DIVISION.                                            DBANK06P
002700                                                                  DBANK06P
002800 DATA DIVISION.                                                   DBANK06P
002900                                                                  DBANK06P
003000 WORKING-STORAGE SECTION.                                         DBANK06P
004100     EXEC SQL                                                     DBANK04P
004200          BEGIN DECLARE SECTION                                   DBANK04P
004300     END-EXEC.                                                    DBANK04P
003100 01  WS-MISC-STORAGE.                                             DBANK06P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK06P
003300       VALUE 'DBANK06P'.                                          DBANK06P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK06P
003500   05  WS-SUB1                               PIC S9(4) COMP.      DBANK06P
003600   05  WS-SUB2                               PIC S9(4) COMP.      DBANK06P
003700   05  WS-TRANS-COUNT                        PIC S9(9) COMP.      DBANK06P
003800                                                                  DBANK06P
003900 01  WS-COMMAREA.                                                 DBANK06P
004000     EXEC SQL                                                     DBANK06P
004100          INCLUDE CBANKD06                                        DBANK06P
004200     END-EXEC.                                                    DBANK06P
004300                                                                  DBANK06P
004700     EXEC SQL                                                     DBANK06P
004800          INCLUDE CBANKSTX                                        DBANK06P
004900     END-EXEC.                                                    DBANK06P
005000     EXEC SQL                                                     DBANK06P
005100          INCLUDE CBANKTXD                                        DBANK06P
005200     END-EXEC.                                                    DBANK06P
005300     EXEC SQL                                                     DBANK06P
005400          INCLUDE SQLCA                                           DBANK06P
005500     END-EXEC.                                                    DBANK06P
005600                                                                  DBANK06P
005700 COPY CABENDD.                                                    DBANK06P
004100     EXEC SQL                                                     DBANK04P
004200          END DECLARE SECTION                                     DBANK04P
004300     END-EXEC.                                                    DBANK04P
005800                                                                  DBANK06P
005900 LINKAGE SECTION.                                                 DBANK06P
006000 01  DFHCOMMAREA.                                                 DBANK06P
006100   05  LK-COMMAREA                           PIC X(1)             DBANK06P
006200         OCCURS 1 TO 6144 TIMES                                   DBANK06P
006300           DEPENDING ON WS-COMMAREA-LENGTH.                       DBANK06P
006400                                                                  DBANK06P
006500 COPY CENTRY.                                                     DBANK06P
006600***************************************************************** DBANK06P
006700* Move the passed data to our area                              * DBANK06P
006800***************************************************************** DBANK06P
006900     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK06P
007000     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK06P
007100                                                                  DBANK06P
007200***************************************************************** DBANK06P
007300* Initialize our output area                                    * DBANK06P
007400***************************************************************** DBANK06P
007500     MOVE SPACES TO CD06O-DATA.                                   DBANK06P
007600                                                                  DBANK06P
007700***************************************************************** DBANK06P
007800* Insert two rows/records into the database/file                * DBANK06P
007900***************************************************************** DBANK06P
008000***************************************************************** DBANK06P
008100* First row/record is for the from-to transaction               * DBANK06P
008200***************************************************************** DBANK06P
008300     MOVE '0' TO CD06I-TIMESTAMP (26:1).                          DBANK06P
008400     MOVE CD06I-FROM-DESC TO TXN-T1-OLD-DESC.                     DBANK06P
008500     EXEC SQL                                                     DBANK06P
008600          INSERT                                                  DBANK06P
008700          INTO BNKTXN (BTX_PID,                                   DBANK06P
008800                       BTX_TYPE,                                  DBANK06P
008900                       BTX_SUB_TYPE,                              DBANK06P
009000                       BTX_ACCNO,                                 DBANK06P
009100                       BTX_TIMESTAMP,                             DBANK06P
009200                       BTX_AMOUNT,                                DBANK06P
009300                       BTX_DATA_OLD)                              DBANK06P
009400          VALUES (:CD06I-PERSON-PID,                              DBANK06P
009500                  '1',                                            DBANK06P
009600                  '1',                                            DBANK06P
009700                  :CD06I-FROM-ACC,                                DBANK06P
009800                  :CD06I-TIMESTAMP,                               DBANK06P
009900                  :CD06I-FROM-AMOUNT,                             DBANK06P
010000                  :TXN-T1-OLD-DESC)                               DBANK06P
010100     END-EXEC.                                                    DBANK06P
010200                                                                  DBANK06P
010300***************************************************************** DBANK06P
010400* Did we get the record OK                                      * DBANK06P
010500***************************************************************** DBANK06P
010600     IF SQLCODE IS NOT EQUAL TO ZERO                              DBANK06P
010700        SET CD06O-UPDATE-FAIL TO TRUE                             DBANK06P
010800        MOVE 'Unable to insert FROM transaction record'           DBANK06P
010900          TO CD06O-MSG                                            DBANK06P
011000     END-IF.                                                      DBANK06P
011100                                                                  DBANK06P
011200***************************************************************** DBANK06P
011300* Second row/record is for the from-to transaction              * DBANK06P
011400***************************************************************** DBANK06P
011500     MOVE '1' TO CD06I-TIMESTAMP (26:1).                          DBANK06P
011600     MOVE CD06I-TO-DESC TO TXN-T1-OLD-DESC.                       DBANK06P
011700     EXEC SQL                                                     DBANK06P
011800          INSERT                                                  DBANK06P
011900          INTO BNKTXN (BTX_PID,                                   DBANK06P
012000                       BTX_TYPE,                                  DBANK06P
012100                       BTX_SUB_TYPE,                              DBANK06P
012200                       BTX_ACCNO,                                 DBANK06P
012300                       BTX_TIMESTAMP,                             DBANK06P
012400                       BTX_AMOUNT,                                DBANK06P
012500                       BTX_DATA_OLD)                              DBANK06P
012600          VALUES (:CD06I-PERSON-PID,                              DBANK06P
012700                  '1',                                            DBANK06P
012800                  '2',                                            DBANK06P
012900                  :CD06I-TO-ACC,                                  DBANK06P
013000                  :CD06I-TIMESTAMP,                               DBANK06P
013100                  :CD06I-TO-AMOUNT,                               DBANK06P
013200                  :TXN-T1-OLD-DESC)                               DBANK06P
013300     END-EXEC.                                                    DBANK06P
013400                                                                  DBANK06P
013500***************************************************************** DBANK06P
013600* Did we get the record OK                                      * DBANK06P
013700***************************************************************** DBANK06P
013800     IF SQLCODE IS NOT EQUAL TO ZERO                              DBANK06P
013900        SET CD06O-UPDATE-FAIL TO TRUE                             DBANK06P
014000        MOVE 'Unable to insert TO transaction record'             DBANK06P
014100          TO CD06O-MSG                                            DBANK06P
014200     END-IF.                                                      DBANK06P
014300                                                                  DBANK06P
014400                                                                  DBANK06P
014500***************************************************************** DBANK06P
014600* Move the result back to the callers area                      * DBANK06P
014700***************************************************************** DBANK06P
014800     MOVE WS-COMMAREA TO DFHCOMMAREA (1:WS-COMMAREA-LENGTH).      DBANK06P
014900                                                                  DBANK06P
015000***************************************************************** DBANK06P
015100* Return to our caller                                          * DBANK06P
015200***************************************************************** DBANK06P
015300 COPY CRETURN.                                                    DBANK06P
015400                                                                  DBANK06P
