000100***************************************************************** DBANK07P
000200*                                                               * DBANK07P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK07P
000400*   This demonstration program is provided for use by users     * DBANK07P
000500*   of Micro Focus products and may be used, modified and       * DBANK07P
000600*   distributed as part of your application provided that       * DBANK07P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK07P
000800*   in this material.                                           * DBANK07P
000900*                                                               * DBANK07P
001000***************************************************************** DBANK07P
001100                                                                  DBANK07P
001200***************************************************************** DBANK07P
001300* Program:     DBANK07P.CBL                                     * DBANK07P
001400* Function:    Write transaction records for audit trail        * DBANK07P
001500*              SQL version                                      * DBANK07P
001600***************************************************************** DBANK07P
001700                                                                  DBANK07P
001800 IDENTIFICATION DIVISION.                                         DBANK07P
001900 PROGRAM-ID.                                                      DBANK07P
002000     DBANK07P.                                                    DBANK07P
002100 DATE-WRITTEN.                                                    DBANK07P
002200     September 2002.                                              DBANK07P
002300 DATE-COMPILED.                                                   DBANK07P
002400     Today.                                                       DBANK07P
002500                                                                  DBANK07P
002600 ENVIRONMENT DIVISION.                                            DBANK07P
002700                                                                  DBANK07P
002800 DATA DIVISION.                                                   DBANK07P
002900                                                                  DBANK07P
003000 WORKING-STORAGE SECTION.                                         DBANK07P
004100     EXEC SQL                                                     DBANK04P
004200          BEGIN DECLARE SECTION                                   DBANK04P
004300     END-EXEC.                                                    DBANK04P
003100
       01  WS-MISC-STORAGE.                                             DBANK07P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK07P
003300       VALUE 'DBANK07P'.                                          DBANK07P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK07P
003500   05  WS-SUB1                               PIC S9(4) COMP.      DBANK07P
003600   05  WS-SUB2                               PIC S9(4) COMP.      DBANK07P
003700*  05  WS-TRANS-COUNT                        PIC S9(9) COMP.      DBANK07P
003800                                                                  DBANK07P
003900 01  WS-COMMAREA.                                                 DBANK07P
004000     EXEC SQL                                                     DBANK07P
004100          INCLUDE CBANKD07                                        DBANK07P
004200     END-EXEC.                                                    DBANK07P
004300                                                                  DBANK07P
004700     EXEC SQL                                                     DBANK07P
004800          INCLUDE CBANKSTX                                        DBANK07P
004900     END-EXEC.                                                    DBANK07P
005000     EXEC SQL                                                     DBANK07P
005100          INCLUDE CBANKTXD                                        DBANK07P
005200     END-EXEC.                                                    DBANK07P
005300     EXEC SQL                                                     DBANK07P
005400          INCLUDE SQLCA                                           DBANK07P
005500     END-EXEC.                                                    DBANK07P
005600                                                                  DBANK07P
005700 COPY CTSTAMPD.                                                   DBANK07P
005800                                                                  DBANK07P
005900 COPY CABENDD.                                                    DBANK07P
           EXEC SQL                                                     DBANK01P
004200          END DECLARE SECTION                                     DBANK01P
004300     END-EXEC.                                                    DBANK01P
006000                                                                  DBANK07P
006100 LINKAGE SECTION.                                                 DBANK07P
006200 01  DFHCOMMAREA.                                                 DBANK07P
006300   05  LK-COMMAREA                           PIC X(1)             DBANK07P
006400         OCCURS 1 TO 6144 TIMES                                   DBANK07P
006500           DEPENDING ON WS-COMMAREA-LENGTH.                       DBANK07P
006600                                                                  DBANK07P
006700 COPY CENTRY.                                                     DBANK07P
006800***************************************************************** DBANK07P
006900* Move the passed data to our area                              * DBANK07P
007000***************************************************************** DBANK07P
007100     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK07P
007200     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK07P
007300                                                                  DBANK07P
007400***************************************************************** DBANK07P
007500* Initialize our output area                                    * DBANK07P
007600***************************************************************** DBANK07P
007700     MOVE SPACES TO CD07O-DATA.                                   DBANK07P
007800                                                                  DBANK07P
007900***************************************************************** DBANK07P
008000* Insert row/record into the database/file                      * DBANK07P
008100***************************************************************** DBANK07P
008200*COPY CTSTAMPP.                                                   DBANK07P
008300*    MOVE WS-TIMESTAMP TO CD07I-TIMESTAMP                         DBANK07P
008400     EXEC SQL                                                     DBANK07P
008500       SET :CD07I-TIMESTAMP = CURRENT TIMESTAMP                   DBANK07P
008600     END-EXEC                                                     DBANK07P
008700     MOVE '0' TO CD07I-TIMESTAMP (26:1).                          DBANK07P
008800     EXEC SQL                                                     DBANK07P
008900          INSERT                                                  DBANK07P
009000          INTO BNKTXN (BTX_PID,                                   DBANK07P
009100                       BTX_TYPE,                                  DBANK07P
009200                       BTX_SUB_TYPE,                              DBANK07P
009300                       BTX_TIMESTAMP,                             DBANK07P
009400                       BTX_DATA_OLD,                              DBANK07P
009500                       BTX_DATA_NEW)                              DBANK07P
009600          VALUES (:CD07I-PERSON-PID,                              DBANK07P
009700                  '2',                                            DBANK07P
009800                  ' ',                                            DBANK07P
009900                  :CD07I-TIMESTAMP,                               DBANK07P
010000                  :CD07I-OLD-DATA,                                DBANK07P
010100                  :CD07I-NEW-DATA)                                DBANK07P
010200     END-EXEC.                                                    DBANK07P
010300                                                                  DBANK07P
010400***************************************************************** DBANK07P
010500* Did we get the record OK                                      * DBANK07P
010600***************************************************************** DBANK07P
010700     IF SQLCODE IS NOT EQUAL TO ZERO                              DBANK07P
010800        SET CD07O-UPDATE-FAIL TO TRUE                             DBANK07P
010900        MOVE 'Unable to insert contact info audit record'         DBANK07P
011000          TO CD07O-MSG                                            DBANK07P
011100     END-IF.                                                      DBANK07P
011200                                                                  DBANK07P
011300***************************************************************** DBANK07P
011400* Move the result back to the callers area                      * DBANK07P
011500***************************************************************** DBANK07P
011600 FINISH.                                                          DBANK07P
011700     MOVE WS-COMMAREA TO DFHCOMMAREA (1:WS-COMMAREA-LENGTH).      DBANK07P
011800                                                                  DBANK07P
011900***************************************************************** DBANK07P
012000* Return to our caller                                          * DBANK07P
012100***************************************************************** DBANK07P
012200 COPY CRETURN.                                                    DBANK07P
012300                                                                  DBANK07P
