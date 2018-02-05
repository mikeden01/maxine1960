000100***************************************************************** DBANK01P
000200*                                                               * DBANK01P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK01P
000400*   This demonstration program is provided for use by users     * DBANK01P
000500*   of Micro Focus products and may be used, modified and       * DBANK01P
000600*   distributed as part of your application provided that       * DBANK01P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK01P
000800*   in this material.                                           * DBANK01P
000900*                                                               * DBANK01P
001000***************************************************************** DBANK01P
001100                                                                  DBANK01P
001200***************************************************************** DBANK01P
001300* Program:     DBANK01P.CBL                                     * DBANK01P
001400* Function:    Obtain User details                              * DBANK01P
001500*              SQL version                                      * DBANK01P
001600***************************************************************** DBANK01P
001700                                                                  DBANK01P
001800 IDENTIFICATION DIVISION.                                         DBANK01P
001900 PROGRAM-ID.                                                      DBANK01P
002000     DBANK01P.                                                    DBANK01P
002100 DATE-WRITTEN.                                                    DBANK01P
002200     September 2002.                                              DBANK01P
002300 DATE-COMPILED.                                                   DBANK01P
002400     Today.                                                       DBANK01P
002500                                                                  DBANK01P
002600 ENVIRONMENT DIVISION.                                            DBANK01P
002700                                                                  DBANK01P
002800 DATA DIVISION.                                                   DBANK01P
002900                                                                  DBANK01P
003000 WORKING-STORAGE SECTION.                                         DBANK01P
003100 01  WS-MISC-STORAGE.                                             DBANK01P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK01P
003300       VALUE 'DBANK01P'.                                          DBANK01P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK01P
003500   05  SQLCODE-DISP                          PIC 9(5).            DBANK01P
004100
           EXEC SQL                                                     DBANK01P
004200          BEGIN DECLARE SECTION                                   DBANK01P
004300     END-EXEC.                                                    DBANK01P
003600
       01  WS-COMMAREA.                                                 DBANK01P
003700     EXEC SQL                                                     DBANK01P
003800          INCLUDE CBANKD01                                        DBANK01P
003900     END-EXEC.                                                    DBANK01P
004000                                                                  DBANK01P
004400     EXEC SQL                                                     DBANK01P
004500          INCLUDE CBANKSCS                                        DBANK01P
004600     END-EXEC.                                                    DBANK01P
004700     EXEC SQL                                                     DBANK01P
004800          INCLUDE SQLCA                                           DBANK01P
004900     END-EXEC.                                                    DBANK01P
005000                                                                  DBANK01P
005100 COPY CABENDD.                                                    DBANK01P
           EXEC SQL                                                     DBANK01P
004200          END DECLARE SECTION                                     DBANK01P
004300     END-EXEC.                                                    DBANK01P
005200                                                                  DBANK01P
005300 LINKAGE SECTION.                                                 DBANK01P
005400 01  DFHCOMMAREA.                                                 DBANK01P
005500   05  LK-COMMAREA                           PIC X(1)             DBANK01P
005600       OCCURS 1 TO 6144 TIMES                                     DBANK01P
005700         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK01P
005800                                                                  DBANK01P
005900 COPY CENTRY.                                                     DBANK01P
006000***************************************************************** DBANK01P
006100* Move the passed data to our area                              * DBANK01P
006200***************************************************************** DBANK01P
006300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK01P
006400     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK01P
006500                                                                  DBANK01P
006600***************************************************************** DBANK01P
006700* Initialize our output area                                    * DBANK01P
006800***************************************************************** DBANK01P
006900     MOVE SPACES TO CD01O-DATA.                                   DBANK01P
007000                                                                  DBANK01P
007100***************************************************************** DBANK01P
007200* Now attempt to get the requested record                       * DBANK01P
007300***************************************************************** DBANK01P
007400     EXEC SQL                                                     DBANK01P
007500          SELECT CS.BCS_PID,                                      DBANK01P
007600                 CS.BCS_NAME                                      DBANK01P
007700          INTO :DCL-BCS-PID,                                      DBANK01P
007800               :DCL-BCS-NAME                                      DBANK01P
007900          FROM BNKCUST CS                                         DBANK01P
008000          WHERE CS.BCS_PID = :CD01I-PERSON-PID                    DBANK01P
008100     END-EXEC.                                                    DBANK01P
008200                                                                  DBANK01P
008600     EVALUATE SQLCODE
008300***************************************************************** DBANK01P
008400* Did we get the record OK                                      * DBANK01P
008500***************************************************************** DBANK01P
              WHEN ZERO                                                 DBANK01P
008700             MOVE DCL-BCS-PID TO CD01O-PERSON-PID                 DBANK01P
008800             MOVE DCL-BCS-NAME TO CD01O-PERSON-NAME               DBANK01P
008900        WHEN +100                                                 DBANK01P
009100***************************************************************** DBANK01P
009200* Was the record not found?                                     * DBANK01P
009300***************************************************************** DBANK01P
009500             MOVE SPACES TO CD01O-PERSON-PID                      DBANK01P
009600             MOVE 'Person not found' TO CD01O-PERSON-NAME         DBANK01P
009700        WHEN OTHER                                                DBANK01P
009500             MOVE SPACES TO CD01O-PERSON-PID                      DBANK01P
009600             MOVE SQLCODE to SQLCODE-DISP
                   STRING 'SQLCODE : ' delimited by size
                          SQLCODE-DISP delimited by size
                     INTO CD01O-PERSON-NAME                             DBANK01P
009800             END-STRING
           END-EVALUATE                                                 DBANK01P
009900***************************************************************** DBANK01P
010000* Move the result back to the callers area                      * DBANK01P
010100***************************************************************** DBANK01P
010200     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK01P
010300                                                                  DBANK01P
010400***************************************************************** DBANK01P
010500* Return to our caller                                          * DBANK01P
010600***************************************************************** DBANK01P
010700 COPY CRETURN.                                                    DBANK01P
010800                                                                  DBANK01P
