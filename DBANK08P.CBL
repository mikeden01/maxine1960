000100***************************************************************** DBANK08P
000200*                                                               * DBANK08P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK08P
000400*   This demonstration program is provided for use by users     * DBANK08P
000500*   of Micro Focus products and may be used, modified and       * DBANK08P
000600*   distributed as part of your application provided that       * DBANK08P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK08P
000800*   in this material.                                           * DBANK08P
000900*                                                               * DBANK08P
001000***************************************************************** DBANK08P
001100                                                                  DBANK08P
001200***************************************************************** DBANK08P
001300* Program:     DBANK08P.CBL                                     * DBANK08P
001400* Function:    Obtain count of number of accounts user has      * DBANK08P
001500*              SQL version                                      * DBANK08P
001600***************************************************************** DBANK08P
001700                                                                  DBANK08P
001800 IDENTIFICATION DIVISION.                                         DBANK08P
001900 PROGRAM-ID.                                                      DBANK08P
002000     DBANK08P.                                                    DBANK08P
002100 DATE-WRITTEN.                                                    DBANK08P
002200     September 2002.                                              DBANK08P
002300 DATE-COMPILED.                                                   DBANK08P
002400     Today.                                                       DBANK08P
002500                                                                  DBANK08P
002600 ENVIRONMENT DIVISION.                                            DBANK08P
002700                                                                  DBANK08P
002800 DATA DIVISION.                                                   DBANK08P
002900                                                                  DBANK08P
003000 WORKING-STORAGE SECTION.                                         DBANK08P
004500     EXEC SQL                                                     DBANK08P
004600          BEGIN DECLARE SECTION                                   DBANK08P
004700     END-EXEC.                                                    DBANK08P
003100
       01  WS-MISC-STORAGE.                                             DBANK08P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK08P
003300       VALUE 'DBANK08P'.                                          DBANK08P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK08P
003500   05  WS-SUB1                               PIC S9(4) COMP.      DBANK08P
003600   05  WS-COUNT                              PIC S9(10) COMP-3.   DBANK08P
003700   05  WS-TXN-TYPE                           PIC X(1)             DBANK08P
003800       VALUE '1'.                                                 DBANK08P
003900                                                                  DBANK08P
004000 01  WS-COMMAREA.                                                 DBANK08P
004100     EXEC SQL                                                     DBANK08P
004200          INCLUDE CBANKD08                                        DBANK08P
004300     END-EXEC.                                                    DBANK08P
004400                                                                  DBANK08P
004800     EXEC SQL                                                     DBANK08P
004900          INCLUDE CBANKSAC                                        DBANK08P
005000     END-EXEC.                                                    DBANK08P
005100     EXEC SQL                                                     DBANK08P
005200          INCLUDE CBANKSTX                                        DBANK08P
005300     END-EXEC.                                                    DBANK08P
005400     EXEC SQL                                                     DBANK08P
005500          INCLUDE CBANKTXD                                        DBANK08P
005600     END-EXEC.                                                    DBANK08P
005700     EXEC SQL                                                     DBANK08P
005800          INCLUDE SQLCA                                           DBANK08P
005900     END-EXEC.                                                    DBANK08P
006000                                                                  DBANK08P
006100 COPY CABENDD.                                                    DBANK08P
           EXEC SQL                                                     DBANK01P
004200          END DECLARE SECTION                                     DBANK01P
004300     END-EXEC.                                                    DBANK01P
006200                                                                  DBANK08P
006300 LINKAGE SECTION.                                                 DBANK08P
006400 01  DFHCOMMAREA.                                                 DBANK08P
006500   05  LK-COMMAREA                           PIC X(1)             DBANK08P
006600       OCCURS 1 TO 6144 TIMES                                     DBANK08P
006700         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK08P
006800                                                                  DBANK08P
006900 COPY CENTRY.                                                     DBANK08P
007000***************************************************************** DBANK08P
007100* Move the passed data to our area                              * DBANK08P
007200***************************************************************** DBANK08P
007300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK08P
007400     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK08P
007500                                                                  DBANK08P
007600***************************************************************** DBANK08P
007700* Initialize our output area                                    * DBANK08P
007800***************************************************************** DBANK08P
007900     MOVE SPACES TO CD08O-DATA.                                   DBANK08P
008000                                                                  DBANK08P
008100***************************************************************** DBANK08P
008200* Count how many acounts this person has                        * DBANK08P
008300***************************************************************** DBANK08P
008400     MOVE ZERO TO WS-COUNT.                                       DBANK08P
008500     EXEC SQL                                                     DBANK08P
008600          SELECT COUNT(*)                                         DBANK08P
008700          INTO :WS-COUNT                                          DBANK08P
008800          FROM BNKACC BAC                                         DBANK08P
008900          WHERE BAC.BAC_PID = :CD08I-CONTACT-ID                   DBANK08P
009000     END-EXEC                                                     DBANK08P
009100     MOVE WS-COUNT TO CD08O-COUNT.                                DBANK08P
009200                                                                  DBANK08P
009300***************************************************************** DBANK08P
009400* Move the result back to the callers area                      * DBANK08P
009500***************************************************************** DBANK08P
009600     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK08P
009700                                                                  DBANK08P
009800***************************************************************** DBANK08P
009900* Return to our caller                                          * DBANK08P
010000***************************************************************** DBANK08P
010100 COPY CRETURN.                                                    DBANK08P
010200                                                                  DBANK08P
