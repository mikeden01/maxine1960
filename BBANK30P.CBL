000100***************************************************************** BBANK30P
000200*                                                               * BBANK30P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK30P
000400*   This demonstration program is provided for use by users     * BBANK30P
000500*   of Micro Focus products and may be used, modified and       * BBANK30P
000600*   distributed as part of your application provided that       * BBANK30P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK30P
000800*   in this material.                                           * BBANK30P
000900*                                                               * BBANK30P
001000***************************************************************** BBANK30P
001100                                                                  BBANK30P
001200***************************************************************** BBANK30P
001300* Program:     BBANK30P.CBL                                     * BBANK30P
001400* Layer:       Business logic                                   * BBANK30P
001500* Function:    Display account balances                         * BBANK30P
001600***************************************************************** BBANK30P
001700                                                                  BBANK30P
001800 IDENTIFICATION DIVISION.                                         BBANK30P
001900 PROGRAM-ID.                                                      BBANK30P
002000     BBANK30P.                                                    BBANK30P
002100 DATE-WRITTEN.                                                    BBANK30P
002200     September 2002.                                              BBANK30P
002300 DATE-COMPILED.                                                   BBANK30P
002400     Today.                                                       BBANK30P
002500                                                                  BBANK30P
002600 ENVIRONMENT DIVISION.                                            BBANK30P
002700                                                                  BBANK30P
002800 DATA DIVISION.                                                   BBANK30P
002900 WORKING-STORAGE SECTION.                                         BBANK30P
003000 01  WS-MISC-STORAGE.                                             BBANK30P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK30P
003200       VALUE 'BBANK30P'.                                          BBANK30P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK30P
003400     88  INPUT-OK                            VALUE '0'.           BBANK30P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK30P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK30P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK30P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK30P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK30P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK30P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK30P
004200     88  PFK-VALID                           VALUE '0'.           BBANK30P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK30P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK30P
004500   05  WS-SUB1                               PIC S9(4) COMP.      BBANK30P
004600   05  WS-SUB-LIMIT                          PIC S9(4) COMP.      BBANK30P
004700   05  WS-EDIT-BALANCE                       PIC Z,ZZZ,ZZ9.99-.   BBANK30P
004800   05  WS-DYNAMIC-PGM                        PIC X(8)             BBANK30P
004900       VALUE 'UNKNOWN'.                                           BBANK30P
005000                                                                  BBANK30P
005100 01  WS-SERVICE-CHARGES.                                          BBANK30P
005200   05  WS-SRV-MSG.                                                BBANK30P
005300     10  FILLER                              PIC X(43)            BBANK30P
005400         VALUE 'Service charges are estimated based on your'.     BBANK30P
005500     10  FILLER                              PIC X(19)            BBANK30P
005600         VALUE ' existing balances '.                             BBANK30P
005700   05  WS-SRV-BAL                            PIC X(9).            BBANK30P
005800   05  WS-SRV-BAL-N REDEFINES WS-SRV-BAL     PIC S9(7)V99.        BBANK30P
005900   05  WS-SRV-CHARGE                         PIC ZZ9.99           BBANK30P
006000                                             BLANK ZERO.          BBANK30P
006100   05  WS-SRV-AMT                            PIC 9(3)V99.         BBANK30P
006200   05  WS-SRV-CHARGE-LIMITS.                                      BBANK30P
006300     10  WS-SRV-BAND0.                                            BBANK30P
006400       15  WS-SRV-BAL0     VALUE 9999999       PIC S9(7).         BBANK30P
006500       15  WS-SRV-CHG0     VALUE 050.00        PIC 9(3)V99.       BBANK30P
006600     10  WS-SRV-BAND1.                                            BBANK30P
006700       15  WS-SRV-BAL1     VALUE 0000000       PIC S9(7).         BBANK30P
006800       15  WS-SRV-CHG1     VALUE 025.00        PIC 9(3)V99.       BBANK30P
006900     10  WS-SRV-BAND2.                                            BBANK30P
007000       15  WS-SRV-BAL2     VALUE 0001000       PIC S9(7).         BBANK30P
007100       15  WS-SRV-CHG2     VALUE 020.00        PIC 9(3)V99.       BBANK30P
007200     10  WS-SRV-BAND3.                                            BBANK30P
007300       15  WS-SRV-BAL3     VALUE 0005000       PIC S9(7).         BBANK30P
007400       15  WS-SRV-CHG3     VALUE 015.00        PIC 9(3)V99.       BBANK30P
007500     10  WS-SRV-BAND4.                                            BBANK30P
007600       15  WS-SRV-BAL4     VALUE 0010000       PIC S9(7).         BBANK30P
007700       15  WS-SRV-CHG4     VALUE 010.00        PIC 9(3)V99.       BBANK30P
007800     10  WS-SRV-BAND5.                                            BBANK30P
007900       15  WS-SRV-BAL5     VALUE 0100000       PIC S9(7).         BBANK30P
008000       15  WS-SRV-CHG5     VALUE 000.00        PIC 9(3)V99.       BBANK30P
008100                                                                  BBANK30P
008200 01  WS-TIME-DATE-WORK-AREA.                                      BBANK30P
008300 COPY CDATED.                                                     BBANK30P
008400                                                                  BBANK30P
008500 01  WS-BANK-DATA.                                                BBANK30P
008600 COPY CBANKDAT.                                                   BBANK30P
008700                                                                  BBANK30P
008800 01  WS-HELP-DATA.                                                BBANK30P
008900 COPY CHELPD01.                                                   BBANK30P
009000                                                                  BBANK30P
009100 01  WS-ACCOUNT-DATA.                                             BBANK30P
009200 COPY CBANKD03.                                                   BBANK30P
009300                                                                  BBANK30P
009400 COPY CABENDD.                                                    BBANK30P
009500                                                                  BBANK30P
009600 LINKAGE SECTION.                                                 BBANK30P
009700 01  DFHCOMMAREA.                                                 BBANK30P
009800   05  LK-COMMAREA                           PIC X(6144).         BBANK30P
009900                                                                  BBANK30P
010000 COPY CENTRY.                                                     BBANK30P
010100***************************************************************** BBANK30P
010200* Make ourselves re-entrant                                     * BBANK30P
010300***************************************************************** BBANK30P
010400     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK30P
010500                                                                  BBANK30P
010600***************************************************************** BBANK30P
010700* Move the passed area to our area                              * BBANK30P
010800***************************************************************** BBANK30P
010900     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK30P
011000                                                                  BBANK30P
011100***************************************************************** BBANK30P
011200* Ensure error message is cleared                               * BBANK30P
011300***************************************************************** BBANK30P
011400     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK30P
011500                                                                  BBANK30P
011600***************************************************************** BBANK30P
011700* This is the main process                                      * BBANK30P
011800***************************************************************** BBANK30P
011900                                                                  BBANK30P
012000***************************************************************** BBANK30P
012100* Save the passed return flag and then turn it off              * BBANK30P
012200***************************************************************** BBANK30P
012300     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     BBANK30P
012400     SET BANK-RETURN-FLAG-OFF TO TRUE.                            BBANK30P
012500                                                                  BBANK30P
012600***************************************************************** BBANK30P
012700* Check the AID to see if its valid at this point               * BBANK30P
012800***************************************************************** BBANK30P
012900     SET PFK-INVALID TO TRUE.                                     BBANK30P
013000     IF BANK-AID-ENTER OR                                         BBANK30P
013100        BANK-AID-PFK03 OR                                         BBANK30P
013200        BANK-AID-PFK04                                            BBANK30P
013300        SET PFK-VALID TO TRUE                                     BBANK30P
013400     END-IF.                                                      BBANK30P
013500     IF BANK-AID-PFK01 AND                                        BBANK30P
013600        BANK-HELP-INACTIVE                                        BBANK30P
013700        SET BANK-HELP-ACTIVE TO TRUE                              BBANK30P
013800        SET PFK-VALID TO TRUE                                     BBANK30P
013900     END-IF.                                                      BBANK30P
014000     IF PFK-INVALID                                               BBANK30P
014100        SET BANK-AID-ENTER TO TRUE                                BBANK30P
014200     END-IF.                                                      BBANK30P
014300                                                                  BBANK30P
014400***************************************************************** BBANK30P
014500* Check the AID to see if we have to quit                       * BBANK30P
014600***************************************************************** BBANK30P
014700     IF BANK-AID-PFK03                                            BBANK30P
014800        MOVE 'BBANK30P' TO BANK-LAST-PROG                         BBANK30P
014900        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK30P
015000        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK30P
015100        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK30P
015200        GO TO COMMON-RETURN                                       BBANK30P
015300     END-IF.                                                      BBANK30P
015400                                                                  BBANK30P
015500***************************************************************** BBANK30P
015600* Check the to see if user needs or has been using help         * BBANK30P
015700***************************************************************** BBANK30P
015800     IF BANK-HELP-ACTIVE                                          BBANK30P
015900        IF BANK-AID-PFK04                                         BBANK30P
016000           SET BANK-HELP-INACTIVE TO TRUE                         BBANK30P
016100           MOVE 00 TO BANK-HELP-SCREEN                            BBANK30P
016200           MOVE 'BBANK30P' TO BANK-LAST-PROG                      BBANK30P
016300           MOVE 'BBANK30P' TO BANK-NEXT-PROG                      BBANK30P
016400           MOVE 'MBANK30' TO BANK-LAST-MAPSET                     BBANK30P
016500           MOVE 'HELP30A' TO BANK-LAST-MAP                        BBANK30P
016600           MOVE 'MBANK30' TO BANK-NEXT-MAPSET                     BBANK30P
016700           MOVE 'BANK30A' TO BANK-NEXT-MAP                        BBANK30P
016800           GO TO COMMON-RETURN                                    BBANK30P
016900        ELSE                                                      BBANK30P
017000           MOVE 01 TO BANK-HELP-SCREEN                            BBANK30P
017100           MOVE 'BBANK30P' TO BANK-LAST-PROG                      BBANK30P
017200           MOVE 'BBANK30P' TO BANK-NEXT-PROG                      BBANK30P
017300           MOVE 'MBANK30' TO BANK-LAST-MAPSET                     BBANK30P
017400           MOVE 'BANK30A' TO BANK-LAST-MAP                        BBANK30P
017500           MOVE 'MBANK30' TO BANK-NEXT-MAPSET                     BBANK30P
017600           MOVE 'HELP30A' TO BANK-NEXT-MAP                        BBANK30P
017700           MOVE 'BANK30' TO HELP01I-SCRN                          BBANK30P
017800           COPY CHELPX01.                                         BBANK30P
017900           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK30P
018000           GO TO COMMON-RETURN                                    BBANK30P
018100     END-IF.                                                      BBANK30P
018200                                                                  BBANK30P
018300***************************************************************** BBANK30P
018400* Check the AID to see if we have to return to previous screen  * BBANK30P
018500***************************************************************** BBANK30P
018600     IF BANK-AID-PFK04                                            BBANK30P
018700        MOVE 'BBANK30P' TO BANK-LAST-PROG                         BBANK30P
018800        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK30P
018900        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK30P
019000        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK30P
019100        SET BANK-AID-ENTER TO TRUE                                BBANK30P
019200        GO TO COMMON-RETURN                                       BBANK30P
019300     END-IF.                                                      BBANK30P
019400                                                                  BBANK30P
019500* Check if we have set the screen up before or is this 1st time   BBANK30P
019600     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK30'                BBANK30P
019700        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK30P
019800        MOVE 'BBANK30P' TO BANK-LAST-PROG                         BBANK30P
019900        MOVE 'BBANK30P' TO BANK-NEXT-PROG                         BBANK30P
020000        MOVE 'MBANK30' TO BANK-LAST-MAPSET                        BBANK30P
020100        MOVE 'BANK30A' TO BANK-LAST-MAP                           BBANK30P
020200        MOVE 'MBANK30' TO BANK-NEXT-MAPSET                        BBANK30P
020300        MOVE 'BANK30A' TO BANK-NEXT-MAP                           BBANK30P
020400        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK30P
020500                POPULATE-SCREEN-DATA-EXIT                         BBANK30P
020600        GO TO COMMON-RETURN                                       BBANK30P
020700     END-IF.                                                      BBANK30P
020800                                                                  BBANK30P
020900     MOVE 0 TO WS-SUB1.                                           BBANK30P
021000     MOVE SPACES TO BANK-SCR40-ACC.                               BBANK30P
021100* Calculate no of entries in table                                BBANK30P
021200     DIVIDE LENGTH OF BANK-SCREEN30-INPUT (1)                     BBANK30P
021300       INTO LENGTH OF BANK-SCREEN30-INPUT-DATA                    BBANK30P
021400         GIVING WS-SUB-LIMIT.                                     BBANK30P
021500 SCAN-INPUT-LOOP.                                                 BBANK30P
021600     ADD 1 TO WS-SUB1.                                            BBANK30P
021700     IF BANK-SCR30-SEL (WS-SUB1)IS NOT = LOW-VALUES               BBANK30P
021800        MOVE LOW-VALUES TO BANK-SCR30-SEL (WS-SUB1)               BBANK30P
021900        IF BANK-SCR30-TXN (WS-SUB1) IS NOT EQUAL TO SPACES        BBANK30P
022000           MOVE BANK-SCR30-ACC (WS-SUB1) TO BANK-SCR40-ACC        BBANK30P
022100           MOVE BANK-SCR30-DSC (WS-SUB1) TO BANK-SCR40-ACCTYPE    BBANK30P
022200           MOVE 'BBANK30P' TO BANK-LAST-PROG                      BBANK30P
022300           MOVE 'BBANK40P' TO BANK-NEXT-PROG                      BBANK30P
022400           GO TO COMMON-RETURN                                    BBANK30P
022500        ELSE                                                      BBANK30P
022600           MOVE 'No transactions to show' TO WS-ERROR-MSG         BBANK30P
022700           MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                    BBANK30P
022800           MOVE 'BBANK30P' TO BANK-LAST-PROG                      BBANK30P
022900           MOVE 'BBANK30P' TO BANK-NEXT-PROG                      BBANK30P
023000           MOVE 'MBANK30' TO BANK-LAST-MAPSET                     BBANK30P
023100           MOVE 'BANK30A' TO BANK-LAST-MAP                        BBANK30P
023200           MOVE 'MBANK30' TO BANK-NEXT-MAPSET                     BBANK30P
023300           MOVE 'BANK30A' TO BANK-NEXT-MAP                        BBANK30P
023400           GO TO COMMON-RETURN                                    BBANK30P
023500        END-IF                                                    BBANK30P
023600     END-IF.                                                      BBANK30P
023700                                                                  BBANK30P
023800     IF WS-SUB1 IS LESS THAN WS-SUB-LIMIT                         BBANK30P
023900        GO TO SCAN-INPUT-LOOP                                     BBANK30P
024000     END-IF.                                                      BBANK30P
024100                                                                  BBANK30P
024200* As this is a display screen we need PFK03/4 to get out so we    BBANK30P
024300* will just redisplay the data                                    BBANK30P
024400     PERFORM POPULATE-SCREEN-DATA THRU                            BBANK30P
024500             POPULATE-SCREEN-DATA-EXIT.                           BBANK30P
024600                                                                  BBANK30P
024700     GO TO COMMON-RETURN.                                         BBANK30P
024800                                                                  BBANK30P
024900***************************************************************** BBANK30P
025000* If we get this far then we have an error in our logic as we   * BBANK30P
025100* don't know where to go next.                                  * BBANK30P
025200***************************************************************** BBANK30P
025300     IF NOT BANK-ENV-CICS                                         BBANK30P
025400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK30P
025500        MOVE '0001' TO ABEND-CODE                                 BBANK30P
025600        MOVE SPACES TO ABEND-REASON                               BBANK30P
025700        COPY CABENDPO.                                            BBANK30P
025800     END-IF.                                                      BBANK30P
025900     GOBACK.                                                      BBANK30P
026000                                                                  BBANK30P
026100 COMMON-RETURN.                                                   BBANK30P
026200     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK30P
026300 COPY CRETURN.                                                    BBANK30P
026400                                                                  BBANK30P
026500 VALIDATE-DATA.                                                   BBANK30P
026600     SET INPUT-OK TO TRUE.                                        BBANK30P
026700     GO TO VALIDATE-DATA-EXIT.                                    BBANK30P
026800                                                                  BBANK30P
026900 VALIDATE-DATA-ERROR.                                             BBANK30P
027000	   SET INPUT-ERROR TO TRUE.					
027100 VALIDATE-DATA-EXIT.                                              BBANK30P
027200     EXIT.                                                        BBANK30P
027300                                                                  BBANK30P
027400 POPULATE-SCREEN-DATA.                                            BBANK30P
027500     MOVE SPACES TO CD03-DATA.                                    BBANK30P
027600     MOVE BANK-USERID TO CD03I-CONTACT-ID.                        BBANK30P
027700* Now go get the data                                             BBANK30P
027800 COPY CBANKX03.                                                   BBANK30P
027900     MOVE CD03O-ACC1 TO BANK-SCR30-ACC1.                          BBANK30P
028000     MOVE CD03O-DSC1 TO BANK-SCR30-DSC1.                          BBANK30P
028100     IF CD03O-BAL1 IS EQUAL TO SPACES                             BBANK30P
028200        MOVE CD03O-BAL1 TO BANK-SCR30-BAL1                        BBANK30P
028300     ELSE                                                         BBANK30P
028400        MOVE CD03O-BAL1N TO WS-EDIT-BALANCE                       BBANK30P
028500        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL1                   BBANK30P
028600     END-IF.                                                      BBANK30P
028700     MOVE CD03O-BAL1 TO WS-SRV-BAL.                               BBANK30P
028800     PERFORM CALC-SERVICE-CHARGE THRU                             BBANK30P
028900             CALC-SERVICE-CHARGE-EXIT.                            BBANK30P
029000     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV1.                       BBANK30P
029100     IF CD03O-DTE1 IS EQUAL TO SPACES                             BBANK30P
029200        MOVE CD03O-DTE1 TO BANK-SCR30-DTE1                        BBANK30P
029300     ELSE                                                         BBANK30P
029400        MOVE CD03O-DTE1 TO DDI-DATA                               BBANK30P
029500        SET DDI-ISO TO TRUE                                       BBANK30P
029600        SET DDO-DD-MMM-YYYY TO TRUE                               BBANK30P
029700        PERFORM CALL-DATECONV THRU                                BBANK30P
029800               CALL-DATECONV-EXIT                                 BBANK30P
029900        MOVE DDO-DATA TO BANK-SCR30-DTE1                          BBANK30P
030000     END-IF.                                                      BBANK30P
030100     MOVE CD03O-TXN1 TO BANK-SCR30-TXN1.                          BBANK30P
030200                                                                  BBANK30P
030300     MOVE CD03O-ACC2 TO BANK-SCR30-ACC2.                          BBANK30P
030400     MOVE CD03O-DSC2 TO BANK-SCR30-DSC2.                          BBANK30P
030500     IF CD03O-BAL2 IS EQUAL TO SPACES                             BBANK30P
030600        MOVE CD03O-BAL2 TO BANK-SCR30-BAL2                        BBANK30P
030700     ELSE                                                         BBANK30P
030800        MOVE CD03O-BAL2N TO WS-EDIT-BALANCE                       BBANK30P
030900        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL2                   BBANK30P
031000     END-IF.                                                      BBANK30P
031100     MOVE CD03O-BAL2 TO WS-SRV-BAL                                BBANK30P
031200     PERFORM CALC-SERVICE-CHARGE THRU                             BBANK30P
031300             CALC-SERVICE-CHARGE-EXIT.                            BBANK30P
031400     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV2.                       BBANK30P
031500     IF CD03O-DTE2 IS EQUAL TO SPACES                             BBANK30P
031600        MOVE CD03O-DTE2 TO BANK-SCR30-DTE2                        BBANK30P
031700     ELSE                                                         BBANK30P
031800        MOVE CD03O-DTE2 TO DDI-DATA                               BBANK30P
031900        SET DDI-ISO TO TRUE                                       BBANK30P
032000        SET DDO-DD-MMM-YYYY TO TRUE                               BBANK30P
032100        PERFORM CALL-DATECONV THRU                                BBANK30P
032200                CALL-DATECONV-EXIT                                BBANK30P
032300        MOVE DDO-DATA TO BANK-SCR30-DTE2                          BBANK30P
032400     END-IF.                                                      BBANK30P
032500     MOVE CD03O-TXN2 TO BANK-SCR30-TXN2.                          BBANK30P
032600                                                                  BBANK30P
032700     MOVE CD03O-ACC3 TO BANK-SCR30-ACC3.                          BBANK30P
032800     MOVE CD03O-DSC3 TO BANK-SCR30-DSC3.                          BBANK30P
032900     IF CD03O-BAL3 IS EQUAL TO SPACES                             BBANK30P
033000        MOVE CD03O-BAL3 TO BANK-SCR30-BAL3                        BBANK30P
033100     ELSE                                                         BBANK30P
033200        MOVE CD03O-BAL3N TO WS-EDIT-BALANCE                       BBANK30P
033300        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL3                   BBANK30P
033400     END-IF.                                                      BBANK30P
033500     MOVE CD03O-BAL3 TO WS-SRV-BAL.                               BBANK30P
033600     PERFORM CALC-SERVICE-CHARGE THRU                             BBANK30P
033700             CALC-SERVICE-CHARGE-EXIT.                            BBANK30P
033800     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV3.                       BBANK30P
033900     IF CD03O-DTE3 IS EQUAL TO SPACES                             BBANK30P
034000        MOVE CD03O-DTE3 TO BANK-SCR30-DTE3                        BBANK30P
034100     ELSE                                                         BBANK30P
034200        MOVE CD03O-DTE3 TO DDI-DATA                               BBANK30P
034300        SET DDI-ISO TO TRUE                                       BBANK30P
034400        SET DDO-DD-MMM-YYYY TO TRUE                               BBANK30P
034500        PERFORM CALL-DATECONV THRU                                BBANK30P
034600                CALL-DATECONV-EXIT                                BBANK30P
034700        MOVE DDO-DATA TO BANK-SCR30-DTE3                          BBANK30P
034800     END-IF.                                                      BBANK30P
034900     MOVE CD03O-TXN3 TO BANK-SCR30-TXN3.                          BBANK30P
035000                                                                  BBANK30P
035100     MOVE CD03O-ACC4 TO BANK-SCR30-ACC4.                          BBANK30P
035200     MOVE CD03O-DSC4 TO BANK-SCR30-DSC4.                          BBANK30P
035300     IF CD03O-BAL4 IS EQUAL TO SPACES                             BBANK30P
035400        MOVE CD03O-BAL4 TO BANK-SCR30-BAL4                        BBANK30P
035500     ELSE                                                         BBANK30P
035600        MOVE CD03O-BAL4N TO WS-EDIT-BALANCE                       BBANK30P
035700        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL4                   BBANK30P
035800     END-IF.                                                      BBANK30P
035900     MOVE CD03O-BAL4 TO WS-SRV-BAL.                               BBANK30P
036000     PERFORM CALC-SERVICE-CHARGE THRU                             BBANK30P
036100             CALC-SERVICE-CHARGE-EXIT.                            BBANK30P
036200     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV4.                       BBANK30P
036300     IF CD03O-DTE4 IS EQUAL TO SPACES                             BBANK30P
036400        MOVE CD03O-DTE4 TO BANK-SCR30-DTE4                        BBANK30P
036500     ELSE                                                         BBANK30P
036600        MOVE CD03O-DTE4 TO DDI-DATA                               BBANK30P
036700        SET DDI-ISO TO TRUE                                       BBANK30P
036800        SET DDO-DD-MMM-YYYY TO TRUE                               BBANK30P
036900        PERFORM CALL-DATECONV THRU                                BBANK30P
037000                CALL-DATECONV-EXIT                                BBANK30P
037100        MOVE DDO-DATA TO BANK-SCR30-DTE4                          BBANK30P
037200     END-IF.                                                      BBANK30P
037300     MOVE CD03O-TXN4 TO BANK-SCR30-TXN4.                          BBANK30P
037400                                                                  BBANK30P
037500     MOVE CD03O-ACC5 TO BANK-SCR30-ACC5.                          BBANK30P
037600     MOVE CD03O-DSC5 TO BANK-SCR30-DSC5.                          BBANK30P
037700     IF CD03O-BAL5 IS EQUAL TO SPACES                             BBANK30P
037800        MOVE CD03O-BAL5 TO BANK-SCR30-BAL5                        BBANK30P
037900     ELSE                                                         BBANK30P
038000        MOVE CD03O-BAL5N TO WS-EDIT-BALANCE                       BBANK30P
038100        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL5                   BBANK30P
038200     END-IF.                                                      BBANK30P
038300                                                                  BBANK30P
038400     MOVE CD03O-BAL5 TO WS-SRV-BAL.                               BBANK30P
038500     PERFORM CALC-SERVICE-CHARGE THRU                             BBANK30P
038600             CALC-SERVICE-CHARGE-EXIT.                            BBANK30P
038700     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV5.                       BBANK30P
038800     IF CD03O-DTE5 IS EQUAL TO SPACES                             BBANK30P
038900        MOVE CD03O-DTE5 TO BANK-SCR30-DTE5                        BBANK30P
039000     ELSE                                                         BBANK30P
039100        MOVE CD03O-DTE5 TO DDI-DATA                               BBANK30P
039200        SET DDI-ISO TO TRUE                                       BBANK30P
039300        SET DDO-DD-MMM-YYYY TO TRUE                               BBANK30P
039400        PERFORM CALL-DATECONV THRU                                BBANK30P
039500                CALL-DATECONV-EXIT                                BBANK30P
039600        MOVE DDO-DATA TO BANK-SCR30-DTE5                          BBANK30P
039700     END-IF.                                                      BBANK30P
039800     MOVE CD03O-TXN5 TO BANK-SCR30-TXN5.                          BBANK30P
039900     IF BANK-SCR30-SRV1 IS NOT EQUAL TO SPACES OR                 BBANK30P
040000        BANK-SCR30-SRV2 IS NOT EQUAL TO SPACES OR                 BBANK30P
040100        BANK-SCR30-SRV3 IS NOT EQUAL TO SPACES OR                 BBANK30P
040200        BANK-SCR30-SRV4 IS NOT EQUAL TO SPACES OR                 BBANK30P
040300        BANK-SCR30-SRV5 IS NOT EQUAL TO SPACES                    BBANK30P
040400        MOVE WS-SRV-MSG TO BANK-SCR30-SRVMSG                      BBANK30P
040500     ELSE                                                         BBANK30P
040600        MOVE SPACES TO BANK-SCR30-SRVMSG                          BBANK30P
040700     END-IF.                                                      BBANK30P
040800                                                                  BBANK30P
040900 POPULATE-SCREEN-DATA-EXIT.                                       BBANK30P
041000     EXIT.                                                        BBANK30P
041100                                                                  BBANK30P
041200                                                                  BBANK30P
041300***************************************************************** BBANK30P
041400* Calculate any service charges based on provided balance       * BBANK30P
041500***************************************************************** BBANK30P
041600 CALC-SERVICE-CHARGE.                                             BBANK30P
041700     IF WS-SRV-BAL IS EQUAL TO SPACES                             BBANK30P
041800        MOVE 0 TO WS-SRV-AMT                                      BBANK30P
041900        GO TO CALC-SERVICE-CHARGE-EDIT                            BBANK30P
042000     END-IF.                                                      BBANK30P
042100     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL5                  BBANK30P
042200        MOVE WS-SRV-CHG5 TO WS-SRV-AMT                            BBANK30P
042300        GO TO CALC-SERVICE-CHARGE-EDIT                            BBANK30P
042400     END-IF.                                                      BBANK30P
042500     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL4                  BBANK30P
042600        MOVE WS-SRV-CHG4 TO WS-SRV-AMT                            BBANK30P
042700        GO TO CALC-SERVICE-CHARGE-EDIT                            BBANK30P
042800     END-IF.                                                      BBANK30P
042900     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL3                  BBANK30P
043000        MOVE WS-SRV-CHG3 TO WS-SRV-AMT                            BBANK30P
043100        GO TO CALC-SERVICE-CHARGE-EDIT                            BBANK30P
043200     END-IF.                                                      BBANK30P
043300     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL2                  BBANK30P
043400        MOVE WS-SRV-CHG2 TO WS-SRV-AMT                            BBANK30P
043500        GO TO CALC-SERVICE-CHARGE-EDIT                            BBANK30P
043600     END-IF.                                                      BBANK30P
043700     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL1                  BBANK30P
043800        MOVE WS-SRV-CHG1 TO WS-SRV-AMT                            BBANK30P
043900        GO TO CALC-SERVICE-CHARGE-EDIT                            BBANK30P
044000     ELSE                                                         BBANK30P
044100        MOVE WS-SRV-CHG0 TO WS-SRV-AMT                            BBANK30P
044200        GO TO CALC-SERVICE-CHARGE-EDIT                            BBANK30P
044300     END-IF.                                                      BBANK30P
044400 CALC-SERVICE-CHARGE-EDIT.                                        BBANK30P
044500     MOVE WS-SRV-AMT TO WS-SRV-CHARGE.                            BBANK30P
044600 CALC-SERVICE-CHARGE-EXIT.                                        BBANK30P
044700     EXIT.                                                        BBANK30P
044800                                                                  BBANK30P
044900***************************************************************** BBANK30P
045000* Call common routine to perform date conversions               * BBANK30P
045100***************************************************************** BBANK30P
045200 CALL-DATECONV.                                                   BBANK30P
045300     MOVE BANK-ENV TO DD-ENV.                                     BBANK30P
045400     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           BBANK30P
045500     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            BBANK30P
045600 CALL-DATECONV-EXIT.                                              BBANK30P
045700     EXIT.                                                        BBANK30P
045800                                                                  BBANK30P
