000100***************************************************************** BBANK50P
000200*                                                               * BBANK50P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK50P
000400*   This demonstration program is provided for use by users     * BBANK50P
000500*   of Micro Focus products and may be used, modified and       * BBANK50P
000600*   distributed as part of your application provided that       * BBANK50P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK50P
000800*   in this material.                                           * BBANK50P
000900*                                                               * BBANK50P
001000***************************************************************** BBANK50P
001100                                                                  BBANK50P
001200***************************************************************** BBANK50P
001300* Program:     BBANK50P.CBL                                     * BBANK50P
001400* Layer:       Business logic                                   * BBANK50P
001500* Function:    Transfer funds between accounts                  * BBANK50P
001600***************************************************************** BBANK50P
001700                                                                  BBANK50P
001800 IDENTIFICATION DIVISION.                                         BBANK50P
001900 PROGRAM-ID.                                                      BBANK50P
002000     BBANK50P.                                                    BBANK50P
002100 DATE-WRITTEN.                                                    BBANK50P
002200     September 2002.                                              BBANK50P
002300 DATE-COMPILED.                                                   BBANK50P
002400     Today.                                                       BBANK50P
002500                                                                  BBANK50P
002600 ENVIRONMENT DIVISION.                                            BBANK50P
002700                                                                  BBANK50P
002800 DATA DIVISION.                                                   BBANK50P
002900 WORKING-STORAGE SECTION.                                         BBANK50P
003000 01  WS-MISC-STORAGE.                                             BBANK50P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK50P
003200       VALUE 'BBANK50P'.                                          BBANK50P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK50P
003400     88  INPUT-OK                            VALUE '0'.           BBANK50P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK50P
003600	 05  WS-RETURN-FLAG			   PIC X(1).		
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK50P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK50P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK50P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK50P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK50P
004200     88  PFK-VALID                           VALUE '0'.           BBANK50P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK50P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK50P
004500   05  WS-EDIT-BALANCE                       PIC Z,ZZZ,ZZ9.99-.   BBANK50P
004600   05  WS-SEL-COUNT                          PIC 9(1).            BBANK50P
004700   05  WS-XFER-AMT                           PIC X(8).            BBANK50P
004800   05  WS-XFER-AMT-TMP                       PIC X(9).            BBANK50P
004900   05  WS-XFER-AMT-TMP-N REDEFINES WS-XFER-AMT-TMP                BBANK50P
005000                                             PIC S9(7)V99.        BBANK50P
005100   05  WS-XFER-AMT-NUM                       PIC X(9).            BBANK50P
005200   05  WS-XFER-AMT-NUM-N REDEFINES WS-XFER-AMT-NUM                BBANK50P
005300                                             PIC S9(7)V99.        BBANK50P
005400   05  WS-XFER-ACCT-FROM                     PIC X(9).            BBANK50P
005500   05  WS-XFER-ACCT-FROM-BAL                 PIC X(13).           BBANK50P
005600   05  WS-XFER-ACCT-FROM-BAL-N               PIC S9(7)V99.        BBANK50P
005700   05  WS-XFER-ACCT-FROM-NEW-BAL-N           PIC S9(7)V99.        BBANK50P
005800   05  WS-XFER-ACCT-TO                       PIC X(9).            BBANK50P
005900   05  WS-XFER-ACCT-TO-BAL                   PIC X(13).           BBANK50P
006000   05  WS-XFER-ACCT-TO-BAL-N                 PIC S9(7)V99.        BBANK50P
006100   05  WS-XFER-ACCT-TO-NEW-BAL-N             PIC S9(7)V99.        BBANK50P
006200                                                                  BBANK50P
006300 01  WS-BANK-DATA.                                                BBANK50P
006400 COPY CBANKDAT.                                                   BBANK50P
006500                                                                  BBANK50P
006600 01  WS-HELP-DATA.                                                BBANK50P
006700 COPY CHELPD01.                                                   BBANK50P
006800                                                                  BBANK50P
006900 01  WS-ACCT-DATA.                                                BBANK50P
007000 COPY CBANKD03.                                                   BBANK50P
007100                                                                  BBANK50P
007200 01  WS-XFER-DATA.                                                BBANK50P
007300 COPY CBANKD04.                                                   BBANK50P
007400                                                                  BBANK50P
007500 01  WS-TXN-DATA.                                                 BBANK50P
007600 COPY CBANKD06.                                                   BBANK50P
007700                                                                  BBANK50P
007800 COPY CABENDD.                                                    BBANK50P
007900                                                                  BBANK50P
008000 LINKAGE SECTION.                                                 BBANK50P
008100 01  DFHCOMMAREA.                                                 BBANK50P
008200   05  LK-COMMAREA                           PIC X(6144).         BBANK50P
008300                                                                  BBANK50P
008400 COPY CENTRY.                                                     BBANK50P
008500***************************************************************** BBANK50P
008600* Make ourselves re-entrant                                     * BBANK50P
008700***************************************************************** BBANK50P
008800     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK50P
008900                                                                  BBANK50P
009000***************************************************************** BBANK50P
009100* Move the passed area to Bour area                              *BBANK50P
009200***************************************************************** BBANK50P
009300     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK50P
009400                                                                  BBANK50P
009500***************************************************************** BBANK50P
009600* Ensure error message is cleared                               * BBANK50P
009700***************************************************************** BBANK50P
009800     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK50P
009900                                                                  BBANK50P
010000***************************************************************** BBANK50P
010100* This is the main process                                      * BBANK50P
010200***************************************************************** BBANK50P
010300                                                                  BBANK50P
010400***************************************************************** BBANK50P
010500* Save the passed return message and then turn it off           * BBANK50P
010600***************************************************************** BBANK50P
010700     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANK50P
010800     SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANK50P
010900                                                                  BBANK50P
011000***************************************************************** BBANK50P
011100* Check the AID to see if its valid at this point               * BBANK50P
011200***************************************************************** BBANK50P
011300     SET PFK-INVALID TO TRUE.                                     BBANK50P
011400     IF BANK-AID-ENTER OR                                         BBANK50P
011500        BANK-AID-PFK03 OR                                         BBANK50P
011600        BANK-AID-PFK04                                            BBANK50P
011700        SET PFK-VALID TO TRUE                                     BBANK50P
011800     END-IF.                                                      BBANK50P
011900     IF BANK-AID-PFK01 AND                                        BBANK50P
012000        BANK-HELP-INACTIVE                                        BBANK50P
012100        SET BANK-HELP-ACTIVE TO TRUE                              BBANK50P
012200        SET PFK-VALID TO TRUE                                     BBANK50P
012300     END-IF.                                                      BBANK50P
012400     IF PFK-INVALID                                               BBANK50P
012500        SET BANK-AID-ENTER TO TRUE                                BBANK50P
012600     END-IF.                                                      BBANK50P
012700                                                                  BBANK50P
012800***************************************************************** BBANK50P
012900* Check the AID to see if we have to quit                       * BBANK50P
013000***************************************************************** BBANK50P
013100     IF BANK-AID-PFK03                                            BBANK50P
013200        MOVE 'BBANK50P' TO BANK-LAST-PROG                         BBANK50P
013300        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK50P
013400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK50P
013500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK50P
013600        GO TO COMMON-RETURN                                       BBANK50P
013700     END-IF.                                                      BBANK50P
013800                                                                  BBANK50P
013900***************************************************************** BBANK50P
014000* Check the to see if user needs or has been using help         * BBANK50P
014100***************************************************************** BBANK50P
014200     IF BANK-HELP-ACTIVE                                          BBANK50P
014300        IF BANK-AID-PFK04                                         BBANK50P
014400           SET BANK-HELP-INACTIVE TO TRUE                         BBANK50P
014500           MOVE 00 TO BANK-HELP-SCREEN                            BBANK50P
014600           MOVE 'BBANK50P' TO BANK-LAST-PROG                      BBANK50P
014700           MOVE 'BBANK50P' TO BANK-NEXT-PROG                      BBANK50P
014800           MOVE 'MBANK50' TO BANK-LAST-MAPSET                     BBANK50P
014900           MOVE 'HELP50A' TO BANK-LAST-MAP                        BBANK50P
015000           MOVE 'MBANK50' TO BANK-NEXT-MAPSET                     BBANK50P
015100           MOVE 'BANK50A' TO BANK-NEXT-MAP                        BBANK50P
015200           GO TO COMMON-RETURN                                    BBANK50P
015300        ELSE                                                      BBANK50P
015400           MOVE 01 TO BANK-HELP-SCREEN                            BBANK50P
015500           MOVE 'BBANK50P' TO BANK-LAST-PROG                      BBANK50P
015600           MOVE 'BBANK50P' TO BANK-NEXT-PROG                      BBANK50P
015700           MOVE 'MBANK50' TO BANK-LAST-MAPSET                     BBANK50P
015800           MOVE 'BANK50A' TO BANK-LAST-MAP                        BBANK50P
015900           MOVE 'MBANK50' TO BANK-NEXT-MAPSET                     BBANK50P
016000           MOVE 'HELP50A' TO BANK-NEXT-MAP                        BBANK50P
016100           MOVE 'BANK50' TO HELP01I-SCRN                          BBANK50P
016200           COPY CHELPX01.                                         BBANK50P
016300           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK50P
016400           GO TO COMMON-RETURN                                    BBANK50P
016500     END-IF.                                                      BBANK50P
016600                                                                  BBANK50P
016700***************************************************************** BBANK50P
016800* Check the AID to see if we have to return to previous screen  * BBANK50P
016900***************************************************************** BBANK50P
017000     IF BANK-AID-PFK04                                            BBANK50P
017100        MOVE 'BBANK50P' TO BANK-LAST-PROG                         BBANK50P
017200        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK50P
017300        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK50P
017400        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK50P
017500        SET BANK-AID-ENTER TO TRUE                                BBANK50P
017600        GO TO COMMON-RETURN                                       BBANK50P
017700     END-IF.                                                      BBANK50P
017800                                                                  BBANK50P
017900* Check if we have set the screen up before or is this 1st time   BBANK50P
018000     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK50'                BBANK50P
018100        MOVE SPACES TO BANK-SCR50-XFER                            BBANK50P
018200        MOVE '_' TO BANK-SCR50-FRM1                               BBANK50P
018300        MOVE '_' TO BANK-SCR50-FRM2                               BBANK50P
018400        MOVE '_' TO BANK-SCR50-FRM3                               BBANK50P
018500        MOVE '_' TO BANK-SCR50-FRM4                               BBANK50P
018600        MOVE '_' TO BANK-SCR50-FRM5                               BBANK50P
018700        MOVE '_' TO BANK-SCR50-TO1                                BBANK50P
018800        MOVE '_' TO BANK-SCR50-TO2                                BBANK50P
018900        MOVE '_' TO BANK-SCR50-TO3                                BBANK50P
019000        MOVE '_' TO BANK-SCR50-TO4                                BBANK50P
019100        MOVE '_' TO BANK-SCR50-TO5                                BBANK50P
019200        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK50P
019300        MOVE 'BBANK50P' TO BANK-LAST-PROG                         BBANK50P
019400        MOVE 'BBANK50P' TO BANK-NEXT-PROG                         BBANK50P
019500        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        BBANK50P
019600        MOVE 'BANK50A' TO BANK-LAST-MAP                           BBANK50P
019700        MOVE 'MBANK50' TO BANK-NEXT-MAPSET                        BBANK50P
019800        MOVE 'BANK50A' TO BANK-NEXT-MAP                           BBANK50P
019900        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK50P
020000                POPULATE-SCREEN-DATA-EXIT                         BBANK50P
020100        GO TO COMMON-RETURN                                       BBANK50P
020200     END-IF.                                                      BBANK50P
020300                                                                  BBANK50P
020400     PERFORM VALIDATE-DATA THRU                                   BBANK50P
020500             VALIDATE-DATA-EXIT.                                  BBANK50P
020600                                                                  BBANK50P
020700* If we had an error display error and return                     BBANK50P
020800     IF INPUT-ERROR                                               BBANK50P
020900        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK50P
021000        MOVE 'BBANK50P' TO BANK-LAST-PROG                         BBANK50P
021100        MOVE 'BBANK50P' TO BANK-NEXT-PROG                         BBANK50P
021200        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        BBANK50P
021300        MOVE 'BANK50A' TO BANK-LAST-MAP                           BBANK50P
021400        MOVE 'MBANK50' TO BANK-NEXT-MAPSET                        BBANK50P
021500        MOVE 'BANK50A' TO BANK-NEXT-MAP                           BBANK50P
021600        GO TO COMMON-RETURN                                       BBANK50P
021700     END-IF.                                                      BBANK50P
021800                                                                  BBANK50P
021900     MOVE SPACES TO CD04-DATA.                                    BBANK50P
022000     MOVE BANK-USERID TO CD04I-PERSON-PID.                        BBANK50P
022100     MOVE WS-XFER-ACCT-FROM TO CD04I-FROM-ACC.                    BBANK50P
022200     MOVE WS-XFER-ACCT-FROM-BAL-N TO CD04I-FROM-OLD-BAL.          BBANK50P
022300     SUBTRACT WS-XFER-AMT-NUM-N FROM WS-XFER-ACCT-FROM-BAL-N      BBANK50P
022400       GIVING WS-XFER-ACCT-FROM-NEW-BAL-N.                        BBANK50P
022500     MOVE WS-XFER-ACCT-FROM-NEW-BAL-N TO CD04I-FROM-NEW-BAL.      BBANK50P
022600     MOVE WS-XFER-ACCT-TO TO CD04I-TO-ACC.                        BBANK50P
022700     MOVE WS-XFER-ACCT-TO-BAL-N TO CD04I-TO-OLD-BAL.              BBANK50P
022800     ADD WS-XFER-AMT-NUM-N TO WS-XFER-ACCT-TO-BAL-N               BBANK50P
022900       GIVING WS-XFER-ACCT-TO-NEW-BAL-N.                          BBANK50P
023000     MOVE WS-XFER-ACCT-TO-NEW-BAL-N TO CD04I-TO-NEW-BAL.          BBANK50P
023100     MOVE WS-XFER-ACCT-FROM-BAL-N TO CD04I-FROM-OLD-BAL.          BBANK50P
023200* Now go attempt to update the data                               BBANK50P
023300 COPY CBANKX04.                                                   BBANK50P
023400     IF NOT CD04O-UPDATE-OK                                       BBANK50P
023500        MOVE 'Unable to transfer funds. Update failed.'           BBANK50P
023600          TO BANK-ERROR-MSG                                       BBANK50P
023700     ELSE                                                         BBANK50P
023800        MOVE SPACES TO BANK-ERROR-MSG                             BBANK50P
023900        STRING 'Transferred ' DELIMITED BY SIZE                   BBANK50P
024000               BANK-SCR50-XFER DELIMITED BY SIZE                  BBANK50P
024100               ' from ' DELIMITED BY SIZE                         BBANK50P
024200               WS-XFER-ACCT-FROM DELIMITED BY SIZE                BBANK50P
024300               ' to ' DELIMITED BY SIZE                           BBANK50P
024400               WS-XFER-ACCT-TO DELIMITED BY SIZE                  BBANK50P
024500          INTO BANK-ERROR-MSG                                     BBANK50P
024600* Now produce the audit trail                                     BBANK50P
024700        MOVE SPACES TO CD06-DATA                                  BBANK50P
024800        MOVE BANK-USERID TO CD06I-PERSON-PID                      BBANK50P
024900        MOVE CD04O-TIMESTAMP TO CD06I-TIMESTAMP                   BBANK50P
025000        MOVE CD04I-FROM-ACC TO CD06I-FROM-ACC                     BBANK50P
025100        MULTIPLY WS-XFER-AMT-NUM-N BY -1                          BBANK50P
025200          GIVING CD06I-FROM-AMOUNT                                BBANK50P
025300        STRING 'Transferred to a/c ' DELIMITED BY SIZE            BBANK50P
025400               CD04I-TO-ACC DELIMITED BY SIZE                     BBANK50P
025500          INTO CD06I-FROM-DESC                                    BBANK50P
025600        MOVE CD04I-TO-ACC TO CD06I-TO-ACC                         BBANK50P
025700        MULTIPLY WS-XFER-AMT-NUM-N BY +1                          BBANK50P
025800          GIVING CD06I-TO-AMOUNT                                  BBANK50P
025900        STRING 'Transferred from a/c ' DELIMITED BY SIZE          BBANK50P
026000               CD04I-FROM-ACC DELIMITED BY SIZE                   BBANK50P
026100          INTO CD06I-TO-DESC                                      BBANK50P
026200 COPY CBANKX06.                                                   BBANK50P
026300                                                                  BBANK50P
026400        MOVE SPACES TO BANK-SCR50-XFER                            BBANK50P
026500        MOVE '_' TO BANK-SCR50-FRM1                               BBANK50P
026600        MOVE '_' TO BANK-SCR50-FRM2                               BBANK50P
026700        MOVE '_' TO BANK-SCR50-FRM3                               BBANK50P
026800        MOVE '_' TO BANK-SCR50-FRM4                               BBANK50P
026900        MOVE '_' TO BANK-SCR50-FRM5                               BBANK50P
027000        MOVE '_' TO BANK-SCR50-TO1                                BBANK50P
027100        MOVE '_' TO BANK-SCR50-TO2                                BBANK50P
027200        MOVE '_' TO BANK-SCR50-TO3                                BBANK50P
027300        MOVE '_' TO BANK-SCR50-TO4                                BBANK50P
027400        MOVE '_' TO BANK-SCR50-TO5                                BBANK50P
027500     END-IF.                                                      BBANK50P
027600                                                                  BBANK50P
027700     PERFORM POPULATE-SCREEN-DATA THRU                            BBANK50P
027800             POPULATE-SCREEN-DATA-EXIT.                           BBANK50P
027900                                                                  BBANK50P
028000     GO TO COMMON-RETURN.                                         BBANK50P
028100                                                                  BBANK50P
028200                                                                  BBANK50P
028300***************************************************************** BBANK50P
028400* If we get this far then we have an error in our logic as we   * BBANK50P
028500* don't know where to go next.                                  * BBANK50P
028600***************************************************************** BBANK50P
028700     IF NOT BANK-ENV-CICS                                         BBANK50P
028800        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK50P
028900        MOVE '0001' TO ABEND-CODE                                 BBANK50P
029000        MOVE SPACES TO ABEND-REASON                               BBANK50P
029100        COPY CABENDPO.                                            BBANK50P
029200     END-IF.                                                      BBANK50P
029300     GOBACK.                                                      BBANK50P
029400                                                                  BBANK50P
029500 COMMON-RETURN.                                                   BBANK50P
029600     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK50P
029700 COPY CRETURN.                                                    BBANK50P
029800                                                                  BBANK50P
029900 VALIDATE-DATA.                                                   BBANK50P
030000     SET INPUT-OK TO TRUE.                                        BBANK50P
030100                                                                  BBANK50P
030200     MOVE BANK-SCR50-XFER TO WS-XFER-AMT.                         BBANK50P
030300     PERFORM VALIDATE-XFER THRU                                   BBANK50P
030400             VALIDATE-XFER-EXIT.                                  BBANK50P
030500     IF NOT INPUT-OK                                              BBANK50P
030600        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
030700     END-IF.                                                      BBANK50P
030800                                                                  BBANK50P
030900     MOVE ZERO TO WS-SEL-COUNT.                                   BBANK50P
031000                                                                  BBANK50P
031100     MOVE SPACES TO WS-XFER-ACCT-FROM.                            BBANK50P
031200     MOVE SPACES TO WS-XFER-ACCT-FROM-BAL.                        BBANK50P
031300     MOVE SPACES TO WS-XFER-ACCT-TO.                              BBANK50P
031400                                                                  BBANK50P
031500     IF BANK-SCR50-FRM1 IS NOT EQUAL TO LOW-VALUES                BBANK50P
031600        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
031700        MOVE BANK-SCR50-ACC1 TO WS-XFER-ACCT-FROM                 BBANK50P
031800        MOVE BANK-SCR50-BAL1 TO WS-XFER-ACCT-FROM-BAL             BBANK50P
031900     END-IF.                                                      BBANK50P
032000     IF BANK-SCR50-FRM2 IS NOT EQUAL TO LOW-VALUES                BBANK50P
032100        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
032200        MOVE BANK-SCR50-ACC2 TO WS-XFER-ACCT-FROM                 BBANK50P
032300        MOVE BANK-SCR50-BAL2 TO WS-XFER-ACCT-FROM-BAL             BBANK50P
032400     END-IF.                                                      BBANK50P
032500     IF BANK-SCR50-FRM3 IS NOT EQUAL TO LOW-VALUES                BBANK50P
032600        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
032700        MOVE BANK-SCR50-ACC3 TO WS-XFER-ACCT-FROM                 BBANK50P
032800        MOVE BANK-SCR50-BAL3 TO WS-XFER-ACCT-FROM-BAL             BBANK50P
032900     END-IF.                                                      BBANK50P
033000     IF BANK-SCR50-FRM4 IS NOT EQUAL TO LOW-VALUES                BBANK50P
033100        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
033200        MOVE BANK-SCR50-ACC4 TO WS-XFER-ACCT-FROM                 BBANK50P
033300        MOVE BANK-SCR50-BAL4 TO WS-XFER-ACCT-FROM-BAL             BBANK50P
033400     END-IF.                                                      BBANK50P
033500     IF BANK-SCR50-FRM5 IS NOT EQUAL TO LOW-VALUES                BBANK50P
033600        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
033700        MOVE BANK-SCR50-ACC5 TO WS-XFER-ACCT-FROM                 BBANK50P
033800        MOVE BANK-SCR50-BAL5 TO WS-XFER-ACCT-FROM-BAL             BBANK50P
033900     END-IF.                                                      BBANK50P
034000                                                                  BBANK50P
034100     IF WS-SEL-COUNT IS EQUAL TO ZERO                             BBANK50P
034200        MOVE 'Please select an account to transfer from'          BBANK50P
034300          TO WS-ERROR-MSG                                         BBANK50P
034400        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
034500     END-IF.                                                      BBANK50P
034600                                                                  BBANK50P
034700     IF WS-SEL-COUNT IS GREATER THAN 1                            BBANK50P
034800        MOVE 'Please select a single account to transfer from'    BBANK50P
034900          TO WS-ERROR-MSG                                         BBANK50P
035000        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
035100     END-IF.                                                      BBANK50P
035200                                                                  BBANK50P
035300     MOVE ZERO TO WS-SEL-COUNT.                                   BBANK50P
035400                                                                  BBANK50P
035500     IF BANK-SCR50-TO1 IS NOT EQUAL TO LOW-VALUES                 BBANK50P
035600        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
035700        MOVE BANK-SCR50-ACC1 TO WS-XFER-ACCT-TO                   BBANK50P
035800        MOVE BANK-SCR50-BAL1 TO WS-XFER-ACCT-TO-BAL               BBANK50P
035900     END-IF.                                                      BBANK50P
036000     IF BANK-SCR50-TO2 IS NOT EQUAL TO LOW-VALUES                 BBANK50P
036100        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
036200        MOVE BANK-SCR50-ACC2 TO WS-XFER-ACCT-TO                   BBANK50P
036300        MOVE BANK-SCR50-BAL2 TO WS-XFER-ACCT-TO-BAL               BBANK50P
036400     END-IF.                                                      BBANK50P
036500     IF BANK-SCR50-TO3 IS NOT EQUAL TO LOW-VALUES                 BBANK50P
036600        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
036700        MOVE BANK-SCR50-ACC3 TO WS-XFER-ACCT-TO                   BBANK50P
036800        MOVE BANK-SCR50-BAL3 TO WS-XFER-ACCT-TO-BAL               BBANK50P
036900     END-IF.                                                      BBANK50P
037000     IF BANK-SCR50-TO4 IS NOT EQUAL TO LOW-VALUES                 BBANK50P
037100        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
037200        MOVE BANK-SCR50-ACC4 TO WS-XFER-ACCT-TO                   BBANK50P
037300        MOVE BANK-SCR50-BAL4 TO WS-XFER-ACCT-TO-BAL               BBANK50P
037400     END-IF.                                                      BBANK50P
037500     IF BANK-SCR50-TO5 IS NOT EQUAL TO LOW-VALUES                 BBANK50P
037600        ADD 1 TO WS-SEL-COUNT                                     BBANK50P
037700        MOVE BANK-SCR50-ACC5 TO WS-XFER-ACCT-TO                   BBANK50P
037800        MOVE BANK-SCR50-BAL5 TO WS-XFER-ACCT-TO-BAL               BBANK50P
037900     END-IF.                                                      BBANK50P
038000                                                                  BBANK50P
038100     IF WS-SEL-COUNT IS EQUAL TO ZERO                             BBANK50P
038200        MOVE 'Please select an account to transfer to'            BBANK50P
038300          TO WS-ERROR-MSG                                         BBANK50P
038400        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
038500     END-IF.                                                      BBANK50P
038600                                                                  BBANK50P
038700     IF WS-SEL-COUNT IS GREATER THAN 1                            BBANK50P
038800        MOVE 'Please select a single account to transfer to'      BBANK50P
038900          TO WS-ERROR-MSG                                         BBANK50P
039000        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
039100     END-IF.                                                      BBANK50P
039200                                                                  BBANK50P
039300     IF WS-XFER-ACCT-FROM IS EQUAL TO WS-XFER-ACCT-TO             BBANK50P
039400        MOVE 'Please select an different to & from accounts'      BBANK50P
039500          TO WS-ERROR-MSG                                         BBANK50P
039600        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
039700     END-IF.                                                      BBANK50P
039800                                                                  BBANK50P
039900* Reformat balance in from a/c from 9,999,999.99- to 999999999.   BBANK50P
040000     MOVE WS-XFER-ACCT-FROM-BAL (1:1) TO WS-XFER-AMT-TMP (1:1).   BBANK50P
040100     MOVE WS-XFER-ACCT-FROM-BAL (3:3) TO WS-XFER-AMT-TMP (2:3).   BBANK50P
040200     MOVE WS-XFER-ACCT-FROM-BAL (7:3) TO WS-XFER-AMT-TMP (5:3).   BBANK50P
040300     MOVE WS-XFER-ACCT-FROM-BAL (11:2) TO WS-XFER-AMT-TMP (8:2).  BBANK50P
040400     INSPECT WS-XFER-AMT-TMP REPLACING LEADING SPACES BY ZEROS.   BBANK50P
040500     IF WS-XFER-ACCT-FROM-BAL (13:1) IS EQUAL TO '-'              BBANK50P
040600        MULTIPLY -1 BY WS-XFER-AMT-TMP-N                          BBANK50P
040700          GIVING WS-XFER-ACCT-FROM-BAL-N                          BBANK50P
040800     ELSE                                                         BBANK50P
040900        MULTIPLY +1 BY WS-XFER-AMT-TMP-N                          BBANK50P
041000          GIVING WS-XFER-ACCT-FROM-BAL-N                          BBANK50P
041100     END-IF.                                                      BBANK50P
041200* Reformat balance in to a/c from 9,999,999.99- to 999999999.     BBANK50P
041300     MOVE WS-XFER-ACCT-TO-BAL (1:1) TO WS-XFER-AMT-TMP (1:1).     BBANK50P
041400     MOVE WS-XFER-ACCT-TO-BAL (3:3) TO WS-XFER-AMT-TMP (2:3).     BBANK50P
041500     MOVE WS-XFER-ACCT-TO-BAL (7:3) TO WS-XFER-AMT-TMP (5:3).     BBANK50P
041600     MOVE WS-XFER-ACCT-TO-BAL (11:2) TO WS-XFER-AMT-TMP (8:2).    BBANK50P
041700     INSPECT WS-XFER-AMT-TMP REPLACING LEADING SPACES BY ZEROS.   BBANK50P
041800     IF WS-XFER-ACCT-TO-BAL (13:1) IS EQUAL TO '-'                BBANK50P
041900        MULTIPLY -1 BY WS-XFER-AMT-TMP-N                          BBANK50P
042000          GIVING WS-XFER-ACCT-TO-BAL-N                            BBANK50P
042100     ELSE                                                         BBANK50P
042200        MULTIPLY +1 BY WS-XFER-AMT-TMP-N                          BBANK50P
042300          GIVING WS-XFER-ACCT-TO-BAL-N                            BBANK50P
042400     END-IF.                                                      BBANK50P
042500     IF WS-XFER-ACCT-FROM-BAL-N IS LESS THAN ZERO                 BBANK50P
042600        MOVE 'Cannot transfer from a negative balance'            BBANK50P
042700          TO WS-ERROR-MSG                                         BBANK50P
042800        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
042900     END-IF.                                                      BBANK50P
043000     IF WS-XFER-AMT-NUM-N IS GREATER THAN WS-XFER-ACCT-FROM-BAL-N BBANK50P
043100        MOVE 'Insufficient funds in from account'                 BBANK50P
043200          TO WS-ERROR-MSG                                         BBANK50P
043300        GO TO VALIDATE-DATA-ERROR                                 BBANK50P
043400     END-IF.                                                      BBANK50P
043500                                                                  BBANK50P
043600     GO TO VALIDATE-DATA-EXIT.                                    BBANK50P
043700                                                                  BBANK50P
043800 VALIDATE-DATA-ERROR.                                             BBANK50P
043900     SET INPUT-ERROR TO TRUE.                                     BBANK50P
044000 VALIDATE-DATA-EXIT.                                              BBANK50P
044100     EXIT.                                                        BBANK50P
044200                                                                  BBANK50P
044300 VALIDATE-XFER.                                                   BBANK50P
044400 VALIDATE-XFER-RIGHT-JUSTIFY.                                     BBANK50P
044500     IF WS-XFER-AMT IS EQUAL TO SPACES OR                         BBANK50P
044600        WS-XFER-AMT IS EQUAL TO LOW-VALUES                        BBANK50P
044700        MOVE 'Please enter transfer amount'                       BBANK50P
044800          TO WS-ERROR-MSG                                         BBANK50P
044900        GO TO VALIDATE-XFER-ERROR                                 BBANK50P
045000     END-IF.                                                      BBANK50P
045100     IF WS-XFER-AMT (8:1) IS EQUAL TO SPACE OR                    BBANK50P
045200        WS-XFER-AMT (8:1) IS EQUAL TO LOW-VALUE                   BBANK50P
045300        MOVE WS-XFER-AMT (1:7) TO WS-XFER-AMT-TMP                 BBANK50P
045400        MOVE SPACES TO WS-XFER-AMT                                BBANK50P
045500        MOVE WS-XFER-AMT-TMP TO WS-XFER-AMT (2:7)                 BBANK50P
045600        GO TO VALIDATE-XFER-RIGHT-JUSTIFY                         BBANK50P
045700     END-IF.                                                      BBANK50P
045800     IF WS-XFER-AMT (6:1) IS NOT EQUAL TO '.'                     BBANK50P
045900        MOVE 'Period missing/misplaced in transfer amount'        BBANK50P
046000          TO WS-ERROR-MSG                                         BBANK50P
046100        GO TO VALIDATE-XFER-ERROR                                 BBANK50P
046200     END-IF.                                                      BBANK50P
046300     MOVE SPACES TO WS-XFER-AMT-NUM.                              BBANK50P
046400     MOVE WS-XFER-AMT (1:5) TO WS-XFER-AMT-NUM (3:5).             BBANK50P
046500     MOVE WS-XFER-AMT (7:2) TO WS-XFER-AMT-NUM (8:2).             BBANK50P
046600     INSPECT WS-XFER-AMT-NUM REPLACING LEADING SPACES BY ZEROS.   BBANK50P
046700     IF WS-XFER-AMT-NUM IS NOT NUMERIC                            BBANK50P
046800        MOVE 'Transfer amount is invalid (not numeric)'           BBANK50P
046900          TO WS-ERROR-MSG                                         BBANK50P
047000        GO TO VALIDATE-XFER-ERROR                                 BBANK50P
047100     END-IF.                                                      BBANK50P
047200     IF WS-XFER-AMT-NUM IS EQUAL TO ZERO                          BBANK50P
047300        MOVE 'Please enter a non-zero transfer amount'            BBANK50P
047400          TO WS-ERROR-MSG                                         BBANK50P
047500        GO TO VALIDATE-XFER-ERROR                                 BBANK50P
047600     END-IF.                                                      BBANK50P
047700                                                                  BBANK50P
047800     GO TO VALIDATE-XFER-EXIT.                                    BBANK50P
047900                                                                  BBANK50P
048000 VALIDATE-XFER-ERROR.                                             BBANK50P
048100     SET INPUT-ERROR TO TRUE.                                     BBANK50P
048200 VALIDATE-XFER-EXIT.                                              BBANK50P
048300     EXIT.                                                        BBANK50P
048400                                                                  BBANK50P
048500 POPULATE-SCREEN-DATA.                                            BBANK50P
048600     MOVE SPACES TO CD03-DATA.                                    BBANK50P
048700     MOVE BANK-USERID TO CD03I-CONTACT-ID.                        BBANK50P
048800* Now go get the data                                             BBANK50P
048900 COPY CBANKX03.                                                   BBANK50P
049000     MOVE CD03O-ACC1 TO BANK-SCR50-ACC1.                          BBANK50P
049100     MOVE CD03O-DSC1 TO BANK-SCR50-DSC1.                          BBANK50P
049200     IF CD03O-BAL1 IS EQUAL TO SPACES                             BBANK50P
049300        MOVE CD03O-BAL1 TO BANK-SCR50-BAL1                        BBANK50P
049400     ELSE                                                         BBANK50P
049500        MOVE CD03O-BAL1N TO WS-EDIT-BALANCE                       BBANK50P
049600        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL1.                  BBANK50P
049700     MOVE CD03O-ACC2 TO BANK-SCR50-ACC2.                          BBANK50P
049800     MOVE CD03O-DSC2 TO BANK-SCR50-DSC2.                          BBANK50P
049900     IF CD03O-BAL2 IS EQUAL TO SPACES                             BBANK50P
050000        MOVE CD03O-BAL2 TO BANK-SCR50-BAL2                        BBANK50P
050100     ELSE                                                         BBANK50P
050200        MOVE CD03O-BAL2N TO WS-EDIT-BALANCE                       BBANK50P
050300        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL2.                  BBANK50P
050400     MOVE CD03O-ACC3 TO BANK-SCR50-ACC3.                          BBANK50P
050500     MOVE CD03O-DSC3 TO BANK-SCR50-DSC3.                          BBANK50P
050600     IF CD03O-BAL3 IS EQUAL TO SPACES                             BBANK50P
050700        MOVE CD03O-BAL3 TO BANK-SCR50-BAL3                        BBANK50P
050800     ELSE                                                         BBANK50P
050900        MOVE CD03O-BAL3N TO WS-EDIT-BALANCE                       BBANK50P
051000        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL3.                  BBANK50P
051100     MOVE CD03O-ACC4 TO BANK-SCR50-ACC4.                          BBANK50P
051200     MOVE CD03O-DSC4 TO BANK-SCR50-DSC4.                          BBANK50P
051300     IF CD03O-BAL4 IS EQUAL TO SPACES                             BBANK50P
051400        MOVE CD03O-BAL4 TO BANK-SCR50-BAL4                        BBANK50P
051500     ELSE                                                         BBANK50P
051600        MOVE CD03O-BAL4N TO WS-EDIT-BALANCE                       BBANK50P
051700        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL4.                  BBANK50P
051800     MOVE CD03O-ACC5 TO BANK-SCR50-ACC5.                          BBANK50P
051900     MOVE CD03O-DSC5 TO BANK-SCR50-DSC5.                          BBANK50P
052000     IF CD03O-BAL5 IS EQUAL TO SPACES                             BBANK50P
052100        MOVE CD03O-BAL5 TO BANK-SCR50-BAL5                        BBANK50P
052200     ELSE                                                         BBANK50P
052300        MOVE CD03O-BAL5N TO WS-EDIT-BALANCE                       BBANK50P
052400        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL5.                  BBANK50P
052500                                                                  BBANK50P
052600 POPULATE-SCREEN-DATA-EXIT.                                       BBANK50P
052700     EXIT.                                                        BBANK50P
052800                                                                  BBANK50P
