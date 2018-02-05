000100***************************************************************** BBANK70P
000200*                                                               * BBANK70P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK70P
000400*   This demonstration program is provided for use by users     * BBANK70P
000500*   of Micro Focus products and may be used, modified and       * BBANK70P
000600*   distributed as part of your application provided that       * BBANK70P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK70P
000800*   in this material.                                           * BBANK70P
000900*                                                               * BBANK70P
001000***************************************************************** BBANK70P
001100                                                                  BBANK70P
001200***************************************************************** BBANK70P
001300* Program:     BBANK70P.CBL                                     * BBANK70P
001400* Layer:       Business logic                                   * BBANK70P
001500* Function:    Calculate cost of loan                           * BBANK70P
001600***************************************************************** BBANK70P
001700                                                                  BBANK70P
001800 IDENTIFICATION DIVISION.                                         BBANK70P
001900 PROGRAM-ID.                                                      BBANK70P
002000     BBANK70P.                                                    BBANK70P
002100 DATE-WRITTEN.                                                    BBANK70P
002200     September 2002.                                              BBANK70P
002300 DATE-COMPILED.                                                   BBANK70P
002400     Today.                                                       BBANK70P
002500                                                                  BBANK70P
002600 ENVIRONMENT DIVISION.                                            BBANK70P
002700                                                                  BBANK70P
002800 DATA DIVISION.                                                   BBANK70P
002900 WORKING-STORAGE SECTION.                                         BBANK70P
003000 01  WS-MISC-STORAGE.                                             BBANK70P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK70P
003200       VALUE 'BBANK70P'.                                          BBANK70P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK70P
003400     88  INPUT-OK                            VALUE '0'.           BBANK70P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK70P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK70P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK70P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK70P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK70P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK70P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK70P
004200     88  PFK-VALID                           VALUE '0'.           BBANK70P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK70P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK70P
004500*                                                                 BBANK70P
004600   05  WS-CALC-WORK-AREAS.                                        BBANK70P
004700* Used to count no of periods in rate                             BBANK70P
004800     10  WS-CALC-WORK-RATE-PERIOD            PIC 9(1).            BBANK70P
004900* Generate work area                                              BBANK70P
005000     10  WS-CALC-WORK-TEMP                   PIC X(7).            BBANK70P
005100* Work area for AMOUNT                                            BBANK70P
005200     10  WS-CALC-WORK-AMOUNT                 PIC X(7).            BBANK70P
005300     10  WS-CALC-WORK-AMOUNT-N REDEFINES WS-CALC-WORK-AMOUNT      BBANK70P
005400                                             PIC 9(7).            BBANK70P
005500* Work area for RATE                                              BBANK70P
005600     10  WS-CALC-WORK-RATE                   PIC X(7).            BBANK70P
005700* Used to hold first part of rate (before the period)             BBANK70P
005800     10  WS-CALC-WORK-RATE-P1                PIC X(6).            BBANK70P
005900     10  WS-CALC-WORK-RATE-P1-N REDEFINES WS-CALC-WORK-RATE-P1    BBANK70P
006000                                             PIC 9(6).            BBANK70P
006100* Used to hold second part of rate (before the period)            BBANK70P
006200     10  WS-CALC-WORK-RATE-P2                PIC X(6).            BBANK70P
006300     10  WS-CALC-WORK-RATE-P2-N REDEFINES WS-CALC-WORK-RATE-P2    BBANK70P
006400                                             PIC 9(6).            BBANK70P
006500* Used to hold rate as percentage (xxxvxxx)                       BBANK70P
006600     10  WS-CALC-WORK-PERC                   PIC X(6).            BBANK70P
006700     10  WS-CALC-WORK-PERC-N REDEFINES WS-CALC-WORK-PERC          BBANK70P
006800                                             PIC 9(3)V9(3).       BBANK70P
006900* Work area for TERM                                              BBANK70P
007000     10  WS-CALC-WORK-TERM                   PIC X(5).            BBANK70P
007100     10  WS-CALC-WORK-TERM-N REDEFINES WS-CALC-WORK-TERM          BBANK70P
007200                                             PIC 9(5).            BBANK70P
007300* Work area for PAYMENT                                           BBANK70P
007400     10  WS-CALC-WORK-PAYMENT                PIC X(9).            BBANK70P
007500     10  WS-CALC-WORK-PAYMENT-N REDEFINES WS-CALC-WORK-PAYMENT    BBANK70P
007600                                             PIC $$$$$9.99.       BBANK70P
007700                                                                  BBANK70P
007800   05  WS-LOAN-AREAS.                                             BBANK70P
007900     10  WS-LOAN-PRINCIPAL                   PIC S9(7).           BBANK70P
008000     10  WS-LOAN-INTEREST                    PIC SV9(8).          BBANK70P
008100     10  WS-LOAN-TERM                        PIC S9(5).           BBANK70P
008200     10  WS-LOAN-MONTHLY-PAYMENT             PIC S9(6)V99.        BBANK70P
008300                                                                  BBANK70P
008400 01  WS-BANK-DATA.                                                BBANK70P
008500 COPY CBANKDAT.                                                   BBANK70P
008600                                                                  BBANK70P
008700 01  WS-HELP-DATA.                                                BBANK70P
008800 COPY CHELPD01.                                                   BBANK70P
008900                                                                  BBANK70P
009000 COPY CABENDD.                                                    BBANK70P
009100                                                                  BBANK70P
009200 LINKAGE SECTION.                                                 BBANK70P
009300 01  DFHCOMMAREA.                                                 BBANK70P
009400   05  LK-COMMAREA                           PIC X(6144).         BBANK70P
009500                                                                  BBANK70P
009600 COPY CENTRY.                                                     BBANK70P
009700***************************************************************** BBANK70P
009800* Make ourselves re-entrant                                     * BBANK70P
009900***************************************************************** BBANK70P
010000     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK70P
010100                                                                  BBANK70P
010200***************************************************************** BBANK70P
010300* Move the passed area to Bour area                              *BBANK70P
010400***************************************************************** BBANK70P
010500     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK70P
010600                                                                  BBANK70P
010700***************************************************************** BBANK70P
010800* Ensure error message is cleared                               * BBANK70P
010900***************************************************************** BBANK70P
011000     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK70P
011100                                                                  BBANK70P
011200***************************************************************** BBANK70P
011300* This is the main process                                      * BBANK70P
011400***************************************************************** BBANK70P
011500                                                                  BBANK70P
011600***************************************************************** BBANK70P
011700* Save the passed return message and then turn it off           * BBANK70P
011800***************************************************************** BBANK70P
011900     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANK70P
012000     SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANK70P
012100                                                                  BBANK70P
012200***************************************************************** BBANK70P
012300* Check the AID to see if its valid at this point               * BBANK70P
012400***************************************************************** BBANK70P
012500     SET PFK-INVALID TO TRUE.                                     BBANK70P
012600     IF BANK-AID-ENTER OR                                         BBANK70P
012700        BANK-AID-PFK03 OR                                         BBANK70P
012800        BANK-AID-PFK04                                            BBANK70P
012900        SET PFK-VALID TO TRUE                                     BBANK70P
013000     END-IF.                                                      BBANK70P
013100     IF BANK-AID-PFK01 AND                                        BBANK70P
013200        BANK-HELP-INACTIVE                                        BBANK70P
013300        SET BANK-HELP-ACTIVE TO TRUE                              BBANK70P
013400        SET PFK-VALID TO TRUE                                     BBANK70P
013500     END-IF.                                                      BBANK70P
013600     IF PFK-INVALID                                               BBANK70P
013700        SET BANK-AID-ENTER TO TRUE                                BBANK70P
013800     END-IF.                                                      BBANK70P
013900                                                                  BBANK70P
014000***************************************************************** BBANK70P
014100* Check the AID to see if we have to quit                       * BBANK70P
014200***************************************************************** BBANK70P
014300     IF BANK-AID-PFK03                                            BBANK70P
014400        MOVE 'BBANK70P' TO BANK-LAST-PROG                         BBANK70P
014500        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK70P
014600        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK70P
014700        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK70P
014800        GO TO COMMON-RETURN                                       BBANK70P
014900     END-IF.                                                      BBANK70P
015000                                                                  BBANK70P
015100***************************************************************** BBANK70P
015200* Check the to see if user needs or has been using help         * BBANK70P
015300***************************************************************** BBANK70P
015400     IF BANK-HELP-ACTIVE                                          BBANK70P
015500        IF BANK-AID-PFK04                                         BBANK70P
015600           SET BANK-HELP-INACTIVE TO TRUE                         BBANK70P
015700           MOVE 00 TO BANK-HELP-SCREEN                            BBANK70P
015800           MOVE 'BBANK70P' TO BANK-LAST-PROG                      BBANK70P
015900           MOVE 'BBANK70P' TO BANK-NEXT-PROG                      BBANK70P
016000           MOVE 'MBANK70' TO BANK-LAST-MAPSET                     BBANK70P
016100           MOVE 'HELP70A' TO BANK-LAST-MAP                        BBANK70P
016200           MOVE 'MBANK70' TO BANK-NEXT-MAPSET                     BBANK70P
016300           MOVE 'BANK70A' TO BANK-NEXT-MAP                        BBANK70P
016400           GO TO COMMON-RETURN                                    BBANK70P
016500        ELSE                                                      BBANK70P
016600           MOVE 01 TO BANK-HELP-SCREEN                            BBANK70P
016700           MOVE 'BBANK70P' TO BANK-LAST-PROG                      BBANK70P
016800           MOVE 'BBANK70P' TO BANK-NEXT-PROG                      BBANK70P
016900           MOVE 'MBANK70' TO BANK-LAST-MAPSET                     BBANK70P
017000           MOVE 'BANK70A' TO BANK-LAST-MAP                        BBANK70P
017100           MOVE 'MBANK70' TO BANK-NEXT-MAPSET                     BBANK70P
017200           MOVE 'HELP70A' TO BANK-NEXT-MAP                        BBANK70P
017300           MOVE 'BANK70' TO HELP01I-SCRN                          BBANK70P
017400           COPY CHELPX01.                                         BBANK70P
017500           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK70P
017600           GO TO COMMON-RETURN                                    BBANK70P
017700     END-IF.                                                      BBANK70P
017800                                                                  BBANK70P
017900***************************************************************** BBANK70P
018000* Check the AID to see if we have to return to previous screen  * BBANK70P
018100***************************************************************** BBANK70P
018200     IF BANK-AID-PFK04                                            BBANK70P
018300        MOVE 'BBANK70P' TO BANK-LAST-PROG                         BBANK70P
018400        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK70P
018500        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK70P
018600        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK70P
018700        SET BANK-AID-ENTER TO TRUE                                BBANK70P
018800        GO TO COMMON-RETURN                                       BBANK70P
018900     END-IF.                                                      BBANK70P
019000                                                                  BBANK70P
019100* Check if we have set the screen up before or is this 1st time   BBANK70P
019200     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK70'                BBANK70P
019300        MOVE SPACES TO BANK-SCR70-AMOUNT                          BBANK70P
019400        MOVE SPACES TO BANK-SCR70-RATE                            BBANK70P
019500        MOVE SPACES TO BANK-SCR70-TERM                            BBANK70P
019600        MOVE SPACES TO BANK-SCR70-PAYMENT                         BBANK70P
019700        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK70P
019800        MOVE 'BBANK70P' TO BANK-LAST-PROG                         BBANK70P
019900        MOVE 'BBANK70P' TO BANK-NEXT-PROG                         BBANK70P
020000        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        BBANK70P
020100        MOVE 'BANK70A' TO BANK-LAST-MAP                           BBANK70P
020200        MOVE 'MBANK70' TO BANK-NEXT-MAPSET                        BBANK70P
020300        MOVE 'BANK70A' TO BANK-NEXT-MAP                           BBANK70P
020400        GO TO COMMON-RETURN                                       BBANK70P
020500     END-IF.                                                      BBANK70P
020600                                                                  BBANK70P
020700     PERFORM VALIDATE-DATA THRU                                   BBANK70P
020800             VALIDATE-DATA-EXIT.                                  BBANK70P
020900                                                                  BBANK70P
021000* If we had an error display error and return                     BBANK70P
021100     IF INPUT-ERROR                                               BBANK70P
021200        MOVE SPACES TO BANK-SCR70-PAYMENT                         BBANK70P
021300        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK70P
021400        MOVE 'BBANK70P' TO BANK-LAST-PROG                         BBANK70P
021500        MOVE 'BBANK70P' TO BANK-NEXT-PROG                         BBANK70P
021600        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        BBANK70P
021700        MOVE 'BANK70A' TO BANK-LAST-MAP                           BBANK70P
021800        MOVE 'MBANK70' TO BANK-NEXT-MAPSET                        BBANK70P
021900        MOVE 'BANK70A' TO BANK-NEXT-MAP                           BBANK70P
022000        GO TO COMMON-RETURN                                       BBANK70P
022100     END-IF.                                                      BBANK70P
022200                                                                  BBANK70P
022300* Now calculate the monthly cost of the loan                      BBANK70P
022400     MOVE WS-CALC-WORK-AMOUNT-N TO WS-LOAN-PRINCIPAL.             BBANK70P
022500     DIVIDE 100 INTO WS-CALC-WORK-PERC-N                          BBANK70P
022600       GIVING WS-LOAN-INTEREST.                                   BBANK70P
022700     MOVE WS-CALC-WORK-TERM-N TO WS-LOAN-TERM.                    BBANK70P
022800     MOVE ZERO TO WS-LOAN-MONTHLY-PAYMENT.                        BBANK70P
022900                                                                  BBANK70P
023000     DIVIDE WS-LOAN-INTEREST BY 12                                BBANK70P
023100       GIVING WS-LOAN-INTEREST ROUNDED.                           BBANK70P
023200     COMPUTE WS-LOAN-MONTHLY-PAYMENT ROUNDED =                    BBANK70P
023300       ((WS-LOAN-INTEREST * ((1 + WS-LOAN-INTEREST)               BBANK70P
023400           ** WS-LOAN-TERM)) /                                    BBANK70P
023500       (((1 + WS-LOAN-INTEREST) ** WS-LOAN-TERM) - 1 ))           BBANK70P
023600         * WS-LOAN-PRINCIPAL.                                     BBANK70P
023700     MOVE WS-LOAN-MONTHLY-PAYMENT TO WS-CALC-WORK-PAYMENT-N.      BBANK70P
023800     MOVE WS-CALC-WORK-PAYMENT TO BANK-SCR70-PAYMENT.             BBANK70P
023900* Left justify the result                                         BBANK70P
024000 LEFT-JUST-PAYMENT.                                               BBANK70P
024100     IF BANK-SCR70-PAYMENT (1:1) IS EQUAL TO ' '                  BBANK70P
024200        MOVE BANK-SCR70-PAYMENT (2:8) TO BANK-SCR70-PAYMENT (1:8) BBANK70P
024300        MOVE ' ' TO BANK-SCR70-PAYMENT (9:1)                      BBANK70P
024400        GO TO LEFT-JUST-PAYMENT                                   BBANK70P
024500     END-IF                                                       BBANK70P
024600                                                                  BBANK70P
024700     GO TO COMMON-RETURN.                                         BBANK70P
024800                                                                  BBANK70P
024900                                                                  BBANK70P
025000***************************************************************** BBANK70P
025100* If we get this far then we have an error in our logic as we   * BBANK70P
025200* don't know where to go next.                                  * BBANK70P
025300***************************************************************** BBANK70P
025400     IF NOT BANK-ENV-CICS                                         BBANK70P
025500        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK70P
025600        MOVE '0001' TO ABEND-CODE                                 BBANK70P
025700        MOVE SPACES TO ABEND-REASON                               BBANK70P
025800        COPY CABENDPO.                                            BBANK70P
025900     END-IF.                                                      BBANK70P
026000     GOBACK.                                                      BBANK70P
026100                                                                  BBANK70P
026200 COMMON-RETURN.                                                   BBANK70P
026300     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK70P
026400 COPY CRETURN.                                                    BBANK70P
026500                                                                  BBANK70P
026600 VALIDATE-DATA.                                                   BBANK70P
026700     SET INPUT-OK TO TRUE.                                        BBANK70P
026800                                                                  BBANK70P
026900     MOVE BANK-SCR70-AMOUNT TO WS-CALC-WORK-AMOUNT.               BBANK70P
027000     PERFORM VALIDATE-AMOUNT THRU                                 BBANK70P
027100             VALIDATE-AMOUNT-EXIT.                                BBANK70P
027200     IF NOT INPUT-OK                                              BBANK70P
027300        GO TO VALIDATE-DATA-ERROR                                 BBANK70P
027400     END-IF.                                                      BBANK70P
027500                                                                  BBANK70P
027600     MOVE BANK-SCR70-RATE TO WS-CALC-WORK-RATE.                   BBANK70P
027700     PERFORM VALIDATE-RATE THRU                                   BBANK70P
027800             VALIDATE-RATE-EXIT.                                  BBANK70P
027900     IF NOT INPUT-OK                                              BBANK70P
028000        GO TO VALIDATE-DATA-ERROR                                 BBANK70P
028100     END-IF.                                                      BBANK70P
028200                                                                  BBANK70P
028300     MOVE BANK-SCR70-TERM TO WS-CALC-WORK-TERM.                   BBANK70P
028400     PERFORM VALIDATE-TERM THRU                                   BBANK70P
028500             VALIDATE-TERM-EXIT.                                  BBANK70P
028600     IF NOT INPUT-OK                                              BBANK70P
028700        GO TO VALIDATE-DATA-ERROR                                 BBANK70P
028800     END-IF.                                                      BBANK70P
028900                                                                  BBANK70P
029000     GO TO VALIDATE-DATA-EXIT.                                    BBANK70P
029100                                                                  BBANK70P
029200 VALIDATE-DATA-ERROR.                                             BBANK70P
029300     SET INPUT-ERROR TO TRUE.                                     BBANK70P
029400 VALIDATE-DATA-EXIT.                                              BBANK70P
029500     EXIT.                                                        BBANK70P
029600                                                                  BBANK70P
029700 VALIDATE-AMOUNT.                                                 BBANK70P
029800 VALIDATE-AMOUNT-RIGHT-JUSTIFY.                                   BBANK70P
029900     IF WS-CALC-WORK-AMOUNT IS EQUAL TO SPACES OR                 BBANK70P
030000        WS-CALC-WORK-AMOUNT IS EQUAL TO LOW-VALUES                BBANK70P
030100        MOVE 'Please enter an amount'                             BBANK70P
030200          TO WS-ERROR-MSG                                         BBANK70P
030300        GO TO VALIDATE-AMOUNT-ERROR                               BBANK70P
030400     END-IF.                                                      BBANK70P
030500     IF WS-CALC-WORK-AMOUNT (7:1) IS EQUAL TO SPACES OR           BBANK70P
030600        WS-CALC-WORK-AMOUNT (7:1) IS EQUAL TO LOW-VALUE           BBANK70P
030700        MOVE WS-CALC-WORK-AMOUNT (1:6) TO WS-CALC-WORK-TEMP       BBANK70P
030800        MOVE SPACES TO WS-CALC-WORK-AMOUNT                        BBANK70P
030900        MOVE WS-CALC-WORK-TEMP (1:6) TO WS-CALC-WORK-AMOUNT (2:6) BBANK70P
031000        GO TO VALIDATE-AMOUNT-RIGHT-JUSTIFY                       BBANK70P
031100     END-IF.                                                      BBANK70P
031200     INSPECT WS-CALC-WORK-AMOUNT                                  BBANK70P
031300       REPLACING LEADING SPACES BY ZEROS.                         BBANK70P
031400     IF WS-CALC-WORK-AMOUNT IS NOT NUMERIC                        BBANK70P
031500        MOVE 'Amount is invalid (not numeric)'                    BBANK70P
031600          TO WS-ERROR-MSG                                         BBANK70P
031700        GO TO VALIDATE-AMOUNT-ERROR                               BBANK70P
031800     END-IF.                                                      BBANK70P
031900     IF WS-CALC-WORK-AMOUNT IS EQUAL TO ZERO                      BBANK70P
032000        MOVE 'Please enter a non-zero amount'                     BBANK70P
032100          TO WS-ERROR-MSG                                         BBANK70P
032200        GO TO VALIDATE-AMOUNT-ERROR                               BBANK70P
032300     END-IF.                                                      BBANK70P
032400                                                                  BBANK70P
032500     GO TO VALIDATE-AMOUNT-EXIT.                                  BBANK70P
032600                                                                  BBANK70P
032700 VALIDATE-AMOUNT-ERROR.                                           BBANK70P
032800     SET INPUT-ERROR TO TRUE.                                     BBANK70P
032900 VALIDATE-AMOUNT-EXIT.                                            BBANK70P
033000     EXIT.                                                        BBANK70P
033100                                                                  BBANK70P
033200 VALIDATE-RATE.                                                   BBANK70P
033300 VALIDATE-RATE-RIGHT-JUSTIFY.                                     BBANK70P
033400     IF WS-CALC-WORK-RATE IS EQUAL TO SPACES OR                   BBANK70P
033500        WS-CALC-WORK-RATE IS EQUAL TO LOW-VALUES                  BBANK70P
033600        MOVE 'Please enter an interest rate in the form 999.999'  BBANK70P
033700          TO WS-ERROR-MSG                                         BBANK70P
033800        GO TO VALIDATE-RATE-ERROR                                 BBANK70P
033900     END-IF.                                                      BBANK70P
034000     IF WS-CALC-WORK-RATE (7:1) IS EQUAL TO SPACES OR             BBANK70P
034100        WS-CALC-WORK-RATE (7:1) IS EQUAL TO LOW-VALUE             BBANK70P
034200        MOVE WS-CALC-WORK-RATE (1:6) TO WS-CALC-WORK-TEMP         BBANK70P
034300        MOVE SPACES TO WS-CALC-WORK-RATE                          BBANK70P
034400        MOVE WS-CALC-WORK-TEMP (1:6) TO WS-CALC-WORK-RATE (2:6)   BBANK70P
034500        GO TO VALIDATE-RATE-RIGHT-JUSTIFY                         BBANK70P
034600     END-IF.                                                      BBANK70P
034700     INSPECT WS-CALC-WORK-RATE REPLACING LEADING SPACES BY ZERO.  BBANK70P
034800     MOVE ZERO TO WS-CALC-WORK-RATE-PERIOD.                       BBANK70P
034900     MOVE ZEROS TO WS-CALC-WORK-RATE-P1.                          BBANK70P
035000     MOVE ZEROS TO WS-CALC-WORK-RATE-P2.                          BBANK70P
035100* Rate is in form .xxxxxx                                         BBANK70P
035200     IF WS-CALC-WORK-RATE (1:1) IS EQUAL TO '.'                   BBANK70P
035300        MOVE ZEROS                   TO WS-CALC-WORK-RATE-P1      BBANK70P
035400        MOVE WS-CALC-WORK-RATE (2:6) TO WS-CALC-WORK-RATE-P2 (1:6)BBANK70P
035500        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         BBANK70P
035600     END-IF.                                                      BBANK70P
035700* Rate is in form x.xxxxx                                         BBANK70P
035800     IF WS-CALC-WORK-RATE (2:1) IS EQUAL TO '.'                   BBANK70P
035900        MOVE WS-CALC-WORK-RATE (1:1) TO WS-CALC-WORK-RATE-P1 (6:1)BBANK70P
036000        MOVE WS-CALC-WORK-RATE (3:5) TO WS-CALC-WORK-RATE-P2 (1:5)BBANK70P
036100        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         BBANK70P
036200     END-IF.                                                      BBANK70P
036300* Rate is in form xx.xxxx                                         BBANK70P
036400     IF WS-CALC-WORK-RATE (3:1) IS EQUAL TO '.'                   BBANK70P
036500        MOVE WS-CALC-WORK-RATE (1:2) TO WS-CALC-WORK-RATE-P1 (5:2)BBANK70P
036600        MOVE WS-CALC-WORK-RATE (4:4) TO WS-CALC-WORK-RATE-P2 (1:4)BBANK70P
036700        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         BBANK70P
036800     END-IF.                                                      BBANK70P
036900* Rate is in form xxx.xxx                                         BBANK70P
037000     IF WS-CALC-WORK-RATE (4:1) IS EQUAL TO '.'                   BBANK70P
037100        MOVE WS-CALC-WORK-RATE (1:3) TO WS-CALC-WORK-RATE-P1 (4:3)BBANK70P
037200        MOVE WS-CALC-WORK-RATE (5:3) TO WS-CALC-WORK-RATE-P2 (1:3)BBANK70P
037300        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         BBANK70P
037400     END-IF.                                                      BBANK70P
037500* Rate is in form xxxx.xx                                         BBANK70P
037600     IF WS-CALC-WORK-RATE (5:1) IS EQUAL TO '.'                   BBANK70P
037700        MOVE WS-CALC-WORK-RATE (1:4) TO WS-CALC-WORK-RATE-P1 (3:4)BBANK70P
037800        MOVE WS-CALC-WORK-RATE (6:2) TO WS-CALC-WORK-RATE-P2 (1:2)BBANK70P
037900        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         BBANK70P
038000     END-IF.                                                      BBANK70P
038100* Rate is in form xxxxx.x                                         BBANK70P
038200     IF WS-CALC-WORK-RATE (6:1) IS EQUAL TO '.'                   BBANK70P
038300        MOVE WS-CALC-WORK-RATE (1:5) TO WS-CALC-WORK-RATE-P1 (2:5)BBANK70P
038400        MOVE WS-CALC-WORK-RATE (7:1) TO WS-CALC-WORK-RATE-P2 (1:1)BBANK70P
038500        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         BBANK70P
038600     END-IF.                                                      BBANK70P
038700* Rate is in form xxxxxx.                                         BBANK70P
038800     IF WS-CALC-WORK-RATE (7:1) IS EQUAL TO '.'                   BBANK70P
038900        MOVE WS-CALC-WORK-RATE (1:6) TO WS-CALC-WORK-RATE-P1 (1:6)BBANK70P
039000        MOVE ZEROS                   TO WS-CALC-WORK-RATE-P2 (1:1)BBANK70P
039100        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         BBANK70P
039200     END-IF.                                                      BBANK70P
039300     IF WS-CALC-WORK-RATE-PERIOD IS NOT EQUAL TO 1                BBANK70P
039400        MOVE 'Decimal point missing/misplaced in interest rate'   BBANK70P
039500          TO WS-ERROR-MSG                                         BBANK70P
039600        GO TO VALIDATE-RATE-ERROR                                 BBANK70P
039700     END-IF.                                                      BBANK70P
039800     IF WS-CALC-WORK-RATE-P1 IS NOT NUMERIC OR                    BBANK70P
039900        WS-CALC-WORK-RATE-P2 IS NOT NUMERIC                       BBANK70P
040000        MOVE 'Rate is not numeric'                                BBANK70P
040100          TO WS-ERROR-MSG                                         BBANK70P
040200        GO TO VALIDATE-RATE-ERROR                                 BBANK70P
040300     END-IF.                                                      BBANK70P
040400     IF WS-CALC-WORK-RATE-P2 (4:3) IS NOT EQUAL TO '000'          BBANK70P
040500        MOVE 'Rate has too many decimal places'                   BBANK70P
040600          TO WS-ERROR-MSG                                         BBANK70P
040700        GO TO VALIDATE-RATE-ERROR                                 BBANK70P
040800     END-IF.                                                      BBANK70P
040900* Bring parts of rate together with no physical decimal point     BBANK70P
041000     MOVE WS-CALC-WORK-RATE-P1 (4:3) TO WS-CALC-WORK-PERC (1:3).  BBANK70P
041100     MOVE WS-CALC-WORK-RATE-P2 (1:3) TO WS-CALC-WORK-PERC (4:3).  BBANK70P
041200                                                                  BBANK70P
041300     IF WS-CALC-WORK-PERC-N IS NOT GREATER THAN ZERO              BBANK70P
041400        MOVE 'Nothing''s free. Rate must be greater than 0%'      BBANK70P
041500          TO WS-ERROR-MSG                                         BBANK70P
041600        GO TO VALIDATE-RATE-ERROR                                 BBANK70P
041700     END-IF.                                                      BBANK70P
041800     IF WS-CALC-WORK-PERC-N IS NOT LESS THAN 100.000              BBANK70P
041900        MOVE 'Outrageous rate - 100% or more'                     BBANK70P
042000          TO WS-ERROR-MSG                                         BBANK70P
042100        GO TO VALIDATE-RATE-ERROR                                 BBANK70P
042200     END-IF.                                                      BBANK70P
042300                                                                  BBANK70P
042400     GO TO VALIDATE-RATE-EXIT.                                    BBANK70P
042500                                                                  BBANK70P
042600 VALIDATE-RATE-ERROR.                                             BBANK70P
042700     SET INPUT-ERROR TO TRUE.                                     BBANK70P
042800 VALIDATE-RATE-EXIT.                                              BBANK70P
042900     EXIT.                                                        BBANK70P
043000                                                                  BBANK70P
043100 VALIDATE-TERM.                                                   BBANK70P
043200 VALIDATE-TERM-RIGHT-JUSTIFY.                                     BBANK70P
043300     IF WS-CALC-WORK-TERM IS EQUAL TO SPACES OR                   BBANK70P
043400        WS-CALC-WORK-TERM IS EQUAL TO LOW-VALUES                  BBANK70P
043500        MOVE 'Please enter a term as a number of months'          BBANK70P
043600          TO WS-ERROR-MSG                                         BBANK70P
043700        GO TO VALIDATE-TERM-ERROR                                 BBANK70P
043800     END-IF.                                                      BBANK70P
043900     IF WS-CALC-WORK-TERM (5:1) IS EQUAL TO SPACES OR             BBANK70P
044000        WS-CALC-WORK-TERM (5:1) IS EQUAL TO LOW-VALUE             BBANK70P
044100        MOVE WS-CALC-WORK-TERM (1:4) TO WS-CALC-WORK-TEMP         BBANK70P
044200        MOVE SPACES TO WS-CALC-WORK-TERM                          BBANK70P
044300        MOVE WS-CALC-WORK-TEMP (1:4) TO WS-CALC-WORK-TERM (2:4)   BBANK70P
044400        GO TO VALIDATE-TERM-RIGHT-JUSTIFY                         BBANK70P
044500     END-IF.                                                      BBANK70P
044600     INSPECT WS-CALC-WORK-TERM                                    BBANK70P
044700       REPLACING LEADING SPACES BY ZEROS.                         BBANK70P
044800     IF WS-CALC-WORK-TERM IS NOT NUMERIC                          BBANK70P
044900        MOVE 'Term is invalid (not numeric)'                      BBANK70P
045000          TO WS-ERROR-MSG                                         BBANK70P
045100        GO TO VALIDATE-TERM-ERROR                                 BBANK70P
045200     END-IF.                                                      BBANK70P
045300     IF WS-CALC-WORK-TERM IS EQUAL TO ZERO                        BBANK70P
045400        MOVE 'Please enter a non-zero term'                       BBANK70P
045500          TO WS-ERROR-MSG                                         BBANK70P
045600        GO TO VALIDATE-TERM-ERROR                                 BBANK70P
045700     END-IF.                                                      BBANK70P
045800     IF WS-CALC-WORK-TERM-N IS GREATER THAN 1200                  BBANK70P
045900        MOVE 'Term exceeds 100 years!'                            BBANK70P
046000          TO WS-ERROR-MSG                                         BBANK70P
046100        GO TO VALIDATE-TERM-ERROR                                 BBANK70P
046200     END-IF.                                                      BBANK70P
046300                                                                  BBANK70P
046400     GO TO VALIDATE-TERM-EXIT.                                    BBANK70P
046500                                                                  BBANK70P
046600 VALIDATE-TERM-ERROR.                                             BBANK70P
046700     SET INPUT-ERROR TO TRUE.                                     BBANK70P
046800 VALIDATE-TERM-EXIT.                                              BBANK70P
046900     EXIT.                                                        BBANK70P
047000                                                                  BBANK70P
