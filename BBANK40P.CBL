000100***************************************************************** BBANK40P
000200*                                                               * BBANK40P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK40P
000400*   This demonstration program is provided for use by users     * BBANK40P
000500*   of Micro Focus products and may be used, modified and       * BBANK40P
000600*   distributed as part of your application provided that       * BBANK40P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK40P
000800*   in this material.                                           * BBANK40P
000900*                                                               * BBANK40P
001000***************************************************************** BBANK40P
001100                                                                  BBANK40P
001200***************************************************************** BBANK40P
001300* Program:     BBANK40P.CBL                                     * BBANK40P
001400* Layer:       Business logic                                   * BBANK40P
001500* Function:    Populate transaction deltails list for user      * BBANK40P
001600***************************************************************** BBANK40P
001700                                                                  BBANK40P
001800 IDENTIFICATION DIVISION.                                         BBANK40P
001900 PROGRAM-ID.                                                      BBANK40P
002000     BBANK40P.                                                    BBANK40P
002100 DATE-WRITTEN.                                                    BBANK40P
002200     September 2002.                                              BBANK40P
002300 DATE-COMPILED.                                                   BBANK40P
002400     Today.                                                       BBANK40P
002500                                                                  BBANK40P
002600 ENVIRONMENT DIVISION.                                            BBANK40P
002700                                                                  BBANK40P
002800 DATA DIVISION.                                                   BBANK40P
002900 WORKING-STORAGE SECTION.                                         BBANK40P
003000 01  WS-MISC-STORAGE.                                             BBANK40P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK40P
003200       VALUE 'BBANK40P'.                                          BBANK40P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK40P
003400     88  INPUT-OK                            VALUE '0'.           BBANK40P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK40P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK40P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK40P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK40P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK40P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK40P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK40P
004200     88  PFK-VALID                           VALUE '0'.           BBANK40P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK40P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK40P
004500   05  WS-EDIT-AMT                           PIC Z,ZZZ,ZZ9.99-.   BBANK40P
004600   05  WS-DYNAMIC-PGM                        PIC X(8)             BBANK40P
004700       VALUE 'UNKNOWN'.                                           BBANK40P
004800   05  WS-SUB1                               PIC S9(4) COMP.      BBANK40P
004900   05  WS-SUB-LIMIT                          PIC S9(4) COMP.      BBANK40P
005000   05  WS-TEMP-TIME-IP                       PIC X(8).            BBANK40P
005100   05  WS-TEMP-TIME-OP                       PIC X(8).            BBANK40P
005200                                                                  BBANK40P
005300 01  WS-TIME-DATE-WORK-AREA.                                      BBANK40P
005400 COPY CDATED.                                                     BBANK40P
005500                                                                  BBANK40P
005600 01  WS-BANK-DATA.                                                BBANK40P
005700 COPY CBANKDAT.                                                   BBANK40P
005800                                                                  BBANK40P
005900 01  WS-HELP-DATA.                                                BBANK40P
006000 COPY CHELPD01.                                                   BBANK40P
006100                                                                  BBANK40P
006200 01  WS-TXN-LIST.                                                 BBANK40P
006300 COPY CBANKD05.                                                   BBANK40P
006400                                                                  BBANK40P
006500 COPY CABENDD.                                                    BBANK40P
006600                                                                  BBANK40P
006700 LINKAGE SECTION.                                                 BBANK40P
006800 01  DFHCOMMAREA.                                                 BBANK40P
006900   05  LK-COMMAREA                           PIC X(6144).         BBANK40P
007000                                                                  BBANK40P
007100 COPY CENTRY.                                                     BBANK40P
007200***************************************************************** BBANK40P
007300* Make ourselves re-entrant                                     * BBANK40P
007400***************************************************************** BBANK40P
007500     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK40P
007600                                                                  BBANK40P
007700***************************************************************** BBANK40P
007800* Move the passed area to our area                              * BBANK40P
007900***************************************************************** BBANK40P
008000     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK40P
008100                                                                  BBANK40P
008200***************************************************************** BBANK40P
008300* Ensure error message is cleared                               * BBANK40P
008400***************************************************************** BBANK40P
008500     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK40P
008600                                                                  BBANK40P
008700***************************************************************** BBANK40P
008800* This is the main process                                      * BBANK40P
008900***************************************************************** BBANK40P
009000                                                                  BBANK40P
009100***************************************************************** BBANK40P
009200* Save the passed return flag and then turn it off              * BBANK40P
009300***************************************************************** BBANK40P
009400     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     BBANK40P
009500     SET BANK-RETURN-FLAG-OFF TO TRUE.                            BBANK40P
009600                                                                  BBANK40P
009700***************************************************************** BBANK40P
009800* Check the AID to see if its valid at this point               * BBANK40P
009900***************************************************************** BBANK40P
010000     SET PFK-INVALID TO TRUE.                                     BBANK40P
010100     IF BANK-AID-ENTER OR                                         BBANK40P
010200        BANK-AID-PFK03 OR                                         BBANK40P
010300        BANK-AID-PFK04 OR                                         BBANK40P
010400        BANK-AID-PFK07 OR                                         BBANK40P
010500        BANK-AID-PFK08                                            BBANK40P
010600        SET PFK-VALID TO TRUE                                     BBANK40P
010700     END-IF.                                                      BBANK40P
010800     IF BANK-AID-PFK01 AND                                        BBANK40P
010900        BANK-HELP-INACTIVE                                        BBANK40P
011000        SET BANK-HELP-ACTIVE TO TRUE                              BBANK40P
011100        SET PFK-VALID TO TRUE                                     BBANK40P
011200     END-IF.                                                      BBANK40P
011300     IF PFK-INVALID                                               BBANK40P
011400        SET BANK-AID-ENTER TO TRUE                                BBANK40P
011500     END-IF.                                                      BBANK40P
011600                                                                  BBANK40P
011700***************************************************************** BBANK40P
011800* Check the AID to see if we have to quit                       * BBANK40P
011900***************************************************************** BBANK40P
012000     IF BANK-AID-PFK03                                            BBANK40P
012100        MOVE 'BBANK40P' TO BANK-LAST-PROG                         BBANK40P
012200        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK40P
012300        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK40P
012400        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK40P
012500        GO TO COMMON-RETURN                                       BBANK40P
012600     END-IF.                                                      BBANK40P
012700                                                                  BBANK40P
012800***************************************************************** BBANK40P
012900* Check the to see if user needs or has been using help         * BBANK40P
013000***************************************************************** BBANK40P
013100     IF BANK-HELP-ACTIVE                                          BBANK40P
013200        IF BANK-AID-PFK04                                         BBANK40P
013300           SET BANK-HELP-INACTIVE TO TRUE                         BBANK40P
013400           MOVE 00 TO BANK-HELP-SCREEN                            BBANK40P
013500           MOVE 'BBANK40P' TO BANK-LAST-PROG                      BBANK40P
013600           MOVE 'BBANK40P' TO BANK-NEXT-PROG                      BBANK40P
013700           MOVE 'MBANK40' TO BANK-LAST-MAPSET                     BBANK40P
013800           MOVE 'HELP40A' TO BANK-LAST-MAP                        BBANK40P
013900           MOVE 'MBANK40' TO BANK-NEXT-MAPSET                     BBANK40P
014000           MOVE 'BANK40A' TO BANK-NEXT-MAP                        BBANK40P
014100           GO TO COMMON-RETURN                                    BBANK40P
014200        ELSE                                                      BBANK40P
014300           MOVE 01 TO BANK-HELP-SCREEN                            BBANK40P
014400           MOVE 'BBANK40P' TO BANK-LAST-PROG                      BBANK40P
014500           MOVE 'BBANK40P' TO BANK-NEXT-PROG                      BBANK40P
014600           MOVE 'MBANK40' TO BANK-LAST-MAPSET                     BBANK40P
014700           MOVE 'BANK40A' TO BANK-LAST-MAP                        BBANK40P
014800           MOVE 'MBANK40' TO BANK-NEXT-MAPSET                     BBANK40P
014900           MOVE 'HELP40A' TO BANK-NEXT-MAP                        BBANK40P
015000           MOVE 'BANK40' TO HELP01I-SCRN                          BBANK40P
015100           COPY CHELPX01.                                         BBANK40P
015200           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK40P
015300           GO TO COMMON-RETURN                                    BBANK40P
015400     END-IF.                                                      BBANK40P
015500                                                                  BBANK40P
015600***************************************************************** BBANK40P
015700* Check the AID to see if we have to return to previous screen  * BBANK40P
015800***************************************************************** BBANK40P
015900     IF BANK-AID-PFK04                                            BBANK40P
016000        MOVE 'BBANK40P' TO BANK-LAST-PROG                         BBANK40P
016100        MOVE 'BBANK30P' TO BANK-NEXT-PROG                         BBANK40P
016200        MOVE 'MBANK30' TO BANK-NEXT-MAPSET                        BBANK40P
016300        MOVE 'BANK30A' TO BANK-NEXT-MAP                           BBANK40P
016400        SET BANK-AID-ENTER TO TRUE                                BBANK40P
016500        GO TO COMMON-RETURN                                       BBANK40P
016600     END-IF.                                                      BBANK40P
016700                                                                  BBANK40P
016800* Check if we have set the screen up before or is this 1st time   BBANK40P
016900     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK40'                BBANK40P
017000        MOVE 'BBANK40P' TO BANK-LAST-PROG                         BBANK40P
017100        MOVE 'BBANK40P' TO BANK-NEXT-PROG                         BBANK40P
017200        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        BBANK40P
017300        MOVE 'BANK40A' TO BANK-LAST-MAP                           BBANK40P
017400        MOVE 'MBANK40' TO BANK-NEXT-MAPSET                        BBANK40P
017500        MOVE 'BANK40A' TO BANK-NEXT-MAP                           BBANK40P
017600        SET BANK-PAGING-OFF TO TRUE                               BBANK40P
017700        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK40P
017800                POPULATE-SCREEN-DATA-EXIT                         BBANK40P
017900        GO TO COMMON-RETURN                                       BBANK40P
018000     END-IF.                                                      BBANK40P
018100                                                                  BBANK40P
018200***************************************************************** BBANK40P
018300* Check to see if we have a paging request                      * BBANK40P
018400***************************************************************** BBANK40P
018500     IF BANK-AID-PFK07 OR                                         BBANK40P
018600        BANK-AID-PFK08                                            BBANK40P
018700        MOVE 'BBANK40P' TO BANK-LAST-PROG                         BBANK40P
018800        MOVE 'BBANK40P' TO BANK-NEXT-PROG                         BBANK40P
018900        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        BBANK40P
019000        MOVE 'BANK40A' TO BANK-LAST-MAP                           BBANK40P
019100        MOVE 'MBANK40' TO BANK-NEXT-MAPSET                        BBANK40P
019200        MOVE 'BANK40A' TO BANK-NEXT-MAP                           BBANK40P
019300        IF BANK-AID-PFK07 AND                                     BBANK40P
019400           (BANK-PAGING-OFF OR                                    BBANK40P
019500            BANK-PAGING-FIRST)                                    BBANK40P
019600           MOVE 'Already at first page. Cannot page back.'        BBANK40P
019700             TO BANK-ERROR-MSG                                    BBANK40P
019800           GO TO COMMON-RETURN                                    BBANK40P
019900        END-IF                                                    BBANK40P
020000        IF BANK-AID-PFK08 AND                                     BBANK40P
020100           (BANK-PAGING-OFF OR                                    BBANK40P
020200            BANK-PAGING-LAST)                                     BBANK40P
020300           MOVE 'Already at last page. Cannot page forward.'      BBANK40P
020400             TO BANK-ERROR-MSG                                    BBANK40P
020500           GO TO COMMON-RETURN                                    BBANK40P
020600        END-IF                                                    BBANK40P
020700        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK40P
020800                POPULATE-SCREEN-DATA-EXIT                         BBANK40P
020900        GO TO COMMON-RETURN                                       BBANK40P
021000     END-IF.                                                      BBANK40P
021100                                                                  BBANK40P
021200* Check if we have set the screen up before or is this 1st time   BBANK40P
021300     IF BANK-LAST-MAPSET IS EQUAL TO 'MBANK40'                    BBANK40P
021400        MOVE 'BBANK40P' TO BANK-LAST-PROG                         BBANK40P
021500        MOVE 'BBANK40P' TO BANK-NEXT-PROG                         BBANK40P
021600        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        BBANK40P
021700        MOVE 'BANK40A' TO BANK-LAST-MAP                           BBANK40P
021800        MOVE 'MBANK40' TO BANK-NEXT-MAPSET                        BBANK40P
021900        MOVE 'BANK40A' TO BANK-NEXT-MAP                           BBANK40P
022000        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK40P
022100                POPULATE-SCREEN-DATA-EXIT                         BBANK40P
022200        GO TO COMMON-RETURN                                       BBANK40P
022300     END-IF.                                                      BBANK40P
022400                                                                  BBANK40P
022500***************************************************************** BBANK40P
022600* If we get this far then we have an error in our logic as we   * BBANK40P
022700* don't know where to go next.                                  * BBANK40P
022800***************************************************************** BBANK40P
022900     IF NOT BANK-ENV-CICS                                         BBANK40P
023000        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK40P
023100        MOVE '0001' TO ABEND-CODE                                 BBANK40P
023200        MOVE SPACES TO ABEND-REASON                               BBANK40P
023300        COPY CABENDPO.                                            BBANK40P
023400     END-IF.                                                      BBANK40P
023500     GOBACK.                                                      BBANK40P
023600                                                                  BBANK40P
023700 COMMON-RETURN.                                                   BBANK40P
023800     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK40P
023900 COPY CRETURN.                                                    BBANK40P
024000                                                                  BBANK40P
024100 POPULATE-SCREEN-DATA.                                            BBANK40P
024200     MOVE SPACES TO CD05-DATA.                                    BBANK40P
024300     MOVE BANK-SCR40-ACC TO CD05I-ACC.                            BBANK40P
024400* Set criteria for search to populate screen                      BBANK40P
024500     IF BANK-PAGING-OFF                                           BBANK40P
024600        MOVE LOW-VALUES TO CD05I-START-ID                         BBANK40P
024700        MOVE '0001-01-01-00.00.00.000000' TO CD05I-START-ID       BBANK40P
024800        SET CD05-START-EQUAL TO TRUE                              BBANK40P
024900     ELSE                                                         BBANK40P
025000        IF WS-RETURN-FLAG-ON                                      BBANK40P
025100           MOVE BANK-PAGING-FIRST-ENTRY TO CD05I-START-ID         BBANK40P
025200           SET CD05-START-EQUAL TO TRUE                           BBANK40P
025300        END-IF                                                    BBANK40P
025400        IF WS-RETURN-FLAG-OFF                                     BBANK40P
025500           IF BANK-AID-PFK07                                      BBANK40P
025600              MOVE BANK-PAGING-FIRST-ENTRY TO CD05I-START-ID      BBANK40P
025700              SET CD05-START-LOW TO TRUE                          BBANK40P
025800           ELSE                                                   BBANK40P
025900              IF BANK-AID-PFK08                                   BBANK40P
026000                 MOVE BANK-PAGING-LAST-ENTRY TO CD05I-START-ID    BBANK40P
026100                 SET CD05-START-HIGH TO TRUE                      BBANK40P
026200              ELSE                                                BBANK40P
026300                 MOVE BANK-PAGING-FIRST-ENTRY TO CD05I-START-ID   BBANK40P
026400                 SET CD05-START-EQUAL TO TRUE                     BBANK40P
026500               END-IF                                             BBANK40P
026600           END-IF                                                 BBANK40P
026700        END-IF                                                    BBANK40P
026800     END-IF.                                                      BBANK40P
026900* Now go get the data                                             BBANK40P
027000 COPY CBANKX05.                                                   BBANK40P
027100     IF WS-RETURN-FLAG-OFF                                        BBANK40P
027200        IF BANK-PAGING-OFF AND                                    BBANK40P
027300           CD05-IS-MORE-DATA                                      BBANK40P
027400           SET BANK-PAGING-FIRST TO TRUE                          BBANK40P
027500        ELSE                                                      BBANK40P
027600           IF NOT BANK-AID-ENTER                                  BBANK40P
027700              IF BANK-PAGING-FIRST                                BBANK40P
027800                 IF CD05-IS-MORE-DATA                             BBANK40P
027900                    SET BANK-PAGING-MIDDLE TO TRUE                BBANK40P
028000                 END-IF                                           BBANK40P
028100                 IF CD05-NO-MORE-DATA                             BBANK40P
028200                    SET BANK-PAGING-LAST TO TRUE                  BBANK40P
028300                 END-IF                                           BBANK40P
028400              ELSE                                                BBANK40P
028500                 IF BANK-PAGING-MIDDLE                            BBANK40P
028600                    IF BANK-AID-PFK08 AND                         BBANK40P
028700                       CD05-NO-MORE-DATA                          BBANK40P
028800                       SET BANK-PAGING-LAST TO TRUE               BBANK40P
028900                    END-IF                                        BBANK40P
029000                    IF BANK-AID-PFK07 AND                         BBANK40P
029100                       CD05-NO-MORE-DATA                          BBANK40P
029200                       SET BANK-PAGING-FIRST TO TRUE              BBANK40P
029300                    END-IF                                        BBANK40P
029400                 ELSE                                             BBANK40P
029500                    IF BANK-PAGING-LAST                           BBANK40P
029600                       IF CD05-IS-MORE-DATA                       BBANK40P
029700                          SET BANK-PAGING-MIDDLE TO TRUE          BBANK40P
029800                       END-IF                                     BBANK40P
029900                    IF CD05-NO-MORE-DATA                          BBANK40P
030000                       SET BANK-PAGING-FIRST TO TRUE              BBANK40P
030100                    END-IF                                        BBANK40P
030200                 END-IF                                           BBANK40P
030300              END-IF                                              BBANK40P
030400           END-IF                                                 BBANK40P
030500        END-IF                                                    BBANK40P
030600     END-IF.                                                      BBANK40P
030700     MOVE LOW-VALUES TO BANK-SCR40-TXN-FIELDS.                    BBANK40P
030800     MOVE CD05O-ID1 TO BANK-PAGING-FIRST-ENTRY.                   BBANK40P
030900     MOVE CD05O-ID1 TO BANK-PAGING-LAST-ENTRY.                    BBANK40P
031000     MOVE 0 TO WS-SUB1.                                           BBANK40P
031100     PERFORM POPULATE-ENTRY THRU                                  BBANK40P
031200             POPULATE-ENTRY-EXIT 8 TIMES.                         BBANK40P
031300     GO TO POPULATE-SCREEN-DATA-EXIT.                             BBANK40P
031400 POPULATE-ENTRY.                                                  BBANK40P
031500     ADD 1 TO WS-SUB1.                                            BBANK40P
031600     IF CD05O-DATE (WS-SUB1) IS EQUAL TO SPACES                   BBANK40P
031700        MOVE CD05O-DATE (WS-SUB1) TO BANK-SCR40-DATE (WS-SUB1)    BBANK40P
031800     ELSE                                                         BBANK40P
031900        MOVE CD05O-DATE (WS-SUB1) TO DDI-DATA                     BBANK40P
032000        SET DDI-ISO TO TRUE                                       BBANK40P
032100        SET DDO-DD-MMM-YYYY TO TRUE                               BBANK40P
032200        PERFORM CALL-DATECONV THRU                                BBANK40P
032300               CALL-DATECONV-EXIT                                 BBANK40P
032400        MOVE DDO-DATA TO BANK-SCR40-DATE (WS-SUB1)                BBANK40P
032500     END-IF.                                                      BBANK40P
032600     IF CD05O-TIME (WS-SUB1) IS EQUAL TO SPACES                   BBANK40P
032700        MOVE CD05O-TIME (WS-SUB1) TO BANK-SCR40-TIME (WS-SUB1)    BBANK40P
032800     ELSE                                                         BBANK40P
032900        MOVE CD05O-TIME (WS-SUB1) TO WS-TEMP-TIME-IP              BBANK40P
033000        MOVE WS-TEMP-TIME-IP (1:2) TO WS-TEMP-TIME-OP (1:2)       BBANK40P
033100        MOVE ':' TO WS-TEMP-TIME-OP (3:1)                         BBANK40P
033200        MOVE WS-TEMP-TIME-IP (4:2) TO WS-TEMP-TIME-OP (4:2)       BBANK40P
033300        MOVE ':' TO WS-TEMP-TIME-OP (6:1)                         BBANK40P
033400        MOVE WS-TEMP-TIME-IP (7:2) TO WS-TEMP-TIME-OP (7:2)       BBANK40P
033500        MOVE WS-TEMP-TIME-OP TO BANK-SCR40-TIME (WS-SUB1)         BBANK40P
033600     END-IF.                                                      BBANK40P
033700     IF CD05O-AMT (WS-SUB1) IS EQUAL TO SPACES                    BBANK40P
033800        MOVE CD05O-AMT (WS-SUB1) TO BANK-SCR40-AMNT (WS-SUB1)     BBANK40P
033900     ELSE                                                         BBANK40P
034000        MOVE CD05O-AMT-N (WS-SUB1) TO WS-EDIT-AMT                 BBANK40P
034100        MOVE WS-EDIT-AMT TO BANK-SCR40-AMNT (WS-SUB1)             BBANK40P
034200     END-IF.                                                      BBANK40P
034300     MOVE CD05O-DESC (WS-SUB1) TO BANK-SCR40-DESC (WS-SUB1).      BBANK40P
034400     MOVE CD05O-ID (WS-SUB1) TO BANK-PAGING-LAST-ENTRY.           BBANK40P
034500 POPULATE-ENTRY-EXIT.                                             BBANK40P
034600     EXIT.                                                        BBANK40P
034700                                                                  BBANK40P
034800 POPULATE-SCREEN-DATA-EXIT.                                       BBANK40P
034900     EXIT.                                                        BBANK40P
035000                                                                  BBANK40P
035100***************************************************************** BBANK40P
035200* Call common routine to perform date conversions               * BBANK40P
035300***************************************************************** BBANK40P
035400 CALL-DATECONV.                                                   BBANK40P
035500     MOVE BANK-ENV TO DD-ENV.                                     BBANK40P
035600     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           BBANK40P
035700     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            BBANK40P
035800 CALL-DATECONV-EXIT.                                              BBANK40P
035900     EXIT.                                                        BBANK40P
036000                                                                  BBANK40P
