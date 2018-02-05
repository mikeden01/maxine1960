000100***************************************************************** SBANK80P
000200*                                                               * SBANK80P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * SBANK80P
000400*   This demonstration program is provided for use by users     * SBANK80P
000500*   of Micro Focus products and may be used, modified and       * SBANK80P
000600*   distributed as part of your application provided that       * SBANK80P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK80P
000800*   in this material.                                           * SBANK80P
000900*                                                               * SBANK80P
001000***************************************************************** SBANK80P
001100                                                                  SBANK80P
001200***************************************************************** SBANK80P
001300* Program:     SBANK80P.CBL (CICS Version)                      * SBANK80P
001400* Layer:       Screen handling                                  * SBANK80P
001500* Function:    Display request for informtion                   * SBANK80P
001600***************************************************************** SBANK80P
001700                                                                  SBANK80P
001800 IDENTIFICATION DIVISION.                                         SBANK80P
001900 PROGRAM-ID.                                                      SBANK80P
002000     SBANK80P.                                                    SBANK80P
002100 DATE-WRITTEN.                                                    SBANK80P
002200     September 2002.                                              SBANK80P
002300 DATE-COMPILED.                                                   SBANK80P
002400     Today.                                                       SBANK80P
002500                                                                  SBANK80P
002600 ENVIRONMENT DIVISION.                                            SBANK80P
002700                                                                  SBANK80P
002800 DATA DIVISION.                                                   SBANK80P
002900 WORKING-STORAGE SECTION.                                         SBANK80P
003000 01  WS-MISC-STORAGE.                                             SBANK80P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK80P
003200       VALUE 'SBANK80P'.                                          SBANK80P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK80P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK80P
003500       VALUE SPACES.                                              SBANK80P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK80P
003700       VALUE 'UNKNOWN'.                                           SBANK80P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK80P
003900                                                                  SBANK80P
004000 01  MAPAREA                                 PIC X(2048).         SBANK80P
004100 COPY MBANK80.                                                    SBANK80P
004200                                                                  SBANK80P
004300 01  WS-TIME-DATE-WORK-AREA.                                      SBANK80P
004400 COPY CDATED.                                                     SBANK80P
004500                                                                  SBANK80P
004600 01  WS-BANK-DATA-AREAS.                                          SBANK80P
004700   05  WS-BANK-DATA.                                              SBANK80P
004800 COPY CBANKDAT.                                                   SBANK80P
004900   05  WS-BANK-EXT-DATA.                                          SBANK80P
005000 COPY CBANKEXT.                                                   SBANK80P
005100                                                                  SBANK80P
005200 COPY CSCRNHDD.                                                   SBANK80P
005300                                                                  SBANK80P
005400 COPY DFHAID.                                                     SBANK80P
005500                                                                  SBANK80P
005600 COPY DFHBMSCA.                                                   SBANK80P
005700                                                                  SBANK80P
005800 COPY CABENDD.                                                    SBANK80P
005900                                                                  SBANK80P
006000 LINKAGE SECTION.                                                 SBANK80P
006100 01  DFHCOMMAREA.                                                 SBANK80P
006200   05  FILLER                                PIC X(7168).         SBANK80P
006300                                                                  SBANK80P
006400 PROCEDURE DIVISION.                                              SBANK80P
006500***************************************************************** SBANK80P
006600* Write entry to log to show we have been invoked               * SBANK80P
006700***************************************************************** SBANK80P
006800     COPY CTRACE.                                                 SBANK80P
006900                                                                  SBANK80P
007000***************************************************************** SBANK80P
007100* Store our transaction-id                                      * SBANK80P
007200***************************************************************** SBANK80P
007300     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK80P
007400                                                                  SBANK80P
007500***************************************************************** SBANK80P
007600* Store passed data or abend if there wasn't any                * SBANK80P
007700***************************************************************** SBANK80P
007800     IF EIBCALEN IS EQUAL TO 0                                    SBANK80P
007900        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK80P
008000        MOVE '0001' TO ABEND-CODE                                 SBANK80P
008100        MOVE SPACES TO ABEND-REASON                               SBANK80P
008200        COPY CABENDPO.                                            SBANK80P
008300     ELSE                                                         SBANK80P
008400        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK80P
008500        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK80P
008600        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK80P
008700     END-IF.                                                      SBANK80P
008800                                                                  SBANK80P
008900***************************************************************** SBANK80P
009000* This is the main process                                      * SBANK80P
009100***************************************************************** SBANK80P
009200                                                                  SBANK80P
009300***************************************************************** SBANK80P
009400* Determine what we have to do (read from or send to screen)    * SBANK80P
009500***************************************************************** SBANK80P
009600     MOVE LOW-VALUE TO MAPAREA.                                   SBANK80P
009700     EVALUATE TRUE                                                SBANK80P
009800       WHEN BANK-MAP-FUNCTION-GET                                 SBANK80P
009900         PERFORM SCREEN80-READ THRU                               SBANK80P
010000                 SCREEN80-READ-EXIT                               SBANK80P
010100       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK80P
010200         PERFORM SCREEN80-BUILD-AND-SEND THRU                     SBANK80P
010300                 SCREEN80-BUILD-AND-SEND-EXIT                     SBANK80P
010400       WHEN OTHER                                                 SBANK80P
010500         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK80P
010600         MOVE '0001' TO ABEND-CODE                                SBANK80P
010700         MOVE SPACES TO ABEND-REASON                              SBANK80P
010800         COPY CABENDPO.                                           SBANK80P
010900     END-EVALUATE.                                                SBANK80P
011000                                                                  SBANK80P
011100* Call the appropriate routine to handle the business logic       SBANK80P
011200     IF BANK-MAP-FUNCTION-GET                                     SBANK80P
011300        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK80P
011400                       COMMAREA(WS-BANK-DATA)                     SBANK80P
011500                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK80P
011600        END-EXEC                                                  SBANK80P
011700     END-IF.                                                      SBANK80P
011800                                                                  SBANK80P
011900***************************************************************** SBANK80P
012000* Now we have to have finished and can return to our invoker.   * SBANK80P
012100***************************************************************** SBANK80P
012200* Now return to CICS                                              SBANK80P
012300     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK80P
012400          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK80P
012500     EXEC CICS                                                    SBANK80P
012600          RETURN                                                  SBANK80P
012700     END-EXEC.                                                    SBANK80P
012800     GOBACK.                                                      SBANK80P
012900                                                                  SBANK80P
013000***************************************************************** SBANK80P
013100* Screen processing for MBANK80                                 * SBANK80P
013200*---------------------------------------------------------------* SBANK80P
013300* Retrieve data from screen and format it                       * SBANK80P
013400***************************************************************** SBANK80P
013500 SCREEN80-READ.                                                   SBANK80P
013600     MOVE 'BBANK80P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK80P
013700     IF BANK-AID-CLEAR                                            SBANK80P
013800        SET BANK-AID-PFK03 TO TRUE                                SBANK80P
013900        GO TO SCREEN80-READ-EXIT                                  SBANK80P
014000     END-IF.                                                      SBANK80P
014100     IF BANK-ENV-CICS                                             SBANK80P
014200        GO TO SCREEN80-READ-CICS                                  SBANK80P
014300     ELSE                                                         SBANK80P
014400        GO TO SCREEN80-READ-INET                                  SBANK80P
014500     END-IF.                                                      SBANK80P
014600                                                                  SBANK80P
014700 SCREEN80-READ-CICS.                                              SBANK80P
014800     IF BANK-HELP-INACTIVE                                        SBANK80P
014900        EXEC CICS RECEIVE MAP('BANK80A')                          SBANK80P
015000                          MAPSET('MBANK80')                       SBANK80P
015100        END-EXEC                                                  SBANK80P
015200     ELSE                                                         SBANK80P
015300        EXEC CICS RECEIVE MAP('HELP80A')                          SBANK80P
015400                          MAPSET('MBANK80')                       SBANK80P
015500        END-EXEC                                                  SBANK80P
015600        GO TO SCREEN80-READ-EXIT                                  SBANK80P
015700     END-IF.                                                      SBANK80P
015800                                                                  SBANK80P
015900     GO TO SCREEN80-READ-EXIT.                                    SBANK80P
016000                                                                  SBANK80P
016100 SCREEN80-READ-INET.                                              SBANK80P
016200     GO TO SCREEN80-READ-EXIT.                                    SBANK80P
016300                                                                  SBANK80P
016400 SCREEN80-READ-EXIT.                                              SBANK80P
016500     EXIT.                                                        SBANK80P
016600                                                                  SBANK80P
016700***************************************************************** SBANK80P
016800* Screen processing for SCREEN80 (BANK80/HELP80)                * SBANK80P
016900*---------------------------------------------------------------* SBANK80P
017000* Build the output screen and send it                           * SBANK80P
017100***************************************************************** SBANK80P
017200 SCREEN80-BUILD-AND-SEND.                                         SBANK80P
017300* Clear map area, get date & time and move to the map             SBANK80P
017400     MOVE LOW-VALUES TO BANK80AO.                                 SBANK80P
017500     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK80P
017600     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK80P
017700     SET DDI-YYDDD TO TRUE.                                       SBANK80P
017800     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK80P
017900     PERFORM CALL-DATECONV THRU                                   SBANK80P
018000             CALL-DATECONV-EXIT.                                  SBANK80P
018100* Ensure the last map fields are correct                          SBANK80P
018200     IF BANK-HELP-ACTIVE                                          SBANK80P
018300        MOVE 'MHELP80' TO BANK-LAST-MAPSET                        SBANK80P
018400        MOVE 'HELP80A' TO BANK-LAST-MAP                           SBANK80P
018500     ELSE                                                         SBANK80P
018600        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        SBANK80P
018700        MOVE 'BANK80A' TO BANK-LAST-MAP                           SBANK80P
018800     END-IF.                                                      SBANK80P
018900     IF BANK-ENV-CICS                                             SBANK80P
019000        GO TO SCREEN80-BUILD-AND-SEND-CICS                        SBANK80P
019100     ELSE                                                         SBANK80P
019200        GO TO SCREEN80-BUILD-AND-SEND-INET                        SBANK80P
019300     END-IF.                                                      SBANK80P
019400                                                                  SBANK80P
019500 SCREEN80-BUILD-AND-SEND-CICS.                                    SBANK80P
019600     IF BANK-LAST-MAP IS EQUAL TO 'BANK80A'                       SBANK80P
019700        GO TO BANK80-BUILD-AND-SEND-CICS                          SBANK80P
019800     END-IF.                                                      SBANK80P
019900     IF BANK-LAST-MAP IS EQUAL TO 'HELP80A'                       SBANK80P
020000        GO TO HELP80-BUILD-AND-SEND-CICS                          SBANK80P
020100     END-IF.                                                      SBANK80P
020200     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK80P
020300     MOVE '0003' TO ABEND-CODE                                    SBANK80P
020400     MOVE SPACES TO ABEND-REASON                                  SBANK80P
020500     COPY CABENDPO.                                               SBANK80P
020600     GOBACK.                                                      SBANK80P
020700                                                                  SBANK80P
020800 BANK80-BUILD-AND-SEND-CICS.                                      SBANK80P
020900     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK80AO==.        SBANK80P
021000     MOVE WS-TRAN-ID TO TRANO IN BANK80AO.                        SBANK80P
021100     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK80AO.                    SBANK80P
021200     MOVE DDO-DATA TO DATEO IN BANK80AO.                          SBANK80P
021300* Move in any error message                                       SBANK80P
021400     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK80AO.                  SBANK80P
021500* Move in screen specific fields                                  SBANK80P
021600* Move in screen specific fields                                  SBANK80P
021700        MOVE BANK-SCR80-LINE (01) TO INF01O IN BANK80AO.          SBANK80P
021800        MOVE BANK-SCR80-LINE (02) TO INF02O IN BANK80AO.          SBANK80P
021900        MOVE BANK-SCR80-LINE (03) TO INF03O IN BANK80AO.          SBANK80P
022000        MOVE BANK-SCR80-LINE (04) TO INF04O IN BANK80AO.          SBANK80P
022100        MOVE BANK-SCR80-LINE (05) TO INF05O IN BANK80AO.          SBANK80P
022200        MOVE BANK-SCR80-LINE (06) TO INF06O IN BANK80AO.          SBANK80P
022300        MOVE BANK-SCR80-LINE (07) TO INF07O IN BANK80AO.          SBANK80P
022400        MOVE BANK-SCR80-LINE (08) TO INF08O IN BANK80AO.          SBANK80P
022500        MOVE BANK-SCR80-LINE (09) TO INF09O IN BANK80AO.          SBANK80P
022600        MOVE BANK-SCR80-LINE (10) TO INF10O IN BANK80AO.          SBANK80P
022700        MOVE BANK-SCR80-LINE (11) TO INF11O IN BANK80AO.          SBANK80P
022800        MOVE BANK-SCR80-LINE (12) TO INF12O IN BANK80AO.          SBANK80P
022900        MOVE BANK-SCR80-LINE (13) TO INF13O IN BANK80AO.          SBANK80P
023000        MOVE BANK-SCR80-LINE (14) TO INF14O IN BANK80AO.          SBANK80P
023100        MOVE BANK-SCR80-LINE (15) TO INF15O IN BANK80AO.          SBANK80P
023200        MOVE BANK-SCR80-LINE (16) TO INF16O IN BANK80AO.          SBANK80P
023300        MOVE BANK-SCR80-LINE (17) TO INF17O IN BANK80AO.          SBANK80P
023400        MOVE BANK-SCR80-LINE (18) TO INF18O IN BANK80AO.          SBANK80P
023500        MOVE BANK-SCR80-LINE (19) TO INF19O IN BANK80AO.          SBANK80P
023600* Turn colour off if required                                     SBANK80P
023700     IF COLOUR-OFF                                                SBANK80P
023800        MOVE DFHGREEN TO TXT01C IN BANK80AO                       SBANK80P
023900        MOVE DFHGREEN TO SCRNC IN BANK80AO                        SBANK80P
024000        MOVE DFHGREEN TO HEAD1C IN BANK80AO                       SBANK80P
024100        MOVE DFHGREEN TO DATEC IN BANK80AO                        SBANK80P
024200        MOVE DFHGREEN TO TXT02C IN BANK80AO                       SBANK80P
024300        MOVE DFHGREEN TO TRANC IN BANK80AO                        SBANK80P
024400        MOVE DFHGREEN TO HEAD2C IN BANK80AO                       SBANK80P
024500        MOVE DFHGREEN TO TIMEC IN BANK80AO                        SBANK80P
024600        MOVE DFHGREEN TO INF01C IN BANK80AO                       SBANK80P
024700        MOVE DFHGREEN TO INF02C IN BANK80AO                       SBANK80P
024800        MOVE DFHGREEN TO INF03C IN BANK80AO                       SBANK80P
024900        MOVE DFHGREEN TO INF04C IN BANK80AO                       SBANK80P
025000        MOVE DFHGREEN TO INF05C IN BANK80AO                       SBANK80P
025100        MOVE DFHGREEN TO INF06C IN BANK80AO                       SBANK80P
025200        MOVE DFHGREEN TO INF07C IN BANK80AO                       SBANK80P
025300        MOVE DFHGREEN TO INF08C IN BANK80AO                       SBANK80P
025400        MOVE DFHGREEN TO INF09C IN BANK80AO                       SBANK80P
025500        MOVE DFHGREEN TO INF10C IN BANK80AO                       SBANK80P
025600        MOVE DFHGREEN TO INF11C IN BANK80AO                       SBANK80P
025700        MOVE DFHGREEN TO INF12C IN BANK80AO                       SBANK80P
025800        MOVE DFHGREEN TO INF13C IN BANK80AO                       SBANK80P
025900        MOVE DFHGREEN TO INF14C IN BANK80AO                       SBANK80P
026000        MOVE DFHGREEN TO INF15C IN BANK80AO                       SBANK80P
026100        MOVE DFHGREEN TO INF16C IN BANK80AO                       SBANK80P
026200        MOVE DFHGREEN TO INF17C IN BANK80AO                       SBANK80P
026300        MOVE DFHGREEN TO INF18C IN BANK80AO                       SBANK80P
026400        MOVE DFHGREEN TO INF19C IN BANK80AO                       SBANK80P
026500        MOVE DFHGREEN TO TXT03C IN BANK80AO                       SBANK80P
026600        MOVE DFHGREEN TO ERRMSGC IN BANK80AO                      SBANK80P
026700     END-IF.                                                      SBANK80P
026800                                                                  SBANK80P
026900     EXEC CICS SEND MAP('BANK80A')                                SBANK80P
027000                    MAPSET('MBANK80')                             SBANK80P
027100                    ERASE                                         SBANK80P
027200                    FREEKB                                        SBANK80P
027300     END-EXEC.                                                    SBANK80P
027400     GO TO SCREEN80-BUILD-AND-SEND-EXIT.                          SBANK80P
027500                                                                  SBANK80P
027600 HELP80-BUILD-AND-SEND-CICS.                                      SBANK80P
027700     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK80P
027800                             ==<<SCRN>>== BY ==HELP80AO==.        SBANK80P
027900                                                                  SBANK80P
028000     EXEC CICS SEND MAP('HELP80A')                                SBANK80P
028100                    MAPSET('MBANK80')                             SBANK80P
028200                    ERASE                                         SBANK80P
028300                    FREEKB                                        SBANK80P
028400     END-EXEC.                                                    SBANK80P
028500     GO TO SCREEN80-BUILD-AND-SEND-EXIT.                          SBANK80P
028600                                                                  SBANK80P
028700 SCREEN80-BUILD-AND-SEND-INET.                                    SBANK80P
028800     MOVE SPACES TO EXT-OP-DATA.                                  SBANK80P
028900     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK80P
029000     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK80P
029100     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK80P
029200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK80P
029300     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK80P
029400     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK80P
029500* Move in screen name                                             SBANK80P
029600     MOVE 'BANK80' TO EXT-OP-SCREEN.                              SBANK80P
029700* Move in userid and any error message                            SBANK80P
029800     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK80P
029900     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK80P
030000     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK80P
030100* Move in screen specific fields                                  SBANK80P
030200*    MOVE BANK-SCR20-SEL5TX TO EXT-OP20-SEL5TX.                   SBANK80P
030300     GO TO SCREEN80-BUILD-AND-SEND-EXIT.                          SBANK80P
030400                                                                  SBANK80P
030500 SCREEN80-BUILD-AND-SEND-EXIT.                                    SBANK80P
030600     EXIT.                                                        SBANK80P
030700                                                                  SBANK80P
030800***************************************************************** SBANK80P
030900* Call common routine to perform date conversions               * SBANK80P
031000***************************************************************** SBANK80P
031100 CALL-DATECONV.                                                   SBANK80P
031200     MOVE BANK-ENV TO DD-ENV.                                     SBANK80P
031300     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK80P
031400     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK80P
031500 CALL-DATECONV-EXIT.                                              SBANK80P
031600     EXIT.                                                        SBANK80P
031700                                                                  SBANK80P
