000100***************************************************************** SBANK99P
000200*                                                               * SBANK99P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * SBANK99P
000400*   This demonstration program is provided for use by users     * SBANK99P
000500*   of Micro Focus products and may be used, modified and       * SBANK99P
000600*   distributed as part of your application provided that       * SBANK99P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK99P
000800*   in this material.                                           * SBANK99P
000900*                                                               * SBANK99P
001000***************************************************************** SBANK99P
001100                                                                  SBANK99P
001200***************************************************************** SBANK99P
001300* Program:     SBANK99P.CBL (CICS Version)                      * SBANK99P
001400* Layer:       Screen handling                                  * SBANK99P
001500* Function:    Terminate the pseudo conversation                * SBANK99P
001600***************************************************************** SBANK99P
001700                                                                  SBANK99P
001800 IDENTIFICATION DIVISION.                                         SBANK99P
001900 PROGRAM-ID.                                                      SBANK99P
002000     SBANK99P.                                                    SBANK99P
002100 DATE-WRITTEN.                                                    SBANK99P
002200     September 2002.                                              SBANK99P
002300 DATE-COMPILED.                                                   SBANK99P
002400     Today.                                                       SBANK99P
002500                                                                  SBANK99P
002600 ENVIRONMENT DIVISION.                                            SBANK99P
002700                                                                  SBANK99P
002800 DATA DIVISION.                                                   SBANK99P
002900 WORKING-STORAGE SECTION.                                         SBANK99P
003000 01  WS-MISC-STORAGE.                                             SBANK99P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK99P
003200       VALUE 'SBANK99P'.                                          SBANK99P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK99P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK99P
003500       VALUE SPACES.                                              SBANK99P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK99P
003700       VALUE 'UNKNOWN'.                                           SBANK99P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK99P
003900                                                                  SBANK99P
004000 01  MAPAREA                                 PIC X(2048).         SBANK99P
004100 COPY MBANK99.                                                    SBANK99P
004200                                                                  SBANK99P
004300 01  WS-TIME-DATE-WORK-AREA.                                      SBANK99P
004400 COPY CDATED.                                                     SBANK99P
004500                                                                  SBANK99P
004600 01  WS-BANK-DATA-AREAS.                                          SBANK99P
004700   05 WS-BANK-DATA.                                               SBANK99P
004800 COPY CBANKDAT.                                                   SBANK99P
004900   05  WS-BANK-EXT-DATA.                                          SBANK99P
005000 COPY CBANKEXT.                                                   SBANK99P
005100                                                                  SBANK99P
005200 COPY CSCRNHDD.                                                   SBANK99P
005300                                                                  SBANK99P
005400 COPY DFHAID.                                                     SBANK99P
005500                                                                  SBANK99P
005600 COPY DFHBMSCA.                                                   SBANK99P
005700                                                                  SBANK99P
005800 COPY CABENDD.                                                    SBANK99P
005900                                                                  SBANK99P
006000 LINKAGE SECTION.                                                 SBANK99P
006100 01  DFHCOMMAREA.                                                 SBANK99P
006200   05  FILLER                                PIC X(7168).         SBANK99P
006300                                                                  SBANK99P
006400 PROCEDURE DIVISION.                                              SBANK99P
006500***************************************************************** SBANK99P
006600* Write entry to log to show we have been invoked               * SBANK99P
006700***************************************************************** SBANK99P
006800     COPY CTRACE.                                                 SBANK99P
006900                                                                  SBANK99P
007000***************************************************************** SBANK99P
007100* Store our transaction-id                                      * SBANK99P
007200***************************************************************** SBANK99P
007300     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK99P
007400                                                                  SBANK99P
007500***************************************************************** SBANK99P
007600* Store passed data or abend if there wasn't any                * SBANK99P
007700***************************************************************** SBANK99P
007800     IF EIBCALEN IS EQUAL TO 0                                    SBANK99P
007900        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK99P
008000        MOVE '0001' TO ABEND-CODE                                 SBANK99P
008100        MOVE SPACES TO ABEND-REASON                               SBANK99P
008200        COPY CABENDPO.                                            SBANK99P
008300     ELSE                                                         SBANK99P
008400        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK99P
008500        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK99P
008600        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK99P
008700     END-IF.                                                      SBANK99P
008800                                                                  SBANK99P
008900***************************************************************** SBANK99P
009000* This is the main process                                      * SBANK99P
009100***************************************************************** SBANK99P
009200                                                                  SBANK99P
009300***************************************************************** SBANK99P
009400* Determine what we have to do (read from or send to screen)    * SBANK99P
009500***************************************************************** SBANK99P
009600     MOVE LOW-VALUE TO MAPAREA.                                   SBANK99P
009700     EVALUATE TRUE                                                SBANK99P
009800       WHEN BANK-MAP-FUNCTION-GET                                 SBANK99P
009900         PERFORM BANK99-READ THRU                                 SBANK99P
010000                 BANK99-READ-EXIT                                 SBANK99P
010100       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK99P
010200         PERFORM BANK99-BUILD-AND-SEND THRU                       SBANK99P
010300                 BANK99-BUILD-AND-SEND-EXIT                       SBANK99P
010400       WHEN OTHER                                                 SBANK99P
010500         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK99P
010600         MOVE '0002' TO ABEND-CODE                                SBANK99P
010700         MOVE SPACES TO ABEND-REASON                              SBANK99P
010800         COPY CABENDPO.                                           SBANK99P
010900     END-EVALUATE.                                                SBANK99P
011000                                                                  SBANK99P
011100* Call the appropriate routine to handle the business logic       SBANK99P
011200     IF BANK-MAP-FUNCTION-GET                                     SBANK99P
011300        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK99P
011400                       COMMAREA(WS-BANK-DATA)                     SBANK99P
011500                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK99P
011600        END-EXEC                                                  SBANK99P
011700     END-IF.                                                      SBANK99P
011800                                                                  SBANK99P
011900***************************************************************** SBANK99P
012000* Now we have to have finished and can return to our invoker.   * SBANK99P
012100***************************************************************** SBANK99P
012200* Now return to CICS                                              SBANK99P
012300     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK99P
012400          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK99P
012500     EXEC CICS                                                    SBANK99P
012600          RETURN                                                  SBANK99P
012700     END-EXEC.                                                    SBANK99P
012800     GOBACK.                                                      SBANK99P
012900                                                                  SBANK99P
013000***************************************************************** SBANK99P
013100* Screen processing for MBANK99                                 * SBANK99P
013200*---------------------------------------------------------------* SBANK99P
013300* Retrieve data from screen and format it                       * SBANK99P
013400***************************************************************** SBANK99P
013500 BANK99-READ.                                                     SBANK99P
013600     MOVE 'BBANK99P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK99P
013700     IF BANK-AID-CLEAR                                            SBANK99P
013800        SET BANK-AID-PFK03 TO TRUE                                SBANK99P
013900        GO TO BANK99-READ-EXIT                                    SBANK99P
014000     END-IF.                                                      SBANK99P
014100     IF BANK-LAST-MAPSET IS EQUAL TO SPACES                       SBANK99P
014200        GO TO BANK99-READ-EXIT                                    SBANK99P
014300     END-IF.                                                      SBANK99P
014400     IF BANK-ENV-CICS                                             SBANK99P
014500        GO TO BANK99-READ-CICS                                    SBANK99P
014600     ELSE                                                         SBANK99P
014700        GO TO BANK99-READ-INET                                    SBANK99P
014800     END-IF.                                                      SBANK99P
014900                                                                  SBANK99P
015000 BANK99-READ-CICS.                                                SBANK99P
015100     GO TO BANK99-READ-EXIT.                                      SBANK99P
015200                                                                  SBANK99P
015300 BANK99-READ-INET.                                                SBANK99P
015400     GO TO BANK99-READ-EXIT.                                      SBANK99P
015500                                                                  SBANK99P
015600 BANK99-READ-EXIT.                                                SBANK99P
015700     EXIT.                                                        SBANK99P
015800                                                                  SBANK99P
015900***************************************************************** SBANK99P
016000* Screen processing for MBANK99                                 * SBANK99P
016100*---------------------------------------------------------------* SBANK99P
016200* Build the output screen and send it                           * SBANK99P
016300***************************************************************** SBANK99P
016400 BANK99-BUILD-AND-SEND.                                           SBANK99P
016500* Clear map area, get date & time and move to the map             SBANK99P
016600     MOVE LOW-VALUES TO BANK99AO.                                 SBANK99P
016700     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK99P
016800     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK99P
016900     SET DDI-YYDDD TO TRUE.                                       SBANK99P
017000     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK99P
017100     PERFORM CALL-DATECONV THRU                                   SBANK99P
017200             CALL-DATECONV-EXIT.                                  SBANK99P
017300* Ensure the last map fields are correct                          SBANK99P
017400     MOVE 'MBANK99' TO BANK-LAST-MAPSET.                          SBANK99P
017500     MOVE 'BANK99A' TO BANK-LAST-MAP.                             SBANK99P
017600     IF BANK-ENV-CICS                                             SBANK99P
017700        GO TO BANK99-BUILD-AND-SEND-CICS                          SBANK99P
017800     ELSE                                                         SBANK99P
017900        GO TO BANK99-BUILD-AND-SEND-INET                          SBANK99P
018000     END-IF.                                                      SBANK99P
018100                                                                  SBANK99P
018200 BANK99-BUILD-AND-SEND-CICS.                                      SBANK99P
018300     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK99AO==.        SBANK99P
018400     MOVE WS-TRAN-ID TO TRANO IN BANK99AO.                        SBANK99P
018500     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK99AO.                    SBANK99P
018600     MOVE DDO-DATA TO DATEO IN BANK99AO.                          SBANK99P
018700* Move in any error message                                       SBANK99P
018800* Move in screen specific fields                                  SBANK99P
018900     MOVE 'MBANK99' TO BANK-LAST-MAPSET.                          SBANK99P
019000     MOVE 'BANK99A' TO BANK-LAST-MAP.                             SBANK99P
019100* Turn colour off if required                                     SBANK99P
019200     IF COLOUR-OFF                                                SBANK99P
019300        MOVE DFHGREEN TO TXT01C IN BANK99AO                       SBANK99P
019400        MOVE DFHGREEN TO SCRNC IN BANK99AO                        SBANK99P
019500        MOVE DFHGREEN TO HEAD1C IN BANK99AO                       SBANK99P
019600        MOVE DFHGREEN TO DATEC IN BANK99AO                        SBANK99P
019700        MOVE DFHGREEN TO TXT02C IN BANK99AO                       SBANK99P
019800        MOVE DFHGREEN TO TRANC IN BANK99AO                        SBANK99P
019900        MOVE DFHGREEN TO HEAD2C IN BANK99AO                       SBANK99P
020000        MOVE DFHGREEN TO TIMEC IN BANK99AO                        SBANK99P
020100        MOVE DFHGREEN TO TXT03C IN BANK99AO                       SBANK99P
020200        MOVE DFHGREEN TO TXT04C IN BANK99AO                       SBANK99P
020300     END-IF.                                                      SBANK99P
020400     EXEC CICS SEND MAP('BANK99A')                                SBANK99P
020500                    MAPSET('MBANK99')                             SBANK99P
020600                    ERASE                                         SBANK99P
020700                    FREEKB                                        SBANK99P
020800     END-EXEC.                                                    SBANK99P
020900     GO TO BANK99-BUILD-AND-SEND-EXIT.                            SBANK99P
021000                                                                  SBANK99P
021100 BANK99-BUILD-AND-SEND-INET.                                      SBANK99P
021200     MOVE SPACES TO EXT-OP-DATA.                                  SBANK99P
021300     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK99P
021400     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK99P
021500     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK99P
021600     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK99P
021700     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK99P
021800     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK99P
021900* Move in userid and any error message                            SBANK99P
022000* Move in screen specific fields                                  SBANK99P
022100* Move in screen name                                             SBANK99P
022200     MOVE 'BANK99' TO EXT-OP-SCREEN.                              SBANK99P
022300     GO TO BANK99-BUILD-AND-SEND-EXIT.                            SBANK99P
022400                                                                  SBANK99P
022500 BANK99-BUILD-AND-SEND-EXIT.                                      SBANK99P
022600     EXIT.                                                        SBANK99P
022700                                                                  SBANK99P
022800***************************************************************** SBANK99P
022900* Call common routine to perform date conversions               * SBANK99P
023000***************************************************************** SBANK99P
023100 CALL-DATECONV.                                                   SBANK99P
023200     MOVE BANK-ENV TO DD-ENV.                                     SBANK99P
023300     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK99P
023400     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK99P
023500 CALL-DATECONV-EXIT.                                              SBANK99P
023600     EXIT.                                                        SBANK99P
023700                                                                  SBANK99P
