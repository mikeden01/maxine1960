000100***************************************************************** BBANK60P
000200*                                                               * BBANK60P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK60P
000400*   This demonstration program is provided for use by users     * BBANK60P
000500*   of Micro Focus products and may be used, modified and       * BBANK60P
000600*   distributed as part of your application provided that       * BBANK60P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK60P
000800*   in this material.                                           * BBANK60P
000900*                                                               * BBANK60P
001000***************************************************************** BBANK60P
001100                                                                  BBANK60P
001200***************************************************************** BBANK60P
001300* Program:     BBANK60P.CBL                                     * BBANK60P
001400* Layer:       Business logic                                   * BBANK60P
001500* Function:    Update address information                       * BBANK60P
001600***************************************************************** BBANK60P
001700                                                                  BBANK60P
001800 IDENTIFICATION DIVISION.                                         BBANK60P
001900 PROGRAM-ID.                                                      BBANK60P
002000     BBANK60P.                                                    BBANK60P
002100 DATE-WRITTEN.                                                    BBANK60P
002200     September 2002.                                              BBANK60P
002300 DATE-COMPILED.                                                   BBANK60P
002400     Today.                                                       BBANK60P
002500                                                                  BBANK60P
002600 ENVIRONMENT DIVISION.                                            BBANK60P
002700                                                                  BBANK60P
002800 DATA DIVISION.                                                   BBANK60P
002900 WORKING-STORAGE SECTION.                                         BBANK60P
003000 01  WS-MISC-STORAGE.                                             BBANK60P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK60P
003200       VALUE 'BBANK60P'.                                          BBANK60P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK60P
003400     88  INPUT-OK                            VALUE '0'.           BBANK60P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK60P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK60P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK60P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK60P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK60P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK60P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK60P
004200     88  PFK-VALID                           VALUE '0'.           BBANK60P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK60P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK60P
004500   05  WS-ADDR-CHANGE                        PIC X(1).            BBANK60P
004600     88  ADDR-DATA-UNCHANGED                 VALUE '0'.           BBANK60P
004700     88  ADDR-DATA-CHANGED                   VALUE '1'.           BBANK60P
004800                                                                  BBANK60P
004900 01  WS-BANK-DATA.                                                BBANK60P
005000 COPY CBANKDAT.                                                   BBANK60P
005100                                                                  BBANK60P
005200 01  WS-HELP-DATA.                                                BBANK60P
005300 COPY CHELPD01.                                                   BBANK60P
005400                                                                  BBANK60P
005500 01  WS-ADDR-DATA.                                                BBANK60P
005600 COPY CBANKD02.                                                   BBANK60P
005700                                                                  BBANK60P
005800 COPY CBANKD07.                                                   BBANK60P
005900                                                                  BBANK60P
006000 COPY CSTATESD.                                                   BBANK60P
006100                                                                  BBANK60P
006200 COPY CABENDD.                                                    BBANK60P
006300                                                                  BBANK60P
006400 LINKAGE SECTION.                                                 BBANK60P
006500 01  DFHCOMMAREA.                                                 BBANK60P
006600   05  LK-COMMAREA                           PIC X(6144).         BBANK60P
006700                                                                  BBANK60P
006800 COPY CENTRY.                                                     BBANK60P
006900***************************************************************** BBANK60P
007000* Make ourselves re-entrant                                     * BBANK60P
007100***************************************************************** BBANK60P
007200     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK60P
007300                                                                  BBANK60P
007400***************************************************************** BBANK60P
007500* Move the passed area to our area                              * BBANK60P
007600***************************************************************** BBANK60P
007700     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK60P
007800                                                                  BBANK60P
007900***************************************************************** BBANK60P
008000* Ensure error message is cleared                               * BBANK60P
008100***************************************************************** BBANK60P
008200     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK60P
008300                                                                  BBANK60P
008400***************************************************************** BBANK60P
008500* This is the main process                                      * BBANK60P
008600***************************************************************** BBANK60P
008700                                                                  BBANK60P
008800***************************************************************** BBANK60P
008900* Save the passed return flag and then turn it off              * BBANK60P
009000***************************************************************** BBANK60P
009100     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     BBANK60P
009200     SET BANK-RETURN-FLAG-OFF TO TRUE.                            BBANK60P
009300                                                                  BBANK60P
009400***************************************************************** BBANK60P
009500* Check the AID to see if its valid at this point               * BBANK60P
009600***************************************************************** BBANK60P
009700     SET PFK-INVALID TO TRUE.                                     BBANK60P
009800     IF BANK-AID-ENTER OR                                         BBANK60P
009900        BANK-AID-PFK03 OR                                         BBANK60P
010000        BANK-AID-PFK04 OR                                         BBANK60P
010100        BANK-AID-PFK10                                            BBANK60P
010200        SET PFK-VALID TO TRUE                                     BBANK60P
010300     END-IF.                                                      BBANK60P
010400     IF BANK-AID-PFK01 AND                                        BBANK60P
010500        BANK-HELP-INACTIVE                                        BBANK60P
010600        SET BANK-HELP-ACTIVE TO TRUE                              BBANK60P
010700        SET PFK-VALID TO TRUE                                     BBANK60P
010800     END-IF.                                                      BBANK60P
010900     IF PFK-INVALID                                               BBANK60P
011000        SET BANK-AID-ENTER TO TRUE                                BBANK60P
011100     END-IF.                                                      BBANK60P
011200                                                                  BBANK60P
011300***************************************************************** BBANK60P
011400* Check the AID to see if we have to quit                       * BBANK60P
011500***************************************************************** BBANK60P
011600     IF BANK-AID-PFK03                                            BBANK60P
011700        MOVE 'BBANK60P' TO BANK-LAST-PROG                         BBANK60P
011800        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK60P
011900        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK60P
012000        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK60P
012100        GO TO COMMON-RETURN                                       BBANK60P
012200     END-IF.                                                      BBANK60P
012300                                                                  BBANK60P
012400***************************************************************** BBANK60P
012500* Check the to see if user needs or has been using help         * BBANK60P
012600***************************************************************** BBANK60P
012700     IF BANK-HELP-ACTIVE                                          BBANK60P
012800        IF BANK-AID-PFK04                                         BBANK60P
012900           SET BANK-HELP-INACTIVE TO TRUE                         BBANK60P
013000           MOVE 00 TO BANK-HELP-SCREEN                            BBANK60P
013100           MOVE 'BBANK60P' TO BANK-LAST-PROG                      BBANK60P
013200           MOVE 'BBANK60P' TO BANK-NEXT-PROG                      BBANK60P
013300           MOVE 'MBANK60' TO BANK-LAST-MAPSET                     BBANK60P
013400           MOVE 'HELP60A' TO BANK-LAST-MAP                        BBANK60P
013500           MOVE 'MBANK60' TO BANK-NEXT-MAPSET                     BBANK60P
013600           MOVE 'BANK60A' TO BANK-NEXT-MAP                        BBANK60P
013700           GO TO COMMON-RETURN                                    BBANK60P
013800        ELSE                                                      BBANK60P
013900           MOVE 01 TO BANK-HELP-SCREEN                            BBANK60P
014000           MOVE 'BBANK60P' TO BANK-LAST-PROG                      BBANK60P
014100           MOVE 'BBANK60P' TO BANK-NEXT-PROG                      BBANK60P
014200           MOVE 'MBANK60' TO BANK-LAST-MAPSET                     BBANK60P
014300           MOVE 'BANK60A' TO BANK-LAST-MAP                        BBANK60P
014400           MOVE 'MBANK60' TO BANK-NEXT-MAPSET                     BBANK60P
014500           MOVE 'HELP60A' TO BANK-NEXT-MAP                        BBANK60P
014600           MOVE 'BANK60' TO HELP01I-SCRN                          BBANK60P
014700           COPY CHELPX01.                                         BBANK60P
014800           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK60P
014900           GO TO COMMON-RETURN                                    BBANK60P
015000     END-IF.                                                      BBANK60P
015100                                                                  BBANK60P
015200***************************************************************** BBANK60P
015300* Check the AID to see if we have to return to previous screen  * BBANK60P
015400***************************************************************** BBANK60P
015500     IF BANK-AID-PFK04                                            BBANK60P
015600        MOVE 'BBANK60P' TO BANK-LAST-PROG                         BBANK60P
015700        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK60P
015800        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK60P
015900        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK60P
016000        SET BANK-AID-ENTER TO TRUE                                BBANK60P
016100        GO TO COMMON-RETURN                                       BBANK60P
016200     END-IF.                                                      BBANK60P
016300                                                                  BBANK60P
016400* Check if we have set the screen up before or is this 1st time   BBANK60P
016500     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK60'                BBANK60P
016600        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK60P
016700        MOVE 'BBANK60P' TO BANK-LAST-PROG                         BBANK60P
016800        MOVE 'BBANK60P' TO BANK-NEXT-PROG                         BBANK60P
016900        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        BBANK60P
017000        MOVE 'BANK60A' TO BANK-LAST-MAP                           BBANK60P
017100        MOVE 'MBANK60' TO BANK-NEXT-MAPSET                        BBANK60P
017200        MOVE 'BANK60A' TO BANK-NEXT-MAP                           BBANK60P
017300        PERFORM POPULATE-SCREEN-DATA THRU                         BBANK60P
017400                POPULATE-SCREEN-DATA-EXIT                         BBANK60P
017500        GO TO COMMON-RETURN                                       BBANK60P
017600     END-IF.                                                      BBANK60P
017700                                                                  BBANK60P
017800***************************************************************** BBANK60P
017900* Check to see if the data changed                              * BBANK60P
018000***************************************************************** BBANK60P
018100     IF ADDR-CHANGE-REQUEST                                       BBANK60P
018200        IF BANK-SCR60-OLD-ADDR1 IS NOT EQUAL TO                   BBANK60P
018300             BANK-SCR60-NEW-ADDR1 OR                              BBANK60P
018400           BANK-SCR60-OLD-ADDR2 IS NOT EQUAL TO                   BBANK60P
018500             BANK-SCR60-NEW-ADDR2 OR                              BBANK60P
018600           BANK-SCR60-OLD-STATE IS NOT EQUAL TO                   BBANK60P
018700             BANK-SCR60-NEW-STATE OR                              BBANK60P
018800           BANK-SCR60-OLD-CNTRY IS NOT EQUAL TO                   BBANK60P
018900             BANK-SCR60-NEW-CNTRY OR                              BBANK60P
019000           BANK-SCR60-OLD-PSTCDE IS NOT EQUAL TO                  BBANK60P
019100             BANK-SCR60-NEW-PSTCDE OR                             BBANK60P
019200           BANK-SCR60-OLD-TELNO IS NOT EQUAL TO                   BBANK60P
019300             BANK-SCR60-NEW-TELNO OR                              BBANK60P
019400           BANK-SCR60-OLD-EMAIL IS NOT EQUAL TO                   BBANK60P
019500             BANK-SCR60-NEW-EMAIL OR                              BBANK60P
019600           BANK-SCR60-OLD-SEND-MAIL IS NOT EQUAL TO               BBANK60P
019700             BANK-SCR60-NEW-SEND-MAIL OR                          BBANK60P
019800           BANK-SCR60-OLD-SEND-EMAIL IS NOT EQUAL TO              BBANK60P
019900             BANK-SCR60-NEW-SEND-EMAIL                            BBANK60P
020000          SET ADDR-DATA-CHANGED TO TRUE                           BBANK60P
020100        ELSE                                                      BBANK60P
020200          SET ADDR-DATA-UNCHANGED TO TRUE                         BBANK60P
020300        END-IF                                                    BBANK60P
020400     END-IF.                                                      BBANK60P
020500* Data has changed, we need to validate changes                   BBANK60P
020600     IF ADDR-DATA-CHANGED AND                                     BBANK60P
020700        ADDR-CHANGE-REQUEST                                       BBANK60P
020800        PERFORM VALIDATE-DATA THRU                                BBANK60P
020900                VALIDATE-DATA-EXIT                                BBANK60P
021000        IF INPUT-ERROR                                            BBANK60P
021100           MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                    BBANK60P
021200           MOVE 'BBANK60P' TO BANK-LAST-PROG                      BBANK60P
021300           MOVE 'BBANK60P' TO BANK-NEXT-PROG                      BBANK60P
021400           MOVE 'MBANK60' TO BANK-LAST-MAPSET                     BBANK60P
021500           MOVE 'BANK60A' TO BANK-LAST-MAP                        BBANK60P
021600           MOVE 'MBANK60' TO BANK-NEXT-MAPSET                     BBANK60P
021700           MOVE 'BANK60A' TO BANK-NEXT-MAP                        BBANK60P
021800           GO TO COMMON-RETURN                                    BBANK60P
021900        END-IF                                                    BBANK60P
022000     END-IF.                                                      BBANK60P
022100                                                                  BBANK60P
022200* Data has changed, we need to verify the change                  BBANK60P
022300     IF ADDR-DATA-CHANGED AND                                     BBANK60P
022400        ADDR-CHANGE-REQUEST                                       BBANK60P
022500        MOVE 'Please use F10 to confirm changes' TO WS-ERROR-MSG  BBANK60P
022600        SET ADDR-CHANGE-VERIFY TO TRUE                            BBANK60P
022700        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK60P
022800        MOVE 'BBANK60P' TO BANK-LAST-PROG                         BBANK60P
022900        MOVE 'BBANK60P' TO BANK-NEXT-PROG                         BBANK60P
023000        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        BBANK60P
023100        MOVE 'BANK60A' TO BANK-LAST-MAP                           BBANK60P
023200        MOVE 'MBANK60' TO BANK-NEXT-MAPSET                        BBANK60P
023300        MOVE 'BANK60A' TO BANK-NEXT-MAP                           BBANK60P
023400        GO TO COMMON-RETURN                                       BBANK60P
023500     END-IF.                                                      BBANK60P
023600* Data was changed and verified                                   BBANK60P
023700     IF ADDR-CHANGE-VERIFY AND                                    BBANK60P
023800        BANK-AID-PFK10                                            BBANK60P
023900        MOVE SPACES TO CD02-DATA                                  BBANK60P
024000        MOVE BANK-SCR60-CONTACT-ID TO CD02I-CONTACT-ID            BBANK60P
024100* Set criteria for I/O rotine                                     BBANK60P
024200        SET CD02I-WRITE TO TRUE                                   BBANK60P
024300* Move the new data                                               BBANK60P
024400        MOVE BANK-SCR60-CONTACT-NAME TO CD02I-CONTACT-NAME        BBANK60P
024500        MOVE BANK-SCR60-NEW-ADDR1 TO CD02I-CONTACT-ADDR1          BBANK60P
024600        MOVE BANK-SCR60-NEW-ADDR2 TO CD02I-CONTACT-ADDR2          BBANK60P
024700        MOVE BANK-SCR60-NEW-STATE TO CD02I-CONTACT-STATE          BBANK60P
024800        MOVE BANK-SCR60-NEW-CNTRY TO CD02I-CONTACT-CNTRY          BBANK60P
024900        MOVE BANK-SCR60-NEW-PSTCDE TO CD02I-CONTACT-PSTCDE        BBANK60P
025000        MOVE BANK-SCR60-NEW-TELNO TO CD02I-CONTACT-TELNO          BBANK60P
025100        MOVE BANK-SCR60-NEW-EMAIL TO CD02I-CONTACT-EMAIL          BBANK60P
025200        MOVE BANK-SCR60-NEW-SEND-MAIL TO CD02I-CONTACT-SEND-MAIL  BBANK60P
025300        MOVE BANK-SCR60-NEW-SEND-EMAIL TO CD02I-CONTACT-SEND-EMAILBBANK60P
025400* Now go update the data                                          BBANK60P
025500 COPY CBANKX02.                                                   BBANK60P
025600        MOVE SPACES TO CD07-DATA                                  BBANK60P
025700        MOVE BANK-SCR60-CONTACT-ID TO CD07I-PERSON-PID            BBANK60P
025800        MOVE BANK-SCR60-OLD-ADDR1 TO CD07I-OLD-ADDR1              BBANK60P
025900        MOVE BANK-SCR60-OLD-ADDR2 TO CD07I-OLD-ADDR2              BBANK60P
026000        MOVE BANK-SCR60-OLD-STATE TO CD07I-OLD-STATE              BBANK60P
026100        MOVE BANK-SCR60-OLD-CNTRY TO CD07I-OLD-CNTRY              BBANK60P
026200        MOVE BANK-SCR60-OLD-PSTCDE TO CD07I-OLD-PSTCDE            BBANK60P
026300        MOVE BANK-SCR60-OLD-TELNO TO CD07I-OLD-TELNO              BBANK60P
026400        MOVE BANK-SCR60-OLD-EMAIL TO CD07I-OLD-EMAIL              BBANK60P
026500        MOVE BANK-SCR60-OLD-SEND-MAIL TO CD07I-OLD-SEND-MAIL      BBANK60P
026600        MOVE BANK-SCR60-OLD-SEND-EMAIL TO CD07I-OLD-SEND-EMAIL    BBANK60P
026700        MOVE BANK-SCR60-NEW-ADDR1 TO CD07I-NEW-ADDR1              BBANK60P
026800        MOVE BANK-SCR60-NEW-ADDR2 TO CD07I-NEW-ADDR2              BBANK60P
026900        MOVE BANK-SCR60-NEW-STATE TO CD07I-NEW-STATE              BBANK60P
027000        MOVE BANK-SCR60-NEW-CNTRY TO CD07I-NEW-CNTRY              BBANK60P
027100        MOVE BANK-SCR60-NEW-PSTCDE TO CD07I-NEW-PSTCDE            BBANK60P
027200        MOVE BANK-SCR60-NEW-TELNO TO CD07I-NEW-TELNO              BBANK60P
027300        MOVE BANK-SCR60-NEW-EMAIL TO CD07I-NEW-EMAIL              BBANK60P
027400        MOVE BANK-SCR60-NEW-SEND-MAIL TO CD07I-NEW-SEND-MAIL      BBANK60P
027500        MOVE BANK-SCR60-NEW-SEND-EMAIL TO CD07I-NEW-SEND-EMAIL    BBANK60P
027600 COPY CBANKX07.                                                   BBANK60P
027700        MOVE 'Address updated' TO BANK-RETURN-MSG                 BBANK60P
027800        MOVE SPACES TO BANK-SCREEN60-DATA                         BBANK60P
027900        MOVE 'BBANK60P' TO BANK-LAST-PROG                         BBANK60P
028000        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK60P
028100        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK60P
028200        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK60P
028300        SET BANK-AID-ENTER TO TRUE                                BBANK60P
028400        GO TO COMMON-RETURN                                       BBANK60P
028500     END-IF.                                                      BBANK60P
028600                                                                  BBANK60P
028700* Turn off update flags and redisplay                             BBANK60P
028800     SET ADDR-CHANGE-REQUEST TO TRUE.                             BBANK60P
028900     MOVE 'BBANK60P' TO BANK-LAST-PROG                            BBANK60P
029000     MOVE 'BBANK60P' TO BANK-NEXT-PROG                            BBANK60P
029100     MOVE 'MBANK60' TO BANK-LAST-MAPSET                           BBANK60P
029200     MOVE 'BANK60A' TO BANK-LAST-MAP                              BBANK60P
029300     MOVE 'MBANK60' TO BANK-NEXT-MAPSET                           BBANK60P
029400     MOVE 'BANK60A' TO BANK-NEXT-MAP                              BBANK60P
029500     GO TO COMMON-RETURN.                                         BBANK60P
029600                                                                  BBANK60P
029700***************************************************************** BBANK60P
029800* If we get this far then we have an error in our logic as we   * BBANK60P
029900* don't know where to go next.                                  * BBANK60P
030000***************************************************************** BBANK60P
030100     IF NOT BANK-ENV-CICS                                         BBANK60P
030200        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK60P
030300        MOVE '0001' TO ABEND-CODE                                 BBANK60P
030400        MOVE SPACES TO ABEND-REASON                               BBANK60P
030500        COPY CABENDPO.                                            BBANK60P
030600     END-IF.                                                      BBANK60P
030700     GOBACK.                                                      BBANK60P
030800                                                                  BBANK60P
030900 COMMON-RETURN.                                                   BBANK60P
031000     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK60P
031100 COPY CRETURN.                                                    BBANK60P
031200                                                                  BBANK60P
031300 VALIDATE-DATA.                                                   BBANK60P
031400     SET INPUT-OK TO TRUE.                                        BBANK60P
031500     MOVE SPACES TO STATE-PROV-WK-CNTRY.                          BBANK60P
031600     IF BANK-SCR60-NEW-CNTRY IS EQUAL TO 'USA'                    BBANK60P
031700        MOVE 'USA' TO STATE-PROV-WK-CNTRY                         BBANK60P
031800     END-IF.                                                      BBANK60P
031900     IF BANK-SCR60-NEW-CNTRY IS EQUAL TO 'CANADA'                 BBANK60P
032000        MOVE 'CDN' TO STATE-PROV-WK-CNTRY                         BBANK60P
032100     END-IF.                                                      BBANK60P
032200     IF STATE-PROV-WK-CNTRY IS EQUAL TO SPACES                    BBANK60P
032300        MOVE 'Country must be USA or CANADA' TO WS-ERROR-MSG      BBANK60P
032400        GO TO VALIDATE-DATA-ERROR                                 BBANK60P
032500     END-IF.                                                      BBANK60P
032600                                                                  BBANK60P
032700     MOVE 0 TO STATE-PROV-SUB.                                    BBANK60P
032800     DIVIDE LENGTH OF STATE-PROV-DATA (1) INTO                    BBANK60P
032900       LENGTH OF STATE-PROV-TABLE                                 BBANK60P
033000         GIVING STATE-PROV-COUNT.                                 BBANK60P
033100 VALIDATE-DATA-LOOP1.                                             BBANK60P
033200     ADD 1 TO STATE-PROV-SUB.                                     BBANK60P
033300     IF STATE-PROV-SUB IS GREATER THAN STATE-PROV-COUNT           BBANK60P
033400        MOVE 'Invlaid State/Prov code' TO WS-ERROR-MSG            BBANK60P
033500        GO TO VALIDATE-DATA-ERROR                                 BBANK60P
033600     END-IF.                                                      BBANK60P
033700     IF BANK-SCR60-NEW-STATE IS EQUAL TO                          BBANK60P
033800          STATE-PROV-CODE (STATE-PROV-SUB)                        BBANK60P
033900        GO TO VALIDATE-DATA-LOOP1-EXIT                            BBANK60P
034000     END-IF.                                                      BBANK60P
034100     GO TO VALIDATE-DATA-LOOP1.                                   BBANK60P
034200 VALIDATE-DATA-LOOP1-EXIT.                                        BBANK60P
034300     IF STATE-PROV-CNTRY (STATE-PROV-SUB) IS NOT EQUAL TO         BBANK60P
034400        STATE-PROV-WK-CNTRY                                       BBANK60P
034500        MOVE 'State/Prov not valid for Country' TO WS-ERROR-MSG   BBANK60P
034600        GO TO VALIDATE-DATA-ERROR                                 BBANK60P
034700     END-IF.                                                      BBANK60P
034800     IF BANK-SCR60-NEW-EMAIL IS NOT EQUAL TO SPACES               BBANK60P
034900        MOVE 0 TO STATE-PROV-SUB                                  BBANK60P
035000        INSPECT BANK-SCR60-NEW-EMAIL TALLYING STATE-PROV-SUB      BBANK60P
035100          FOR ALL '@'                                             BBANK60P
035200        IF STATE-PROV-SUB IS NOT EQUAL TO 1                       BBANK60P
035300           MOVE 'E-Mail address format invalid' TO WS-ERROR-MSG   BBANK60P
035400           GO TO VALIDATE-DATA-ERROR                              BBANK60P
035500        END-IF                                                    BBANK60P
035600     END-IF.                                                      BBANK60P
035700     IF BANK-SCR60-NEW-SEND-MAIL IS NOT EQUAL TO ' ' AND          BBANK60P
035800        BANK-SCR60-NEW-SEND-MAIL IS NOT EQUAL TO 'N' AND          BBANK60P
035900        BANK-SCR60-NEW-SEND-MAIL IS NOT EQUAL TO 'Y'              BBANK60P
036000        MOVE 'Send mail must be blank, Y or N' TO WS-ERROR-MSG    BBANK60P
036100        GO TO VALIDATE-DATA-ERROR                                 BBANK60P
036200     END-IF.                                                      BBANK60P
036300     IF BANK-SCR60-NEW-SEND-EMAIL IS NOT EQUAL TO ' ' AND         BBANK60P
036400        BANK-SCR60-NEW-SEND-EMAIL IS NOT EQUAL TO 'N' AND         BBANK60P
036500        BANK-SCR60-NEW-SEND-EMAIL IS NOT EQUAL TO 'Y'             BBANK60P
036600        MOVE 'Send E-Mail must be blank, Y or N' TO WS-ERROR-MSG  BBANK60P
036700        GO TO VALIDATE-DATA-ERROR                                 BBANK60P
036800     END-IF.                                                      BBANK60P
036900     IF BANK-SCR60-NEW-SEND-EMAIL IS EQUAL TO 'Y' AND             BBANK60P
037000        BANK-SCR60-NEW-EMAIL IS EQUAL TO SPACES                   BBANK60P
037100        MOVE 'Send E-Mail required E-Mail address' TO WS-ERROR-MSGBBANK60P
037200        GO TO VALIDATE-DATA-ERROR                                 BBANK60P
037300     END-IF.                                                      BBANK60P
037400                                                                  BBANK60P
037500     GO TO VALIDATE-DATA-EXIT.                                    BBANK60P
037600                                                                  BBANK60P
037700 VALIDATE-DATA-ERROR.                                             BBANK60P
037800     SET INPUT-ERROR TO TRUE.                                     BBANK60P
037900 VALIDATE-DATA-EXIT.                                              BBANK60P
038000     EXIT.                                                        BBANK60P
038100                                                                  BBANK60P
038200 POPULATE-SCREEN-DATA.                                            BBANK60P
038300     MOVE SPACES TO CD02-DATA.                                    BBANK60P
038400     MOVE BANK-USERID TO BANK-SCR60-CONTACT-ID.                   BBANK60P
038500     MOVE BANK-SCR60-CONTACT-ID TO CD02I-CONTACT-ID.              BBANK60P
038600* Set criteria for I/O rotine                                     BBANK60P
038700     SET CD02I-READ TO TRUE.                                      BBANK60P
038800* Now go get the data                                             BBANK60P
038900 COPY CBANKX02.                                                   BBANK60P
039000     MOVE SPACES TO BANK-SCR60-OLD-DETS.                          BBANK60P
039100     MOVE SPACES TO BANK-SCR60-NEW-DETS.                          BBANK60P
039200     IF CD02O-CONTACT-ID IS EQUAL TO CD02I-CONTACT-ID             BBANK60P
039300        MOVE CD02O-CONTACT-ID TO BANK-SCR60-CONTACT-ID            BBANK60P
039400        MOVE CD02O-CONTACT-NAME TO BANK-SCR60-CONTACT-NAME        BBANK60P
039500        MOVE CD02O-CONTACT-ADDR1 TO BANK-SCR60-OLD-ADDR1          BBANK60P
039600        MOVE CD02O-CONTACT-ADDR2 TO BANK-SCR60-OLD-ADDR2          BBANK60P
039700        MOVE CD02O-CONTACT-STATE TO BANK-SCR60-OLD-STATE          BBANK60P
039800        MOVE CD02O-CONTACT-CNTRY TO BANK-SCR60-OLD-CNTRY          BBANK60P
039900        MOVE CD02O-CONTACT-PSTCDE TO BANK-SCR60-OLD-PSTCDE        BBANK60P
040000        MOVE CD02O-CONTACT-TELNO TO BANK-SCR60-OLD-TELNO          BBANK60P
040100        MOVE CD02O-CONTACT-EMAIL TO BANK-SCR60-OLD-EMAIL          BBANK60P
040200        MOVE CD02O-CONTACT-SEND-MAIL TO BANK-SCR60-OLD-SEND-MAIL  BBANK60P
040300        MOVE CD02O-CONTACT-SEND-EMAIL TO BANK-SCR60-OLD-SEND-EMAILBBANK60P
040400        MOVE CD02O-CONTACT-ADDR1 TO BANK-SCR60-NEW-ADDR1          BBANK60P
040500        MOVE CD02O-CONTACT-ADDR2 TO BANK-SCR60-NEW-ADDR2          BBANK60P
040600        MOVE CD02O-CONTACT-STATE TO BANK-SCR60-NEW-STATE          BBANK60P
040700        MOVE CD02O-CONTACT-CNTRY TO BANK-SCR60-NEW-CNTRY          BBANK60P
040800        MOVE CD02O-CONTACT-PSTCDE TO BANK-SCR60-NEW-PSTCDE        BBANK60P
040900        MOVE CD02O-CONTACT-TELNO TO BANK-SCR60-NEW-TELNO          BBANK60P
041000        MOVE CD02O-CONTACT-EMAIL TO BANK-SCR60-NEW-EMAIL          BBANK60P
041100        MOVE CD02O-CONTACT-SEND-MAIL TO BANK-SCR60-NEW-SEND-MAIL  BBANK60P
041200        MOVE CD02O-CONTACT-SEND-EMAIL TO BANK-SCR60-NEW-SEND-EMAILBBANK60P
041300     ELSE                                                         BBANK60P
041400        MOVE CD02O-CONTACT-NAME TO BANK-SCR60-CONTACT-NAME        BBANK60P
041500     END-IF.                                                      BBANK60P
041600 POPULATE-SCREEN-DATA-EXIT.                                       BBANK60P
041700     EXIT.                                                        BBANK60P
041800                                                                  BBANK60P
