000100***************************************************************** BBANK80P
000200*                                                               * BBANK80P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK80P
000400*   This demonstration program is provided for use by users     * BBANK80P
000500*   of Micro Focus products and may be used, modified and       * BBANK80P
000600*   distributed as part of your application provided that       * BBANK80P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK80P
000800*   in this material.                                           * BBANK80P
000900*                                                               * BBANK80P
001000***************************************************************** BBANK80P
001100                                                                  BBANK80P
001200***************************************************************** BBANK80P
001300* Program:     BBANK80P.CBL                                     * BBANK80P
001400* Layer:       Business logic                                   * BBANK80P
001500* Function:    Obtain data for "more information"               * BBANK80P
001600***************************************************************** BBANK80P
001700                                                                  BBANK80P
001800 IDENTIFICATION DIVISION.                                         BBANK80P
001900 PROGRAM-ID.                                                      BBANK80P
002000     BBANK80P.                                                    BBANK80P
002100 DATE-WRITTEN.                                                    BBANK80P
002200     September 2002.                                              BBANK80P
002300 DATE-COMPILED.                                                   BBANK80P
002400     Today.                                                       BBANK80P
002500                                                                  BBANK80P
002600 ENVIRONMENT DIVISION.                                            BBANK80P
002700                                                                  BBANK80P
002800 DATA DIVISION.                                                   BBANK80P
002900 WORKING-STORAGE SECTION.                                         BBANK80P
003000 01  WS-MISC-STORAGE.                                             BBANK80P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK80P
003200       VALUE 'BBANK80P'.                                          BBANK80P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK80P
003400     88  INPUT-OK                            VALUE '0'.           BBANK80P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK80P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK80P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK80P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK80P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK80P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK80P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK80P
004200     88  PFK-VALID                           VALUE '0'.           BBANK80P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK80P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK80P
004500   05  WS-SUB1                               PIC S9(4) COMP.      BBANK80P
004600   05  WS-SUB1-LIMIT                         PIC S9(4) COMP.      BBANK80P
004700                                                                  BBANK80P
004800 01  WS-INF-DATA.                                                 BBANK80P
004900   05  WS-INF-DATA01.                                             BBANK80P
005000     10  FILLER                              PIC X(40)            BBANK80P
005100         VALUE 'Sorry. The information you requested is '.        BBANK80P
005200     10  FILLER                              PIC X(35)            BBANK80P
005300         VALUE 'not available at this time.        '.             BBANK80P
005400   05  WS-INF-DATA03.                                             BBANK80P
005500     10  FILLER                              PIC X(40)            BBANK80P
005600	       VALUE 'Please try our web site at:	      '.	
005700     10  FILLER                              PIC X(35)            BBANK80P
005800         VALUE '                                   '.             BBANK80P
005900   05  WS-INF-DATA05.                                             BBANK80P
006000     10  FILLER                              PIC X(40)            BBANK80P
006100         VALUE '     http://www.microfocus.com          '.        BBANK80P
006200     10  FILLER                              PIC X(35)            BBANK80P
006300         VALUE '                                   '.             BBANK80P
006400   05  WS-INF-DATA07.                                             BBANK80P
006500     10  FILLER                              PIC X(40)            BBANK80P
006600         VALUE 'or call our office at 1-800-VS-COBOL    '.        BBANK80P
006700     10  FILLER                              PIC X(35)            BBANK80P
006800         VALUE '                                   '.             BBANK80P
006900   05  WS-INF-DATA08.                                             BBANK80P
007000     10  FILLER                              PIC X(40)            BBANK80P
007100         VALUE '                     (1-800-872-6265)   '.        BBANK80P
007200     10  FILLER                              PIC X(35)            BBANK80P
007300         VALUE '                                   '.             BBANK80P
007400   05  WS-INF-DATA10.                                             BBANK80P
007500     10  FILLER                              PIC X(40)            BBANK80P
007600         VALUE 'Thank you for your interest.            '.        BBANK80P
007700     10  FILLER                              PIC X(35)            BBANK80P
007800         VALUE '                                   '.             BBANK80P
007900                                                                  BBANK80P
008000 01  WS-BANK-DATA.                                                BBANK80P
008100 COPY CBANKDAT.                                                   BBANK80P
008200                                                                  BBANK80P
008300 01  WS-HELP-DATA.                                                BBANK80P
008400 COPY CHELPD01.                                                   BBANK80P
008500                                                                  BBANK80P
008600 COPY CABENDD.                                                    BBANK80P
008700                                                                  BBANK80P
008800 LINKAGE SECTION.                                                 BBANK80P
008900 01  DFHCOMMAREA.                                                 BBANK80P
009000   05  LK-COMMAREA                           PIC X(6144).         BBANK80P
009100                                                                  BBANK80P
009200 COPY CENTRY.                                                     BBANK80P
009300***************************************************************** BBANK80P
009400* Make ourselves re-entrant                                     * BBANK80P
009500***************************************************************** BBANK80P
009600     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK80P
009700                                                                  BBANK80P
009800***************************************************************** BBANK80P
009900* Move the passed area to our area                              * BBANK80P
010000***************************************************************** BBANK80P
010100     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK80P
010200                                                                  BBANK80P
010300***************************************************************** BBANK80P
010400* Ensure error message is cleared                               * BBANK80P
010500***************************************************************** BBANK80P
010600     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK80P
010700                                                                  BBANK80P
010800***************************************************************** BBANK80P
010900* This is the main process                                      * BBANK80P
011000***************************************************************** BBANK80P
011100                                                                  BBANK80P
011200***************************************************************** BBANK80P
011300* Save the passed return message and then turn it off           * BBANK80P
011400***************************************************************** BBANK80P
011500     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANK80P
011600     SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANK80P
011700                                                                  BBANK80P
011800     MOVE WS-RETURN-MSG TO WS-ERROR-MSG.                          BBANK80P
011900                                                                  BBANK80P
012000***************************************************************** BBANK80P
012100* Check the AID to see if its valid at this point               * BBANK80P
012200***************************************************************** BBANK80P
012300     SET PFK-INVALID TO TRUE.                                     BBANK80P
012400     IF BANK-AID-ENTER OR                                         BBANK80P
012500        BANK-AID-PFK03 OR                                         BBANK80P
012600        BANK-AID-PFK04                                            BBANK80P
012700        SET PFK-VALID TO TRUE                                     BBANK80P
012800     END-IF.                                                      BBANK80P
012900     IF BANK-AID-PFK01 AND                                        BBANK80P
013000        BANK-HELP-INACTIVE                                        BBANK80P
013100        SET BANK-HELP-ACTIVE TO TRUE                              BBANK80P
013200        SET PFK-VALID TO TRUE                                     BBANK80P
013300     END-IF.                                                      BBANK80P
013400     IF PFK-INVALID                                               BBANK80P
013500        SET BANK-AID-ENTER TO TRUE                                BBANK80P
013600     END-IF.                                                      BBANK80P
013700                                                                  BBANK80P
013800***************************************************************** BBANK80P
013900* Check the AID to see if we have to quit                       * BBANK80P
014000***************************************************************** BBANK80P
014100     IF BANK-AID-PFK03                                            BBANK80P
014200        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
014300        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK80P
014400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK80P
014500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK80P
014600        GO TO COMMON-RETURN                                       BBANK80P
014700     END-IF.                                                      BBANK80P
014800                                                                  BBANK80P
014900***************************************************************** BBANK80P
015000* Check the to see if user needs or has been using help         * BBANK80P
015100***************************************************************** BBANK80P
015200     IF BANK-HELP-ACTIVE                                          BBANK80P
015300        IF BANK-AID-PFK04                                         BBANK80P
015400           SET BANK-HELP-INACTIVE TO TRUE                         BBANK80P
015500           MOVE 00 TO BANK-HELP-SCREEN                            BBANK80P
015600           MOVE 'BBANK80P' TO BANK-LAST-PROG                      BBANK80P
015700           MOVE 'BBANK80P' TO BANK-NEXT-PROG                      BBANK80P
015800           MOVE 'MBANK80' TO BANK-LAST-MAPSET                     BBANK80P
015900           MOVE 'HELP80A' TO BANK-LAST-MAP                        BBANK80P
016000           MOVE 'MBANK80' TO BANK-NEXT-MAPSET                     BBANK80P
016100           MOVE 'BANK80A' TO BANK-NEXT-MAP                        BBANK80P
016200           PERFORM POPULATE-SCR80-DATA THRU                       BBANK80P
016300                   POPULATE-SCR80-DATA-EXIT                       BBANK80P
016400           GO TO COMMON-RETURN                                    BBANK80P
016500        ELSE                                                      BBANK80P
016600           MOVE 01 TO BANK-HELP-SCREEN                            BBANK80P
016700           MOVE 'BBANK80P' TO BANK-LAST-PROG                      BBANK80P
016800           MOVE 'BBANK80P' TO BANK-NEXT-PROG                      BBANK80P
016900           MOVE 'MBANK80' TO BANK-LAST-MAPSET                     BBANK80P
017000           MOVE 'BANK80A' TO BANK-LAST-MAP                        BBANK80P
017100           MOVE 'MBANK80' TO BANK-NEXT-MAPSET                     BBANK80P
017200           MOVE 'HELP80A' TO BANK-NEXT-MAP                        BBANK80P
017300           MOVE 'BANK80' TO HELP01I-SCRN                          BBANK80P
017400           COPY CHELPX01.                                         BBANK80P
017500           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK80P
017600           GO TO COMMON-RETURN                                    BBANK80P
017700     END-IF.                                                      BBANK80P
017800                                                                  BBANK80P
017900***************************************************************** BBANK80P
018000* Check the AID to see if we have to return to previous screen  * BBANK80P
018100***************************************************************** BBANK80P
018200     IF BANK-AID-PFK04                                            BBANK80P
018300        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
018400        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK80P
018500        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK80P
018600        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK80P
018700        SET BANK-AID-ENTER TO TRUE                                BBANK80P
018800        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      BBANK80P
018900        GO TO COMMON-RETURN                                       BBANK80P
019000     END-IF.                                                      BBANK80P
019100                                                                  BBANK80P
019200* Check if we have set the screen up before or is this 1st time   BBANK80P
019300     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK80'                BBANK80P
019400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK80P
019500        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
019600        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         BBANK80P
019700        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        BBANK80P
019800        MOVE 'BANK80A' TO BANK-LAST-MAP                           BBANK80P
019900        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        BBANK80P
020000        MOVE 'BANK80A' TO BANK-NEXT-MAP                           BBANK80P
020100        PERFORM POPULATE-SCR80-DATA THRU                          BBANK80P
020200                POPULATE-SCR80-DATA-EXIT                          BBANK80P
020300        GO TO COMMON-RETURN                                       BBANK80P
020400     END-IF.                                                      BBANK80P
020500                                                                  BBANK80P
020600     PERFORM VALIDATE-DATA THRU                                   BBANK80P
020700             VALIDATE-DATA-EXIT.                                  BBANK80P
020800                                                                  BBANK80P
020900* If we had an error display error and return                     BBANK80P
021000     IF INPUT-ERROR                                               BBANK80P
021100        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK80P
021200        MOVE 'BBANK80P' TO BANK-LAST-PROG                         BBANK80P
021300        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         BBANK80P
021400        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        BBANK80P
021500        MOVE 'BANK80A' TO BANK-LAST-MAP                           BBANK80P
021600        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        BBANK80P
021700        MOVE 'BANK80A' TO BANK-NEXT-MAP                           BBANK80P
021800        GO TO COMMON-RETURN                                       BBANK80P
021900     END-IF.                                                      BBANK80P
022000                                                                  BBANK80P
022100     PERFORM POPULATE-SCR80-DATA THRU                             BBANK80P
022200             POPULATE-SCR80-DATA-EXIT.                            BBANK80P
022300     GO TO COMMON-RETURN.                                         BBANK80P
022400                                                                  BBANK80P
022500***************************************************************** BBANK80P
022600* If we get this far then we have an error in our logic as we   * BBANK80P
022700* don't know where to go next.                                  * BBANK80P
022800***************************************************************** BBANK80P
022900     IF BANK-ENV-CICS                                             BBANK80P
023000        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK80P
023100        MOVE '0001' TO ABEND-CODE                                 BBANK80P
023200        MOVE SPACES TO ABEND-REASON                               BBANK80P
023300        COPY CABENDPO.                                            BBANK80P
023400     END-IF.                                                      BBANK80P
023500     GOBACK.                                                      BBANK80P
023600                                                                  BBANK80P
023700 COMMON-RETURN.                                                   BBANK80P
023800     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK80P
023900 COPY CRETURN.                                                    BBANK80P
024000                                                                  BBANK80P
024100 VALIDATE-DATA.                                                   BBANK80P
024200     SET INPUT-OK TO TRUE.                                        BBANK80P
024300                                                                  BBANK80P
024400     GO TO VALIDATE-DATA-EXIT.                                    BBANK80P
024500 VALIDATE-DATA-ERROR.                                             BBANK80P
024600     SET INPUT-ERROR TO TRUE.                                     BBANK80P
024700 VALIDATE-DATA-EXIT.                                              BBANK80P
024800     EXIT.                                                        BBANK80P
024900                                                                  BBANK80P
025000 POPULATE-SCR80-DATA.                                             BBANK80P
025100     MOVE 'INFO80' TO HELP01I-SCRN.                               BBANK80P
025200     COPY CHELPX01.                                               BBANK80P
025300     IF HELP-NOT-FOUND                                            BBANK80P
025400        MOVE SPACES TO HELP01O-INDIVIDUAL-LINES                   BBANK80P
025500        MOVE WS-INF-DATA01 TO HELP01O-L01                         BBANK80P
025600        MOVE WS-INF-DATA03 TO HELP01O-L03                         BBANK80P
025700        MOVE WS-INF-DATA05 TO HELP01O-L05                         BBANK80P
025800        MOVE WS-INF-DATA07 TO HELP01O-L07                         BBANK80P
025900        MOVE WS-INF-DATA08 TO HELP01O-L08                         BBANK80P
026000        MOVE WS-INF-DATA10 TO HELP01O-L10                         BBANK80P
026100     END-IF.                                                      BBANK80P
026200     MOVE HELP01O-DATA TO BANK-SCREEN80-DATA.                     BBANK80P
026300 POPULATE-SCR80-DATA-EXIT.                                        BBANK80P
026400     EXIT.                                                        BBANK80P
026500                                                                  BBANK80P
