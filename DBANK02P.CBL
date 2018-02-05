000100***************************************************************** DBANK02P
000200*                                                               * DBANK02P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK02P
000400*   This demonstration program is provided for use by users     * DBANK02P
000500*   of Micro Focus products and may be used, modified and       * DBANK02P
000600*   distributed as part of your application provided that       * DBANK02P
000700*   you properly acknowledge the copyright of Micro Focus       * DBANK02P
000800*   in this material.                                           * DBANK02P
000900*                                                               * DBANK02P
001000***************************************************************** DBANK02P
001100                                                                  DBANK02P
001200***************************************************************** DBANK02P
001300* Program:     DBANK02P.CBL                                     * DBANK02P
001400* Function:    Obtain/update address information                * DBANK02P
001500*              SQL version                                      * DBANK02P
001600***************************************************************** DBANK02P
001700                                                                  DBANK02P
001800 IDENTIFICATION DIVISION.                                         DBANK02P
001900 PROGRAM-ID.                                                      DBANK02P
002000     DBANK02P.                                                    DBANK02P
002100 DATE-WRITTEN.                                                    DBANK02P
002200     September 2002.                                              DBANK02P
002300 DATE-COMPILED.                                                   DBANK02P
002400     Today.                                                       DBANK02P
002500                                                                  DBANK02P
002600 ENVIRONMENT DIVISION.                                            DBANK02P
002700                                                                  DBANK02P
002800 DATA DIVISION.                                                   DBANK02P
002900                                                                  DBANK02P
003000 WORKING-STORAGE SECTION.                                         DBANK02P
003100 01  WS-MISC-STORAGE.                                             DBANK02P
003200   05  WS-PROGRAM-ID                         PIC X(8)             DBANK02P
003300       VALUE 'DBANK02P'.                                          DBANK02P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK02P
004100
           EXEC SQL                                                     DBANK02P
004200          BEGIN DECLARE SECTION                                   DBANK02P
004300     END-EXEC.                                                    DBANK02P
003500                                                                  DBANK02P
003600 01  WS-COMMAREA.                                                 DBANK02P
003700     EXEC SQL                                                     DBANK02P
003800          INCLUDE CBANKD02                                        DBANK02P
003900     END-EXEC.                                                    DBANK02P
004000                                                                  DBANK02P
004400     EXEC SQL                                                     DBANK02P
004500          INCLUDE CBANKSCS                                        DBANK02P
004600     END-EXEC.                                                    DBANK02P
004700     EXEC SQL                                                     DBANK02P
004800          INCLUDE SQLCA                                           DBANK02P
004900     END-EXEC.                                                    DBANK02P
005000                                                                  DBANK02P
005100 COPY CABENDD.                                                    DBANK02P
004100     EXEC SQL                                                     DBANK02P
004200          END DECLARE SECTION                                     DBANK02P
004300     END-EXEC.                                                    DBANK02P
005200                                                                  DBANK02P
005300 LINKAGE SECTION.                                                 DBANK02P
005400 01  DFHCOMMAREA.                                                 DBANK02P
005500   05  LK-COMMAREA                           PIC X(1)             DBANK02P
005600       OCCURS 1 TO 6144 TIMES                                     DBANK02P
005700         DEPENDING ON WS-COMMAREA-LENGTH.                         DBANK02P
005800                                                                  DBANK02P
005900 COPY CENTRY.                                                     DBANK02P
006000***************************************************************** DBANK02P
006100* Move the passed data to our area                              * DBANK02P
006200***************************************************************** DBANK02P
006300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK02P
006400     MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK02P
006500                                                                  DBANK02P
006600***************************************************************** DBANK02P
006700* Initialize our output area                                    * DBANK02P
006800***************************************************************** DBANK02P
006900     MOVE SPACES TO CD02O-DATA.                                   DBANK02P
007000                                                                  DBANK02P
007100***************************************************************** DBANK02P
007200* See if we have a read or write request and react accordingly  * DBANK02P
007300***************************************************************** DBANK02P
007400     EVALUATE TRUE                                                DBANK02P
007500       WHEN CD02I-READ                                            DBANK02P
007600         PERFORM READ-PROCESSING THRU                             DBANK02P
007700                 READ-PROCESSING-EXIT                             DBANK02P
007800       WHEN CD02I-WRITE                                           DBANK02P
007900         PERFORM WRITE-PROCESSING THRU                            DBANK02P
008000                 WRITE-PROCESSING-EXIT                            DBANK02P
008100       WHEN OTHER                                                 DBANK02P
008200         MOVE HIGH-VALUES TO CD02O-CONTACT-ID                     DBANK02P
008300         MOVE 'Bad request code' TO CD02O-CONTACT-NAME            DBANK02P
008400     END-EVALUATE.                                                DBANK02P
008500                                                                  DBANK02P
008600***************************************************************** DBANK02P
008700* Move the result back to the callers area                      * DBANK02P
008800***************************************************************** DBANK02P
008900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       DBANK02P
009000                                                                  DBANK02P
009100***************************************************************** DBANK02P
009200* Return to our caller                                          * DBANK02P
009300***************************************************************** DBANK02P
009400 COPY CRETURN.                                                    DBANK02P
009500                                                                  DBANK02P
009600***************************************************************** DBANK02P
009700* Read request                                                  * DBANK02P
009800***************************************************************** DBANK02P
009900 READ-PROCESSING.                                                 DBANK02P
010000     EXEC SQL                                                     DBANK02P
010100          SELECT CS.BCS_PID,                                      DBANK02P
010200                 CS.BCS_NAME,                                     DBANK02P
010300                 CS.BCS_ADDR1,                                    DBANK02P
010400                 CS.BCS_ADDR2,                                    DBANK02P
010500                 CS.BCS_STATE,                                    DBANK02P
010600                 CS.BCS_COUNTRY,                                  DBANK02P
010700                 CS.BCS_POST_CODE,                                DBANK02P
010800                 CS.BCS_TEL,                                      DBANK02P
010900                 CS.BCS_EMAIL,                                    DBANK02P
011000                 CS.BCS_SEND_MAIL,                                DBANK02P
011100                 CS.BCS_SEND_EMAIL                                DBANK02P
011200          INTO :DCL-BCS-PID,                                      DBANK02P
011300               :DCL-BCS-NAME,                                     DBANK02P
011400               :DCL-BCS-ADDR1,                                    DBANK02P
011500               :DCL-BCS-ADDR2,                                    DBANK02P
011600               :DCL-BCS-STATE,                                    DBANK02P
011700               :DCL-BCS-COUNTRY,                                  DBANK02P
011800               :DCL-BCS-POST-CODE,                                DBANK02P
011900               :DCL-BCS-TEL,                                      DBANK02P
012000               :DCL-BCS-EMAIL,                                    DBANK02P
012100               :DCL-BCS-SEND-MAIL,                                DBANK02P
012200               :DCL-BCS-SEND-EMAIL                                DBANK02P
012300          FROM BNKCUST CS                                         DBANK02P
012400          WHERE CS.BCS_PID = :CD02I-CONTACT-ID                    DBANK02P
012500     END-EXEC.                                                    DBANK02P
012600                                                                  DBANK02P
012700***************************************************************** DBANK02P
012800* Did we get the record OK                                      * DBANK02P
012900***************************************************************** DBANK02P
013000     IF SQLCODE IS EQUAL TO ZERO                                  DBANK02P
013100        MOVE DCL-BCS-PID TO CD02O-CONTACT-ID                      DBANK02P
013200        MOVE DCL-BCS-NAME TO CD02O-CONTACT-NAME                   DBANK02P
013300        MOVE DCL-BCS-ADDR1 TO CD02O-CONTACT-ADDR1                 DBANK02P
013400        MOVE DCL-BCS-ADDR2 TO CD02O-CONTACT-ADDR2                 DBANK02P
013500        MOVE DCL-BCS-STATE TO CD02O-CONTACT-STATE                 DBANK02P
013600        MOVE DCL-BCS-COUNTRY TO CD02O-CONTACT-CNTRY               DBANK02P
013700        MOVE DCL-BCS-POST-CODE TO CD02O-CONTACT-PSTCDE            DBANK02P
013800        MOVE DCL-BCS-TEL TO CD02O-CONTACT-TELNO                   DBANK02P
013900        MOVE DCL-BCS-EMAIL TO CD02O-CONTACT-EMAIL                 DBANK02P
014000        MOVE DCL-BCS-SEND-MAIL TO CD02O-CONTACT-SEND-MAIL         DBANK02P
014100        MOVE DCL-BCS-SEND-EMAIL TO CD02O-CONTACT-SEND-EMAIL       DBANK02P
014200     END-IF.                                                      DBANK02P
014300                                                                  DBANK02P
014400***************************************************************** DBANK02P
014500* Was the record not found?                                     * DBANK02P
014600***************************************************************** DBANK02P
014700     IF SQLCODE IS NOT EQUAL TO ZERO                              DBANK02P
014800        MOVE SPACES TO CD02O-DATA                                 DBANK02P
014900        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      DBANK02P
015000        MOVE 'Bad SQL code' TO CD02O-CONTACT-NAME                 DBANK02P
015100     END-IF.                                                      DBANK02P
015200                                                                  DBANK02P
015300 READ-PROCESSING-EXIT.                                            DBANK02P
015400     EXIT.                                                        DBANK02P
015500                                                                  DBANK02P
015600***************************************************************** DBANK02P
015700* Write request                                                 * DBANK02P
015800***************************************************************** DBANK02P
015900 WRITE-PROCESSING.                                                DBANK02P
016000     MOVE CD02I-CONTACT-ADDR1 TO DCL-BCS-ADDR1.                   DBANK02P
016100     MOVE CD02I-CONTACT-ADDR2 TO DCL-BCS-ADDR2.                   DBANK02P
016200     MOVE CD02I-CONTACT-STATE TO DCL-BCS-STATE.                   DBANK02P
016300     MOVE CD02I-CONTACT-CNTRY TO DCL-BCS-COUNTRY.                 DBANK02P
016400     MOVE CD02I-CONTACT-PSTCDE TO DCL-BCS-POST-CODE.              DBANK02P
016500     MOVE CD02I-CONTACT-TELNO TO DCL-BCS-TEL.                     DBANK02P
016600     MOVE CD02I-CONTACT-EMAIL TO DCL-BCS-EMAIL.                   DBANK02P
016700     MOVE CD02I-CONTACT-SEND-MAIL TO DCL-BCS-SEND-MAIL.           DBANK02P
016800     MOVE CD02I-CONTACT-SEND-EMAIL TO DCL-BCS-SEND-EMAIL.         DBANK02P
016900     EXEC SQL                                                     DBANK02P
017000          UPDATE BNKCUST                                          DBANK02P
017100          SET BCS_ADDR1 = :DCL-BCS-ADDR1,                         DBANK02P
017200              BCS_ADDR2 = :DCL-BCS-ADDR2,                         DBANK02P
017300              BCS_STATE = :DCL-BCS-STATE,                         DBANK02P
017400              BCS_COUNTRY = :DCL-BCS-COUNTRY,                     DBANK02P
017500              BCS_POST_CODE = :DCL-BCS-POST-CODE,                 DBANK02P
017600              BCS_TEL = :DCL-BCS-TEL,                             DBANK02P
017700              BCS_EMAIL = :DCL-BCS-EMAIL,                         DBANK02P
017800              BCS_SEND_MAIL = :DCL-BCS-SEND-MAIL,                 DBANK02P
017900              BCS_SEND_EMAIL = :DCL-BCS-SEND-EMAIL                DBANK02P
018000          WHERE BCS_PID = :CD02I-CONTACT-ID                       DBANK02P
018100     END-EXEC.                                                    DBANK02P
018200                                                                  DBANK02P
018300***************************************************************** DBANK02P
018400* Did we get the record OK                                      * DBANK02P
018500***************************************************************** DBANK02P
018600     IF SQLCODE IS EQUAL TO ZERO                                  DBANK02P
018700        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      DBANK02P
018800        MOVE 'Update OK' TO CD02O-CONTACT-NAME                    DBANK02P
018900     END-IF.                                                      DBANK02P
019000                                                                  DBANK02P
019100***************************************************************** DBANK02P
019200* Was the record not found?                                     * DBANK02P
019300***************************************************************** DBANK02P
019400     IF SQLCODE IS NOT EQUAL TO ZERO                              DBANK02P
019500        MOVE SPACES TO CD02O-DATA                                 DBANK02P
019600        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      DBANK02P
019700        MOVE 'Bad SQL code' TO CD02O-CONTACT-NAME                 DBANK02P
019800     END-IF.                                                      DBANK02P
019900                                                                  DBANK02P
020000 WRITE-PROCESSING-EXIT.                                           DBANK02P
020100     EXIT.                                                        DBANK02P
020200                                                                  DBANK02P
