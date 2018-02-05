000100***************************************************************** BBANK99P
000200*                                                               * BBANK99P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK99P
000400*   This demonstration program is provided for use by users     * BBANK99P
000500*   of Micro Focus products and may be used, modified and       * BBANK99P
000600*   distributed as part of your application provided that       * BBANK99P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK99P
000800*   in this material.                                           * BBANK99P
000900*                                                               * BBANK99P
001000***************************************************************** BBANK99P
001100                                                                  BBANK99P
001200***************************************************************** BBANK99P
001300* Program:     BBANK99P.CBL                                     * BBANK99P
001400* Layer:       Business logic                                   * BBANK99P
001500* Function:    Terminate the pseudo converation                 * BBANK99P
001600***************************************************************** BBANK99P
001700                                                                  BBANK99P
001800 IDENTIFICATION DIVISION.                                         BBANK99P
001900 PROGRAM-ID.                                                      BBANK99P
002000     BBANK99P.                                                    BBANK99P
002100 DATE-WRITTEN.                                                    BBANK99P
002200     September 2002.                                              BBANK99P
002300 DATE-COMPILED.                                                   BBANK99P
002400     Today.                                                       BBANK99P
002500                                                                  BBANK99P
002600 ENVIRONMENT DIVISION.                                            BBANK99P
002700                                                                  BBANK99P
002800 DATA DIVISION.                                                   BBANK99P
002900 WORKING-STORAGE SECTION.                                         BBANK99P
003000 01  WS-MISC-STORAGE.                                             BBANK99P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK99P
003200       VALUE 'BBANK99P'.                                          BBANK99P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK99P
003400     88  INPUT-OK                            VALUE '0'.           BBANK99P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK99P
003600   05  WS-ERROR-MSG                          PIC X(75).           BBANK99P
003700                                                                  BBANK99P
003800 01  WS-BANK-DATA.                                                BBANK99P
003900 COPY CBANKDAT.                                                   BBANK99P
004000                                                                  BBANK99P
004100 COPY CABENDD.                                                    BBANK99P
004200                                                                  BBANK99P
004300 LINKAGE SECTION.                                                 BBANK99P
004400 01  DFHCOMMAREA.                                                 BBANK99P
004500   05  LK-COMMAREA                           PIC X(6144).         BBANK99P
004600                                                                  BBANK99P
004700 COPY CENTRY.                                                     BBANK99P
004800                                                                  BBANK99P
004900***************************************************************** BBANK99P
005000* Make ourselves re-entrant                                     * BBANK99P
005100***************************************************************** BBANK99P
005200     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK99P
005300                                                                  BBANK99P
005400***************************************************************** BBANK99P
005500* Move the passed area to our area                              * BBANK99P
005600***************************************************************** BBANK99P
005700     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK99P
005800                                                                  BBANK99P
005900***************************************************************** BBANK99P
006000* Ensure error message is cleared                               * BBANK99P
006100***************************************************************** BBANK99P
006200     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK99P
006300                                                                  BBANK99P
006400***************************************************************** BBANK99P
006500* This is the main process                                      * BBANK99P
006600***************************************************************** BBANK99P
006700     MOVE SPACES TO BANK-IMS-SPA-TRANCODE.                        BBANK99P
006800     MOVE 'BBANK99P' TO BANK-LAST-PROG                            BBANK99P
006900     MOVE 'BBANK99P' TO BANK-NEXT-PROG                            BBANK99P
007000     MOVE 'MBANK99' TO BANK-LAST-MAPSET                           BBANK99P
007100     MOVE 'BANK99A' TO BANK-LAST-MAP                              BBANK99P
007200     MOVE 'MBANK99' TO BANK-NEXT-MAPSET                           BBANK99P
007300     MOVE 'BANK99A' TO BANK-NEXT-MAP                              BBANK99P
007400     GO TO COMMON-RETURN.                                         BBANK99P
007500                                                                  BBANK99P
007600 COMMON-RETURN.                                                   BBANK99P
007700     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK99P
007800 COPY CRETURN.                                                    BBANK99P
007900                                                                  BBANK99P
