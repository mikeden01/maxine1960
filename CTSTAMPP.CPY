000100***************************************************************** CTSTAMPP
000200*                                                               * CTSTAMPP
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CTSTAMPP
000400*                                                               * CTSTAMPP
000500***************************************************************** CTSTAMPP
000600                                                                  CTSTAMPP
000700***************************************************************** CTSTAMPP
000800* CTIMERP.CPY                                                   * CTSTAMPP
000900*---------------------------------------------------------------* CTSTAMPP
001000* Simulate SQL TIMESTAMP function                                 CTSTAMPP
001100***************************************************************** CTSTAMPP
001200     ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD.                    CTSTAMPP
001300     ACCEPT WS-ACCEPT-TIME FROM TIME.                             CTSTAMPP
001400     MOVE WS-ACCEPT-DATE (1:4) TO WS-TS-DATE-YYYY.                CTSTAMPP
001500     MOVE '-' TO WS-TS-DATE-DASH1.                                CTSTAMPP
001600     MOVE WS-ACCEPT-DATE (5:2) TO WS-TS-DATE-MM.                  CTSTAMPP
001700     MOVE '-' TO WS-TS-DATE-DASH2.                                CTSTAMPP
001800     MOVE WS-ACCEPT-DATE (7:2) TO WS-TS-DATE-DD.                  CTSTAMPP
001900     MOVE '-' TO WS-TS-DATE-DASH3.                                CTSTAMPP
002000     MOVE WS-ACCEPT-TIME (1:2) TO WS-TS-TIME-HH.                  CTSTAMPP
002100     MOVE '.' TO WS-TS-TIME-DOT1.                                 CTSTAMPP
002200     MOVE WS-ACCEPT-TIME (3:2) TO WS-TS-TIME-MM.                  CTSTAMPP
002300     MOVE '.' TO WS-TS-TIME-DOT2.                                 CTSTAMPP
002400     MOVE WS-ACCEPT-TIME (5:2) TO WS-TS-TIME-SS.                  CTSTAMPP
002500     MOVE '.' TO WS-TS-TIME-DOT3.                                 CTSTAMPP
002600     MOVE WS-ACCEPT-TIME (7:2) TO WS-TS-TIME-DDDDDD (1:2).        CTSTAMPP
002700     MOVE '0000' TO WS-TS-TIME-DDDDDD (3:4).                      CTSTAMPP
002800                                                                  CTSTAMPP
