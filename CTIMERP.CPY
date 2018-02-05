000100***************************************************************** CTIMERP
000200*                                                               * CTIMERP
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CTIMERP
000400*                                                               * CTIMERP
000500***************************************************************** CTIMERP
000600                                                                  CTIMERP
000700***************************************************************** CTIMERP
000800* CTIMERP.CPY                                                   * CTIMERP
000900*---------------------------------------------------------------* CTIMERP
001000* Procedure code to calculate & display run time                * CTIMERP
001100***************************************************************** CTIMERP
001200***************************************************************** CTIMERP
001300* Establish program run time                                    * CTIMERP
001400***************************************************************** CTIMERP
001500 RUN-TIME.                                                        CTIMERP
001600     IF TIMER-START IS EQUAL TO ZERO                              CTIMERP
001700        ACCEPT TIMER-START FROM TIME                              CTIMERP
001800        MOVE 'Timer started' TO WS-CONSOLE-MESSAGE                CTIMERP
001900        PERFORM DISPLAY-CONSOLE-MESSAGE                           CTIMERP
002000     ELSE                                                         CTIMERP
002100        ACCEPT TIMER-END FROM TIME                                CTIMERP
002200        MOVE 'Timer stopped' TO WS-CONSOLE-MESSAGE                CTIMERP
002300        PERFORM DISPLAY-CONSOLE-MESSAGE                           CTIMERP
002400        COMPUTE TIMER-ELAPSED =                                   CTIMERP
002500                  ((TIMER-END-HH * 60 * 60 * 100) +               CTIMERP
002600                   (TIMER-END-MM * 60 * 100) +                    CTIMERP
002700                   (TIMER-END-SS * 100) +                         CTIMERP
002800                    TIMER-END-DD) -                               CTIMERP
002900                  ((TIMER-START-HH * 60 * 60 * 100) +             CTIMERP
003000                   (TIMER-START-MM * 60 * 100) +                  CTIMERP
003100                   (TIMER-START-SS * 100) +                       CTIMERP
003200                    TIMER-START-DD)                               CTIMERP
003300        MOVE TIMER-ELAPSED-R TO TIMER-RUN-TIME-ELAPSED            CTIMERP
003400        MOVE TIMER-RUN-TIME TO WS-CONSOLE-MESSAGE                 CTIMERP
003500        PERFORM DISPLAY-CONSOLE-MESSAGE                           CTIMERP
003600     END-IF.                                                      CTIMERP
003700                                                                  CTIMERP
