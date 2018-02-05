000100***************************************************************** CTIMERD
000200*                                                               * CTIMERD
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CTIMERD
000400*                                                               * CTIMERD
000500***************************************************************** CTIMERD
000600                                                                  CTIMERD
000700***************************************************************** CTIMERD
000800* CTIMERD.CPY                                                   * CTIMERD
000900*---------------------------------------------------------------* CTIMERD
001000* Work areas for run timer                                      * CTIMERD
001100***************************************************************** CTIMERD
001200 01  TIMER-DATA.                                                  CTIMERD
001300   05  TIMER-START                           PIC 9(8)             CTIMERD
001400       VALUE ZERO.                                                CTIMERD
001500   05  FILLER REDEFINES TIMER-START.                              CTIMERD
001600     10  TIMER-START-HH                      PIC 9(2).            CTIMERD
001700     10  TIMER-START-MM                      PIC 9(2).            CTIMERD
001800     10  TIMER-START-SS                      PIC 9(2).            CTIMERD
001900     10  TIMER-START-DD                      PIC 9(2).            CTIMERD
002000   05  TIMER-END                             PIC 9(8)             CTIMERD
002100       VALUE ZERO.                                                CTIMERD
002200   05  FILLER REDEFINES TIMER-END.                                CTIMERD
002300     10  TIMER-END-HH                        PIC 9(2).            CTIMERD
002400     10  TIMER-END-MM                        PIC 9(2).            CTIMERD
002500     10  TIMER-END-SS                        PIC 9(2).            CTIMERD
002600     10  TIMER-END-DD                        PIC 9(2).            CTIMERD
002700   05  TIMER-ELAPSED                         PIC 9(8).            CTIMERD
002800   05  TIMER-ELAPSED-R REDEFINES TIMER-ELAPSED                    CTIMERD
002900                                             PIC 9(6)V9(2).       CTIMERD
003000   05  TIMER-RUN-TIME.                                            CTIMERD
003100     10  FILLER                              PIC X(17)            CTIMERD
003200         VALUE 'Elaped run time: '.                               CTIMERD
003300     10  TIMER-RUN-TIME-ELAPSED              PIC Z(5)9.99.        CTIMERD
003400     10  FILLER                              PIC X(8)             CTIMERD
003500         VALUE ' seconds'.                                        CTIMERD
003600                                                                  CTIMERD
