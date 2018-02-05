000100***************************************************************** CBANKD52
000200*                                                               * CBANKD52
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD52
000400*                                                               * CBANKD52
000500***************************************************************** CBANKD52
000600                                                                  CBANKD52
000700****************************************************************  CBANKD52
000800* CD52DATA.CPY                                                 *  CBANKD52
000900*--------------------------------------------------------------*  CBANKD52
001000* This area is used to pass data between ????????????????????  *  CBANKD52
001100* display program and the I/O program (DBANK52P) which         *  CBANKD52
001200* retrieves the data requested ????????????????????????????    *  CBANKD52
001300****************************************************************  CBANKD52
001400   05  CD52-DATA.                                                 CBANKD52
001500     10  CD52I-DATA.                                              CBANKD52
001600       15  CD52I-PID                         PIC X(5).            CBANKD52
001700     10  CD52O-DATA.                                              CBANKD52
001800       15  CD52O-PID                         PIC X(5).            CBANKD52
001900       15  CD52O-ACC-NO                      PIC X(9).            CBANKD52
002000       15  CD52O-TIMESTAMP                   PIC X(26).           CBANKD52
002100       15  CD52O-AMOUNT                      PIC S9(7)V99 COMP-3. CBANKD52
002200       15  CD52O-DESC                        PIC X(30).           CBANKD52
002300                                                                  CBANKD52
