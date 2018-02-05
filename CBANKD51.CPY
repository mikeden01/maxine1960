000100***************************************************************** CBANKD51
000200*                                                               * CBANKD51
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD51
000400*                                                               * CBANKD51
000500***************************************************************** CBANKD51
000600                                                                  CBANKD51
000700****************************************************************  CBANKD51
000800* CD51DATA.CPY                                                 *  CBANKD51
000900*--------------------------------------------------------------*  CBANKD51
001000* This area is used to pass data between ????????????????????  *  CBANKD51
001100* display program and the I/O program (DBANK51P) which         *  CBANKD51
001200* retrieves the data requested ????????????????????????????    *  CBANKD51
001300****************************************************************  CBANKD51
001400   05  CD51-DATA.                                                 CBANKD51
001500     10  CD51I-DATA.                                              CBANKD51
001600       15  CD51I-PID                         PIC X(5).            CBANKD51
001700     10  CD51O-DATA.                                              CBANKD51
001800       15  CD51O-PID                         PIC X(5).            CBANKD51
001900       15  CD51O-NAME                        PIC X(25).           CBANKD51
002000       15  CD51O-ADDR1                       PIC X(25).           CBANKD51
002100       15  CD51O-ADDR2                       PIC X(25).           CBANKD51
002200       15  CD51O-STATE                       PIC X(2).            CBANKD51
002300       15  CD51O-CNTRY                       PIC X(6).            CBANKD51
002400       15  CD51O-POST-CODE                   PIC X(6).            CBANKD51
002500       15  CD51O-ACC-NO                      PIC X(9).            CBANKD51
002600       15  CD51O-ACC-DESC                    PIC X(15).           CBANKD51
002700       15  CD51O-ACC-CURR-BAL                PIC S9(7)V99 COMP-3. CBANKD51
002800       15  CD51O-ACC-LAST-STMT-DATE          PIC X(10).           CBANKD51
002900       15  CD51O-ACC-LAST-STMT-BAL           PIC S9(7)V99 COMP-3. CBANKD51
003000                                                                  CBANKD51
