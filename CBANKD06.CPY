000100***************************************************************** CBANKD06
000200*                                                               * CBANKD06
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD06
000400*                                                               * CBANKD06
000500***************************************************************** CBANKD06
000600                                                                  CBANKD06
000700***************************************************************** CBANKD06
000800* CBANKD06.CPY                                                  * CBANKD06
000900*---------------------------------------------------------------* CBANKD06
001000* This area is used to pass data between a requesting program   * CBANKD06
001100* and the I/O program (DBANK06P) which inserts transaction      * CBANKD06
001200* records to provide an audit trail.                            * CBANKD06
001300***************************************************************** CBANKD06
001400   05  CD06-DATA.                                                 CBANKD06
001500     10  CD06I-DATA.                                              CBANKD06
001600       15  CD06I-PERSON-PID                  PIC X(5).            CBANKD06
001700       15  CD06I-TIMESTAMP                   PIC X(26).           CBANKD06
001800       15  CD06I-FROM-ACC                    PIC X(9).            CBANKD06
001900       15  CD06I-FROM-AMOUNT                 PIC S9(7)V99 COMP-3. CBANKD06
002000       15  CD06I-FROM-DESC                   PIC X(30).           CBANKD06
002100       15  CD06I-TO-ACC                      PIC X(9).            CBANKD06
002200       15  CD06I-TO-AMOUNT                   PIC S9(7)V99 COMP-3. CBANKD06
002300       15  CD06I-TO-DESC                     PIC X(30).           CBANKD06
002400     10  CD06O-DATA.                                              CBANKD06
002500       15  CD06O-RESULT                      PIC X(1).            CBANKD06
002600         88  CD06O-UPDATE-OK                 VALUE '0'.           CBANKD06
002700         88  CD06O-UPDATE-FAIL               VALUE '1'.           CBANKD06
002800       15  CD06O-MSG                         PIC X(62).           CBANKD06
002900                                                                  CBANKD06
003000                                                                  CBANKD06
