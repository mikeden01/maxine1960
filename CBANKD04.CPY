000100***************************************************************** CBANKD04
000200*                                                               * CBANKD04
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD04
000400*                                                               * CBANKD04
000500***************************************************************** CBANKD04
000600                                                                  CBANKD04
000700***************************************************************** CBANKD04
000800* CBANKD04.CPY                                                  * CBANKD04
000900*---------------------------------------------------------------* CBANKD04
001000* This area is used to pass data between a requesting program   * CBANKD04
001100* and the I/O program (DBANK04P) which updates account          * CBANKD04
001200* information.                                                  * CBANKD04
001300***************************************************************** CBANKD04
001400   05  CD04-DATA.                                                 CBANKD04
001500     10  CD04I-DATA.                                              CBANKD04
001600       15  CD04I-PERSON-PID                  PIC X(5).            CBANKD04
001700       15  CD04I-FROM-ACC                    PIC X(9).            CBANKD04
001800       15  CD04I-FROM-OLD-BAL                PIC S9(7)V99 COMP-3. CBANKD04
001900       15  CD04I-FROM-NEW-BAL                PIC S9(7)V99 COMP-3. CBANKD04
002000       15  CD04I-TO-ACC                      PIC X(9).            CBANKD04
002100       15  CD04I-TO-OLD-BAL                  PIC S9(7)V99 COMP-3. CBANKD04
002200       15  CD04I-TO-NEW-BAL                  PIC S9(7)V99 COMP-3. CBANKD04
002300     10  CD04O-DATA.                                              CBANKD04
002400       15  CD04O-RESULT                      PIC X(1).            CBANKD04
002500         88  CD04O-UPDATE-OK                 VALUE '0'.           CBANKD04
002600         88  CD04O-UPDATE-FAIL               VALUE '1'.           CBANKD04
002700       15  CD04O-TIMESTAMP                   PIC X(26).           CBANKD04
002800       15  CD04O-TIMESTAMP-R REDEFINES CD04O-TIMESTAMP.           CBANKD04
002900         20  CD04O-DATE                      PIC X(10).           CBANKD04
003000         20  CD040-FIL1                      PIC X(1).            CBANKD04
003100         20  CD04O-TIME                      PIC X(8).            CBANKD04
003200         20  CD040-FIL2                      PIC X(1).            CBANKD04
003300         20  CD040-MSEC                      PIC X(6).            CBANKD04
003400       15  CD04O-MSG                         PIC X(62).           CBANKD04
003500                                                                  CBANKD04
