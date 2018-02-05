000100***************************************************************** CBANKD03
000200*                                                               * CBANKD03
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD03
000400*                                                               * CBANKD03
000500***************************************************************** CBANKD03
000600                                                                  CBANKD03
000700***************************************************************** CBANKD03
000800* CBANKD03.CPY                                                  * CBANKD03
000900*---------------------------------------------------------------* CBANKD03
001000* This area is used to pass data between a requesting program   * CBANKD03
001100* and the I/O program (DBANK03P) which retrieves information    * CBANKD03
001200* regarding customer's accounts                                 * CBANKD03
001300***************************************************************** CBANKD03
001400   05  CD03-DATA.                                                 CBANKD03
001500     10  CD03I-DATA.                                              CBANKD03
001600       15  CD03I-CONTACT-ID                  PIC X(5).            CBANKD03
001700     10  CD03O-DATA.                                              CBANKD03
001800       15  CD03O-ACC1                        PIC X(9).            CBANKD03
001900       15  CD03O-DSC1                        PIC X(15).           CBANKD03
002000       15  CD03O-BAL1                        PIC X(9).            CBANKD03
002100       15  CD03O-BAL1N REDEFINES CD03O-BAL1  PIC S9(7)V99.        CBANKD03
002200       15  CD03O-DTE1                        PIC X(10).           CBANKD03
002300       15  CD03O-TXN1                        PIC X(1).            CBANKD03
002400       15  CD03O-ACC2                        PIC X(9).            CBANKD03
002500       15  CD03O-DSC2                        PIC X(15).           CBANKD03
002600       15  CD03O-BAL2                        PIC X(9).            CBANKD03
002700       15  CD03O-BAL2N REDEFINES CD03O-BAL2  PIC S9(7)V99.        CBANKD03
002800       15  CD03O-DTE2                        PIC X(10).           CBANKD03
002900       15  CD03O-TXN2                        PIC X(1).            CBANKD03
003000       15  CD03O-ACC3                        PIC X(9).            CBANKD03
003100       15  CD03O-DSC3                        PIC X(15).           CBANKD03
003200       15  CD03O-BAL3                        PIC X(9).            CBANKD03
003300       15  CD03O-BAL3N REDEFINES CD03O-BAL3  PIC S9(7)V99.        CBANKD03
003400       15  CD03O-DTE3                        PIC X(10).           CBANKD03
003500       15  CD03O-TXN3                        PIC X(1).            CBANKD03
003600       15  CD03O-ACC4                        PIC X(9).            CBANKD03
003700       15  CD03O-DSC4                        PIC X(15).           CBANKD03
003800       15  CD03O-BAL4                        PIC X(9).            CBANKD03
003900       15  CD03O-BAL4N REDEFINES CD03O-BAL4  PIC S9(7)V99.        CBANKD03
004000       15  CD03O-DTE4                        PIC X(10).           CBANKD03
004100       15  CD03O-TXN4                        PIC X(1).            CBANKD03
004200       15  CD03O-ACC5                        PIC X(9).            CBANKD03
004300       15  CD03O-DSC5                        PIC X(15).           CBANKD03
004400       15  CD03O-BAL5                        PIC X(9).            CBANKD03
004500       15  CD03O-BAL5N REDEFINES CD03O-BAL5  PIC S9(7)V99.        CBANKD03
004600       15  CD03O-DTE5                        PIC X(10).           CBANKD03
004700       15  CD03O-TXN5                        PIC X(1).            CBANKD03
004800     10  CD03O-DATA-R REDEFINES CD03O-DATA.                       CBANKD03
004900       15  CD03O-ACC-INFO                    OCCURS 5 TIMES.      CBANKD03
005000         20  CD03O-ACC-NO                    PIC X(9).            CBANKD03
005100         20  CD03O-ACC-DESC                  PIC X(15).           CBANKD03
005200         20  CD03O-ACC-BAL                   PIC X(9).            CBANKD03
005300         20  CD03O-ACC-BAL-N REDEFINES CD03O-ACC-BAL              CBANKD03
005400                                             PIC S9(7)V99.        CBANKD03
005500         20  CD03O-DTE                       PIC X(10).           CBANKD03
005600         20  CD03O-TXN                       PIC X(1).            CBANKD03
005700                                                                  CBANKD03
