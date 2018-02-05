000100***************************************************************** CBANKXT1
000200*                                                               * CBANKXT1
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKXT1
000400*                                                               * CBANKXT1
000500***************************************************************** CBANKXT1
000600                                                                  CBANKXT1
000700***************************************************************** CBANKXT1
000800* CBANKXT1.CPY                                                  * CBANKXT1
000900*---------------------------------------------------------------* CBANKXT1
001000* This is the record file record layout used to extract data    * CBANKXT1
001100* from the bank file to produce statements.                     * CBANKXT1
001200***************************************************************** CBANKXT1
001300 01  BANKXT01-REC1.                                               CBANKXT1
001400   10  BANKXT01-1-TYPE                       PIC X(1).            CBANKXT1
001500   10  BANKXT01-1-PID                        PIC X(5).            CBANKXT1
001600   10  BANKXT01-1-NAME                       PIC X(25).           CBANKXT1
001700   10  BANKXT01-1-ADDR1                      PIC X(25).           CBANKXT1
001800   10  BANKXT01-1-ADDR2                      PIC X(25).           CBANKXT1
001900   10  BANKXT01-1-STATE                      PIC X(2).            CBANKXT1
002000   10  BANKXT01-1-CNTRY                      PIC X(6).            CBANKXT1
002100   10  BANKXT01-1-PST-CDE                    PIC X(6).            CBANKXT1
002200 01  BANKXT01-REC2.                                               CBANKXT1
002300   10  BANKXT01-2-TYPE                       PIC X(1).            CBANKXT1
002400   10  BANKXT01-2-PID                        PIC X(5).            CBANKXT1
002500   10  BANKXT01-2-ACC-NO                     PIC X(9).            CBANKXT1
002600   10  BANKXT01-2-ACC-DESC                   PIC X(15).           CBANKXT1
002700   10  BANKXT01-2-ACC-CURR-BAL               PIC S9(7)V99 COMP-3. CBANKXT1
002800   10  BANKXT01-2-ACC-LAST-STMT-DTE          PIC X(26).           CBANKXT1
002900   10  BANKXT01-2-ACC-LAST-STMT-BAL          PIC S9(7)V99 COMP-3. CBANKXT1
003000 01  BANKXT01-REC3.                                               CBANKXT1
003100   10  BANKXT01-3-TYPE                       PIC X(1).            CBANKXT1
003200   10  BANKXT01-3-PID                        PIC X(5).            CBANKXT1
003300   10  BANKXT01-3-ACC-NO                     PIC X(9).            CBANKXT1
003400   10  BANKXT01-3-TIMESTAMP                  PIC X(26).           CBANKXT1
003500   10  BANKXT01-3-AMOUNT                     PIC S9(7)V99 COMP-3. CBANKXT1
003600   10  BANKXT01-3-DESC                       PIC X(30).           CBANKXT1
003700                                                                  CBANKXT1
