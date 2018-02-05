000100***************************************************************** CBANKD07
000200*                                                               * CBANKD07
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD07
000400*                                                               * CBANKD07
000500***************************************************************** CBANKD07
000600* hjsfhjsghjgsjg                                                  CBANKD07
000700***************************************************************** CBANKD07
000800* CBANKD07.CPY                                                  * CBANKD07
000900*---------------------------------------------------------------* CBANKD07
001000* This area is used to pass data between a requesting program   * CBANKD07
001100* and the I/O program (DBANK07P) which retrieves or updates     * CBANKD07
001200* address information.                                          * CBANKD07
001300***************************************************************** CBANKD07
001400   05  CD07-DATA.                                                 CBANKD07
001500     10  CD07I-DATA.                                              CBANKD07
001600       15  CD07I-PERSON-PID                  PIC X(5).            CBANKD07
001700       15  CD07I-TIMESTAMP                   PIC X(26).           CBANKD07
001800       15  CD07I-OLD-DATA                    PIC X(150).          CBANKD07
001900       15  FILLER REDEFINES CD07I-OLD-DATA.                       CBANKD07
002000         20  CD07I-OLD-ADDR1                 PIC X(25).           CBANKD07
002100         20  CD07I-OLD-ADDR2                 PIC X(25).           CBANKD07
002200         20  CD07I-OLD-STATE                 PIC X(2).            CBANKD07
002300         20  CD07I-OLD-CNTRY                 PIC X(6).            CBANKD07
002400         20  CD07I-OLD-PSTCDE                PIC X(6).            CBANKD07
002500         20  CD07I-OLD-TELNO                 PIC X(12).           CBANKD07
002600         20  CD07I-OLD-EMAIL                 PIC X(30).           CBANKD07
002700         20  CD07I-OLD-SEND-MAIL             PIC X(1).            CBANKD07
002800         20  CD07I-OLD-SEND-EMAIL            PIC X(1).            CBANKD07
002900       15  CD07I-NEW-DATA                    PIC X(150).          CBANKD07
003000       15  FILLER REDEFINES CD07I-NEW-DATA.                       CBANKD07
003100         20  CD07I-NEW-ADDR1                 PIC X(25).           CBANKD07
003200         20  CD07I-NEW-ADDR2                 PIC X(25).           CBANKD07
003300         20  CD07I-NEW-STATE                 PIC X(2).            CBANKD07
003400         20  CD07I-NEW-CNTRY                 PIC X(6).            CBANKD07
003500         20  CD07I-NEW-PSTCDE                PIC X(6).            CBANKD07
003600         20  CD07I-NEW-TELNO                 PIC X(12).           CBANKD07
003700         20  CD07I-NEW-EMAIL                 PIC X(30).           CBANKD07
003800         20  CD07I-NEW-SEND-MAIL             PIC X(1).            CBANKD07
003900         20  CD07I-NEW-SEND-EMAIL            PIC X(1).            CBANKD07
004000     10  CD07O-DATA.                                              CBANKD07
004100       15  CD07O-RESULT                      PIC X(1).            CBANKD07
004200         88  CD07O-UPDATE-OK                 VALUE '0'.           CBANKD07
004300         88  CD07O-UPDATE-FAIL               VALUE '1'.           CBANKD07
004400       15  CD07O-MSG                         PIC X(62).           CBANKD07
004500                                                                  CBANKD07
