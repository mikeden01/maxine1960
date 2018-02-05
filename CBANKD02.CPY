000100***************************************************************** CBANKD02
000200*                                                               * CBANKD02
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD02
000400*                                                               * CBANKD02
000500***************************************************************** CBANKD02
000600                                                                  CBANKD02
000700***************************************************************** CBANKD02
000800* CBANKD02.CPY                                                  * CBANKD02
000900*---------------------------------------------------------------* CBANKD02
001000* This area is used to pass data between a requesting program   * CBANKD02
001100* and the I/O program (DBANK02P) which retrieves or updates     * CBANKD02
001200* address information.                                          * CBANKD02
001300***************************************************************** CBANKD02
001400   05  CD02-DATA.                                                 CBANKD02
001500     10  CD02I-DATA.                                              CBANKD02
001600       15  CD02I-FUNCTION                    PIC X(1).            CBANKD02
001700         88  CD02I-READ                      VALUE 'R'.           CBANKD02
001800         88  CD02I-WRITE                     VALUE 'W'.           CBANKD02
001900       15  CD02I-CONTACT-ID                  PIC X(5).            CBANKD02
002000       15  CD02I-CONTACT-NAME                PIC X(25).           CBANKD02
002100       15  CD02I-CONTACT-ADDR1               PIC X(25).           CBANKD02
002200       15  CD02I-CONTACT-ADDR2               PIC X(25).           CBANKD02
002300       15  CD02I-CONTACT-STATE               PIC X(2).            CBANKD02
002400       15  CD02I-CONTACT-CNTRY               PIC X(6).            CBANKD02
002500       15  CD02I-CONTACT-PSTCDE              PIC X(6).            CBANKD02
002600       15  CD02I-CONTACT-TELNO               PIC X(12).           CBANKD02
002700       15  CD02I-CONTACT-EMAIL               PIC X(30).           CBANKD02
002800       15  CD02I-CONTACT-SEND-MAIL           PIC X(1).            CBANKD02
002900       15  CD02I-CONTACT-SEND-EMAIL          PIC X(1).            CBANKD02
003000     10  CD02O-DATA.                                              CBANKD02
003100       15  CD02O-CONTACT-ID                  PIC X(5).            CBANKD02
003200       15  CD02O-CONTACT-NAME                PIC X(25).           CBANKD02
003300       15  CD02O-CONTACT-ADDR1               PIC X(25).           CBANKD02
003400       15  CD02O-CONTACT-ADDR2               PIC X(25).           CBANKD02
003500       15  CD02O-CONTACT-STATE               PIC X(2).            CBANKD02
003600       15  CD02O-CONTACT-CNTRY               PIC X(6).            CBANKD02
003700       15  CD02O-CONTACT-PSTCDE              PIC X(6).            CBANKD02
003800       15  CD02O-CONTACT-TELNO               PIC X(12).           CBANKD02
003900       15  CD02O-CONTACT-EMAIL               PIC X(30).           CBANKD02
004000       15  CD02O-CONTACT-SEND-MAIL           PIC X(1).            CBANKD02
004100       15  CD02O-CONTACT-SEND-EMAIL          PIC X(1).            CBANKD02
004200                                                                  CBANKD02
