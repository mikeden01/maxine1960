000100***************************************************************** CBANKXT2
000200*                                                               * CBANKXT2
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKXT2
000400*                                                               * CBANKXT2
000500***************************************************************** CBANKXT2
000600                                                                  CBANKXT2
000700***************************************************************** CBANKXT2
000800* CBANKXT2.CPY                                                  * CBANKXT2
000900*---------------------------------------------------------------* CBANKXT2
001000* This is the record file record layout used to extract data    * CBANKXT2
001100* from the bank files to produce mailing addres labels.         * CBANKXT2
001200***************************************************************** CBANKXT2
001300   05  XTACT02-RECORD.                                            CBANKXT2
001400     10  BANKXT02-NAME                       PIC X(25).           CBANKXT2
001500     10  BANKXT02-ADDR1                      PIC X(25).           CBANKXT2
001600     10  BANKXT02-ADDR2                      PIC X(25).           CBANKXT2
001700     10  BANKXT02-ADDR3                      PIC X(25).           CBANKXT2
001800     10  BANKXT02-ZIP                        PIC X(6).            CBANKXT2
001900     10  BANKXT02-PID                        PIC X(5).            CBANKXT2
002000     10  BANKXT02-POLICY-ID                  PIC X(8).            CBANKXT2
002100                                                                  CBANKXT2
