000100***************************************************************** CBANKD01
000200*                                                               * CBANKD01
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD01
000400*                                                               * CBANKD01
000500***************************************************************** CBANKD01
000600                                                                  CBANKD01
000700***************************************************************** CBANKD01
000800* CBANKD01.CPY                                                  * CBANKD01
000900*---------------------------------------------------------------* CBANKD01
001000* This area is used to pass data between a requesting program   * CBANKD01
001100* and the I/O program (DBANK01P) which retrieves the customer   * CBANKD01
001200* information.                                                  * CBANKD01
001300***************************************************************** CBANKD01
001400   05  CD01-DATA.                                                 CBANKD01
001500     10  CD01I-DATA.                                              CBANKD01
001600       15  CD01I-PERSON-PID                  PIC X(5).            CBANKD01
001700     10  CD01O-DATA.                                              CBANKD01
001800       15  CD01O-PERSON-PID                  PIC X(5).            CBANKD01
001900       15  CD01O-PERSON-NAME                 PIC X(25).           CBANKD01
002000                                                                  CBANKD01
