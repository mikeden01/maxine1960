000100***************************************************************** CBANKD08
000200*                                                               * CBANKD08
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD08
000400*                                                               * CBANKD08
000500***************************************************************** CBANKD08
000600                                                                  CBANKD08
000700***************************************************************** CBANKD08
000800* CBANKD08.CPY                                                  * CBANKD08
000900*---------------------------------------------------------------* CBANKD08
001000* This area is used to pass data between a requesting program   * CBANKD08
001100* and the I/O program (DBANK03P) which retrieves information    * CBANKD08
001200* regarding customer's accounts                                 * CBANKD08
001300***************************************************************** CBANKD08
001400   05  CD08-DATA.                                                 CBANKD08
001500     10  CD08I-DATA.                                              CBANKD08
001600       15  CD08I-CONTACT-ID                  PIC X(5).            CBANKD08
001700     10  CD08O-DATA.                                              CBANKD08
001800       15  CD08O-COUNT                       PIC 9(3).            CBANKD08
001900                                                                  CBANKD08
