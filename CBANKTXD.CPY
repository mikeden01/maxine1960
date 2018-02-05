000100***************************************************************** CBANKTXD
000200*                                                               * CBANKTXD
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKTXD
000400*                                                               * CBANKTXD
000500***************************************************************** CBANKTXD
000600                                                                  CBANKTXD
000700***************************************************************** CBANKTXD
000800* CBANKTXD.CPY                                                  * CBANKTXD
000900*---------------------------------------------------------------* CBANKTXD
001000* This area is used to pass data between a requesting program   * CBANKTXD
001100* and the I/O program (DBANK01P) which retrieves the customer   * CBANKTXD
001200* information.                                                  * CBANKTXD
001300***************************************************************** CBANKTXD
001400 01  TXN-DATA.                                                    CBANKTXD
001500   05  TXN-TYPE                              PIC X(1).            CBANKTXD
001600     88  TXN-TRANSFER-MONEY                  VALUE '1'.           CBANKTXD
001700     88  TXN-CHANGE-CONTACT-INFO             VALUE '2'.           CBANKTXD
001800   05  TXN-SUB-TYPE                          PIC X(1).            CBANKTXD
001900     88  TXN-TRANSFER-MONEY-FROM             VALUE '1'.           CBANKTXD
002000     88  TXN-TRANSFER-MONEY-TO               VALUE '2'.           CBANKTXD
002100   05  TXN-DATA-OLD                          PIC X(150).          CBANKTXD
002200   05  TXN-T1-OLD REDEFINES TXN-DATA-OLD.                         CBANKTXD
002300     15  TXN-T1-OLD-DESC                     PIC X(30).           CBANKTXD
002400   05  TXN-T2-OLD REDEFINES TXN-DATA-OLD.                         CBANKTXD
002500     15  TXN-T2-OLD-ADDR1                    PIC X(25).           CBANKTXD
002600     15  TXN-T2-OLD-ADDR2                    PIC X(25).           CBANKTXD
002700     15  TXN-T2-OLD-STATE                    PIC X(2).            CBANKTXD
002800     15  TXN-T2-OLD-CNTRY                    PIC X(6).            CBANKTXD
002900     15  TXN-T2-OLD-PSTCDE                   PIC X(6).            CBANKTXD
003000     15  TXN-T2-OLD-TELNO                    PIC X(12).           CBANKTXD
003100     15  TXN-T2-OLD-EMAIL                    PIC X(30).           CBANKTXD
003200     15  TXN-T2-OLD-SEND-MAIL                PIC X(1).            CBANKTXD
003300     15  TXN-T2-OLD-SEND-EMAIL               PIC X(1).            CBANKTXD
003400   05  TXN-DATA-NEW                          PIC X(150).          CBANKTXD
003500   05  TXN-T2-NEW REDEFINES TXN-DATA-NEW.                         CBANKTXD
003600     15  TXN-T2-NEW-ADDR1                    PIC X(25).           CBANKTXD
003700     15  TXN-T2-NEW-ADDR2                    PIC X(25).           CBANKTXD
003800     15  TXN-T2-NEW-STATE                    PIC X(2).            CBANKTXD
003900     15  TXN-T2-NEW-CNTRY                    PIC X(6).            CBANKTXD
004000     15  TXN-T2-NEW-PSTCDE                   PIC X(6).            CBANKTXD
004100     15  TXN-T2-NEW-TELNO                    PIC X(12).           CBANKTXD
004200     15  TXN-T2-NEW-EMAIL                    PIC X(30).           CBANKTXD
004300     15  TXN-T2-NEW-SEND-MAIL                PIC X(1).            CBANKTXD
004400     15  TXN-T2-NEW-SEND-EMAIL               PIC X(1).            CBANKTXD
004500                                                                  CBANKTXD
