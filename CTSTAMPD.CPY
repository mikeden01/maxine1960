000100***************************************************************** CTSTAMPD
000200*                                                               * CTSTAMPD
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CTSTAMPD
000400*                                                               * CTSTAMPD
000500***************************************************************** CTSTAMPD
000600                                                                  CTSTAMPD
000700***************************************************************** CTSTAMPD
000800* CTSTAMPD.CPY                                                  * CTSTAMPD
000900*---------------------------------------------------------------* CTSTAMPD
001000* Work areas for timestamp creation                             * CTSTAMPD
001100***************************************************************** CTSTAMPD
001200 01  WS-TIMESTAMP-AREAS.                                          CTSTAMPD
001300   05  WS-ACCEPT-DATE                        PIC 9(8).            CTSTAMPD
001400   05  WS-ACCEPT-TIME                        PIC 9(8).            CTSTAMPD
001500   05  WS-TIMESTAMP.                                              CTSTAMPD
001600     10  WS-TS-DATE.                                              CTSTAMPD
001700       15  WS-TS-DATE-YYYY                   PIC X(4).            CTSTAMPD
001800       15  WS-TS-DATE-DASH1                  PIC X(1).            CTSTAMPD
001900       15  WS-TS-DATE-MM                     PIC X(2).            CTSTAMPD
002000       15  WS-TS-DATE-DASH2                  PIC X(1).            CTSTAMPD
002100       15  WS-TS-DATE-DD                     PIC X(2).            CTSTAMPD
002200       15  WS-TS-DATE-DASH3                  PIC X(1).            CTSTAMPD
002300     10  WS-TS-TIME.                                              CTSTAMPD
002400       15  WS-TS-TIME-HH                     PIC X(2).            CTSTAMPD
002500       15  WS-TS-TIME-DOT1                   PIC X(1).            CTSTAMPD
002600       15  WS-TS-TIME-MM                     PIC X(2).            CTSTAMPD
002700       15  WS-TS-TIME-DOT2                   PIC X(1).            CTSTAMPD
002800       15  WS-TS-TIME-SS                     PIC X(2).            CTSTAMPD
002900       15  WS-TS-TIME-DOT3                   PIC X(1).            CTSTAMPD
003000       15  WS-TS-TIME-DDDDDD                 PIC X(6).            CTSTAMPD
003100                                                                  CTSTAMPD
