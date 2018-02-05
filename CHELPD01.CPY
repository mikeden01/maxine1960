000100***************************************************************** CHELPD01
000200*                                                               * CHELPD01
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CHELPD01
000400*                                                               * CHELPD01
000500***************************************************************** CHELPD01
000600                                                                  CHELPD01
000700***************************************************************** CHELPD01
000800* CHELPD01.CPY                                                  * CHELPD01
000900*---------------------------------------------------------------* CHELPD01
001000* This area is used to pass data between a requesting program   * CHELPD01
001100* and the I/O program (DHELP01P) which retrieves screen help    * CHELPD01
001200* information.                                                  * CHELPD01
001300***************************************************************** CHELPD01
001400   05  HELP01-DATA.                                               CHELPD01
001500     10  HELPO1I-DATA.                                            CHELPD01
001600       15  HELP01I-SCRN                      PIC X(6).            CHELPD01
001700     10  HELP01O-DATA.                                            CHELPD01
001800       15  HELP01O-SCRN                      PIC X(6).            CHELPD01
001900       15  HELP01O-FOUND                     PIC X(1).            CHELPD01
002000         88 HELP-FOUND                       VALUE 'Y'.           CHELPD01
002100         88 HELP-NOT-FOUND                   VALUE 'N'.           CHELPD01
002200       15  HELP01O-INDIVIDUAL-LINES.                              CHELPD01
002300         20  HELP01O-L01                     PIC X(75).           CHELPD01
002400         20  HELP01O-L02                     PIC X(75).           CHELPD01
002500         20  HELP01O-L03                     PIC X(75).           CHELPD01
002600         20  HELP01O-L04                     PIC X(75).           CHELPD01
002700         20  HELP01O-L05                     PIC X(75).           CHELPD01
002800         20  HELP01O-L06                     PIC X(75).           CHELPD01
002900         20  HELP01O-L07                     PIC X(75).           CHELPD01
003000         20  HELP01O-L08                     PIC X(75).           CHELPD01
003100         20  HELP01O-L09                     PIC X(75).           CHELPD01
003200         20  HELP01O-L10                     PIC X(75).           CHELPD01
003300         20  HELP01O-L11                     PIC X(75).           CHELPD01
003400         20  HELP01O-L12                     PIC X(75).           CHELPD01
003500         20  HELP01O-L13                     PIC X(75).           CHELPD01
003600         20  HELP01O-L14                     PIC X(75).           CHELPD01
003700         20  HELP01O-L15                     PIC X(75).           CHELPD01
003800         20  HELP01O-L16                     PIC X(75).           CHELPD01
003900         20  HELP01O-L17                     PIC X(75).           CHELPD01
004000         20  HELP01O-L18                     PIC X(75).           CHELPD01
004100         20  HELP01O-L19                     PIC X(75).           CHELPD01
004200       15  FILLER REDEFINES HELP01O-INDIVIDUAL-LINES.             CHELPD01
004300         20  HELP01O-LINE                    PIC X(75)            CHELPD01
004400             OCCURS 19 TIMES.                                     CHELPD01
004500                                                                  CHELPD01
