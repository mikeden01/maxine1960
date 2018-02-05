000100***************************************************************** CHELPVSM
000200*                                                               * CHELPVSM
000300*  Copyright(C) 1998-2004 Micro Focus. All Rights Reserved.     * CHELPVSM
000400*                                                               * CHELPVSM
000500***************************************************************** CHELPVSM
000600                                                                  CHELPVSM
000700***************************************************************** CHELPVSM
000800* CHELPSQL.CPY                                                  * CHELPVSM
000900*---------------------------------------------------------------* CHELPVSM
001000* This is the record file record layout for help records        * CHELPVSM
001100***************************************************************** CHELPVSM
001200   05  HLP-RECORD                            PIC X(83).           CHELPVSM
001300   05  FILLER REDEFINES HLP-RECORD.                               CHELPVSM
001400     10  HLP-KEY.                                                 CHELPVSM
001500       15  HLP-SCRN                          PIC X(6).            CHELPVSM
001600       15  HLP-LINE                          PIC X(2).            CHELPVSM
001700     10  HLP-TEXT                            PIC X(75).           CHELPVSM
001800                                                                  CHELPVSM
