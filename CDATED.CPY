000100***************************************************************** CDATED
000200*                                                               * CDATED
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CDATED
000400*                                                               * CDATED
000500***************************************************************** CDATED
000600                                                                  CDATED
000700***************************************************************** CDATED
000800* CDATED.CPY                                                    * CDATED
000900*---------------------------------------------------------------* CDATED
001000* Area used to pass data to/from date conversion routine        * CDATED
001100***************************************************************** CDATED
001200   05  DD-AREAS.                                                  CDATED
001300     10  DD-ENV                              PIC X(4).            CDATED
001400       88  DD-ENV-NULL                       VALUE LOW-VALUES.    CDATED
001500       88  DD-ENV-CICS                       VALUE 'CICS'.        CDATED
001600       88  DD-ENV-IMS                        VALUE 'IMS'.         CDATED
001700       88  DD-ENV-INET                       VALUE 'INET'.        CDATED
001800     10  DD-TIME-AREAS.                                           CDATED
001900       15  DD-TIME-INPUT                     PIC X(7).            CDATED
002000       15  DD-TIME-INPUT-N REDEFINES DD-TIME-INPUT                CDATED
002100                                             PIC 9(7).            CDATED
002200       15  DD-TIME-OUTPUT.                                        CDATED
002300         20  DD-TIME-OUTPUT-HH               PIC X(2).            CDATED
002400         20  DD-TIME-OUTPUT-SEP1             PIC X(1).            CDATED
002500         20  DD-TIME-OUTPUT-MM               PIC X(2).            CDATED
002600         20  DD-TIME-OUTPUT-SEP2             PIC X(1).            CDATED
002700         20  DD-TIME-OUTPUT-SS               PIC X(2).            CDATED
002800     10  DD-DATE-AREAS.                                           CDATED
002900       15  DD-INPUT.                                              CDATED
003000         20  DDI-TYPE                        PIC X(1).            CDATED
003100           88  DDI-ISO                       VALUE '0'.           CDATED
003200           88  DDI-YYYYMMDD                  VALUE '1'.           CDATED
003300           88  DDI-YYMMDD                    VALUE '2'.           CDATED
003400           88  DDI-YYDDD                     VALUE '3'.           CDATED
003500         20  DDI-DATA                        PIC X(20).           CDATED
003600         20  DDI-DATA-ISO REDEFINES DDI-DATA.                     CDATED
003700           25  DDI-DATA-ISO-YYYY             PIC X(4).            CDATED
003800           25  DDI-DATA-ISO-YYYY-N REDEFINES                      CDATED
003900               DDI-DATA-ISO-YYYY             PIC 9(4).            CDATED
004000           25  DDI-DATA-ISO-DASH1            PIC X(1).            CDATED
004100           25  DDI-DATA-ISO-MM               PIC X(2).            CDATED
004200           25  DDI-DATA-ISO-MM-N REDEFINES                        CDATED
004300               DDI-DATA-ISO-MM               PIC 9(2).            CDATED
004400           25  DDI-DATA-ISO-DASH2            PIC X(1).            CDATED
004500           25  DDI-DATA-ISO-DD               PIC X(2).            CDATED
004600           25  DDI-DATA-ISO-DD-N REDEFINES                        CDATED
004700               DDI-DATA-ISO-DD               PIC 9(2).            CDATED
004800         20  DDI-DATA-YYYYMMDD REDEFINES DDI-DATA.                CDATED
004900           25  DDI-DATA-YYYYMMDD-YYYY        PIC X(4).            CDATED
005000           25  DDI-DATA-YYYYMMDD-YYYY-N REDEFINES                 CDATED
005100               DDI-DATA-YYYYMMDD-YYYY        PIC 9(4).            CDATED
005200           25  DDI-DATA-YYYYMMDD-MM          PIC X(2).            CDATED
005300           25  DDI-DATA-YYYYMMDD-MM-N REDEFINES                   CDATED
005400               DDI-DATA-YYYYMMDD-MM          PIC 9(2).            CDATED
005500           25  DDI-DATA-YYYYMMDD-DD          PIC X(2).            CDATED
005600           25  DDI-DATA-YYYYMMDD-DD-N REDEFINES                   CDATED
005700               DDI-DATA-YYYYMMDD-DD          PIC 9(2).            CDATED
005800         20  DDI-DATA-YYMMDD REDEFINES DDI-DATA.                  CDATED
005900           25  DDI-DATA-YYMMDD-YY            PIC X(2).            CDATED
006000           25  DDI-DATA-YYMMDD-YY-N REDEFINES                     CDATED
006100               DDI-DATA-YYMMDD-YY            PIC 9(2).            CDATED
006200           25  DDI-DATA-YYMMDD-MM            PIC X(2).            CDATED
006300           25  DDI-DATA-YYMMDD-MM-N REDEFINES                     CDATED
006400               DDI-DATA-YYMMDD-MM            PIC 9(2).            CDATED
006500           25  DDI-DATA-YYMMDD-DD            PIC X(2).            CDATED
006600           25  DDI-DATA-YYMMDD-DD-N REDEFINES                     CDATED
006700               DDI-DATA-YYMMDD-DD            PIC 9(2).            CDATED
006800         20  DDI-DATA-YYDDD REDEFINES DDI-DATA.                   CDATED
006900           25  DDI-DATA-YYDDD-YYDDD          PIC X(5).            CDATED
007000           25  DDI-DATA-YYDDD-YYDDD-N REDEFINES                   CDATED
007100               DDI-DATA-YYDDD-YYDDD          PIC 9(5).            CDATED
007200           25  DDI-DATA-YYDDD-YYDDD-SPLIT REDEFINES               CDATED
007300               DDI-DATA-YYDDD-YYDDD.                              CDATED
007400             30  DDI-DATA-YYDDD-YY           PIC X(2).            CDATED
007500             30  DDI-DATA-YYDDD-YY-N REDEFINES                    CDATED
007600                 DDI-DATA-YYDDD-YY           PIC 9(2).            CDATED
007700             30  DDI-DATA-YYDDD-DDD          PIC X(3).            CDATED
007800             30  DDI-DATA-YYDDD-DDD-N REDEFINES                   CDATED
007900                 DDI-DATA-YYDDD-DDD          PIC 9(3).            CDATED
008000                                                                  CDATED
008100       15  DD-OUTPUT.                                             CDATED
008200         20  DDO-TYPE                        PIC X(1).            CDATED
008300           88  DDO-DD-MMM-YY                 VALUE '1'.           CDATED
008400           88  DDO-DD-MMM-YYYY               VALUE '2'.           CDATED
008500         20  DDO-DATA                        PIC X(20).           CDATED
008600         20  DDO-DATA-DD-MMM-YY REDEFINES DDO-DATA.               CDATED
008700           25  DDO-DATA-DD-MMM-YY-DD         PIC X(2).            CDATED
008800           25  DDO-DATA-DD-MMM-YY-DOT1       PIC X(1).            CDATED
008900           25  DDO-DATA-DD-MMM-YY-MMM        PIC X(3).            CDATED
009000           25  DDO-DATA-DD-MMM-YY-DOT2       PIC X(1).            CDATED
009100           25  DDO-DATA-DD-MMM-YY-YY         PIC X(2).            CDATED
009200         20  DDO-DATA-DD-MMM-YYYY REDEFINES DDO-DATA.             CDATED
009300           25  DDO-DATA-DD-MMM-YYYY-DD       PIC X(2).            CDATED
009400           25  DDO-DATA-DD-MMM-YYYY-DOT1     PIC X(1).            CDATED
009500           25  DDO-DATA-DD-MMM-YYYY-MMM      PIC X(3).            CDATED
009600           25  DDO-DATA-DD-MMM-YYYY-DOT2     PIC X(1).            CDATED
009700           25  DDO-DATA-DD-MMM-YYYY-YYYY     PIC X(4).            CDATED
009800                                                                  CDATED
