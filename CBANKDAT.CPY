000100***************************************************************** CBANKDAT
000200*                                                               * CBANKDAT
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKDAT
000400*                                                               * CBANKDAT
000500***************************************************************** CBANKDAT
000600*  change for cibc free banking                                   CBANKDAT
000700***************************************************************** CBANKDAT
000800* CBANKDAT.CPY                                                  * CBANKDAT
000900*---------------------------------------------------------------* CBANKDAT
001000* Common data passed between the major components               * CBANKDAT
001100***************************************************************** CBANKDAT
001200     10  BANK-EVERYTHING                     PIC X(6144).         CBANKDAT
001300     10  FILLER REDEFINES BANK-EVERYTHING.                        CBANKDAT
001400       15  BANK-PREFIX                       PIC X(22).           CBANKDAT
001500       15  BANK-IMS-PREFIX REDEFINES BANK-PREFIX.                 CBANKDAT
001600         20  BANK-IMS-SPA-LL                 PIC S9(8) COMP.      CBANKDAT
001700         20  BANK-IMS-SPA-ZZ                 PIC X(2).            CBANKDAT
001800         20  BANK-IMS-SPA-TRANCODE           PIC X(8).            CBANKDAT
001900         20  BANK-IMS-SPA-PASSED-DATA        PIC X(8).            CBANKDAT
002000         20  BANK-IMS-SPA-PASSED-DATA-R1 REDEFINES                CBANKDAT
002100             BANK-IMS-SPA-PASSED-DATA.                            CBANKDAT
002200           25  BANK-IMS-IO-PCB-DATE          PIC S9(7) COMP-3.    CBANKDAT
002300           25  BANK-IMS-IO-PCB-TIME          PIC S9(7) COMP-3.    CBANKDAT
002400         20  BANK-IMS-SPA-PASSED-DATA-R2 REDEFINES                CBANKDAT
002500             BANK-IMS-SPA-PASSED-DATA.                            CBANKDAT
002600           25  BANK-IMS-SPA-PASSED-LITERAL   PIC X(7).            CBANKDAT
002700           25  BANK-IMS-SPA-PASSED-COLOUR    PIC X(1).            CBANKDAT
002800       15  BANK-CICS-PREFIX REDEFINES BANK-PREFIX.                CBANKDAT
002900         20  BANK-CICS-LL                    PIC S9(4) COMP.      CBANKDAT
003000         20  FILLER                          PIC X(4).            CBANKDAT
003100         20  BANK-CICS-TRANCODE-PLUS4        PIC X(8).            CBANKDAT
003200         20  FILLER REDEFINES BANK-CICS-TRANCODE-PLUS4.           CBANKDAT
003300           25  BANK-CICS-TRANCODE            PIC X(4).            CBANKDAT
003400           25  FILLER                        PIC X(4).            CBANKDAT
003500       15  BANK-CONTROL-FIELDS.                                   CBANKDAT
003600         20  BANK-ENV                        PIC X(4).            CBANKDAT
003700           88  BANK-ENV-NULL                 VALUE LOW-VALUES.    CBANKDAT
003800           88  BANK-ENV-CICS                 VALUE 'CICS'.        CBANKDAT
003900           88  BANK-ENV-IMS                  VALUE 'IMS '.        CBANKDAT
004000           88  BANK-ENV-INET                 VALUE 'INET'.        CBANKDAT
004100         20  BANK-COLOUR-SETTING             PIC X(1).            CBANKDAT
004200           88  COLOUR-ON                     VALUE '1'.           CBANKDAT
004300           88  COLOUR-OFF                    VALUE '0'.           CBANKDAT
004400         20  BANK-CONVERSATION               PIC X(1).            CBANKDAT
004500           88  BANK-NO-CONV-IN-PROGRESS      VALUE '0'.           CBANKDAT
004600           88  BANK-CONV-IN-PROGRESS         VALUE '1'.           CBANKDAT
004700         20  BANK-TS-QUEUE-NAME              PIC X(8).            CBANKDAT
004800         20  BANK-AID                        PIC X(5).            CBANKDAT
004900           88  BANK-AID-ENTER                VALUE 'ENTER'.       CBANKDAT
005000           88  BANK-AID-CLEAR                VALUE 'CLEAR'.       CBANKDAT
005100           88  BANK-AID-PA1                  VALUE 'PA1  '.       CBANKDAT
005200           88  BANK-AID-PA2                  VALUE 'PA2  '.       CBANKDAT
005300           88  BANK-AID-PFK01                VALUE 'PFK01'.       CBANKDAT
005400           88  BANK-AID-PFK02                VALUE 'PFK02'.       CBANKDAT
005500           88  BANK-AID-PFK03                VALUE 'PFK03'.       CBANKDAT
005600           88  BANK-AID-PFK04                VALUE 'PFK04'.       CBANKDAT
005700           88  BANK-AID-PFK05                VALUE 'PFK05'.       CBANKDAT
005800           88  BANK-AID-PFK06                VALUE 'PFK06'.       CBANKDAT
005900           88  BANK-AID-PFK07                VALUE 'PFK07'.       CBANKDAT
006000           88  BANK-AID-PFK08                VALUE 'PFK08'.       CBANKDAT
006100           88  BANK-AID-PFK09                VALUE 'PFK09'.       CBANKDAT
006200           88  BANK-AID-PFK10                VALUE 'PFK10'.       CBANKDAT
006300           88  BANK-AID-PFK11                VALUE 'PFK11'.       CBANKDAT
006400           88  BANK-AID-PFK12                VALUE 'PFK12'.       CBANKDAT
006500         20  BANK-LAST-PROG                  PIC X(8).            CBANKDAT
006600         20  BANK-NEXT-PROG                  PIC X(8).            CBANKDAT
006700         20  BANK-RETURN-TO-PROG             PIC X(8).            CBANKDAT
006800         20  BANK-LAST-MAPSET                PIC X(7).            CBANKDAT
006900         20  BANK-LAST-MAP                   PIC X(7).            CBANKDAT
007000         20  BANK-NEXT-MAPSET                PIC X(7).            CBANKDAT
007100         20  BANK-NEXT-MAP                   PIC X(7).            CBANKDAT
007200         20  BANK-MAP-FUNCTION               PIC X(3).            CBANKDAT
007300           88  BANK-MAP-FUNCTION-GET         VALUE 'GET'.         CBANKDAT
007400           88  BANK-MAP-FUNCTION-PUT         VALUE 'PUT'.         CBANKDAT
007500         20  BANK-HELP-FIELDS.                                    CBANKDAT
007600           25  BANK-HELP-FLAG                PIC X(4).            CBANKDAT
007700             88  BANK-HELP-ACTIVE            VALUE 'HELP'.        CBANKDAT
007800             88  BANK-HELP-INACTIVE          VALUE LOW-VALUES.    CBANKDAT
007900           25  BANK-HELP-SCREEN              PIC 9(2).            CBANKDAT
008000         20  BANK-PAGING-FIELDS.                                  CBANKDAT
008100           25  BANK-PAGING-STATUS            PIC X(1).            CBANKDAT
008200             88  BANK-PAGING-OFF             VALUE LOW-VALUES.    CBANKDAT
008300             88  BANK-PAGING-FIRST           VALUE '1'.           CBANKDAT
008400             88  BANK-PAGING-MIDDLE          VALUE '2'.           CBANKDAT
008500             88  BANK-PAGING-LAST            VALUE '3'.           CBANKDAT
008600           25  BANK-PAGING-FIRST-ENTRY       PIC X(26).           CBANKDAT
008700           25  BANK-PAGING-LAST-ENTRY        PIC X(26).           CBANKDAT
008800         20  BANK-RETURN-FLAG                PIC X(1).            CBANKDAT
008900           88  BANK-RETURN-FLAG-OFF          VALUE LOW-VALUES.    CBANKDAT
009000           88  BANK-RETURN-FLAG-ON           VALUE '1'.           CBANKDAT
009100         20  BANK-RETURN-MSG                 PIC X(75).           CBANKDAT
009200           88  BANK-RETURN-MSG-OFF           VALUE LOW-VALUES.    CBANKDAT
009300       15  BANK-ERROR-MSG                    PIC X(75).           CBANKDAT
009400       15  BANK-USER-DETAILS.                                     CBANKDAT
009500         20  BANK-USERID                     PIC X(5).            CBANKDAT
009600           88  GUEST                         VALUE 'GUEST'.       CBANKDAT
009700         20  BANK-USERID-NAME                PIC X(25).           CBANKDAT
009800         20  BANK-PSWD                       PIC X(8).            CBANKDAT
009900       15  BANK-HELP-DATA.                                        CBANKDAT
010000         20  BANK-HELP-SCRN                  PIC X(6).            CBANKDAT
010100         20  BANK-HELP-STATUS                PIC X(1).            CBANKDAT
010200           88 BANK-HELP-FOUND                VALUE 'Y'.           CBANKDAT
010300           88 BANK-HELP-NOT-FOUND            VALUE 'N'.           CBANKDAT
010400         20  BANK-HELP-LINE                  PIC X(75)            CBANKDAT
010500             OCCURS 19 TIMES.                                     CBANKDAT
010600       15  BANK-SCREEN-DATA.                                      CBANKDAT
010700         20  BANK-SCREEN10-DATA.                                  CBANKDAT
010800           25  FILLER                        PIC X(1).            CBANKDAT
010900         20  BANK-SCREEN20-DATA.                                  CBANKDAT
011000           25  BANK-SCR20-SEL1ID             PIC X(1).            CBANKDAT
011100           25  BANK-SCR20-SEL1IP             PIC X(1).            CBANKDAT
011200           25  BANK-SCR20-SEL1TX             PIC X(40).           CBANKDAT
011300           25  BANK-SCR20-SEL2ID             PIC X(1).            CBANKDAT
011400           25  BANK-SCR20-SEL2IP             PIC X(1).            CBANKDAT
011500           25  BANK-SCR20-SEL2TX             PIC X(40).           CBANKDAT
011600           25  BANK-SCR20-SEL3ID             PIC X(1).            CBANKDAT
011700           25  BANK-SCR20-SEL3IP             PIC X(1).            CBANKDAT
011800           25  BANK-SCR20-SEL3TX             PIC X(40).           CBANKDAT
011900           25  BANK-SCR20-SEL4ID             PIC X(1).            CBANKDAT
012000           25  BANK-SCR20-SEL4IP             PIC X(1).            CBANKDAT
012100           25  BANK-SCR20-SEL4TX             PIC X(40).           CBANKDAT
012200           25  BANK-SCR20-SEL5ID             PIC X(1).            CBANKDAT
012300           25  BANK-SCR20-SEL5IP             PIC X(1).            CBANKDAT
012400           25  BANK-SCR20-SEL5TX             PIC X(40).           CBANKDAT
012500         20  BANK-SCREEN20-DATA-R REDEFINES BANK-SCREEN20-DATA.   CBANKDAT
012600           25  BANK-SCREEN20-FIELD           OCCURS 5 TIMES.      CBANKDAT
012700             30  BANK-SCR20-ID               PIC X(1).            CBANKDAT
012800             30  BANK-SCR20-IP               PIC X(1).            CBANKDAT
012900             30  BANK-SCR20-TX               PIC X(40).           CBANKDAT
013000         20  BANK-SCREEN30-DATA.                                  CBANKDAT
013100           25  BANK-SCR30-SEL1               PIC X(1).            CBANKDAT
013200           25  BANK-SCR30-ACC1               PIC X(9).            CBANKDAT
013300           25  BANK-SCR30-DSC1               PIC X(15).           CBANKDAT
013400           25  BANK-SCR30-BAL1               PIC X(13).           CBANKDAT
013500           25  BANK-SCR30-SRV1               PIC X(6).            CBANKDAT
013600           25  BANK-SCR30-DTE1               PIC X(11).           CBANKDAT
013700           25  BANK-SCR30-TXN1               PIC X(1).            CBANKDAT
013800           25  BANK-SCR30-SEL2               PIC X(1).            CBANKDAT
013900           25  BANK-SCR30-ACC2               PIC X(9).            CBANKDAT
014000           25  BANK-SCR30-DSC2               PIC X(15).           CBANKDAT
014100           25  BANK-SCR30-BAL2               PIC X(13).           CBANKDAT
014200           25  BANK-SCR30-SRV2               PIC X(6).            CBANKDAT
014300           25  BANK-SCR30-DTE2               PIC X(11).           CBANKDAT
014400           25  BANK-SCR30-TXN2               PIC X(1).            CBANKDAT
014500           25  BANK-SCR30-SEL3               PIC X(1).            CBANKDAT
014600           25  BANK-SCR30-ACC3               PIC X(9).            CBANKDAT
014700           25  BANK-SCR30-DSC3               PIC X(15).           CBANKDAT
014800           25  BANK-SCR30-BAL3               PIC X(13).           CBANKDAT
014900           25  BANK-SCR30-SRV3               PIC X(6).            CBANKDAT
015000           25  BANK-SCR30-DTE3               PIC X(11).           CBANKDAT
015100           25  BANK-SCR30-TXN3               PIC X(1).            CBANKDAT
015200           25  BANK-SCR30-SEL4               PIC X(1).            CBANKDAT
015300           25  BANK-SCR30-ACC4               PIC X(9).            CBANKDAT
015400           25  BANK-SCR30-DSC4               PIC X(15).           CBANKDAT
015500           25  BANK-SCR30-BAL4               PIC X(13).           CBANKDAT
015600           25  BANK-SCR30-SRV4               PIC X(6).            CBANKDAT
015700           25  BANK-SCR30-DTE4               PIC X(11).           CBANKDAT
015800           25  BANK-SCR30-TXN4               PIC X(1).            CBANKDAT
015900           25  BANK-SCR30-SEL5               PIC X(1).            CBANKDAT
016000           25  BANK-SCR30-ACC5               PIC X(9).            CBANKDAT
016100           25  BANK-SCR30-DSC5               PIC X(15).           CBANKDAT
016200           25  BANK-SCR30-BAL5               PIC X(13).           CBANKDAT
016300           25  BANK-SCR30-SRV5               PIC X(6).            CBANKDAT
016400           25  BANK-SCR30-DTE5               PIC X(11).           CBANKDAT
016500           25  BANK-SCR30-TXN5               PIC X(1).            CBANKDAT
016600           25  BANK-SCR30-SRVMSG             PIC X(62).           CBANKDAT
016700         20  BANK-SCREEN30-DATA-R REDEFINES BANK-SCREEN30-DATA.   CBANKDAT
016800           25  BANK-SCREEN30-INPUT-DATA.                          CBANKDAT
016900             30  BANK-SCREEN30-INPUT         OCCURS 5 TIMES.      CBANKDAT
017000               35  BANK-SCR30-SEL            PIC X(1).            CBANKDAT
017100               35  BANK-SCR30-ACC            PIC X(9).            CBANKDAT
017200               35  BANK-SCR30-DSC            PIC X(15).           CBANKDAT
017300               35  BANK-SCR30-BAL            PIC X(13).           CBANKDAT
017400               35  BANK-SCR30-SRV            PIC X(6).            CBANKDAT
017500               35  BANK-SCR30-DTE            PIC X(11).           CBANKDAT
017600               35  BANK-SCR30-TXN            PIC X(1).            CBANKDAT
017700                 88  BANK-SCR30-TXN-PRESENT  VALUE '*'.           CBANKDAT
017800         20  BANK-SCREEN40-DATA.                                  CBANKDAT
017900           25  BANK-SCR40-ACC                PIC X(9).            CBANKDAT
018000           25  BANK-SCR40-ACCTYPE            PIC X(15).           CBANKDAT
018100           25  BANK-SCR40-TXN-FIELDS.                             CBANKDAT
018200             30  BANK-SCR40-DAT1             PIC X(11).           CBANKDAT
018300             30  BANK-SCR40-TIM1             PIC X(8).            CBANKDAT
018400             30  BANK-SCR40-AMT1             PIC X(13).           CBANKDAT
018500             30  BANK-SCR40-DSC1             PIC X(30).           CBANKDAT
018600             30  BANK-SCR40-DAT2             PIC X(11).           CBANKDAT
018700             30  BANK-SCR40-TIM2             PIC X(8).            CBANKDAT
018800             30  BANK-SCR40-AMT2             PIC X(13).           CBANKDAT
018900             30  BANK-SCR40-DSC2             PIC X(30).           CBANKDAT
019000             30  BANK-SCR40-DAT3             PIC X(11).           CBANKDAT
019100             30  BANK-SCR40-TIM3             PIC X(8).            CBANKDAT
019200             30  BANK-SCR40-AMT3             PIC X(13).           CBANKDAT
019300             30  BANK-SCR40-DSC3             PIC X(30).           CBANKDAT
019400             30  BANK-SCR40-DAT4             PIC X(11).           CBANKDAT
019500             30  BANK-SCR40-TIM4             PIC X(8).            CBANKDAT
019600             30  BANK-SCR40-AMT4             PIC X(13).           CBANKDAT
019700             30  BANK-SCR40-DSC4             PIC X(30).           CBANKDAT
019800             30  BANK-SCR40-DAT5             PIC X(11).           CBANKDAT
019900             30  BANK-SCR40-TIM5             PIC X(8).            CBANKDAT
020000             30  BANK-SCR40-AMT5             PIC X(13).           CBANKDAT
020100             30  BANK-SCR40-DSC5             PIC X(30).           CBANKDAT
020200             30  BANK-SCR40-DAT6             PIC X(11).           CBANKDAT
020300             30  BANK-SCR40-TIM6             PIC X(8).            CBANKDAT
020400             30  BANK-SCR40-AMT6             PIC X(13).           CBANKDAT
020500             30  BANK-SCR40-DSC6             PIC X(30).           CBANKDAT
020600             30  BANK-SCR40-DAT7             PIC X(11).           CBANKDAT
020700             30  BANK-SCR40-TIM7             PIC X(8).            CBANKDAT
020800             30  BANK-SCR40-AMT7             PIC X(13).           CBANKDAT
020900             30  BANK-SCR40-DSC7             PIC X(30).           CBANKDAT
021000             30  BANK-SCR40-DAT8             PIC X(11).           CBANKDAT
021100             30  BANK-SCR40-TIM8             PIC X(8).            CBANKDAT
021200             30  BANK-SCR40-AMT8             PIC X(13).           CBANKDAT
021300             30  BANK-SCR40-DSC8             PIC X(30).           CBANKDAT
021400         20  BANK-SCREEN40-DATA-R REDEFINES BANK-SCREEN40-DATA.   CBANKDAT
021500           25  BANK-SCR40-ACC-R              PIC X(9).            CBANKDAT
021600           25  BANK-SCR40-ACCTYPE-R          PIC X(15).           CBANKDAT
021700           25  BANK-SCREEN40-TXNS            OCCURS 8 TIMES.      CBANKDAT
021800             30  BANK-SCR40-DATE             PIC X(11).           CBANKDAT
021900             30  BANK-SCR40-TIME             PIC X(8).            CBANKDAT
022000             30  BANK-SCR40-AMNT             PIC X(13).           CBANKDAT
022100             30  BANK-SCR40-DESC             PIC X(30).           CBANKDAT
022200         20  BANK-SCREEN50-DATA.                                  CBANKDAT
022300           25  BANK-SCR50-XFER               PIC X(8).            CBANKDAT
022400           25  BANK-SCR50-FRM1               PIC X(1).            CBANKDAT
022500           25  BANK-SCR50-TO1                PIC X(1).            CBANKDAT
022600           25  BANK-SCR50-ACC1               PIC X(9).            CBANKDAT
022700           25  BANK-SCR50-DSC1               PIC X(15).           CBANKDAT
022800           25  BANK-SCR50-BAL1               PIC X(13).           CBANKDAT
022900           25  BANK-SCR50-FRM2               PIC X(1).            CBANKDAT
023000           25  BANK-SCR50-TO2                PIC X(1).            CBANKDAT
023100           25  BANK-SCR50-ACC2               PIC X(9).            CBANKDAT
023200           25  BANK-SCR50-DSC2               PIC X(15).           CBANKDAT
023300           25  BANK-SCR50-BAL2               PIC X(13).           CBANKDAT
023400           25  BANK-SCR50-FRM3               PIC X(1).            CBANKDAT
023500           25  BANK-SCR50-TO3                PIC X(1).            CBANKDAT
023600           25  BANK-SCR50-ACC3               PIC X(9).            CBANKDAT
023700           25  BANK-SCR50-DSC3               PIC X(15).           CBANKDAT
023800           25  BANK-SCR50-BAL3               PIC X(13).           CBANKDAT
023900           25  BANK-SCR50-FRM4               PIC X(1).            CBANKDAT
024000           25  BANK-SCR50-TO4                PIC X(1).            CBANKDAT
024100           25  BANK-SCR50-ACC4               PIC X(9).            CBANKDAT
024200           25  BANK-SCR50-DSC4               PIC X(15).           CBANKDAT
024300           25  BANK-SCR50-BAL4               PIC X(13).           CBANKDAT
024400           25  BANK-SCR50-FRM5               PIC X(1).            CBANKDAT
024500           25  BANK-SCR50-TO5                PIC X(1).            CBANKDAT
024600           25  BANK-SCR50-ACC5               PIC X(9).            CBANKDAT
024700           25  BANK-SCR50-DSC5               PIC X(15).           CBANKDAT
024800           25  BANK-SCR50-BAL5               PIC X(13).           CBANKDAT
024900           25  BANK-SCR50-ERRMSG             PIC X(62).           CBANKDAT
025000         20  BANK-SCREEN60-DATA.                                  CBANKDAT
025100           25  BANK-SCR60-RETURN-TO          PIC X(8).            CBANKDAT
025200           25  BANK-SCR60-CONTACT-ID         PIC X(5).            CBANKDAT
025300           25  BANK-SCR60-CONTACT-NAME       PIC X(25).           CBANKDAT
025400           25  BANK-SCR60-CHANGE-ACTION      PIC X(1).            CBANKDAT
025500             88  ADDR-CHANGE-REQUEST         VALUE ' '.           CBANKDAT
025600             88  ADDR-CHANGE-VERIFY          VALUE 'V'.           CBANKDAT
025700             88  ADDR-CHANGE-COMMIT          VALUE 'U'.           CBANKDAT
025800           25  BANK-SCR60-OLD-DETS.                               CBANKDAT
025900             30  BANK-SCR60-OLD-ADDR1        PIC X(25).           CBANKDAT
026000             30  BANK-SCR60-OLD-ADDR2        PIC X(25).           CBANKDAT
026100             30  BANK-SCR60-OLD-STATE        PIC X(2).            CBANKDAT
026200             30  BANK-SCR60-OLD-CNTRY        PIC X(6).            CBANKDAT
026300             30  BANK-SCR60-OLD-PSTCDE       PIC X(6).            CBANKDAT
026400             30  BANK-SCR60-OLD-TELNO        PIC X(12).           CBANKDAT
026500             30  BANK-SCR60-OLD-EMAIL        PIC X(30).           CBANKDAT
026600             30  BANK-SCR60-OLD-SEND-MAIL    PIC X(1).            CBANKDAT
026700             30  BANK-SCR60-OLD-SEND-EMAIL   PIC X(1).            CBANKDAT
026800           25  BANK-SCR60-NEW-DETS.                               CBANKDAT
026900             30  BANK-SCR60-NEW-ADDR1        PIC X(25).           CBANKDAT
027000             30  BANK-SCR60-NEW-ADDR2        PIC X(25).           CBANKDAT
027100             30  BANK-SCR60-NEW-STATE        PIC X(2).            CBANKDAT
027200             30  BANK-SCR60-NEW-CNTRY        PIC X(6).            CBANKDAT
027300             30  BANK-SCR60-NEW-PSTCDE       PIC X(6).            CBANKDAT
027400             30  BANK-SCR60-NEW-TELNO        PIC X(12).           CBANKDAT
027500             30  BANK-SCR60-NEW-EMAIL        PIC X(30).           CBANKDAT
027600             30  BANK-SCR60-NEW-SEND-MAIL    PIC X(1).            CBANKDAT
027700             30  BANK-SCR60-NEW-SEND-EMAIL   PIC X(1).            CBANKDAT
027800         20  BANK-SCREEN70-DATA.                                  CBANKDAT
027900           25  BANK-SCR70-AMOUNT             PIC X(7).            CBANKDAT
028000           25  BANK-SCR70-RATE               PIC X(7).            CBANKDAT
028100           25  BANK-SCR70-TERM               PIC X(5).            CBANKDAT
028200           25  BANK-SCR70-PAYMENT            PIC X(9).            CBANKDAT
028300          20  BANK-SCREEN80-DATA.                                 CBANKDAT
028400           25  BANK-SCR80-SCRN               PIC X(6).            CBANKDAT
028500           25  BANK-SCR80-SCRN-STATUS        PIC X(1).            CBANKDAT
028600           25 BANK-SCR80-LINE                PIC X(75)            CBANKDAT
028700               OCCURS 19 TIMES.                                   CBANKDAT
028800                                                                  CBANKDAT
