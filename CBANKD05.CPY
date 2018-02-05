000100***************************************************************** CBANKD05
000200*                                                               * CBANKD05
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKD05
000400*                                                               * CBANKD05
000500***************************************************************** CBANKD05
000600                                                                  CBANKD05
000700***************************************************************** CBANKD05
000800* CD05DATA.CPY                                                  * CBANKD05
000900*---------------------------------------------------------------* CBANKD05
001000* This area is used to pass data between the transaction list   * CBANKD05
001100* display program and the I/O program (DBANK05P) which          * CBANKD05
001200* retrieves the data requested (a series of transactions for a  * CBANKD05
001300* specified account).                                           * CBANKD05
001400***************************************************************** CBANKD05
001500   05  CD05-DATA.                                                 CBANKD05
001600     10  CD05I-DATA.                                              CBANKD05
001700       15  CD05I-ACC                         PIC X(9).            CBANKD05
001800       15  CD05I-START-ID                    PIC X(26).           CBANKD05
001900       15  CD05I-START-ID-R REDEFINES CD05I-START-ID.             CBANKD05
002000         20  CD05I-START-DATE                PIC X(10).           CBANKD05
002100         20  CD05I-START-FILL1               PIC X(1).            CBANKD05
002200         20  CD05I-START-TIME                PIC X(8).            CBANKD05
002300         20  CD05I-START-FILL2               PIC X(1).            CBANKD05
002400         20  CD05I-START-MICROSECS           PIC X(6).            CBANKD05
002500       15  CD05I-SEARCH-CRITERIA             PIC X(5).            CBANKD05
002600         88  CD05-START-EQUAL                VALUE 'ENTER'.       CBANKD05
002700         88  CD05-START-LOW                  VALUE 'PFK07'.       CBANKD05
002800         88  CD05-START-HIGH                 VALUE 'PFK08'.       CBANKD05
002900     10  CD05O-DATA.                                              CBANKD05
003000       15  CD05-DATA-STATUS                  PIC X(1).            CBANKD05
003100         88  CD05-NO-DATA                    VALUE '0'.           CBANKD05
003200         88  CD05-IS-DATA                    VALUE '1'.           CBANKD05
003300         88  CD05-IS-MORE-DATA               VALUE '2'.           CBANKD05
003400         88  CD05-NO-MORE-DATA               VALUE '3'.           CBANKD05
003500       15  CD05O-TXN-DETAILS.                                     CBANKD05
003600         20  CD05O-TXN1.                                          CBANKD05
003700           25  CD05O-ID1.                                         CBANKD05
003800             30  CD05O-DAT1                  PIC X(10).           CBANKD05
003900             30  CD05O-FIL1A                 PIC X(1).            CBANKD05
004000             30  CD05O-TIM1                  PIC X(8).            CBANKD05
004100             30  CD05O-FIL1B                 PIC X(1).            CBANKD05
004200             30  CD05O-FIL1C                 PIC X(6).            CBANKD05
004300           25  CD05O-AMT1                    PIC X(9).            CBANKD05
004400           25  CD05O-AMT1-N REDEFINES CD05O-AMT1                  CBANKD05
004500                                             PIC S9(7)V99.        CBANKD05
004600           25  CD05O-DSC1                    PIC X(30).           CBANKD05
004700         20  CD05O-TXN2.                                          CBANKD05
004800           25  CD05O-ID2.                                         CBANKD05
004900             30  CD05O-DAT2                  PIC X(10).           CBANKD05
005000             30  CD05O-FIL2A                 PIC X(1).            CBANKD05
005100             30  CD05O-TIM2                  PIC X(8).            CBANKD05
005200             30  CD05O-FIL2B                 PIC X(1).            CBANKD05
005300             30  CD05O-FIL2C                 PIC X(6).            CBANKD05
005400           25  CD05O-AMT2                    PIC X(9).            CBANKD05
005500           25  CD05O-AMT2-N REDEFINES CD05O-AMT2                  CBANKD05
005600                                             PIC S9(7)V99.        CBANKD05
005700           25  CD05O-DSC2                    PIC X(30).           CBANKD05
005800         20  CD05O-TXN3.                                          CBANKD05
005900           25  CD05O-ID3.                                         CBANKD05
006000             30  CD05O-DAT3                  PIC X(10).           CBANKD05
006100             30  CD05O-FIL3A                 PIC X(1).            CBANKD05
006200             30  CD05O-TIM3                  PIC X(8).            CBANKD05
006300             30  CD05O-FIL3B                 PIC X(1).            CBANKD05
006400             30  CD05O-FIL3C                 PIC X(6).            CBANKD05
006500           25  CD05O-AMT3                    PIC X(9).            CBANKD05
006600           25  CD05O-AMT3-N REDEFINES CD05O-AMT3                  CBANKD05
006700                                             PIC S9(7)V99.        CBANKD05
006800           25  CD05O-DSC3                    PIC X(30).           CBANKD05
006900         20  CD05O-TXN4.                                          CBANKD05
007000           25  CD05O-ID4.                                         CBANKD05
007100             30  CD05O-DAT4                  PIC X(10).           CBANKD05
007200             30  CD05O-FIL4A                 PIC X(1).            CBANKD05
007300             30  CD05O-TIM4                  PIC X(8).            CBANKD05
007400             30  CD05O-FIL4B                 PIC X(1).            CBANKD05
007500             30  CD05O-FIL4C                 PIC X(6).            CBANKD05
007600           25  CD05O-AMT4                    PIC X(9).            CBANKD05
007700           25  CD05O-AMT4-N REDEFINES CD05O-AMT4                  CBANKD05
007800                                             PIC S9(7)V99.        CBANKD05
007900           25  CD05O-DSC4                    PIC X(30).           CBANKD05
008000         20  CD05O-TXN5.                                          CBANKD05
008100           25  CD05O-ID5.                                         CBANKD05
008200             30  CD05O-DAT5                  PIC X(10).           CBANKD05
008300             30  CD05O-FIL5A                 PIC X(1).            CBANKD05
008400             30  CD05O-TIM5                  PIC X(8).            CBANKD05
008500             30  CD05O-FIL5B                 PIC X(1).            CBANKD05
008600             30  CD05O-FIL5C                 PIC X(6).            CBANKD05
008700           25  CD05O-AMT5                    PIC X(9).            CBANKD05
008800           25  CD05O-AMT5-N REDEFINES CD05O-AMT5                  CBANKD05
008900                                             PIC S9(7)V99.        CBANKD05
009000           25  CD05O-DSC5                    PIC X(30).           CBANKD05
009100         20  CD05O-TXN6.                                          CBANKD05
009200           25  CD05O-ID6.                                         CBANKD05
009300             30  CD05O-DAT6                  PIC X(10).           CBANKD05
009400             30  CD05O-FIL6A                 PIC X(1).            CBANKD05
009500             30  CD05O-TIM6                  PIC X(8).            CBANKD05
009600             30  CD05O-FIL6B                 PIC X(1).            CBANKD05
009700             30  CD05O-FIL6C                 PIC X(6).            CBANKD05
009800           25  CD05O-AMT6                    PIC X(9).            CBANKD05
009900           25  CD05O-AMT6-N REDEFINES CD05O-AMT6                  CBANKD05
010000                                             PIC S9(7)V99.        CBANKD05
010100           25  CD05O-DSC6                    PIC X(30).           CBANKD05
010200         20  CD05O-TXN7.                                          CBANKD05
010300           25  CD05O-ID7.                                         CBANKD05
010400             30  CD05O-DAT7                  PIC X(10).           CBANKD05
010500             30  CD05O-FIL7A                 PIC X(1).            CBANKD05
010600             30  CD05O-TIM7                  PIC X(8).            CBANKD05
010700             30  CD05O-FIL7B                 PIC X(1).            CBANKD05
010800             30  CD05O-FIL7C                 PIC X(6).            CBANKD05
010900           25  CD05O-AMT7                    PIC X(9).            CBANKD05
011000           25  CD05O-AMT7-N REDEFINES CD05O-AMT7                  CBANKD05
011100                                             PIC S9(7)V99.        CBANKD05
011200           25  CD05O-DSC7                    PIC X(30).           CBANKD05
011300         20  CD05O-TXN8.                                          CBANKD05
011400           25  CD05O-ID8.                                         CBANKD05
011500             30  CD05O-DAT8                  PIC X(10).           CBANKD05
011600             30  CD05O-FIL8A                 PIC X(1).            CBANKD05
011700             30  CD05O-TIM8                  PIC X(8).            CBANKD05
011800             30  CD05O-FIL8B                 PIC X(1).            CBANKD05
011900             30  CD05O-FIL8C                 PIC X(6).            CBANKD05
012000           25  CD05O-AMT8                    PIC X(9).            CBANKD05
012100           25  CD05O-AMT8-N REDEFINES CD05O-AMT8                  CBANKD05
012200                                             PIC S9(7)V99.        CBANKD05
012300           25  CD05O-DSC8                    PIC X(30).           CBANKD05
012400         20  CD05O-TXN9.                                          CBANKD05
012500           25  CD05O-ID9.                                         CBANKD05
012600             30  CD05O-DAT9                  PIC X(10).           CBANKD05
012700             30  CD05O-FIL9A                 PIC X(1).            CBANKD05
012800             30  CD05O-TIM9                  PIC X(8).            CBANKD05
012900             30  CD05O-FIL9B                 PIC X(1).            CBANKD05
013000             30  CD05O-FIL9C                 PIC X(6).            CBANKD05
013100           25  CD05O-AMT9                    PIC X(9).            CBANKD05
013200           25  CD05O-AMT9-N REDEFINES CD05O-AMT9                  CBANKD05
013300                                             PIC S9(7)V99.        CBANKD05
013400           25  CD05O-DSC9                    PIC X(30).           CBANKD05
013500       15  CD05O-TXN-DETAILS-R REDEFINES CD05O-TXN-DETAILS.       CBANKD05
013600         20  CD05O-TXN-DATA OCCURS 9 TIMES.                       CBANKD05
013700           25  CD05O-ID.                                          CBANKD05
013800             30  CD05O-DATE                  PIC X(10).           CBANKD05
013900             30  CD05O-FILLER1               PIC X(1).            CBANKD05
014000             30  CD05O-TIME                  PIC X(8).            CBANKD05
014100             30  CD05O-FILLER2               PIC X(1).            CBANKD05
014200             30  CD05O-MICROSEC              PIC X(6).            CBANKD05
014300           25  CD05O-AMT                     PIC X(9).            CBANKD05
014400           25  CD05O-AMT-N REDEFINES CD05O-AMT                    CBANKD05
014500                                             PIC S9(7)V99.        CBANKD05
014600           25  CD05O-DESC                    PIC X(30).           CBANKD05
014700                                                                  CBANKD05
