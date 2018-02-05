000100***************************************************************** CSTATESD
000200*                                                               * CSTATESD
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CSTATESD
000400*                                                               * CSTATESD
000500***************************************************************** CSTATESD
000600                                                                  CSTATESD
000700***************************************************************** CSTATESD
000800* CSTATESD.CPY                                                  * CSTATESD
000900*---------------------------------------------------------------* CSTATESD
001000* Work areas for run timer                                      * CSTATESD
001100***************************************************************** CSTATESD
001200 01  STATE-PROV-DATA-AREAS.                                       CSTATESD
001300   05  STATE-PROV-TABLE.                                          CSTATESD
001400     10  FILLER                              PIC X(28)            CSTATESD
001500         VALUE 'USA AK Alaska               '.                    CSTATESD
001600     10  FILLER                              PIC X(28)            CSTATESD
001700         VALUE 'USA AL Alabama              '.                    CSTATESD
001800     10  FILLER                              PIC X(28)            CSTATESD
001900         VALUE 'USA AR Arkansas             '.                    CSTATESD
002000     10  FILLER                              PIC X(28)            CSTATESD
002100         VALUE 'USA AZ Arizona              '.                    CSTATESD
002200     10  FILLER                              PIC X(28)            CSTATESD
002300         VALUE 'USA CA California           '.                    CSTATESD
002400     10  FILLER                              PIC X(28)            CSTATESD
002500         VALUE 'USA CO Colorado             '.                    CSTATESD
002600     10  FILLER                              PIC X(28)            CSTATESD
002700         VALUE 'USA CT Connecticut          '.                    CSTATESD
002800     10  FILLER                              PIC X(28)            CSTATESD
002900         VALUE 'USA DC Washington D.C.      '.                    CSTATESD
003000     10  FILLER                              PIC X(28)            CSTATESD
003100         VALUE 'USA DE Delaware             '.                    CSTATESD
003200     10  FILLER                              PIC X(28)            CSTATESD
003300         VALUE 'USA FL Florida              '.                    CSTATESD
003400     10  FILLER                              PIC X(28)            CSTATESD
003500         VALUE 'USA GA Georgia              '.                    CSTATESD
003600     10  FILLER                              PIC X(28)            CSTATESD
003700         VALUE 'USA HI Hawaii               '.                    CSTATESD
003800     10  FILLER                              PIC X(28)            CSTATESD
003900         VALUE 'USA ID Idaho                '.                    CSTATESD
004000     10  FILLER                              PIC X(28)            CSTATESD
004100         VALUE 'USA IL Illinois             '.                    CSTATESD
004200     10  FILLER                              PIC X(28)            CSTATESD
004300         VALUE 'USA IN Indiana              '.                    CSTATESD
004400     10  FILLER                              PIC X(28)            CSTATESD
004500         VALUE 'USA IA Iowa                 '.                    CSTATESD
004600     10  FILLER                              PIC X(28)            CSTATESD
004700         VALUE 'USA KS Kansas               '.                    CSTATESD
004800     10  FILLER                              PIC X(28)            CSTATESD
004900         VALUE 'USA KY Kentucky             '.                    CSTATESD
005000     10  FILLER                              PIC X(28)            CSTATESD
005100         VALUE 'USA LA Louisiana            '.                    CSTATESD
005200     10  FILLER                              PIC X(28)            CSTATESD
005300         VALUE 'USA MA Massachusetts        '.                    CSTATESD
005400     10  FILLER                              PIC X(28)            CSTATESD
005500         VALUE 'USA MD Maryland             '.                    CSTATESD
005600     10  FILLER                              PIC X(28)            CSTATESD
005700         VALUE 'USA ME Maine                '.                    CSTATESD
005800     10  FILLER                              PIC X(28)            CSTATESD
005900         VALUE 'USA MI Michigan             '.                    CSTATESD
006000     10  FILLER                              PIC X(28)            CSTATESD
006100         VALUE 'USA MN Minnesota            '.                    CSTATESD
006200     10  FILLER                              PIC X(28)            CSTATESD
006300         VALUE 'USA MO Missouri             '.                    CSTATESD
006400     10  FILLER                              PIC X(28)            CSTATESD
006500         VALUE 'USA MS Mississippi          '.                    CSTATESD
006600     10  FILLER                              PIC X(28)            CSTATESD
006700         VALUE 'USA MT Montana              '.                    CSTATESD
006800     10  FILLER                              PIC X(28)            CSTATESD
006900         VALUE 'USA NC North Carolina       '.                    CSTATESD
007000     10  FILLER                              PIC X(28)            CSTATESD
007100         VALUE 'USA ND North Dakota         '.                    CSTATESD
007200     10  FILLER                              PIC X(28)            CSTATESD
007300         VALUE 'USA NE Nebraska             '.                    CSTATESD
007400     10  FILLER                              PIC X(28)            CSTATESD
007500         VALUE 'USA NH New Hampshire        '.                    CSTATESD
007600     10  FILLER                              PIC X(28)            CSTATESD
007700         VALUE 'USA NJ New Jersey           '.                    CSTATESD
007800     10  FILLER                              PIC X(28)            CSTATESD
007900         VALUE 'USA NM New Mexico           '.                    CSTATESD
008000     10  FILLER                              PIC X(28)            CSTATESD
008100         VALUE 'USA NV Nevada               '.                    CSTATESD
008200     10  FILLER                              PIC X(28)            CSTATESD
008300         VALUE 'USA NY New York             '.                    CSTATESD
008400     10  FILLER                              PIC X(28)            CSTATESD
008500         VALUE 'USA OH Ohio                 '.                    CSTATESD
008600     10  FILLER                              PIC X(28)            CSTATESD
008700         VALUE 'USA OK Oklahoma             '.                    CSTATESD
008800     10  FILLER                              PIC X(28)            CSTATESD
008900         VALUE 'USA OR Oregon               '.                    CSTATESD
009000     10  FILLER                              PIC X(28)            CSTATESD
009100         VALUE 'USA PA Pennsylvania         '.                    CSTATESD
009200     10  FILLER                              PIC X(28)            CSTATESD
009300         VALUE 'USA RI Rhode Island         '.                    CSTATESD
009400     10  FILLER                              PIC X(28)            CSTATESD
009500         VALUE 'USA SC South Carolina       '.                    CSTATESD
009600     10  FILLER                              PIC X(28)            CSTATESD
009700         VALUE 'USA SD South Dakota         '.                    CSTATESD
009800     10  FILLER                              PIC X(28)            CSTATESD
009900         VALUE 'USA TN Tennessee            '.                    CSTATESD
010000     10  FILLER                              PIC X(28)            CSTATESD
010100         VALUE 'USA TX Texas                '.                    CSTATESD
010200     10  FILLER                              PIC X(28)            CSTATESD
010300         VALUE 'USA UT Utah                 '.                    CSTATESD
010400     10  FILLER                              PIC X(28)            CSTATESD
010500         VALUE 'USA VA Virginia             '.                    CSTATESD
010600     10  FILLER                              PIC X(28)            CSTATESD
010700         VALUE 'USA VT Vermont              '.                    CSTATESD
010800     10  FILLER                              PIC X(28)            CSTATESD
010900         VALUE 'USA WA Washington           '.                    CSTATESD
011000     10  FILLER                              PIC X(28)            CSTATESD
011100         VALUE 'USA WI Wisconsin            '.                    CSTATESD
011200     10  FILLER                              PIC X(28)            CSTATESD
011300         VALUE 'USA WV West Virginia        '.                    CSTATESD
011400     10  FILLER                              PIC X(28)            CSTATESD
011500         VALUE 'USA WY Wyoming              '.                    CSTATESD
011600     10  FILLER                              PIC X(28)            CSTATESD
011700         VALUE 'USA PR Puerto Rico          '.                    CSTATESD
011800     10  FILLER                              PIC X(28)            CSTATESD
011900         VALUE 'CDN AB Alberta              '.                    CSTATESD
012000     10  FILLER                              PIC X(28)            CSTATESD
012100         VALUE 'CDN BC British Columbia     '.                    CSTATESD
012200     10  FILLER                              PIC X(28)            CSTATESD
012300         VALUE 'CDN MB Manitoba             '.                    CSTATESD
012400     10  FILLER                              PIC X(28)            CSTATESD
012500         VALUE 'CDN NB New Brunswick        '.                    CSTATESD
012600     10  FILLER                              PIC X(28)            CSTATESD
012700         VALUE 'CDN NF Newfoundland         '.                    CSTATESD
012800     10  FILLER                              PIC X(28)            CSTATESD
012900         VALUE 'CDN NS Nova Scotia          '.                    CSTATESD
013000     10  FILLER                              PIC X(28)            CSTATESD
013100         VALUE 'CDN NU Nunavut Territory    '.                    CSTATESD
013200     10  FILLER                              PIC X(28)            CSTATESD
013300         VALUE 'CDN NT Northwest Territories'.                    CSTATESD
013400     10  FILLER                              PIC X(28)            CSTATESD
013500         VALUE 'CDN ON Ontario              '.                    CSTATESD
013600     10  FILLER                              PIC X(28)            CSTATESD
013700         VALUE 'CDN PE Prince Edward Island '.                    CSTATESD
013800     10  FILLER                              PIC X(28)            CSTATESD
013900         VALUE 'CDN QC Quebec               '.                    CSTATESD
014000     10  FILLER                              PIC X(28)            CSTATESD
014100         VALUE 'CDN SK Saskatchewan         '.                    CSTATESD
014200     10  FILLER                              PIC X(28)            CSTATESD
014300         VALUE 'CDN YT Yukon Territory      '.                    CSTATESD
014400   05  STATE-PROV-TABLE-R REDEFINES STATE-PROV-TABLE.             CSTATESD
014500     10  STATE-PROV-DATA                     OCCURS 65 TIMES.     CSTATESD
014600       15  STATE-PROV-CNTRY                  PIC X(3).            CSTATESD
014700       15  FILLER                            PIC X(1).            CSTATESD
014800       15  STATE-PROV-CODE                   PIC X(2).            CSTATESD
014900       15  FILLER                            PIC X(1).            CSTATESD
015000       15  STATE-PROV-NAME                   PIC X(21).           CSTATESD
015100   05  STATE-PROV-COUNT                      PIC 9(2).            CSTATESD
015200   05  STATE-PROV-SUB                        PIC 9(2).            CSTATESD
015300   05  STATE-PROV-WK-CNTRY                   PIC X(3).            CSTATESD
015400   05  STATE-PROV-WK-CODE                    PIC X(20).           CSTATESD
015500   05  STATE-PROV-WK-NAME                    PIC X(20).           CSTATESD
015600                                                                  CSTATESD
