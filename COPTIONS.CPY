000100***************************************************************** COPTIONS
000200*                                                               * COPTIONS
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * COPTIONS
000400*                                                               * COPTIONS
000500***************************************************************** COPTIONS
000600                                                                  COPTIONS
000700***************************************************************** COPTIONS
000800* COPTIONS.CPY                                                  * COPTIONS
000900*---------------------------------------------------------------* COPTIONS
001000* Definitions of demo options                                   * COPTIONS
001100***************************************************************** COPTIONS
001200   05  DEMO-OPTIONS.                                              COPTIONS
001300*  Demonstration menu option 1                                    COPTIONS
001400     10  DEMO-OPTION01.                                           COPTIONS
001500       15  DEMO-OPTION01-TRAN                PIC X(4)             COPTIONS
001600           VALUE 'BANK'.                                          COPTIONS
001700       15  DEMO-OPTION01-PROG                PIC X(8)             COPTIONS
001800           VALUE 'SBANK00P'.                                      COPTIONS
001900       15  DEMO-OPTION01-DESC.                                    COPTIONS
002000         20  FILLER                          PIC X(35)            COPTIONS
002100             VALUE 'Micro Focus Banking Application    '.         COPTIONS
002200         20  FILLER                          PIC X(35)            COPTIONS
002300             VALUE '                                   '.         COPTIONS
002400*  Demonstration menu option 2                                    COPTIONS
002500     10  DEMO-OPTION02.                                           COPTIONS
002600       15  DEMO-OPTION02-TRAN                PIC X(4)             COPTIONS
002700           VALUE 'INSC'.                                          COPTIONS
002800       15  DEMO-OPTION02-PROG                PIC X(8)             COPTIONS
002900           VALUE 'SINSC00P'.                                      COPTIONS
003000       15  DEMO-OPTION02-DESC.                                    COPTIONS
003100         20  FILLER                          PIC X(35)            COPTIONS
003200             VALUE 'Micro Focus Insurance Application  '.         COPTIONS
003300         20  FILLER                          PIC X(35)            COPTIONS
003400             VALUE '                                   '.         COPTIONS
003500*  Demonstration menu option 3                                    COPTIONS
003600     10  DEMO-OPTION03.                                           COPTIONS
003700       15  DEMO-OPTION03-TRAN                PIC X(4)             COPTIONS
003800           VALUE 'PRDA'.                                          COPTIONS
003900       15  DEMO-OPTION03-PROG                PIC X(8)             COPTIONS
004000           VALUE 'SPRDA00P'.                                      COPTIONS
004100       15  DEMO-OPTION03-DESC.                                    COPTIONS
004200         20  FILLER                          PIC X(35)            COPTIONS
004300             VALUE 'Information on Mainframe Developmen'.         COPTIONS
004400          20 FILLER                          PIC X(35)            COPTIONS
004500             VALUE 't Products                         '.         COPTIONS
004600*  Demonstration menu option 4                                    COPTIONS
004700     10  DEMO-OPTION04.                                           COPTIONS
004800       15  DEMO-OPTION04-TRAN                PIC X(4)             COPTIONS
004900           VALUE 'PRDB'.                                          COPTIONS
005000       15  DEMO-OPTION04-PROG                PIC X(8)             COPTIONS
005100           VALUE 'SPRDB00P'.                                      COPTIONS
005200       15  DEMO-OPTION04-DESC.                                    COPTIONS
005300         20  FILLER                          PIC X(35)            COPTIONS
005400             VALUE 'Information on Windows and UNIX dis'.         COPTIONS
005500         20  FILLER                          PIC X(35)            COPTIONS
005600             VALUE 'tributed computing products        '.         COPTIONS
005700   05  DEMO-OPTIONS-R REDEFINES DEMO-OPTIONS.                     COPTIONS
005800     10  DEMO-OPTN                           OCCURS 4 TIMES.      COPTIONS
005900       15  DEMO-OPTN-TRAN                    PIC X(4).            COPTIONS
006000       15  DEMO-OPTN-PROG                    PIC X(8).            COPTIONS
006100       15  DEMO-OPTN-DESC                    PIC X(70).           COPTIONS
006200                                                                  COPTIONS
