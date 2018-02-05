000100***************************************************************** CABENDPO
000200*                                                               * CABENDPO
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CABENDPO
000400*                                                               * CABENDPO
000500***************************************************************** CABENDPO
000600                                                                  CABENDPO
000700***************************************************************** CABENDPO
000800* CABENDP.CPY                                                   * CABENDPO
000900*---------------------------------------------------------------* CABENDPO
001000* Invoke abend processing                                       * CABENDPO
001100* This copybook is used to provide an abend invocation routine  * CABENDPO
001200* that is appropriate to the environment.                       * CABENDPO
001300* There are different versions for BATCH, CICS and IMS.         * CABENDPO
001400***************************************************************** CABENDPO
001500     MOVE SPACES TO ABEND-MSG                                     CABENDPO
001600     STRING ABEND-CULPRIT DELIMITED BY SIZE                       CABENDPO
001700            ' Abend ' DELIMITED BY SIZE                           CABENDPO
001800            ABEND-CODE DELIMITED BY SIZE                          CABENDPO
001900            ' - ' DELIMITED BY SIZE                               CABENDPO
002000            ABEND-REASON DELIMITED BY SIZE                        CABENDPO
002100       INTO ABEND-MSG                                             CABENDPO
002200     EXEC CICS WRITE                                              CABENDPO
002300               OPERATOR                                           CABENDPO
002400               TEXT(ABEND-MSG)                                    CABENDPO
002500               TEXTLENGTH(LENGTH OF ABEND-MSG)                    CABENDPO
002600     END-EXEC                                                     CABENDPO
002700     EXEC CICS ABEND                                              CABENDPO
002800               ABCODE(ABEND-CODE)                                 CABENDPO
002900     END-EXEC                                                     CABENDPO
003000     GOBACK                                                       CABENDPO
003100                                                                  CABENDPO
