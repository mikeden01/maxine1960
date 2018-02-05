000100***************************************************************** CSYNC
000200*                                                               * CSYNC
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CSYNC
000400*                                                               * CSYNC
000500***************************************************************** CSYNC
000600                                                                  CSYNC
000700***************************************************************** CSYNC
000800* CSYNC.CPY (CICS Version)                                      * CSYNC
000900*---------------------------------------------------------------* CSYNC
001000* This copybook is used to call create a SYNC point so that we  * CSYNC
001100* know where we are in a database etc.                          * CSYNC
001200* There are different versions for CICS, IMS and INET.          * CSYNC
001300***************************************************************** CSYNC
001400     EXEC CICS SYNCPOINT                                          CSYNC
001500     END-EXEC.                                                    CSYNC
001600                                                                  CSYNC
