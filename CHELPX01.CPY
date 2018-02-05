000100***************************************************************** CHELPX01
000200*                                                               * CHELPX01
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CHELPX01
000400*                                                               * CHELPX01
000500***************************************************************** CHELPX01
000600                                                                  CHELPX01
000700***************************************************************** CHELPX01
000800* CHELPX01.CPY (CICS Version)                                   * CHELPX01
000900*---------------------------------------------------------------* CHELPX01
001000* This copybook is used to provide an common means of calling   * CHELPX01
001100* data access module DHELP01P so that the that module using     * CHELPX01
001200* this copy book is insensitive to it environment.              * CHELPX01
001300* There are different versions for CICS, IMS and INET.          * CHELPX01
001400***************************************************************** CHELPX01
001500* by default use CICS commands to call the module                 CHELPX01
001600     EXEC CICS LINK PROGRAM('DHELP01P')                           CHELPX01
001700                    COMMAREA(HELP01-DATA)                         CHELPX01
001800                    LENGTH(LENGTH OF HELP01-DATA)                 CHELPX01
001900     END-EXEC                                                     CHELPX01
002000*    CALL 'DHELP01P' USING HELP01-DATA                            CHELPX01
002100                                                                  CHELPX01
