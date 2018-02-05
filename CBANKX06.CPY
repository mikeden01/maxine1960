000100***************************************************************** CBANKX06
000200*                                                               * CBANKX06
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX06
000400*                                                               * CBANKX06
000500***************************************************************** CBANKX06
000600                                                                  CBANKX06
000700***************************************************************** CBANKX06
000800* CBANKX06.CPY (CICS Version)                                   * CBANKX06
000900*---------------------------------------------------------------* CBANKX06
001000* This copybook is used to provide an common means of calling   * CBANKX06
001100* data access module DBANK06P so that the that module using     * CBANKX06
001200* this copy book is insensitive to it environment.              * CBANKX06
001300* There are different versions for CICS, IMS and INET.          * CBANKX06
001400***************************************************************** CBANKX06
001500* by default use CICS commands to call the module                 CBANKX06
001600     EXEC CICS LINK PROGRAM('DBANK06P')                           CBANKX06
001700                    COMMAREA(CD06-DATA)                           CBANKX06
001800                    LENGTH(LENGTH OF CD06-DATA)                   CBANKX06
001900     END-EXEC                                                     CBANKX06
002000*    CALL 'DBANK06P' USING CD06-DATA                              CBANKX06
002100                                                                  CBANKX06
