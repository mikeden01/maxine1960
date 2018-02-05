000100***************************************************************** CBANKX03
000200*                                                               * CBANKX03
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX03
000400*                                                               * CBANKX03
000500***************************************************************** CBANKX03
000600                                                                  CBANKX03
000700***************************************************************** CBANKX03
000800* CBANKX03.CPY (CICS Version)                                   * CBANKX03
000900*---------------------------------------------------------------* CBANKX03
001000* This copybook is used to provide an common means of calling   * CBANKX03
001100* data access module DBANK03P so that the that module using     * CBANKX03
001200* this copy book is insensitive to it environment.              * CBANKX03
001300* There are different versions for CICS, IMS and INET.          * CBANKX03
001400***************************************************************** CBANKX03
001500* by default use CICS commands to call the module                 CBANKX03
001600     EXEC CICS LINK PROGRAM('DBANK03P')                           CBANKX03
001700                    COMMAREA(CD03-DATA)                           CBANKX03
001800                    LENGTH(LENGTH OF CD03-DATA)                   CBANKX03
001900     END-EXEC                                                     CBANKX03
002000*    CALL 'DBANK03P' USING CD03-DATA                              CBANKX03
002100                                                                  CBANKX03
