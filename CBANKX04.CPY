000100***************************************************************** CBANKX04
000200*                                                               * CBANKX04
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX04
000400*                                                               * CBANKX04
000500***************************************************************** CBANKX04
000600                                                                  CBANKX04
000700***************************************************************** CBANKX04
000800* CBANKX04.CPY (CICS Version)                                   * CBANKX04
000900*---------------------------------------------------------------* CBANKX04
001000* This copybook is used to provide an common means of calling   * CBANKX04
001100* data access module DBANK04P so that the that module using     * CBANKX04
001200* this copy book is insensitive to it environment.              * CBANKX04
001300* There are different versions for CICS, IMS and INET.          * CBANKX04
001400***************************************************************** CBANKX04
001500* by default use CICS commands to call the module                 CBANKX04
001600     EXEC CICS LINK PROGRAM('DBANK04P')                           CBANKX04
001700                    COMMAREA(CD04-DATA)                           CBANKX04
001800                    LENGTH(LENGTH OF CD04-DATA)                   CBANKX04
001900     END-EXEC                                                     CBANKX04
002000*    CALL 'DBANK04P' USING CD04-DATA                              CBANKX04
002100                                                                  CBANKX04
