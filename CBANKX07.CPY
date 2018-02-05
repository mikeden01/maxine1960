000100***************************************************************** CBANKX07
000200*                                                               * CBANKX07
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX07
000400*                                                               * CBANKX07
000500***************************************************************** CBANKX07
000600                                                                  CBANKX07
000700***************************************************************** CBANKX07
000800* CBANKX07.CPY (CICS Version)                                   * CBANKX07
000900*---------------------------------------------------------------* CBANKX07
001000* This copybook is used to provide an common means of calling   * CBANKX07
001100* data access module DBANK07P so that the that module using     * CBANKX07
001200* this copy book is insensitive to it environment.              * CBANKX07
001300* There are different versions for CICS, IMS and INET.          * CBANKX07
001400***************************************************************** CBANKX07
001500* by default use CICS commands to call the module                 CBANKX07
001600     EXEC CICS LINK PROGRAM('DBANK07P')                           CBANKX07
001700                    COMMAREA(CD07-DATA)                           CBANKX07
001800                    LENGTH(LENGTH OF CD07-DATA)                   CBANKX07
001900     END-EXEC                                                     CBANKX07
002000*    CALL 'DBANK07P' USING CD07-DATA                              CBANKX07
002100                                                                  CBANKX07
