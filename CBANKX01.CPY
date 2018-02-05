000100***************************************************************** CBANKX01
000200*                                                               * CBANKX01
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX01
000400*                                                               * CBANKX01
000500***************************************************************** CBANKX01
000600                                                                  CBANKX01
000700***************************************************************** CBANKX01
000800* CBANKX01.CPY (CICS Version)                                   * CBANKX01
000900*---------------------------------------------------------------* CBANKX01
001000* This copybook is used to provide an common means of calling   * CBANKX01
001100* data access module DBANK01P so that the that module using     * CBANKX01
001200* this copy book is insensitive to it environment.              * CBANKX01
001300* There are different versions for CICS, IMS and INET.          * CBANKX01
001400***************************************************************** CBANKX01
001500* by default use CICS commands to call the module                 CBANKX01
001600     EXEC CICS LINK PROGRAM('DBANK01P')                           CBANKX01
001700                    COMMAREA(CD01-DATA)                           CBANKX01
001800                    LENGTH(LENGTH OF CD01-DATA)                   CBANKX01
001900     END-EXEC                                                     CBANKX01
002000*    CALL 'DBANK01P' USING CD01-DATA                              CBANKX01
002100                                                                  CBANKX01
