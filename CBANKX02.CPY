000100***************************************************************** CBANKX02
000200*                                                               * CBANKX02
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX02
000400*                                                               * CBANKX02
000500***************************************************************** CBANKX02
000600                                                                  CBANKX02
000700***************************************************************** CBANKX02
000800* CBANKX02.CPY (CICS Version)                                   * CBANKX02
000900*---------------------------------------------------------------* CBANKX02
001000* This copybook is used to provide an common means of calling   * CBANKX02
001100* data access module DBANK02P so that the that module using     * CBANKX02
001200* this copy book is insensitive to it environment.              * CBANKX02
001300* There are different versions for CICS, IMS and INET.          * CBANKX02
001400***************************************************************** CBANKX02
001500* by default use CICS commands to call the module                 CBANKX02
001600     EXEC CICS LINK PROGRAM('DBANK02P')                           CBANKX02
001700                    COMMAREA(CD02-DATA)                           CBANKX02
001800                    LENGTH(LENGTH OF CD02-DATA)                   CBANKX02
001900     END-EXEC                                                     CBANKX02
002000*    CALL 'DBANK02P' USING CD02-DATA                              CBANKX02
002100                                                                  CBANKX02
