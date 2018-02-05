000100***************************************************************** CBANKX08
000200*                                                               * CBANKX08
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX08
000400*                                                               * CBANKX08
000500***************************************************************** CBANKX08
000600                                                                  CBANKX08
000700***************************************************************** CBANKX08
000800* CBANKX08.CPY (CICS Version)                                   * CBANKX08
000900*---------------------------------------------------------------* CBANKX08
001000* This copybook is used to provide an common means of calling   * CBANKX08
001100* data access module DBANK03P so that the that module using     * CBANKX08
001200* this copy book is insensitive to it environment.              * CBANKX08
001300* There are different versions for CICS, IMS and INET.          * CBANKX08
001400***************************************************************** CBANKX08
001500* by default use CICS commands to call the module                 CBANKX08
001600     EXEC CICS LINK PROGRAM('DBANK08P')                           CBANKX08
001700                    COMMAREA(CD08-DATA)                           CBANKX08
001800                    LENGTH(LENGTH OF CD08-DATA)                   CBANKX08
001900     END-EXEC                                                     CBANKX08
002000*    CALL 'DBANK08P' USING CD08-DATA                              CBANKX08
002100                                                                  CBANKX08
