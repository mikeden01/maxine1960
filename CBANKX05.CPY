000100***************************************************************** CBANKX05
000200*                                                               * CBANKX05
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKX05
000400*                                                               * CBANKX05
000500***************************************************************** CBANKX05
000600                                                                  CBANKX05
000700***************************************************************** CBANKX05
000800* CBANKX05.CPY (CICS Version)                                   * CBANKX05
000900*---------------------------------------------------------------* CBANKX05
001000* This copybook is used to provide an common means of calling   * CBANKX05
001100* data access module DBANK05P so that the that module using     * CBANKX05
001200* this copy book is insensitive to it environment.              * CBANKX05
001300* There are different versions for CICS, IMS and INET.          * CBANKX05
001400***************************************************************** CBANKX05
001500* by default use CICS commands to call the module                 CBANKX05
001600     EXEC CICS LINK PROGRAM('DBANK05P')                           CBANKX05
001700                    COMMAREA(CD05-DATA)                           CBANKX05
001800                    LENGTH(LENGTH OF CD05-DATA)                   CBANKX05
001900     END-EXEC                                                     CBANKX05
002000*    CALL 'DBANK05P' USING CD05-DATA                              CBANKX05
002100                                                                  CBANKX05
