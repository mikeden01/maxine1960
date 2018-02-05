000100***************************************************************** CSETUIB
000200*                                                               * CSETUIB
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CSETUIB
000400*                                                               * CSETUIB
000500***************************************************************** CSETUIB
000600                                                                  CSETUIB
000700***************************************************************** CSETUIB
000800* CSETUIB.CPY (CICS Version)                                    * CSETUIB
000900*---------------------------------------------------------------* CSETUIB
001000* This copybook is used to call CBLTDLI to set the address of   * CSETUIB
001100* the DLIUIB (User Interface Block). This needs to be done when * CSETUIB
001200* CICS is accessing DL/I databases.                             * CSETUIB
001300* This does not have to be done in an IMS environment.          * CSETUIB
001400* There are different versions for CICS, IMS and INET.          * CSETUIB
001500***************************************************************** CSETUIB
001600     CALL 'CBLTDLI' USING WS-IMS-PCB                              CSETUIB
001700                          WS-PSB-NAME                             CSETUIB
001800                          ADDRESS OF DLIUIB.                      CSETUIB
001900                                                                  CSETUIB
