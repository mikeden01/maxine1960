000100***************************************************************** CBRETURN
000200*                                                               * CBRETURN
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBRETURN
000400*                                                               * CBRETURN
000500***************************************************************** CBRETURN
000600                                                                  CBRETURN
000700****************************************************************  CBRETURN
000800* CBRETURN.CPY (CICS Version)                                  *  CBRETURN
000900*--------------------------------------------------------------*  CBRETURN
001000* This copybook is used as the RETURN point from data access   *  CBRETURN
001100* routines. It is used as there are different requirements     *  CBRETURN
001200* depending on how the module is called (EXEC CICS LINK or     *  CBRETURN
001300* COBOL CALL statement).                                       *  CBRETURN
001400****************************************************************  CBRETURN
001500     EXEC CICS RETURN                                             CBRETURN
001600     END-EXEC.                                                    CBRETURN
001700     GOBACK.                                                      CBRETURN
001800                                                                  CBRETURN
