000100***************************************************************** CSCRNHP1
000200*                                                               * CSCRNHP1
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CSCRNHP1
000400*                                                               * CSCRNHP1
000500***************************************************************** CSCRNHP1
000600                                                                  CSCRNHP1
000700***************************************************************** CSCRNHP1
000800* CSCRNHP1.CPY                                                  * CSCRNHP1
000900*---------------------------------------------------------------* CSCRNHP1
001000* Procedure code to populate screen titles                      * CSCRNHP1
001100***************************************************************** CSCRNHP1
001200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         CSCRNHP1
001300     MOVE SCREEN-TITLE1 TO HEAD1O IN <<SCRN>>.                    CSCRNHP1
001400     MOVE SCREEN-TITLE2 TO HEAD2O IN <<SCRN>>.                    CSCRNHP1
001500                                                                  CSCRNHP1
