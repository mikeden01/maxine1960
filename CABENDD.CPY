000100***************************************************************** CABENDD
000200*                                                               * CABENDD
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CABENDD
000400*                                                               * CABENDD
000500***************************************************************** CABENDD
000600                                                                  CABENDD
000700***************************************************************** CABENDD
000800* CABENDD.CPY                                                   * CABENDD
000900*---------------------------------------------------------------* CABENDD
001000* Work areas for abend routine                                  * CABENDD
001100***************************************************************** CABENDD
001200 01  ABEND-DATA.                                                  CABENDD
001300   05  ABEND-CODE                            PIC X(4)             CABENDD
001400       VALUE SPACES.                                              CABENDD
001500   05  ABEND-CULPRIT                         PIC X(8)             CABENDD
001600       VALUE SPACES.                                              CABENDD
001700   05  ABEND-REASON                          PIC X(50)            CABENDD
001800       VALUE SPACES.                                              CABENDD
001900   05  ABEND-MSG                             PIC X(72)            CABENDD
002000       VALUE SPACES.                                              CABENDD
002100                                                                  CABENDD
