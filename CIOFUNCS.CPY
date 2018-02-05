000100***************************************************************** CIOFUNCS
000200*                                                               * CIOFUNCS
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CIOFUNCS
000400*                                                               * CIOFUNCS
000500***************************************************************** CIOFUNCS
000600                                                                  CIOFUNCS
000700***************************************************************** CIOFUNCS
000800* CIOFUNCS.CPY                                                  * CIOFUNCS
000900*---------------------------------------------------------------* CIOFUNCS
001000* I/O Request definitions (request functions and status codes)  * CIOFUNCS
001100***************************************************************** CIOFUNCS
001200   05  IO-REQUEST-AREAS.                                          CIOFUNCS
001300     10  IO-REQUEST-FUNCTION                 PIC X(8).            CIOFUNCS
001400       88  IO-REQUEST-FUNCTION-OPEN          VALUE 'OPEN    '.    CIOFUNCS
001500       88  IO-REQUEST-FUNCTION-READ          VALUE 'READ    '.    CIOFUNCS
001600       88  IO-REQUEST-FUNCTION-CLOSE         VALUE 'CLOSE   '.    CIOFUNCS
001700     10  IO-REQUEST-STATUS                   PIC X(8).            CIOFUNCS
001800       88  IO-REQUEST-STATUS-OK              VALUE 'OK      '.    CIOFUNCS
001900       88  IO-REQUEST-STATUS-EOF             VALUE 'EOF     '.    CIOFUNCS
002000       88  IO-REQUEST-STATUS-ERROR           VALUE 'ERROR   '.    CIOFUNCS
002100                                                                  CIOFUNCS
