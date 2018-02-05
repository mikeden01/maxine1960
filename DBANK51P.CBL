      ***************************************************************** DBANK51P
      *                                                               * DBANK51P
      *   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK51P
      *   This demonstration program is provided for use by users     * DBANK51P
      *   of Micro Focus products and may be used, modified and       * DBANK51P
      *   distributed as part of your application provided that       * DBANK51P
      *   you properly acknowledge the copyright of Micro Focus       * DBANK51P
      *   in this material.                                           * DBANK51P
      *                                                               * DBANK51P
      ***************************************************************** DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Program:     DBANK51P.CBL                                     * DBANK51P
      * Function:    Sequential read of bank data for batch job       * DBANK51P
      *              SQL version                                      * DBANK51P
      ***************************************************************** DBANK51P
                                                                        DBANK51P
       IDENTIFICATION DIVISION.                                         DBANK51P
       PROGRAM-ID.                                                      DBANK51P
           DBANK51P.                                                    DBANK51P
       DATE-WRITTEN.                                                    DBANK51P
           September 2002.                                              DBANK51P
       DATE-COMPILED.                                                   DBANK51P
           Today.                                                       DBANK51P
                                                                        DBANK51P
       ENVIRONMENT DIVISION.                                            DBANK51P
                                                                        DBANK51P
       DATA DIVISION.                                                   DBANK51P
                                                                        DBANK51P
       WORKING-STORAGE SECTION.                                         DBANK51P
           EXEC SQL                                                     DBANK51P
                BEGIN DECLARE SECTION                                   DBANK51P
           END-EXEC.                                                    DBANK51P
       01  WS-MISC-STORAGE.                                             DBANK51P
         05  WS-PROGRAM-ID                         PIC X(8)             DBANK51P
             VALUE 'DBANK51P'.                                          DBANK51P
         05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK51P
         05  WS-SUB1                               PIC S9(4) COMP.      DBANK51P
       01  WS-SQLCODE-DISP                         PIC 9(5).            DBANK51P
       01  WS-COMMAREA.                                                 DBANK51P
       COPY CIOFUNCS.                                                   DBANK51P
                                                                        DBANK51P
           EXEC SQL                                                     DBANK51P
                INCLUDE CBANKD51                                        DBANK51P
           END-EXEC.                                                    DBANK51P
           EXEC SQL                                                     DBANK51P
                INCLUDE CBANKSDT                                        DBANK51P
           END-EXEC.                                                    DBANK51P
           EXEC SQL                                                     DBANK51P
                INCLUDE SQLCA                                           DBANK51P
           END-EXEC.                                                    DBANK51P
                                                                        DBANK51P
       COPY CABENDD.                                                    DBANK51P
           EXEC SQL                                                     DBANK01P
004200          END DECLARE SECTION                                     DBANK01P
004300     END-EXEC.                                                    DBANK01P
                                                                        DBANK51P
       LINKAGE SECTION.                                                 DBANK51P
       01  DFHCOMMAREA.                                                 DBANK51P
         05  LK-COMMAREA                           PIC X(1)             DBANK51P
               OCCURS 1 TO 6144 TIMES                                   DBANK51P
                 DEPENDING ON WS-COMMAREA-LENGTH.                       DBANK51P
                                                                        DBANK51P
       PROCEDURE DIVISION USING DFHCOMMAREA.                            DBANK51P
      ***************************************************************** DBANK51P
      * Move the passed data to our area                              * DBANK51P
      ***************************************************************** DBANK51P
           MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK51P
           MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Initialize our output area                                    * DBANK51P
      ***************************************************************** DBANK51P
           MOVE SPACES TO CD51-DATA.                                    DBANK51P
           INITIALIZE WS-SQLCODE-DISP.                                  DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Check what is required                                        * DBANK51P
      ***************************************************************** DBANK51P
           EVALUATE TRUE                                                DBANK51P
             WHEN IO-REQUEST-FUNCTION-OPEN                              DBANK51P
              PERFORM OPEN-CURSOR THRU                                  DBANK51P
                      OPEN-CURSOR-EXIT                                  DBANK51P
             WHEN IO-REQUEST-FUNCTION-READ                              DBANK51P
              PERFORM READ-CURSOR THRU                                  DBANK51P
                      READ-CURSOR-EXIT                                  DBANK51P
             WHEN IO-REQUEST-FUNCTION-CLOSE                             DBANK51P
              PERFORM CLOSE-CURSOR THRU                                 DBANK51P
                      CLOSE-CURSOR-EXIT                                 DBANK51P
             WHEN OTHER                                                 DBANK51P
              SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK51P
           END-EVALUATE.                                                DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Move the result back to the callers area                      * DBANK51P
      ***************************************************************** DBANK51P
           MOVE WS-COMMAREA TO DFHCOMMAREA (1:WS-COMMAREA-LENGTH).      DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Return to our caller                                          * DBANK51P
      ***************************************************************** DBANK51P
           GOBACK.                                                      DBANK51P
                                                                        DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Set up CURSORs so we can browse thru selected data            * DBANK51P
      ***************************************************************** DBANK51P
       OPEN-CURSOR.                                                     DBANK51P
           EXEC SQL                                                     DBANK51P
                DECLARE DETS_CSR CURSOR FOR                             DBANK51P
                SELECT VPID,                                            DBANK51P
                       VNAME,                                           DBANK51P
                       VADDR1,                                          DBANK51P
                       VADDR2,                                          DBANK51P
                       VSTATE,                                          DBANK51P
                       VCNTRY,                                          DBANK51P
                       VPSTCDE,                                         DBANK51P
                       VACCNO,                                          DBANK51P
                       VDESC,                                           DBANK51P
                       VCURRBAL,                                        DBANK51P
                       VLASTSTMTDTE,                                    DBANK51P
                       VLASTSTMTBAL                                     DBANK51P
                FROM VBNKDETS                                           DBANK51P
                ORDER BY VPID
                FOR FETCH ONLY                                          DBANK51P
           END-EXEC.                                                    DBANK51P
           IF SQLCODE IS EQUAL TO ZERO                                  DBANK51P
              EXEC SQL                                                  DBANK51P
                   OPEN DETS_CSR                                        DBANK51P
              END-EXEC                                                  DBANK51P
              IF SQLCODE IS EQUAL TO ZERO                               DBANK51P
                 SET IO-REQUEST-STATUS-OK TO TRUE                       DBANK51P
              ELSE                                                      DBANK51P
                 SET IO-REQUEST-STATUS-ERROR TO TRUE                    DBANK51P
              END-IF                                                    DBANK51P
           ELSE                                                         DBANK51P
              SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK51P
           END-IF.                                                      DBANK51P
       OPEN-CURSOR-EXIT.                                                DBANK51P
           EXIT.                                                        DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Now browse the selected rows and move to our area 1 at a time * DBANK51P
      ***************************************************************** DBANK51P
       READ-CURSOR.                                                     DBANK51P
           EXEC SQL                                                     DBANK51P
                FETCH DETS_CSR                                          DBANK51P
                INTO :VPID,                                             DBANK51P
                     :VNAME,                                            DBANK51P
                     :VADDR1,                                           DBANK51P
                     :VADDR2,                                           DBANK51P
                     :VSTATE,                                           DBANK51P
                     :VCNTRY,                                           DBANK51P
                     :VPSTCDE,                                          DBANK51P
                     :VACCNO,                                           DBANK51P
                     :VDESC,                                            DBANK51P
                     :VCURRBAL,                                         DBANK51P
                     :VLASTSTMTDTE,                                     DBANK51P
                     :VLASTSTMTBAL                                      DBANK51P
           END-EXEC.                                                    DBANK51P
           IF SQLCODE IS EQUAL TO ZERO                                  DBANK51P
              SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK51P
              MOVE VPID TO CD51O-PID                                    DBANK51P
              MOVE VNAME TO CD51O-NAME                                  DBANK51P
              MOVE VADDR1 TO CD51O-ADDR1                                DBANK51P
              MOVE VADDR2 TO CD51O-ADDR2                                DBANK51P
              MOVE VSTATE TO CD51O-STATE                                DBANK51P
              MOVE VCNTRY TO CD51O-CNTRY                                DBANK51P
              MOVE VPSTCDE TO CD51O-POST-CODE                           DBANK51P
              MOVE VACCNO TO CD51O-ACC-NO                               DBANK51P
              MOVE VDESC TO CD51O-ACC-DESC                              DBANK51P
              MOVE VCURRBAL TO CD51O-ACC-CURR-BAL                       DBANK51P
              MOVE VLASTSTMTDTE TO CD51O-ACC-LAST-STMT-DATE             DBANK51P
              MOVE VLASTSTMTBAL TO CD51O-ACC-LAST-STMT-BAL              DBANK51P
           END-IF.                                                      DBANK51P
           IF SQLCODE IS EQUAL TO +100                                  DBANK51P
              SET IO-REQUEST-STATUS-EOF TO TRUE                         DBANK51P
           END-IF.                                                      DBANK51P
           IF SQLCODE IS NOT EQUAL TO ZERO AND                          DBANK51P
              SQLCODE IS NOT EQUAL TO +100                              DBANK51P
              SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK51P
              MOVE SPACES TO CD51O-DATA                                 DBANK51P
           END-IF.                                                      DBANK51P
       READ-CURSOR-EXIT.                                                DBANK51P
           EXIT.                                                        DBANK51P
                                                                        DBANK51P
      ***************************************************************** DBANK51P
      * Close the cursor                                              * DBANK51P
      ***************************************************************** DBANK51P
       CLOSE-CURSOR.                                                    DBANK51P
           EXEC SQL                                                     DBANK51P
                CLOSE DETS_CSR                                          DBANK51P
           END-EXEC.                                                    DBANK51P
           IF SQLCODE IS EQUAL TO ZERO                                  DBANK51P
              SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK51P
           ELSE                                                         DBANK51P
             SET IO-REQUEST-STATUS-ERROR TO TRUE                        DBANK51P
           END-IF.                                                      DBANK51P
       CLOSE-CURSOR-EXIT.                                               DBANK51P
           EXIT.                                                        DBANK51P
                                                                        DBANK51P
