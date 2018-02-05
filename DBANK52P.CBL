      ***************************************************************** DBANK52P
      *                                                               * DBANK52P
      *   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK52P
      *   This demonstration program is provided for use by users     * DBANK52P
      *   of Micro Focus products and may be used, modified and       * DBANK52P
      *   distributed as part of your application provided that       * DBANK52P
      *   you properly acknowledge the copyright of Micro Focus       * DBANK52P
      *   in this material.                                           * DBANK52P
      *                                                               * DBANK52P
      ***************************************************************** DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Program:     DBANK52P.CBL                                     * DBANK52P
      * Function:    Sequential read of bank transaction data for     * DBANK52P
      *              batch job                                        * DBANK52P
      *              SQL version                                      * DBANK52P
      ***************************************************************** DBANK52P
                                                                        DBANK52P
       IDENTIFICATION DIVISION.                                         DBANK52P
       PROGRAM-ID.                                                      DBANK52P
           DBANK52P.                                                    DBANK52P
       DATE-WRITTEN.                                                    DBANK52P
           September 2002.                                              DBANK52P
       DATE-COMPILED.                                                   DBANK52P
           Today.                                                       DBANK52P
                                                                        DBANK52P
       ENVIRONMENT DIVISION.                                            DBANK52P
                                                                        DBANK52P
       DATA DIVISION.                                                   DBANK52P
                                                                        DBANK52P
       WORKING-STORAGE SECTION.
           EXEC SQL                                                     DBANK51P
                BEGIN DECLARE SECTION                                   DBANK51P
           END-EXEC.                                                    DBANK51P
                                                                        DBANK52P
       01  WS-MISC-STORAGE.                                             DBANK52P
         05  WS-PROGRAM-ID                         PIC X(8)             DBANK52P
             VALUE 'DBANK52P'.                                          DBANK52P
         05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK52P
         05  WS-SUB1                               PIC S9(4) COMP.      DBANK52P
         05  WS-TXN-TYPE                           PIC X(1)             DBANK52P
             VALUE '1'.                                                 DBANK52P
                                                                        DBANK52P
       01  WS-COMMAREA.                                                 DBANK52P
       COPY CIOFUNCS.                                                   DBANK52P
                                                                        DBANK52P
           EXEC SQL                                                     DBANK52P
                INCLUDE CBANKD51                                        DBANK52P
           END-EXEC.                                                    DBANK52P
           EXEC SQL                                                     DBANK52P
                INCLUDE CBANKD52                                        DBANK52P
           END-EXEC.                                                    DBANK52P
           EXEC SQL                                                     DBANK52P
                INCLUDE CBANKSTX                                        DBANK52P
           END-EXEC.                                                    DBANK52P
           EXEC SQL                                                     DBANK52P
                INCLUDE CBANKTXD                                        DBANK52P
           END-EXEC.                                                    DBANK52P
           EXEC SQL                                                     DBANK52P
                INCLUDE SQLCA                                           DBANK52P
           END-EXEC.                                                    DBANK52P
                                                                        DBANK52P
       COPY CABENDD.                                                    DBANK52P
004100     EXEC SQL                                                     DBANK04P
004200          END DECLARE SECTION                                     DBANK04P
004300     END-EXEC.                                                    DBANK04P
                                                                        DBANK52P
       LINKAGE SECTION.                                                 DBANK52P
       01  DFHCOMMAREA.                                                 DBANK52P
         05  LK-COMMAREA                           PIC X(1)             DBANK52P
               OCCURS 1 TO 6144 TIMES                                   DBANK52P
                 DEPENDING ON WS-COMMAREA-LENGTH.                       DBANK52P
                                                                        DBANK52P
       PROCEDURE DIVISION USING DFHCOMMAREA.                            DBANK52P
      ***************************************************************** DBANK52P
      * Move the passed data to our area                              * DBANK52P
      ***************************************************************** DBANK52P
           MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK52P
           MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Initialize our output area                                    * DBANK52P
      ***************************************************************** DBANK52P
           MOVE SPACES TO CD52-DATA.                                    DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Check what is required                                        * DBANK52P
      ***************************************************************** DBANK52P
           EVALUATE TRUE                                                DBANK52P
             WHEN IO-REQUEST-FUNCTION-OPEN                              DBANK52P
              PERFORM OPEN-CURSOR THRU                                  DBANK52P
                      OPEN-CURSOR-EXIT                                  DBANK52P
             WHEN IO-REQUEST-FUNCTION-READ                              DBANK52P
              PERFORM READ-CURSOR THRU                                  DBANK52P
                      READ-CURSOR-EXIT                                  DBANK52P
             WHEN IO-REQUEST-FUNCTION-CLOSE                             DBANK52P
              PERFORM CLOSE-CURSOR THRU                                 DBANK52P
                      CLOSE-CURSOR-EXIT                                 DBANK52P
             WHEN OTHER                                                 DBANK52P
              SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK52P
           END-EVALUATE.                                                DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Move the result back to the callers area                      * DBANK52P
      ***************************************************************** DBANK52P
           MOVE WS-COMMAREA TO DFHCOMMAREA (1:WS-COMMAREA-LENGTH).      DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Return to our caller                                          * DBANK52P
      ***************************************************************** DBANK52P
           GOBACK.                                                      DBANK52P
                                                                        DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Set up CURSORs so we can browse thru selected data            * DBANK52P
      ***************************************************************** DBANK52P
       OPEN-CURSOR.                                                     DBANK52P
           EXEC SQL                                                     DBANK52P
                DECLARE TXNS_CSR CURSOR FOR                             DBANK52P
                SELECT BTX_PID,                                         DBANK52P
                       BTX_ACCNO,                                       DBANK52P
                       BTX_TIMESTAMP,                                   DBANK52P
                       BTX_AMOUNT,                                      DBANK52P
                       BTX_DATA_OLD                                     DBANK52P
                FROM BNKTXN                                             DBANK52P
                WHERE BTX_TYPE = :WS-TXN-TYPE                           DBANK52P
                FOR FETCH ONLY                                          DBANK52P
           END-EXEC.                                                    DBANK52P
           IF SQLCODE IS EQUAL TO ZERO                                  DBANK52P
              EXEC SQL                                                  DBANK52P
                   OPEN TXNS_CSR                                        DBANK52P
              END-EXEC                                                  DBANK52P
              IF SQLCODE IS EQUAL TO ZERO                               DBANK52P
                 SET IO-REQUEST-STATUS-OK TO TRUE                       DBANK52P
              ELSE                                                      DBANK52P
                 SET IO-REQUEST-STATUS-ERROR TO TRUE                    DBANK52P
              END-IF                                                    DBANK52P
           ELSE                                                         DBANK52P
              SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK52P
           END-IF.                                                      DBANK52P
       OPEN-CURSOR-EXIT.                                                DBANK52P
           EXIT.                                                        DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Now browse the selected rows and move to our area 1 at a time * DBANK52P
      ***************************************************************** DBANK52P
       READ-CURSOR.                                                     DBANK52P
           EXEC SQL                                                     DBANK52P
                FETCH TXNS_CSR                                          DBANK52P
                INTO :CD52O-PID,                                        DBANK52P
                     :CD52O-ACC-NO,                                     DBANK52P
                     :CD52O-TIMESTAMP,                                  DBANK52P
                     :CD52O-AMOUNT,                                     DBANK52P
                     :TXN-DATA-OLD                                      DBANK52P
           END-EXEC.                                                    DBANK52P
           MOVE TXN-T1-OLD-DESC TO CD52O-DESC.                          DBANK52P
           IF SQLCODE IS EQUAL TO ZERO                                  DBANK52P
              SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK52P
           END-IF.                                                      DBANK52P
           IF SQLCODE IS EQUAL TO +100                                  DBANK52P
              SET IO-REQUEST-STATUS-EOF TO TRUE                         DBANK52P
           END-IF.                                                      DBANK52P
           IF SQLCODE IS NOT EQUAL TO ZERO AND                          DBANK52P
              SQLCODE IS NOT EQUAL TO +100                              DBANK52P
              SET IO-REQUEST-STATUS-ERROR TO TRUE                       DBANK52P
              MOVE SPACES TO CD52O-DATA                                 DBANK52P
           END-IF.                                                      DBANK52P
       READ-CURSOR-EXIT.                                                DBANK52P
           EXIT.                                                        DBANK52P
                                                                        DBANK52P
      ***************************************************************** DBANK52P
      * Close the cursor                                              * DBANK52P
      ***************************************************************** DBANK52P
       CLOSE-CURSOR.                                                    DBANK52P
           EXEC SQL                                                     DBANK52P
                CLOSE TXNS_CSR                                          DBANK52P
           END-EXEC.                                                    DBANK52P
           IF SQLCODE IS EQUAL TO ZERO                                  DBANK52P
              SET IO-REQUEST-STATUS-OK TO TRUE                          DBANK52P
           ELSE                                                         DBANK52P
             SET IO-REQUEST-STATUS-ERROR TO TRUE                        DBANK52P
           END-IF.                                                      DBANK52P
       CLOSE-CURSOR-EXIT.                                               DBANK52P
           EXIT.                                                        DBANK52P
                                                                        DBANK52P
