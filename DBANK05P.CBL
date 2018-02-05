      ***************************************************************** DBANK05P
      *                                                               * DBANK05P
      *   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * DBANK05P
      *   This demonstration program is provided for use by users     * DBANK05P
      *   of Micro Focus products and may be used, modified and       * DBANK05P
      *   distributed as part of your application provided that       * DBANK05P
      *   you properly acknowledge the copyright of Micro Focus       * DBANK05P
      *   in this material.                                           * DBANK05P
      *                                                               * DBANK05P
      ***************************************************************** DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Program:     DBANK05P.CBL                                     * DBANK05P
      * Function:    Obtain list of transactions for an account       * DBANK05P
      *              SQL version                                      * DBANK05P
      ***************************************************************** DBANK05P
                                                                        DBANK05P
       IDENTIFICATION DIVISION.                                         DBANK05P
       PROGRAM-ID.                                                      DBANK05P
           DBANK05P.                                                    DBANK05P
       DATE-WRITTEN.                                                    DBANK05P
           September 2002.                                              DBANK05P
       DATE-COMPILED.                                                   DBANK05P
           Today.                                                       DBANK05P
                                                                        DBANK05P
       ENVIRONMENT DIVISION.                                            DBANK05P
                                                                        DBANK05P
       DATA DIVISION.                                                   DBANK05P
                                                                        DBANK05P
       WORKING-STORAGE SECTION.                                         DBANK05P
004100     EXEC SQL                                                     DBANK04P
004200          BEGIN DECLARE SECTION                                   DBANK04P
004300     END-EXEC.                                                    DBANK04P

       01  WS-MISC-STORAGE.                                             DBANK05P
         05  WS-PROGRAM-ID                         PIC X(8)             DBANK05P
             VALUE 'DBANK05P'.                                          DBANK05P
         05  WS-COMMAREA-LENGTH                    PIC 9(5).            DBANK05P
         05  WS-SUB1                               PIC S9(4) COMP.      DBANK05P
         05  WS-SUB2                               PIC S9(4) COMP.      DBANK05P
         05  WS-TRANS-COUNT                        PIC S9(9) COMP.      DBANK05P
         05  WS-TXN-TYPE                           PIC X(1)             DBANK05P
             VALUE '1'.                                                 DBANK05P
                                                                        DBANK05P
       01  WS-COMMAREA.                                                 DBANK05P
           COPY CBANKD05.                                               DBANK05P
           EXEC SQL                                                     DBANK05P
                INCLUDE CBANKSTX                                        DBANK05P
           END-EXEC.                                                    DBANK05P
           EXEC SQL                                                     DBANK05P
                INCLUDE CBANKTXD                                        DBANK05P
           END-EXEC.                                                    DBANK05P
           EXEC SQL                                                     DBANK05P
                INCLUDE SQLCA                                           DBANK05P
           END-EXEC.                                                    DBANK05P
           EXEC SQL                                                     K05P
                DECLARE TXN_CSR_EQUAL CURSOR FOR                        K05P
                SELECT TX.BTX_TIMESTAMP,                                K05P
                       TX.BTX_AMOUNT,                                   K05P
                       TX.BTX_DATA_OLD                                  K05P
                FROM BNKTXN TX                                          K05P
                WHERE (TX.BTX_ACCNO = :CD05I-ACC) AND                   K05P
                      (TX.BTX_TYPE = :WS-TXN-TYPE) AND                  K05P
                      (TX.BTX_TIMESTAMP>= :CD05I-START-ID)              K05P
                ORDER BY TX.BTX_TIMESTAMP ASC                           K05P
                FOR FETCH ONLY                                          K05P
           END-EXEC.                                                    K05P
           EXEC SQL                                                     K05P
                DECLARE TXN_CSR_HIGH CURSOR FOR                         K05P
                SELECT TX.BTX_TIMESTAMP,                                K05P
                       TX.BTX_AMOUNT,                                   K05P
                       TX.BTX_DATA_OLD                                  K05P
                FROM BNKTXN TX                                          K05P
                WHERE (TX.BTX_ACCNO = :CD05I-ACC) AND                   K05P
                      (TX.BTX_TYPE = :WS-TXN-TYPE) AND                  K05P
                      (TX.BTX_TIMESTAMP > :CD05I-START-ID)              K05P
                ORDER BY TX.BTX_TIMESTAMP ASC                           K05P
                FOR FETCH ONLY                                          K05P
           END-EXEC.                                                    K05P
           EXEC SQL                                                     K05P
                DECLARE TXN_CSR_LOW CURSOR FOR                          K05P
                SELECT TX.BTX_TIMESTAMP,                                K05P
                       TX.BTX_AMOUNT,                                   K05P
                       TX.BTX_DATA_OLD                                  K05P
                FROM BNKTXN TX                                          K05P
                WHERE (TX.BTX_ACCNO = :CD05I-ACC) AND                   K05P
                      (TX.BTX_TYPE = :WS-TXN-TYPE) AND                  K05P
                      (TX.BTX_TIMESTAMP < :CD05I-START-ID)              K05P
                ORDER BY TX.BTX_TIMESTAMP DESC                          K05P
                FOR FETCH ONLY                                          K05P
           END-EXEC.                                                    K05P
                                                                        DBANK05P
       COPY CABENDD.                                                    DBANK05P
004100     EXEC SQL                                                     DBANK04P
004200          END DECLARE SECTION                                     DBANK04P
004300     END-EXEC.                                                    DBANK04P
                                                                        DBANK05P
       LINKAGE SECTION.                                                 DBANK05P
       01  DFHCOMMAREA.                                                 DBANK05P
         05  LK-COMMAREA                           PIC X(1)             DBANK05P
               OCCURS 1 TO 6144 TIMES                                   DBANK05P
                 DEPENDING ON WS-COMMAREA-LENGTH.                       DBANK05P
                                                                        DBANK05P
       COPY CENTRY.                                                     DBANK05P
      ***************************************************************** DBANK05P
      * Move the passed data to our area                              * DBANK05P
      ***************************************************************** DBANK05P
           MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            DBANK05P
           MOVE DFHCOMMAREA TO WS-COMMAREA.                             DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Initialize our output area                                    * DBANK05P
      ***************************************************************** DBANK05P
           MOVE SPACES TO CD05O-DATA.                                   DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Start by settting flag to say there is no data                * DBANK05P
      ***************************************************************** DBANK05P
           SET CD05-NO-DATA TO TRUE.                                    DBANK05P
                                                                        DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Now open the cursor so we can browse the selected rows        * DBANK05P
      ***************************************************************** DBANK05P
           EVALUATE TRUE                                                DBANK05P
             WHEN CD05-START-EQUAL                                      DBANK05P
               EXEC SQL                                                 DBANK05P
                    OPEN TXN_CSR_EQUAL                                  DBANK05P
               END-EXEC                                                 DBANK05P
             WHEN CD05-START-HIGH                                       DBANK05P
               EXEC SQL                                                 DBANK05P
                    OPEN TXN_CSR_HIGH                                   DBANK05P
               END-EXEC                                                 DBANK05P
             WHEN CD05-START-LOW                                        DBANK05P
               EXEC SQL                                                 DBANK05P
                    OPEN TXN_CSR_LOW                                    DBANK05P
               END-EXEC                                                 DBANK05P
           END-EVALUATE.                                                DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Now browse the selected rows are move up to 8 into our area   * DBANK05P
      ***************************************************************** DBANK05P
           MOVE 0 TO WS-SUB1.                                           DBANK05P
       TRANSACTION-FETCH-LOOP.                                          DBANK05P
           ADD 1 TO WS-SUB1.                                            DBANK05P
           IF WS-SUB1 IS GREATER THAN 9                                 DBANK05P
              SET CD05-IS-MORE-DATA TO TRUE                             DBANK05P
              GO TO TRANSACTION-FETCH-LOOP-EXIT.                        DBANK05P
           EVALUATE TRUE                                                DBANK05P
             WHEN CD05-START-EQUAL                                      DBANK05P
               EXEC SQL                                                 DBANK05P
                    FETCH TXN_CSR_EQUAL                                 DBANK05P
                    INTO :DCL-BTX-TIMESTAMP,                            DBANK05P
                         :DCL-BTX-AMOUNT,                               DBANK05P
                         :DCL-BTX-DATA-OLD                              DBANK05P
               END-EXEC                                                 DBANK05P
             WHEN CD05-START-HIGH                                       DBANK05P
               EXEC SQL                                                 DBANK05P
                    FETCH TXN_CSR_HIGH                                  DBANK05P
                    INTO :DCL-BTX-TIMESTAMP,                            DBANK05P
                         :DCL-BTX-AMOUNT,                               DBANK05P
                         :DCL-BTX-DATA-OLD                              DBANK05P
               END-EXEC                                                 DBANK05P
             WHEN CD05-START-LOW                                        DBANK05P
               EXEC SQL                                                 DBANK05P
                    FETCH TXN_CSR_LOW                                   DBANK05P
                    INTO :DCL-BTX-TIMESTAMP,                            DBANK05P
                         :DCL-BTX-AMOUNT,                               DBANK05P
                         :DCL-BTX-DATA-OLD                              DBANK05P
               END-EXEC                                                 DBANK05P
           END-EVALUATE.                                                DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Did we get the record OK                                      * DBANK05P
      ***************************************************************** DBANK05P
           IF CD05-START-LOW                                            DBANK05P
              IF WS-SUB1 IS GREATER THAN 8                              DBANK05P
                 MOVE WS-SUB1 TO WS-SUB2                                DBANK05P
              ELSE                                                      DBANK05P
                 SUBTRACT WS-SUB1 FROM 9 GIVING WS-SUB2                 DBANK05P
              END-IF                                                    DBANK05P
           ELSE                                                         DBANK05P
              MOVE WS-SUB1 TO WS-SUB2                                   DBANK05P
           END-IF.                                                      DBANK05P
           IF SQLCODE IS EQUAL TO ZERO                                  DBANK05P
              SET CD05-IS-DATA TO TRUE                                  DBANK05P
              MOVE DCL-BTX-TIMESTAMP TO CD05O-ID (WS-SUB2)              DBANK05P
              MOVE DCL-BTX-AMOUNT TO CD05O-AMT-N (WS-SUB2)              DBANK05P
              MOVE DCL-BTX-DATA-OLD TO TXN-T1-OLD                       DBANK05P
              MOVE TXN-T1-OLD-DESC TO CD05O-DESC (WS-SUB2)              DBANK05P
              GO TO TRANSACTION-FETCH-LOOP                              DBANK05P
           ELSE                                                         DBANK05P
              SET CD05-NO-MORE-DATA TO TRUE                             DBANK05P
              GO TO TRANSACTION-FETCH-LOOP-EXIT                         DBANK05P
           END-IF.                                                      DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * We quit the loop for some reason                              * DBANK05P
      ***************************************************************** DBANK05P
       TRANSACTION-FETCH-LOOP-EXIT.                                     DBANK05P
           EVALUATE TRUE                                                DBANK05P
             WHEN CD05-START-EQUAL                                      DBANK05P
               EXEC SQL                                                 DBANK05P
                    CLOSE TXN_CSR_EQUAL                                 DBANK05P
               END-EXEC                                                 DBANK05P
             WHEN CD05-START-HIGH                                       DBANK05P
               EXEC SQL                                                 DBANK05P
                    CLOSE TXN_CSR_HIGH                                  DBANK05P
               END-EXEC                                                 DBANK05P
             WHEN CD05-START-LOW                                        DBANK05P
               EXEC SQL                                                 DBANK05P
                    CLOSE TXN_CSR_LOW                                   DBANK05P
               END-EXEC                                                 DBANK05P
           END-EVALUATE.                                                DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Move the result back to the callers area                      * DBANK05P
      ***************************************************************** DBANK05P
           MOVE WS-COMMAREA TO DFHCOMMAREA (1:WS-COMMAREA-LENGTH).      DBANK05P
                                                                        DBANK05P
      ***************************************************************** DBANK05P
      * Return to our caller                                          * DBANK05P
      ***************************************************************** DBANK05P
       COPY CRETURN.                                                    DBANK05P
                                                                        DBANK05P
