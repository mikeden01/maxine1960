      ***************************************************************** BBANK10P
      *                                                               * BBANK10P
      *   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK10P
      *   This demonstration program is provided for use by users     * BBANK10P
      *   of Micro Focus products and may be used, modified and       * BBANK10P
      *   distributed as part of your application provided that       * BBANK10P
      *   you properly acknowledge the copyright of Micro Focus       * BBANK10P
      *   in this material.                                           * BBANK10P
      *                                                               * BBANK10P
      ***************************************************************** BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * Program:     BBANK10P.CBL                                     * BBANK10P
      * Layer:       Business logic                                   * BBANK10P
      * Function:    Signon to system to identify user                * BBANK10P
      ***************************************************************** BBANK10P
                                                                        BBANK10P
       IDENTIFICATION DIVISION.                                         BBANK10P
       PROGRAM-ID.                                                      BBANK10P
           BBANK10P.                                                    BBANK10P
       DATE-WRITTEN.                                                    BBANK10P
           September 2002.                                              BBANK10P
       DATE-COMPILED.                                                   BBANK10P
           Today.                                                       BBANK10P
                                                                        BBANK10P
       ENVIRONMENT DIVISION.                                            BBANK10P
                                                                        BBANK10P
       DATA DIVISION.                                                   BBANK10P
       WORKING-STORAGE SECTION.                                         BBANK10P
       01  WS-MISC-STORAGE.                                             BBANK10P
         05  WS-PROGRAM-ID                         PIC X(8)             BBANK10P
             VALUE 'BBANK10P'.                                          BBANK10P
         05  WS-INPUT-FLAG                         PIC X(1).            BBANK10P
           88  INPUT-OK                            VALUE '0'.           BBANK10P
           88  INPUT-ERROR                         VALUE '1'.           BBANK10P
         05  WS-RETURN-FLAG                        PIC X(1).            BBANK10P
           88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK10P
           88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK10P
         05  WS-RETURN-MSG                         PIC X(75).           BBANK10P
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK10P
         05  WS-PFK-FLAG                           PIC X(1).            BBANK10P
           88  PFK-VALID                           VALUE '0'.           BBANK10P
           88  PFK-INVALID                         VALUE '1'.           BBANK10P
         05  WS-ERROR-MSG                          PIC X(75).           BBANK10P
                                                                        BBANK10P
       01  WS-BANK-DATA.                                                BBANK10P
       COPY CBANKDAT.                                                   BBANK10P
                                                                        BBANK10P
       01  WS-HELP-DATA.                                                BBANK10P
       COPY CHELPD01.                                                   BBANK10P
                                                                        BBANK10P
       01  WS-PERSON.                                                   BBANK10P
       COPY CBANKD01.                                                   BBANK10P
                                                                        BBANK10P
       COPY CABENDD.                                                    BBANK10P
                                                                        BBANK10P
       LINKAGE SECTION.                                                 BBANK10P
       01  DFHCOMMAREA.                                                 BBANK10P
         05  LK-COMMAREA                           PIC X(6144).         BBANK10P
                                                                        BBANK10P
       COPY CENTRY.                                                     BBANK10P
      ***************************************************************** BBANK10P
      * Make ourselves re-entrant                                     * BBANK10P
      ***************************************************************** BBANK10P
           MOVE SPACES TO WS-ERROR-MSG.                                 BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * Move the passed area to our area                              * BBANK10P
      ***************************************************************** BBANK10P
           MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * Ensure error message is cleared                               * BBANK10P
      ***************************************************************** BBANK10P
           MOVE SPACES TO BANK-ERROR-MSG.                               BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * If this is the first time in, then we have to do set up the   * BBANK10P
      * COMMAREA and ask for the first map to be displayed.           * BBANK10P
      ***************************************************************** BBANK10P
           IF BANK-NO-CONV-IN-PROGRESS                                  BBANK10P
              SET BANK-CONV-IN-PROGRESS TO TRUE                         BBANK10P
              MOVE 'BBANK10P' TO BANK-LAST-PROG                         BBANK10P
              MOVE 'BBANK10P' TO BANK-NEXT-PROG                         BBANK10P
              MOVE LOW-VALUES TO BANK-USERID                            BBANK10P
              MOVE LOW-VALUES TO BANK-PSWD                              BBANK10P
              MOVE SPACES TO BANK-LAST-MAPSET                           BBANK10P
              MOVE SPACES TO BANK-LAST-MAP                              BBANK10P
              MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        BBANK10P
              MOVE 'BANK10A' TO BANK-NEXT-MAP                           BBANK10P
              GO TO COMMON-RETURN                                       BBANK10P
           END-IF.                                                      BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * This is the main process                                      * BBANK10P
      ***************************************************************** BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * Save the passed return message and then turn it off           * BBANK10P
      ***************************************************************** BBANK10P
           MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANK10P
           SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * Check the AID to see if its valid at this point               * BBANK10P
      ***************************************************************** BBANK10P
           SET PFK-INVALID TO TRUE.                                     BBANK10P
           IF BANK-AID-ENTER OR                                         BBANK10P
              BANK-AID-PFK03                                            BBANK10P
              SET PFK-VALID TO TRUE                                     BBANK10P
           END-IF.                                                      BBANK10P
           IF BANK-AID-PFK01 AND                                        BBANK10P
              BANK-HELP-INACTIVE                                        BBANK10P
              SET BANK-HELP-ACTIVE TO TRUE                              BBANK10P
              SET PFK-VALID TO TRUE                                     BBANK10P
           END-IF.                                                      BBANK10P
           IF BANK-AID-PFK04 AND                                        BBANK10P
              BANK-HELP-ACTIVE                                          BBANK10P
              SET PFK-VALID TO TRUE                                     BBANK10P
           END-IF.                                                      BBANK10P
           IF PFK-INVALID                                               BBANK10P
              SET BANK-AID-ENTER TO TRUE                                BBANK10P
           END-IF.                                                      BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * Check the AID to see if we have to quit                       * BBANK10P
      ***************************************************************** BBANK10P
           IF BANK-AID-PFK03                                            BBANK10P
              MOVE 'BBANK10P' TO BANK-LAST-PROG                         BBANK10P
              MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK10P
              MOVE 'MBANK10' TO BANK-LAST-MAPSET                        BBANK10P
              MOVE 'BANK10A' TO BANK-LAST-MAP                           BBANK10P
              MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK10P
              MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK10P
              GO TO COMMON-RETURN                                       BBANK10P
           END-IF.                                                      BBANK10P
                                                                        BBANK10P
      ***************************************************************** BBANK10P
      * Check the to see if user needs or has been using help         * BBANK10P
      ***************************************************************** BBANK10P
           IF BANK-HELP-ACTIVE                                          BBANK10P
              IF BANK-AID-PFK04                                         BBANK10P
                 SET BANK-HELP-INACTIVE TO TRUE                         BBANK10P
                 MOVE 00 TO BANK-HELP-SCREEN                            BBANK10P
                 MOVE 'BBANK10P' TO BANK-LAST-PROG                      BBANK10P
                 MOVE 'BBANK10P' TO BANK-NEXT-PROG                      BBANK10P
                 MOVE 'MBANK10' TO BANK-LAST-MAPSET                     BBANK10P
                 MOVE 'HELP10A' TO BANK-LAST-MAP                        BBANK10P
                 MOVE 'MBANK10' TO BANK-NEXT-MAPSET                     BBANK10P
                 MOVE 'BANK10A' TO BANK-NEXT-MAP                        BBANK10P
                 GO TO COMMON-RETURN                                    BBANK10P
              ELSE                                                      BBANK10P
                 MOVE 01 TO BANK-HELP-SCREEN                            BBANK10P
                 MOVE 'BBANK10P' TO BANK-LAST-PROG                      BBANK10P
                 MOVE 'BBANK10P' TO BANK-NEXT-PROG                      BBANK10P
                 MOVE 'MBANK10' TO BANK-LAST-MAPSET                     BBANK10P
                 MOVE 'BANK10A' TO BANK-LAST-MAP                        BBANK10P
                 MOVE 'MBANK10' TO BANK-NEXT-MAPSET                     BBANK10P
                 MOVE 'HELP10A' TO BANK-NEXT-MAP                        BBANK10P
                 MOVE 'BANK10' TO HELP01I-SCRN                          BBANK10P
                 COPY CHELPX01.                                         BBANK10P
                 MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK10P
                 GO TO COMMON-RETURN                                    BBANK10P
           END-IF.                                                      BBANK10P
                                                                        BBANK10P
           PERFORM VALIDATE-USER THRU                                   BBANK10P
                   VALIDATE-USER-EXIT.                                  BBANK10P
                                                                        BBANK10P
      * If we had an error display error and return                     BBANK10P
           IF INPUT-ERROR                                               BBANK10P
              MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK10P
              MOVE 'SBANK10P' TO BANK-LAST-PROG                         BBANK10P
              MOVE 'SBANK10P' TO BANK-NEXT-PROG                         BBANK10P
              MOVE 'MBANK10' TO BANK-LAST-MAPSET                        BBANK10P
              MOVE 'BANK10A' TO BANK-LAST-MAP                           BBANK10P
              MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        BBANK10P
              MOVE 'BANK10A' TO BANK-NEXT-MAP                           BBANK10P
              GO TO COMMON-RETURN                                       BBANK10P
           END-IF.                                                      BBANK10P
                                                                        BBANK10P
           MOVE 'BBANK20P' TO BANK-NEXT-PROG.                           BBANK10P
           GO TO COMMON-RETURN.                                         BBANK10P
                                                                        BBANK10P
       COMMON-RETURN.                                                   BBANK10P
           MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK10P
       COPY CRETURN.                                                    BBANK10P
                                                                        BBANK10P
       VALIDATE-USER.                                                   BBANK10P
           SET INPUT-OK TO TRUE.                                        BBANK10P
           IF BANK-USERID IS EQUAL TO 'GUEST'                           BBANK10P
              MOVE 'Guest' TO BANK-USERID-NAME                          BBANK10P
              GO TO VALIDATE-USER-EXIT                                  BBANK10P
           END-IF.                                                      BBANK10P
           IF BANK-USERID IS EQUAL TO LOW-VALUES                        BBANK10P
              MOVE 'Please input user id' TO WS-ERROR-MSG               BBANK10P
              GO TO VALIDATE-USER-ERROR                                 BBANK10P
           END-IF.                                                      BBANK10P
           IF BANK-PSWD IS EQUAL TO LOW-VALUES                          BBANK10P
              MOVE 'Please input password' TO WS-ERROR-MSG              BBANK10P
              GO TO VALIDATE-USER-ERROR                                 BBANK10P
           END-IF.                                                      BBANK10P
      * We now make sure the user is valid.......                       BBANK10P
           MOVE SPACES TO CD01-DATA.                                    BBANK10P
           MOVE BANK-USERID TO CD01I-PERSON-PID.                        BBANK10P
       COPY CBANKX01.                                                   BBANK10P
           IF CD01O-PERSON-PID IS EQUAL TO SPACES                       BBANK10P
              MOVE CD01O-PERSON-NAME TO WS-ERROR-MSG                    BBANK10P
              GO TO VALIDATE-USER-ERROR                                 BBANK10P
           ELSE                                                         BBANK10P
              MOVE CD01O-PERSON-NAME TO BANK-USERID-NAME                BBANK10P
              GO TO VALIDATE-USER-EXIT                                  BBANK10P
           END-IF.                                                      BBANK10P
       VALIDATE-USER-ERROR.                                             BBANK10P
           SET INPUT-ERROR TO TRUE.                                     BBANK10P
       VALIDATE-USER-EXIT.                                              BBANK10P
           EXIT.                                                        BBANK10P
                                                                        BBANK10P
