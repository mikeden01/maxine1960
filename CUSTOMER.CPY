
           EXEC SQL DECLARE BRIAN.CUSTOMER TABLE
           (
              CUSTNO                         SMALLINT
                                             NOT NULL,
              LSTNM                          CHAR (20)
                                             NOT NULL,
              FSTNM                          CHAR (10)
                                             NOT NULL,
              ADDR1                          CHAR (20),
              CITY                           CHAR (15),
              ST                             CHAR (2),
              ZIP                            CHAR (10)
                                             NOT NULL
           )
           END-EXEC.


       01  DCLCUSTOMER.
           03 CUSTNO                         PIC S9(4) COMP.
           03 LSTNM                          PIC X(20).
           03 FSTNM                          PIC X(10).
           03 ADDR1                          PIC X(20).
           03 CITY                           PIC X(15).
           03 ST                             PIC X(2).
           03 ZIP                            PIC X(10).
