
           EXEC SQL DECLARE BRIAN.ORDERS TABLE
           (
              ORDNO                          SMALLINT,
              CUSTNO                         SMALLINT,
              ITEMNO                         SMALLINT,
              ORDQTY                         SMALLINT,
              ORDDT                          DATE
           )
           END-EXEC.


       01  DCLORDERS.
           03 ORDNO                          PIC S9(4) COMP.
           03 CUSTNO                         PIC S9(4) COMP.
           03 ITEMNO                         PIC S9(4) COMP.
           03 ORDQTY                         PIC S9(4) COMP.
           03 ORDDT                          PIC X(10).
