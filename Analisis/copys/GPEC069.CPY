      * GPEC069:  COMMAREA GP7C0690                                    *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      ******************************************************************
      *B.MF GPEC069                   LINKAGE AREA WITH THE MODULE     *
      *B/MF                           GP7C0690 OF CONTROL TABLE ACCESS *
      *B.IF BYT-LTH-050               BYTES LENGTH                     *
      *B.IF DTA-INP-GPEC069           INPUT FIELDS                     *
      *B.IF COD-OPTION                OPTION CODE                      *
      *B/IF                           '1' UPDATING MADE INCREASING 1   *
      *B/IF                           '2' INQUIRY                      *
      *B/IF                           '3' UPDATING MADE INCREASING 2   *
      *B/IF                           '4' DATE UPDATING                *
      *B.IF COD-CONTROL               CONTROL CODE                     *
      *B.IF FCC                       FOREIGN CURRENCY CODE            *
      *B.OF DTA-OUT-GPEC069           OUTPUT FIELDS                    *
      *B.OF COD-MOD-RTN               MODIFICATION RETURN CODE         *
      *B/OF                            '00' RETURN OK                  *
      *B/OF                            '20' PROCESS ERROR              *
      *B/OF                            '99' DB2 ERROR                  *
      *B.OF TXT-MESSAGE               MESSAGE LITERAL                  *
      *B.OF COD-DB2                   DB2 CODE                         *
      *B.OF SQLCODE                   SQL RETURN CODE                  *
      *B.OF COD-SQL-MSG               SQL MESSAGE CODE                 *
      *B.OF BYT-LTH                   BYTES LENGTH                     *
      *B.OF TXT-DTA-SQL               SQL DATA LITERAL                 *
      *B.OF DES-DB2-TBL               DB2 TABLE DESCRIPTION            *
      *B.OF NUM-CTR-ETY               CONTROL TABLE ENTRY COUNTER      *
      *B.OF AMT-ENTRY                 CONTROL TABLE AMOUNT             *
      *B.OF PER-CONTROL               CONTROL PERCENTAGE               *
      *B.OF DAT-CTRL-TBL              CONTROL TABLE DATE               *
      *B.MF FILL1                     FILLER                           *
      ******************************************************************
         03 GPEC069            .
           08 E069-BYT-LTH-050                       PIC S9(4) COMP
                                    VALUE +390.
           08 E069-DTA-INP-GPEC069         .
             13 E069-COD-OPTION                     PIC S9(1) COMP-3.
             13 E069-COD-CONTROL                     PIC S9(7) COMP-3.
             13 E069-FCC                     PIC X(3).
           08 E069-DTA-OUT-GPEC069         .
             13 E069-COD-MOD-RTN                     PIC X(2).
             13 E069-TXT-MESSAGE                     PIC X(7).
             13 E069-COD-DB2        .
               18 E069-SQLCODE                   PIC S9(9) COMP.
               18 E069-COD-SQL-MSG     .
               23 E069-BYT-LTH                   PIC S9(4) COMP.
               23 E069-TXT-DTA-SQL                   PIC X(30).
               18 E069-DES-DB2-TBL                   PIC X(16).
             13 E069-NUM-CTR-ETY                     PIC S9(7) COMP-3.
             13 E069-AMT-ENTRY   PIC S9(12)V9(2) COMP-3.
             13 E069-PER-CONTROL                PIC S9(5)V9(5) COMP-3.
             13 E069-DAT-CTRL-TBL                     PIC X(10).
      *
             13 E069-FILL1                     PIC X(250).
