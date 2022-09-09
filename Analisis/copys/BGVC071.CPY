      * BGVC071:                                                       *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      ******************************************************************
      ******************************************************************
      *                     LRECL = 247
       01  BGVC071.
           10 V071-ENT                     PIC X(4).
           10 V071-CEN-REG                 PIC X(4).
           10 V071-ACC                     PIC X(10).
           10 V071-NUM-RNWSEQ              PIC S99999V USAGE COMP-3.
           10 V071-NUM-OPERATION           PIC S999999999V USAGE COMP-3.
           10 V071-COD                     PIC X(3).
           10 V071-AMT                     PIC S9999999999999V99 USAGE
               COMP-3.
           10 V071-DAT-ACCT                PIC X(10).
           10 V071-DAT-OPERATION           PIC X(10).
           10 V071-TIM-OPERATION           PIC X(4).
           10 V071-DAT-VALUE               PIC X(10).
           10 V071-FCC                     PIC X(03).
           10 V071-CHECK                   PIC S9999999V USAGE COMP-3.
           10 V071-INTREF                  PIC X(15).
           10 V071-TYP-ACCT                PIC X(3).
           10 V071-OBSERVATIONS            PIC X(31) .
           10 V071-AUTBAL                  PIC S9999999999999V99 USAGE
               COMP-3.
           10 V071-ENT-ACCT                PIC X(4).
           10 V071-CEN-ACCT                PIC X(4).
           10 V071-COD-PRODUCT             PIC X(2).
           10 V071-COD-SPROD               PIC X(4).
           10 V071-FLG-AUTOMAN             PIC X(1).
           10 V071-FLG-UPDATE              PIC X(1).
           10 V071-FLG-MODIFIED            PIC X(1).
           10 V071-FLG-ANN                 PIC X(1).
           10 V071-FLG-CHECKED             PIC X(1).
           10 V071-FLG-CREDEB              PIC X(1).
           10 V071-FLG-FREE2AUX            PIC X(1).
           10 V071-ENT-UPDATE              PIC X(4).
           10 V071-CEN-UPDATE              PIC X(4).
           10 V071-USERUPD                 PIC X(8).
           10 V071-UPDATM                  PIC X(1).
           10 V071-NETNAMEUPD              PIC X(8).
           10 V071-STP                     PIC X(26).
           10 V071-AMT-ORIGIN              PIC S9999999999999V99 USAGE
               COMP-3.
           10 V071-OCC                     PIC X(3).
           10 V071-AMT-FREE1               PIC S9999999999999V99 USAGE
               COMP-3.
           10 V071-AMT-FREE2               PIC S9999999999999V99 USAGE
               COMP-3.
           10 V071-AMT-FREE3               PIC S9999999999999V99 USAGE
               COMP-3.
           10 V071-FLG-FREE1               PIC X(3).
           10 V071-FLG-FREE2               PIC X(1).
           10 V071-FLG-FREE3               PIC X(1).
      *
