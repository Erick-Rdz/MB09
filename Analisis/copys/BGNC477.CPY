      ******************************************************************
      *BGNC477 - COPY DE CONSULTA A DETALLE DE TRASPASOS               *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION.                   *
      *     ---------- ------- -------- ------------------------------ *
      ******************************************************************
       02 BGNC477.
           05 BGNC477-IN.
               10 BGNC477-NUM-OP               PIC 9(09).
               10 BGNC477-ACC                  PIC X(20).

           05 BGNC477-VAL.
               10 BGNC477-REF-CRED             PIC X(20).
               10 BGNC477-REF-DEB              PIC X(20).
               10 BGNC477-ACC-CRED             PIC X(20).
               10 BGNC477-ACC-DEB              PIC X(20).
               10 BGNC477-MOV-CRED             PIC 9(09).
               10 BGNC477-MOV-DEB              PIC 9(09).
               10 BGNC477-AMT                  PIC 9(13)V9(02).
               10 BGNC477-STAT-MOV             PIC X(02).
               10 BGNC477-FCC                  PIC X(03).
               10 BGNC477-CENT                 PIC X(04).
               10 BGNC477-FLG-FREE1            PIC X(01).
               10 BGNC477-FLG-FREE2            PIC X(01).
               10 BGNC477-AMT-FREE1            PIC 9(13)V9(02).
               10 BGNC477-AMT-FREE2            PIC 9(13)V9(02).
               10 BGNC477-DAT-FREE             PIC X(10).
               10 BGNC477-TXT-FREE             PIC X(20).
               10 BGNC477-TIMESTMP             PIC X(26).

           05 BGNC477-RETURN.
               10 BGNC477-COD-ERR              PIC X(07).
               10 BGNC477-ERR-VARIA1           PIC X(20).
               10 BGNC477-ERR-VARIA2           PIC X(20).
               10 BGNC477-ERR-DB2.
                   15 BGNC477-SQLCODE          PIC S9(09) COMP.
                   15 BGNC477-DES-TABLE        PIC X(08).
      ******************************************************************
