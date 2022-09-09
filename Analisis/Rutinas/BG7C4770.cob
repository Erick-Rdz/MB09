      ******************************************************************
      *BG7C4770 CONSULTA A DETALLE DE TRASPASOS                        *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    BG7C4770.
      *
       AUTHOR.        GICE.
      *
       DATE-WRITTEN.  2012-05-08.
      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *                                                                *
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.

      ******************************************************************
      *                       DATA DIVISION                            *
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
              INCLUDE BGGT111
           END-EXEC

       COPY QAWCSQL.

       01 VA-VARIABLES.
           05 VA-ACC-DEB                   PIC X(20).
           05 VA-MOV-DEB                   PIC 9(09).

      ******************************************************************
      *                      LINKAGE SECTION                           *
      ******************************************************************
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
           COPY BGNC477.

      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-START
      *
           PERFORM 2000-PROCESS
      *
           PERFORM 3000-END.
      *
      ******************************************************************
      *1000-START.                                                     *
      ******************************************************************
       1000-START.

           IF BGNC477-NUM-OP EQUAL TO ZEROES OR LOW-VALUES
              MOVE 'BGE1256'               TO BGNC477-COD-ERR
              MOVE 'NUMERO DE OPERACION'   TO BGNC477-ERR-VARIA1
              PERFORM 3000-END
           END-IF

           IF BGNC477-ACC EQUAL TO SPACES OR LOW-VALUES
              MOVE 'BGE1256'               TO BGNC477-COD-ERR
              MOVE 'CUENTA'                TO BGNC477-ERR-VARIA1
              PERFORM 3000-END
           END-IF.
      ******************************************************************
      *2000-PROCESS.                                                   *
      ******************************************************************
       2000-PROCESS.

           MOVE BGNC477-ACC                TO VA-ACC-DEB
           MOVE BGNC477-NUM-OP             TO VA-MOV-DEB
           PERFORM 2100-ACC-BGDT111.
      ******************************************************************
      *2100-ACC-BGDT111.                                               *
      ******************************************************************
       2100-ACC-BGDT111.

           EXEC SQL
              SELECT T111_REF_CREDIT,
                     T111_REF_DEBIT,
                     T111_ACC_CREDIT,
                     T111_ACC_DEBIT,
                     T111_MOV_CREDIT,
                     T111_MOV_DEBIT,
                     T111_AMT,
                     T111_STATUS_MOV,
                     T111_FCC,
                     T111_CENT,
                     T111_FLG_FREE1,
                     T111_FLG_FREE2,
                     T111_AMT_FREE1,
                     T111_AMT_FREE2,
                     T111_DAT_FREE,
                     T111_TXT_FREE,
                     T111_TIMESTAMP
              INTO
                    :T111-REF-CREDIT,
                    :T111-REF-DEBIT,
                    :T111-ACC-CREDIT,
                    :T111-ACC-DEBIT,
                    :T111-MOV-CREDIT,
                    :T111-MOV-DEBIT,
                    :T111-AMT,
                    :T111-STATUS-MOV,
                    :T111-FCC,
                    :T111-CENT,
                    :T111-FLG-FREE1,
                    :T111-FLG-FREE2,
                    :T111-AMT-FREE1,
                    :T111-AMT-FREE2,
                    :T111-DAT-FREE,
                    :T111-TXT-FREE,
                    :T111-TIMESTAMP
              FROM BGDT111 with(nolock)
              WHERE T111_ACC_DEBIT = :VA-ACC-DEB
                AND T111_MOV_DEBIT = :VA-MOV-DEB

           END-EXEC

           MOVE SQLCODE                    TO SQL-VALUES
           EVALUATE TRUE
              WHEN SQL-88-OK
                 PERFORM  2200-LLENA-SAL
              WHEN SQL-88-NOT-FOUND
                 PERFORM 2101-ACC-BGDT111-2
              WHEN OTHER
                 MOVE 'BGE1264'            TO BGNC477-COD-ERR
                 MOVE 'BGDT111/SELECT'     TO BGNC477-ERR-VARIA1
                 MOVE SQLCODE              TO BGNC477-ERR-VARIA2
                 PERFORM 3000-END
              END-EVALUATE.
      ******************************************************************
      *2101-ACC-BGDT111-2.                                             *
      ******************************************************************
       2101-ACC-BGDT111-2.

           EXEC SQL
              SELECT T111_REF_CREDIT,
                     T111_REF_DEBIT,
                     T111_ACC_CREDIT,
                     T111_ACC_DEBIT,
                     T111_MOV_CREDIT,
                     T111_MOV_DEBIT,
                     T111_AMT,
                     T111_STATUS_MOV,
                     T111_FCC,
                     T111_CENT,
                     T111_FLG_FREE1,
                     T111_FLG_FREE2,
                     T111_AMT_FREE1,
                     T111_AMT_FREE2,
                     T111_DAT_FREE,
                     T111_TXT_FREE,
                     T111_TIMESTAMP
              INTO
                    :T111-REF-CREDIT,
                    :T111-REF-DEBIT,
                    :T111-ACC-CREDIT,
                    :T111-ACC-DEBIT,
                    :T111-MOV-CREDIT,
                    :T111-MOV-DEBIT,
                    :T111-AMT,
                    :T111-STATUS-MOV,
                    :T111-FCC,
                    :T111-CENT,
                    :T111-FLG-FREE1,
                    :T111-FLG-FREE2,
                    :T111-AMT-FREE1,
                    :T111-AMT-FREE2,
                    :T111-DAT-FREE,
                    :T111-TXT-FREE,
                    :T111-TIMESTAMP
              FROM BGDT111 with(nolock)

              WHERE T111_ACC_CREDIT = :VA-ACC-DEB
                AND T111_MOV_CREDIT = :VA-MOV-DEB

           END-EXEC

           MOVE SQLCODE                    TO SQL-VALUES
           EVALUATE TRUE
              WHEN SQL-88-OK
                 PERFORM  2200-LLENA-SAL
              WHEN SQL-88-NOT-FOUND
                 MOVE 'BGE0346'            TO BGNC477-COD-ERR
                 PERFORM 3000-END
              WHEN OTHER
                 MOVE 'BGE1264'            TO BGNC477-COD-ERR
                 MOVE 'BGDT111/SELECT'     TO BGNC477-ERR-VARIA1
                 MOVE SQLCODE              TO BGNC477-ERR-VARIA2
                 PERFORM 3000-END
              END-EVALUATE.
      ******************************************************************
      *2200-LLENA-SAL.                                                 *
      ******************************************************************
       2200-LLENA-SAL.

           MOVE T111-REF-CREDIT            TO BGNC477-REF-CRED
           MOVE T111-REF-DEBIT             TO BGNC477-REF-DEB
           MOVE T111-ACC-CREDIT            TO BGNC477-ACC-CRED
           MOVE T111-ACC-DEBIT             TO BGNC477-ACC-DEB
           MOVE T111-MOV-CREDIT            TO BGNC477-MOV-CRED
           MOVE T111-MOV-DEBIT             TO BGNC477-MOV-DEB
           MOVE T111-AMT                   TO BGNC477-AMT
           MOVE T111-STATUS-MOV            TO BGNC477-STAT-MOV
           MOVE T111-FCC                   TO BGNC477-FCC
           MOVE T111-CENT                  TO BGNC477-CENT
           MOVE T111-FLG-FREE1             TO BGNC477-FLG-FREE1
           MOVE T111-FLG-FREE2             TO BGNC477-FLG-FREE2
           MOVE T111-AMT-FREE1             TO BGNC477-AMT-FREE1
           MOVE T111-AMT-FREE2             TO BGNC477-AMT-FREE2
           MOVE T111-DAT-FREE              TO BGNC477-DAT-FREE
           MOVE T111-TXT-FREE              TO BGNC477-TXT-FREE
           MOVE T111-TIMESTAMP             TO BGNC477-TIMESTMP.
      ******************************************************************
      *3000-END.                                                       *
      ******************************************************************
       3000-END.
           GOBACK.
      ******************************************************************
