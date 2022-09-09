      * TC9C9900:                                                      *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    TC9C9900.
      *
       AUTHOR.        ALNOVA TECHNOLOGIES CORPORATION.
      *
       DATE-WRITTEN.  2000-01-24.
      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *.TC  @AS0001     JASL   10-05-04 ACCESO DIRECTO A LA TABLA      *
      *                                 TCDT099                        *
      *A/MD @AS0002DBG  JASL   05/08/04 OPTIMIZAR ACCESO A TABLAS      *
      *     @PE0001     JVR    23/06/11 MODIFICACIONES SUGERIDAS POR BD*
      *                                 PARA REDUCIR TIEMPOS ALTOS     *
      *                                                                *
      *     @MP0002     ARHA   25-10-12 SE INHIBE EL LLAMADO A LA TABLA*
      *                                 TCDT099 E IMPLEMENTA EL USO DE *
      *                                 LA RUTINA TC9CR099 PARA LEER EN*
      *                                 MEMORIA                        *
      ******************************************************************
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SOURCE-COMPUTER. IBM-4381.
      *
       OBJECT-COMPUTER. IBM-4381.
      *
       SPECIAL-NAMES.
      *
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      ******************************************************************
      *                       DATA DIVISION                            *
      ******************************************************************
       DATA DIVISION.
      *
      ******************************************************************
      *                       FILE SECTION                             *
      ******************************************************************
       FILE SECTION.
      *
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
            COPY QAWCSQL.
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
      *
           EXEC SQL
                INCLUDE TCGV0990
           END-EXEC.
      *@MP0002-INI
       COPY DFHEIBLK.
       01 QAECEAREA-01.
          COPY QAECAREA.
       COPY TCECR099.
      *@MP0002-FIN
       COPY QGECLAN.
      *
       COPY QGECFEC.
      *
       COPY TCEC1000.
      *
       COPY TCWC0110.
      *
       COPY TCTC2251.
      *
       01  WORKING-STORAGE-VARIABLES.
           05  VN-ENTITY                    PIC X(4).
           05  VA-LNG-CL                    PIC X.
           05  VA-DAT-INVCOMPMM             PIC X(10).
           05  VA-DAT-STARTR                PIC X(10).
           05  VA-DAT-ENDC                  PIC X(10).
      *
       01 SW-ERROR                          PIC X.
          88 SW-ERR-YES                     VALUE 'Y'.
          88 SW-ERR-NO                      VALUE 'N'.
      *
       01  CA-00                            PIC X(2)         VALUE '00'.
       01  CA-10                            PIC X(2)         VALUE '10'.
       01  CA-70                            PIC X(2)         VALUE '70'.
       01  CA-80                            PIC X(2)         VALUE '80'.
       01  CA-99                            PIC X(2)         VALUE '99'.
      *
       01  CA-0                             PIC X            VALUE '0'.
       01  CA-1                             PIC X            VALUE '1'.
       01  CA-3                             PIC X            VALUE '3'.
       01  CA-NO                            PIC X            VALUE 'N'.
       01  CA-YES                           PIC X            VALUE 'Y'.
       01  CA-S                             PIC X            VALUE 'S'.
       01  CN-NUM-00                        PIC 9(2)         VALUE 00.
       01  CN-NUM-1                         PIC 9            VALUE  1.
       01  CA-TABLE                         PIC X(8)  VALUE 'TCDT099'.
       01  CA-FLG-COEXC                     PIC X(8)  VALUE 'ICONVIVE'.
       01  CA-DAT-COEXC                     PIC X(8)  VALUE 'FCONVIVE'.
       01  CA-TC9C1000                      PIC X(8)  VALUE 'TC9C1000'.
       01  CA-QG9CSWA0                      PIC X(8)  VALUE 'QG9CSWA0'.
      *@MP0002-INI
       01 CA-TC9CR099                       PIC X(8)  VALUE 'TC9CR099'.
      *@MP0002-FIN
      *
      ******************************************************************
      *                      LINKAGE SECTION                           *
      ******************************************************************
       LINKAGE SECTION.
      *
       01 VA-TCEC99001.
          COPY TCEC9900.
      *
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING VA-TCEC99001.
      *
           PERFORM 1000-START.
      *
           IF SW-ERR-NO
              PERFORM 2000-PROCESS
           END-IF.
      *
           PERFORM 3000-END.
      *
      ******************************************************************
      *.PN 1000-START.                                                 *
      ******************************************************************
       1000-START.
      *
           INITIALIZE QAWCSQL.
           INITIALIZE  TCEC9900-OUTPUT.
           INITIALIZE  TCGV0990.
           INITIALIZE  WORKING-STORAGE-VARIABLES.
      *
           MOVE  CA-00             TO  TCEC9900-COD-RETURN.
           MOVE  CA-NO             TO  SW-ERROR
      *
           IF TCEC9900-OPTION EQUAL SPACES OR LOW-VALUES
              MOVE  CA-10          TO  TCEC9900-COD-RETURN
              MOVE  CA-YES          TO  SW-ERROR
           ELSE
               IF (TCEC9900-OPTION EQUAL CA-0 OR CA-1 OR CA-3) AND
                   (TCEC9900-KEY EQUAL SPACES OR
                    TCEC9900-COD-LNGKEY EQUAL SPACES)
                  MOVE  CA-10      TO TCEC9900-COD-RETURN
                  MOVE  CA-YES      TO SW-ERROR
               END-IF
           END-IF.
           IF SW-ERR-NO
                 MOVE TCEC9900-COD-LNGKEY TO VA-LNG-CL
           END-IF.
      *
      ******************************************************************
      *.PN 2000-PROCESS.                                               *
      ******************************************************************
       2000-PROCESS.
      *
           IF SW-ERR-NO
              EVALUATE TCEC9900-OPTION
                 WHEN CA-0
                 WHEN CA-1
                 WHEN CA-3
                      PERFORM 2040-SELECT
                 WHEN OTHER
                      MOVE  CA-10          TO  TCEC9900-COD-RETURN
                      MOVE  CA-YES          TO  SW-ERROR
              END-EVALUATE
           END-IF.
      *
      ******************************************************************
      *.PN 2040-SELECT.                                                *
      ******************************************************************
       2040-SELECT.
      *@MP0002-INI
      *     MOVE TCEC9900-KEY TO VN-ENTITY
      *
           MOVE VA-LNG-CL              TO TCECR099-LNG-INP
           MOVE TCEC9900-KEY           TO TCECR099-COD-ENT-INP
           MOVE '1'                    TO TCECR099-COD-REQOPTION
           MOVE 'TCDT099'              TO EAREA-AREA-NM
      *
           CALL CA-TC9CR099    USING DFHEIBLK
                                     QAECEAREA-01
                                     TCECR099
      *
           EVALUATE TCECR099-COD-RETURN
           WHEN '00'
               PERFORM 2500-INFORM-OUTPUT
           WHEN '10'
               MOVE CA-YES            TO SW-ERROR
               MOVE CA-70             TO TCEC9900-COD-RETURN
           WHEN OTHER
               MOVE CA-YES            TO SW-ERROR
               MOVE CA-99             TO TCEC9900-COD-RETURN
               MOVE CA-TABLE          TO TCEC9900-DES-TABLE
               MOVE TCECR099-SQLCODE  TO TCEC9900-SQLCODE
               MOVE TCECR099-SQLERRM  TO TCEC9900-SQLERRM
           END-EVALUATE.
      *
      *    EXEC SQL
      *       SELECT COD_ENTITY,
      *              LNG_DATA,
      *              DES_ENTITY,
      *              COD_SHORT,
      *              COD_NB,
      *              DES_NB,
      *              COD_NATCC,
      *              DES_NATCC,
      *              SDE_NATCC,
      *              COD_NATCCSHORT,
      *              COD_OCCCTRY,
      *              DES_OCC,
      *              SDE_OCC,
      *              COD_OCCSHORT,
      *              COD_RCC,
      *              DES_RCC,
      *              COD_RCCSHORT,
      *              COD_NRESFCC,
      *              DES_NRESFCC,
      *              COD_NATCTRY,
      *              DES_NATCTRY,
      *              FLG_EURCTRY,
      *              EXCHANGE,
      *              LNG_OFDATA,
      *              LASTMODUSER,
      *              LASTMODTRM,
      *              DAT_LASTMOD,
      *              DES_ENTABR,
      *              FLG_OFCACC
      *        INTO :V099-COD-ENTITY,
      *             :V099-LNG-DATA,
      *             :V099-DES-ENTITY,
      *             :V099-COD,
      *             :V099-ENT-NAC,
      *             :V099-DES-NAC,
      *             :V099-COD-NATCC,
      *             :V099-DES-NATCC,
      *             :V099-SDE-NATCC,
      *             :V099-COD-NATCCSHORT,
      *             :V099-COD-OCCCTRY,
      *             :V099-DES-OCC,
      *             :V099-SDE-OCC,
      *             :V099-COD-OCCSHORT,
      *             :V099-COD-RCC,
      *             :V099-DES-RCC,
      *             :V099-COD-RCCSHORT,
      *             :V099-COD-NRESFCC,
      *             :V099-DES-NRESFCC,
      *             :V099-COD-NATCTRY,
      *             :V099-DES-NATCTRY,
      *             :V099-FLG-EURCTRY,
      *             :V099-EXCHANGE,
      *             :V099-LNG-OFDATA,
      *             :V099-LASTMODUSER,
      *             :V099-LASTMODTRM,
      *             :V099-DAT-LASTMOD,
      *             :V099-DES-ENTABR,
      *             :V099-FLG-OFCACC
      *        FROM TCDT099 with (nolock)
      *        WHERE COD_ENTITY  = :VN-ENTITY AND
      *              LNG_DATA    = :VA-LNG-CL
      *    END-EXEC.
      *
      *       MOVE SQLCODE TO SQL-VALUES
      *       EVALUATE TRUE
      *          WHEN SQL-88-OK
      *               PERFORM 2500-INFORM-OUTPUT
      *          WHEN SQL-88-NOT-FOUND
      *               MOVE CA-YES             TO SW-ERROR
      *               MOVE CA-70             TO TCEC9900-COD-RETURN
      *          WHEN OTHER
      *               MOVE CA-YES             TO SW-ERROR
      *               MOVE CA-99             TO TCEC9900-COD-RETURN
      *               MOVE CA-TABLE          TO TCEC9900-DES-TABLE
      *               MOVE SQLCODE            TO TCEC9900-SQLCODE
      *               MOVE SQLERRM            TO TCEC9900-SQLERRM
      *       END-EVALUATE.
      *@MP0002-FIN
      ******************************************************************
      *.PN 2500-INFORM-OUTPUT.                                         *
      ******************************************************************
       2500-INFORM-OUTPUT.
      *
           INITIALIZE TCEC9900-DATA
      *
           MOVE TCECR099-COD-ENTITY     TO  TCEC9900-COD-ENTITY
           MOVE TCECR099-LNG-DATA       TO  TCEC9900-LNG-DATA
           MOVE TCECR099-DES-ENTITY     TO  TCEC9900-DES-ENTITY
           MOVE TCECR099-COD            TO  TCEC9900-COD-SHORT
           MOVE TCECR099-ENT-NAC        TO  TCEC9900-COD-NB
           MOVE TCECR099-DES-NAC        TO  TCEC9900-DES-NB
           MOVE TCECR099-COD-NATCC      TO  TCEC9900-COD-NATCC
           MOVE TCECR099-DES-NATCC      TO  TCEC9900-DES-NATCC
           MOVE TCECR099-SDE-NATCC      TO  TCEC9900-SDE-FCC
           MOVE TCECR099-COD-NATCCSHORT TO  TCEC9900-COD-NATCCSHORT
           MOVE TCECR099-COD-OCCCTRY    TO  TCEC9900-COD-OCCCTRY
           MOVE TCECR099-DES-OCC        TO  TCEC9900-DES-OCC
           MOVE TCECR099-SDE-OCC        TO  TCEC9900-SDES-ORIFCC
           MOVE TCECR099-COD-OCCSHORT   TO  TCEC9900-COD-OFCCSHORT
           MOVE TCECR099-COD-RCC        TO  TCEC9900-COD-RCC
           MOVE TCECR099-DES-RCC        TO  TCEC9900-DES-REFFCC
           MOVE TCECR099-COD-RCCSHORT   TO  TCEC9900-COD-RFCCSHORT
           MOVE TCECR099-COD-NRESFCC    TO  TCEC9900-COD-NRESFCC
           MOVE TCECR099-DES-NRESFCC    TO  TCEC9900-DES-NRESFCC
           MOVE TCECR099-COD-NATCTRY    TO  TCEC9900-COD-NATCTRY
           MOVE TCECR099-DES-NATCTRY    TO  TCEC9900-DES-NATCTRY
           MOVE TCECR099-FLG-EURCTRY    TO  TCEC9900-SW-FLG-EURCTRY
           MOVE TCECR099-EXCHANGE       TO  TCEC9900-SW-FLG-EXCHANGE
           MOVE TCECR099-LNG-OFDATA     TO  TCEC9900-LNG-OFDATA
           MOVE TCECR099-LASTMODUSER    TO  TCEC9900-LASTMODUSER
           MOVE TCECR099-LASTMODTRM     TO  TCEC9900-LASTMODTRM
           MOVE TCECR099-DAT-LASTMOD    TO  TCEC9900-TIMESTAMP
           MOVE TCECR099-DES-ENTABR     TO  TCEC9900-SDE-ENTITY
           MOVE TCECR099-FLG-OFCACC     TO  TCEC9900-FLG-OFCACC
           MOVE CA-S                    TO  TCEC9900-FCCCOEXC.
      *
      ******************************************************************
      *.PN 3000-END.                                                   *
      ******************************************************************
       3000-END.
      *
           GOBACK.
      *
