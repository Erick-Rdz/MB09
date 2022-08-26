      * TC7C0820: RUTINA QUE REALIZA EL CALCULO DE TC REGIONALIZADO    *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    TC7C0820.
      *
       AUTHOR.        ALNOVA TECHNOLOGIES CORPORATION.
      *
       DATE-WRITTEN.  2001-07-18.
      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *.A.MS AZ0001DTC  JASL   28-10-03 REGIONALIZACION DE TIPO DE     *
      *                                 CAMBIOS                        *
      *.A.MS AZ0002DTC  JASL   28-05-04 CALCULO DE TIPOS DE CAMBIO RE- *
      *                                 DONDEADO A DOS DECIMALES       *
      *.A.MS  AZ0004DTC JRM    28-03-06 REGIONALIZAR TIPO DE CAMBIO    *
      *                                 PARA CUALQUIER DIVISA          *
      *     REMR        EMTZ   05-06-07 MODIFICACION EN EL CALCULO DE  *
      *                                 LA REGIONALIZACION             *
      *     AZ0005DTC   RSC    06-08-12 MODIFICACION EN EL CALCULO DE  *
      *                                 LA REGIONALIZACION             *
      *     AZ0006DTC   AGG    21-11-13 SE MODIFICA REGIONALIZACION    *
      *                                 DEL TIPO DE CAMBIO PARA        *
      *                                 GUATEMALA                      *
      *     AZ0007DTC   RSC    24-04-14 SE ASIGNAN TC DE LA REGION     *
      *                                 TEMPORAL A SUC SIN REGION      *
      *     AZ0008DTC   RSC    24-04-14 SE LIMITA EL TIPO DE CAMBIO    *
      *                                 DE AUTOREGIONALIZACION PARA    *
      *                                 MEXICO                         *
      *     AZ0009DTC   CJG    05-01-15 SE REALIZA VALIDACION PARA EL  *
      *                                 CALCULO DEL TIPO DE CAMBIO     *
      *                                 REGIONALIZADO                  *
      *     @01SARD     DSR    07-09-18 SE INHABILITO LINK A TC7C3000  *
      *                                 AL OBTENER FACTOR DE REGION.   *
      *     BAZ0001     JUGJ   23-05-19 SE MODIFICA LA BUSQUEDA POR    *
      *                                 SUCURSAL Y NO POR REGION PARA  *
      *                                 OBTENER EL SPREAD ASIGNADO.    *
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
      *@01SARD
           COPY QCWCL20.
      *@01SARD
       01  VA-WORKINGVAR.
           02  SW-CONS                      PIC X.
           02  SW-ERROR                     PIC X.
           02  VA-FCC                       PIC X(3).
      *
      *.A.MS AZ0002DTC  I  JASL
       01  VN-VARIABLES.
           02  VN-TC-COMPRA                 PIC S9(9)V99  COMP-3 VALUE
               ZEROS.
           02  VN-TC-VENTA                  PIC S9(9)V99  COMP-3 VALUE
               ZEROS.
           02  VN-TC-PROMED                 PIC S9(9)V99  COMP-3 VALUE
               ZEROS.
      *.A.MS AZ0002DTC  F  JASL
      *
       01      CN-NUM-0                     PIC S9 COMP-3 VALUE +0.
       01      CN-NUM-1                     PIC S9 COMP-3 VALUE +1.
       01      CN-NUM-MINUS1                PIC S9 COMP-3 VALUE -1.
       01      CN-NUM-2                     PIC S9 COMP-3 VALUE +2.
       01      CN-NUM-35                    PIC S99 COMP-3 VALUE +35.
       01      CA-00                        PIC X(2)     VALUE '00'.
       01      CA-5                         PIC XX       VALUE '05'.
       01      CA-10                        PIC XX       VALUE '10'.
       01      CA-15                        PIC XX       VALUE '15'.
       01      CA-20                        PIC XX       VALUE '20'.
       01      CA-25                        PIC XX       VALUE '25'.
       01      CA-30                        PIC XX       VALUE '30'.
       01      CA-35                        PIC XX       VALUE '35'.
       01      CA-40                        PIC XX       VALUE '40'.
       01      CA-45                        PIC XX       VALUE '45'.
       01      CA-50                        PIC XX       VALUE '50'.
       01      CA-55                        PIC XX       VALUE '55'.
       01      CA-60                        PIC XX       VALUE '60'.
       01      CA-85                        PIC XX       VALUE '85'.
       01      CA-99                        PIC XX       VALUE '99'.
       01      CA-TC                        PIC XX       VALUE 'TC'.
      *
       01      CA-NO                        PIC X  VALUE 'N'.
       01      CA-YES                       PIC X  VALUE 'Y'.
       01      CA-S                         PIC X  VALUE 'S'.
      *
       01 CA-TBL-0404                       PIC X(4)  VALUE '0404'.
       01 CA-TBL-0405                       PIC X(4)  VALUE '0405'.
      *AZ0004DTC-INI
      *01 CA-USD                            PIC X(3)  VALUE 'USD'.
      *AZ0004DTC-FIN
       01 VN-CENTRO-COM                     PIC X(4)  VALUE SPACES.
      *
       01  SW-EURCTRY                       PIC X(1)  VALUE 'Y'.
           88  SW-ECY-YES                             VALUE 'Y'.
           88  SW-ECY-NO                              VALUE 'N'.
      *
      *BAZ0001-I
       01  SW-PAIS                       PIC X(04)   VALUE SPACES.
           88 SW-PAIS-MEX                            VALUE '0127'.
       01  SW-AUX-CHANN                  PIC X(02)   VALUE SPACES.
           88 SW-CHANN-01                            VALUE '01'.
           88 SW-CHANN-03                            VALUE '03'.
           88 SW-CHANN-06                            VALUE '06'.
      *BAZ0001-F

       01 VA-TCEC3000.
          COPY TCEC3000.
      *
       COPY TCTC4041.
      *
       COPY TCTC4051.
      *
      *AZ0007DTC-INI
       01  VA-GPEC069-AREA.
           COPY GPEC069.
           03  VA-QBEC999-GPEC069-AREA      PIC X(50).
      *AZ0007DTC-FIN
      *@01SARD
           EXEC SQL
             INCLUDE TCGV0010
           END-EXEC.
      *
           EXEC SQL
             INCLUDE TCGV0100
           END-EXEC.
      *@01SARD
      ******************************************************************
      *                      LINKAGE SECTION                           *
      ******************************************************************
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
         COPY TCEC0820.
         COPY QBEC999.
      *
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *
           PERFORM 1000-START.
      *
           IF SW-ERROR = CA-NO
             PERFORM GENERAL-PROCESS
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
      *
           MOVE TCEC0820-COD-RETURN   TO SW-AUX-CHANN
      *
           INITIALIZE TCEC0820-OUTPUT.
      *
           INITIALIZE VA-WORKINGVAR.
      *
           MOVE ZEROS TO TCEC0820-SQLCODE.
           MOVE ZEROS TO TCEC0820-COD-RETURN.
      *
           MOVE CA-NO TO SW-ERROR.
      *
           PERFORM VALIDATE-INPUT-DATA.
      *
      ******************************************************************
      *.PN VALIDATE-INPUT-DATA.                                        *
      ******************************************************************
       VALIDATE-INPUT-DATA.
      *
           IF SW-ERROR = CA-NO
              IF TCEC0820-COD-ENTITY-A NOT NUMERIC
                 MOVE CA-10          TO TCEC0820-COD-RETURN
                 MOVE CA-YES         TO SW-ERROR
              ELSE
                 IF TCEC0820-COD-ENTITY = 0 OR TCEC0820-COD-ENTITY =
                    LOW-VALUES
                    MOVE CA-15        TO TCEC0820-COD-RETURN
                    MOVE CA-YES       TO SW-ERROR
                 END-IF
              END-IF
           END-IF.

           MOVE  TCEC0820-COD-ENTITY TO SW-PAIS
      *
           IF SW-ERROR = CA-NO
              IF TCEC0820-COD-BRANCH-A NOT NUMERIC
                 MOVE CA-20          TO TCEC0820-COD-RETURN
                 MOVE CA-YES         TO SW-ERROR
              ELSE
                 IF TCEC0820-COD-BRANCH = 0 OR TCEC0820-COD-BRANCH =
                    LOW-VALUES
                    MOVE CA-25        TO TCEC0820-COD-RETURN
                    MOVE CA-YES       TO SW-ERROR
                 END-IF
              END-IF
           END-IF.
      *AZ0004DTC-INI
      *    IF SW-ERROR = CA-NO
      *       IF  TCEC0820-COD-FCC    NOT = CA-USD
      *           MOVE CA-30          TO TCEC0820-COD-RETURN
      *           MOVE CA-YES         TO SW-ERROR
      *       END-IF
      *    END-IF.
      *AZ0004DTC-FIN
           IF SW-ERROR = CA-NO
               IF TCEC0820-TIP-OFFERRATE NOT NUMERIC OR
                  TCEC0820-TIP-BIDRATE   NOT NUMERIC
                   MOVE CA-35 TO TCEC0820-COD-RETURN
                   MOVE CA-YES TO SW-ERROR
               ELSE
                   IF TCEC0820-TIP-OFFERRATE = CN-NUM-0 OR
                      TCEC0820-TIP-BIDRATE   = CN-NUM-0
                      MOVE CA-40 TO TCEC0820-COD-RETURN
                      MOVE CA-YES TO SW-ERROR
                   END-IF
               END-IF
           END-IF.
      *
      ******************************************************************
      *.PN GENERAL-PROCESS.                                            *
      ******************************************************************
       GENERAL-PROCESS.

      *BAZ0001-I
           IF SW-PAIS-MEX
               IF SW-CHANN-06
                   PERFORM BUSCA-SPREAD-XSUC
               ELSE
                   PERFORM BUSCA-BRANCH
      *
                   IF SW-ERROR = CA-NO
                       PERFORM BUSCA-REGION
                   END-IF
               END-IF
           ELSE
               PERFORM BUSCA-BRANCH
      *
               IF SW-ERROR = CA-NO
                   PERFORM BUSCA-REGION
               END-IF
           END-IF
      *BAZ0001-I

           IF SW-ERROR = CA-NO
      *.A.MS AZ0002DTC  I  JASL
      *       TCTC4041-FACTOR-C  > ZEROS   AND
      *       TCTC4041-FACTOR-V  > ZEROS
      *
      *       COMPUTE TCEC0820-OFFERRATE =
      *           TCEC0820-TIP-OFFERRATE -
      *          (TCEC0820-TIP-OFFERRATE * TCTC4041-FACTOR-V)
      *       COMPUTE TCEC0820-BIDRATE   =
      *           TCEC0820-TIP-BIDRATE   +
      *          (TCEC0820-TIP-BIDRATE   * TCTC4041-FACTOR-C)
      *       COMPUTE TCEC0820-FIXRATE   =
      *              (TCEC0820-OFFERRATE +
      *               TCEC0820-BIDRATE)  / 2
      *REMR-INI
      *AZ0005DTC-INI
      *AZ0006DTC-INI
      *       IF TCEC0820-COD-ENTITY <> '0127' AND '0019'
              IF TCEC0820-COD-ENTITY <> '0127' AND '0019' AND '0047'

      *AZ0006DTC-FIN

              COMPUTE VN-TC-VENTA  ROUNDED = TCEC0820-TIP-OFFERRATE *
                                             TCTC4041-FACTOR-V
              COMPUTE VN-TC-COMPRA ROUNDED = TCEC0820-TIP-BIDRATE   *
                                             TCTC4041-FACTOR-C

              ELSE
      *AZ0009DTC-INI
                IF  TCEC0820-COD-ENTITY = '0127' AND
                    TCTC4041-DATA (51:1) = 1

                COMPUTE VN-TC-VENTA  ROUNDED = TCEC0820-TIP-OFFERRATE -
                                             TCTC4041-FACTOR-V
                COMPUTE VN-TC-COMPRA ROUNDED = TCEC0820-TIP-BIDRATE   -
                                             TCTC4041-FACTOR-C
                ELSE

                COMPUTE VN-TC-VENTA  ROUNDED = TCEC0820-TIP-OFFERRATE +
                                             TCTC4041-FACTOR-V
                COMPUTE VN-TC-COMPRA ROUNDED = TCEC0820-TIP-BIDRATE   -
                                             TCTC4041-FACTOR-C
                END-IF
      *AZ0009DTC-FIN
              END-IF
      *AZ0005DTC-FIN
      *       COMPUTE VN-TC-VENTA =
      *           TCEC0820-TIP-OFFERRATE -
      *          (TCEC0820-TIP-OFFERRATE * TCTC4041-FACTOR-V) + .0049
      *       COMPUTE VN-TC-COMPRA =
      *           TCEC0820-TIP-BIDRATE   +
      *          (TCEC0820-TIP-BIDRATE   * TCTC4041-FACTOR-C) + .0049
      *REMR-FIN
              COMPUTE VN-TC-PROMED =
                     (VN-TC-VENTA +
                      VN-TC-COMPRA)  / 2

              MOVE VN-TC-VENTA   TO  TCEC0820-OFFERRATE
              MOVE VN-TC-COMPRA  TO  TCEC0820-BIDRATE
              MOVE VN-TC-PROMED  TO  TCEC0820-FIXRATE
      *.A.MS AZ0002DTC  F  JASL
      *@sard
              MOVE TCTC4041-FACTOR-V  TO TCEC0820-FAC-OFFERRATE
              MOVE TCTC4041-FACTOR-C  TO TCEC0820-FAC-BIDRATE
              MOVE TCTC4051-DATA(1:2) TO TCEC0820(113:2)
      *@sard
           END-IF.
      *
      ******************************************************************
      *.PN BUSCA-BRANCH.                                               *
      ******************************************************************
       BUSCA-BRANCH.
      *
           INITIALIZE TCTC4051
           INITIALIZE TCEC3000
      *
           MOVE CN-NUM-1               TO  TCEC3000-NUM-KEY
           MOVE CA-TBL-0405            TO  TCEC3000-COD-TABLE
           MOVE 'E'                    TO  TCEC3000-COD-BRNLNG
           MOVE TCEC0820-COD-ENTITY    TO  TCEC3000-COD-ENTITY
           MOVE TCEC0820-COD-BRANCH    TO  TCEC3000-KEY-CARD
      *
      *    EXEC CICS
      *         LINK PROGRAM ('TC7C3000')
      *              COMMAREA (TCEC3000)
      *    END-EXEC
      *@01SARD
           EXEC SQL
             SELECT DTA_TBLKEY
               INTO :T010-DTA-TBLKEY
               FROM TCDT010 with(nolock)
              WHERE COD_TABLE = :TCEC3000-COD-TABLE
                AND LNG_DATA  = :TCEC3000-COD-BRNLNG
                AND KEY_TABLE = :TCEC3000-KEY-CARD
                AND ENTITY    = :TCEC3000-COD-ENTITY
           END-EXEC.

           MOVE SQLCODE                TO SQL-VALUES

           IF SQL-88-OK
              MOVE T010-DTA-TBLKEY     TO TCTC4051-DATA
           ELSE
               IF SQL-88-NOT-FOUND
                   IF TCEC0820-COD-ENTITY = '0127'
                       PERFORM BUSCA-REGION-TEMP
                   ELSE
                       MOVE CA-40            TO TCEC0820-COD-RETURN
                       MOVE CA-YES           TO SW-ERROR
                       MOVE TCEC3000-ERR-DB2 TO TCEC0820-ERR-DB2
                   END-IF
               ELSE
                   MOVE CA-40                TO TCEC0820-COD-RETURN
                   MOVE CA-YES               TO SW-ERROR
                   MOVE TCEC3000-ERR-DB2     TO TCEC0820-ERR-DB2
               END-IF
           END-IF.
      *
      *    IF  TCEC3000-COD-RETURN1 = ZEROS
      *        MOVE  TCEC3000-RECCN      TO  TCTC4051-DATA
      *    ELSE
      *AZ0007DTC-INI
      *        IF TCEC3000-COD-RETURN1 = CA-10
      *AZ0008DTC-INI
      *            IF TCEC0820-COD-ENTITY = '0127'
      *                PERFORM BUSCA-REGION-TEMP
      *            ELSE
      *                MOVE CA-40            TO TCEC0820-COD-RETURN
      *                MOVE CA-YES           TO SW-ERROR
      *                MOVE TCEC3000-ERR-DB2 TO TCEC0820-ERR-DB2
      *            END-IF
      *AZ0008DTC-FIN
      *        ELSE
      *AZ0007DTC-FIN
      *        MOVE CA-40                TO TCEC0820-COD-RETURN
      *        MOVE CA-YES               TO SW-ERROR
      *        MOVE TCEC3000-ERR-DB2     TO TCEC0820-ERR-DB2
      *AZ0007DTC-INI
      *        END-IF
      *AZ0007DTC-FIN
      *    END-IF
      *    .
      *
      *AZ0007DTC-INI
      ******************************************************************
      *.PN BUSCA-REGION-TEMP                                           *
      ******************************************************************
       BUSCA-REGION-TEMP.
      *
           INITIALIZE VA-GPEC069-AREA
           MOVE 2                              TO E069-COD-OPTION
           MOVE 59                             TO E069-COD-CONTROL
           MOVE TCEC0820-COD-ENTITY           TO VA-QBEC999-GPEC069-AREA
      *
           EXEC CICS
               LINK PROGRAM('GP7C0690')
               COMMAREA (VA-GPEC069-AREA)
               NOHANDLE
           END-EXEC.
      *
            IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
               MOVE CA-99                      TO TCEC0820-COD-RETURN
               MOVE 'GPE0405'                  TO TCEC0820-DES-TABLE
               MOVE 'CICS. GP7C0690'           TO TCEC0820-DTA-SQLERRM
               PERFORM 3000-END
            END-IF.

           EVALUATE E069-COD-MOD-RTN
             WHEN CA-00
                   MOVE E069-FILL1             TO TCTC4051-DATA
             WHEN CA-99
                   MOVE CA-99                  TO TCEC0820-COD-RETURN
                   MOVE CA-YES                 TO SW-ERROR
                   MOVE 'GAE0075'              TO TCEC0820-DES-TABLE
                   MOVE E069-SQLCODE           TO TCEC0820-SQLCODE
                   MOVE E069-COD-SQL-MSG       TO TCEC0820-SQLERRM
             WHEN OTHER
                   MOVE CA-99                  TO TCEC0820-COD-RETURN
                   MOVE CA-YES                 TO SW-ERROR
                   MOVE E069-TXT-MESSAGE       TO TCEC0820-DES-TABLE
                   MOVE E069-SQLCODE           TO TCEC0820-SQLCODE
                   MOVE E069-COD-SQL-MSG       TO TCEC0820-SQLERRM
           END-EVALUATE.
      *
      *AZ0007DTC-FIN
      ******************************************************************
      *.PN BUSCA-REGION                                                *
      ******************************************************************
       BUSCA-REGION.
      *
           INITIALIZE TCTC4041.
           INITIALIZE TCEC3000.
      *
           MOVE CN-NUM-1            TO  TCEC3000-NUM-KEY
           MOVE CA-TBL-0404         TO  TCEC3000-COD-TABLE
           MOVE 'E'                 TO  TCEC3000-COD-BRNLNG
           MOVE TCEC0820-COD-ENTITY TO  TCEC3000-COD-ENTITY
      *AZ0004DTC-INI
           MOVE TCTC4051-CLAVE-R    TO  TCEC3000-KEY-CARD
           MOVE TCEC0820-COD-FCC    TO  TCEC3000-KEY-CARD(3:3)
      *AZ0004DTC-FIN
      *@01SARD
      *    EXEC CICS
      *         LINK PROGRAM ('TC7C3000')
      *              COMMAREA (TCEC3000)
      *    END-EXEC

           EXEC SQL
             SELECT DTA_TBLKEY
               INTO :T010-DTA-TBLKEY
               FROM TCDT010 with(nolock)
              WHERE COD_TABLE = :TCEC3000-COD-TABLE
                AND LNG_DATA  = :TCEC3000-COD-BRNLNG
                AND KEY_TABLE = :TCEC3000-KEY-CARD
                AND ENTITY    = :TCEC3000-COD-ENTITY
           END-EXEC.

           MOVE SQLCODE                TO SQL-VALUES

      *
      *    IF TCEC3000-COD-RETURN1 = '00'
      *       MOVE TCEC3000-RECCN       TO TCTC4041-DATA
           IF SQL-88-OK
              MOVE T010-DTA-TBLKEY     TO TCTC4041-DATA
      *@01SARD
           ELSE
              MOVE CA-45              TO TCEC0820-COD-RETURN
              MOVE CA-YES             TO SW-ERROR
              MOVE TCEC3000-ERR-DB2   TO TCEC0820-ERR-DB2
           END-IF.
      *
      *BAZ0001-I
      ******************************************************************
      *.PN BUSCA-REGION-NSEG                                           *
      ******************************************************************
       BUSCA-SPREAD-XSUC.
      *
           INITIALIZE                  TCTC4041
                                       TCEC3000
      *
           MOVE CN-NUM-1               TO TCEC3000-NUM-KEY
           MOVE CA-TBL-0404            TO TCEC3000-COD-TABLE
           MOVE 'E'                    TO TCEC3000-COD-BRNLNG
           MOVE TCEC0820-COD-ENTITY    TO TCEC3000-COD-ENTITY
           MOVE TCEC0820-COD-BRANCH-A  TO TCEC3000-KEY-CARD
      *    MOVE TCTC4051-CLAVE-R       TO TCEC3000-KEY-CARD
           MOVE TCEC0820-COD-FCC       TO TCEC3000-KEY-CARD(5:3)
      *
           EXEC SQL
             SELECT DTA_TBLKEY
               INTO :T010-DTA-TBLKEY
               FROM TCDT010 with(nolock)
              WHERE COD_TABLE = :TCEC3000-COD-TABLE
                AND LNG_DATA  = :TCEC3000-COD-BRNLNG
                AND KEY_TABLE = :TCEC3000-KEY-CARD
                AND ENTITY    = :TCEC3000-COD-ENTITY
           END-EXEC.
      *
           MOVE SQLCODE                TO SQL-VALUES
      *
           IF SQL-88-OK
               MOVE T010-DTA-TBLKEY    TO TCTC4041-DATA
           ELSE
               PERFORM BUSCA-BRANCH
               IF SW-ERROR = CA-NO
                   PERFORM BUSCA-REGION
               END-IF
           END-IF
           .
      *BAZ0001-F
      ******************************************************************
      *.PN 3000-END.                                                   *
      ******************************************************************
       3000-END.
      *
           EXEC CICS
             RETURN
           END-EXEC.
      *
