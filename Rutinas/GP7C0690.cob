      * GP7C0690: CONTROL TABLE ACCESS MODULE                          *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    GP7C0690.
      *
       AUTHOR.        ALNOVA TECHNOLOGIES CORPORATION.
      *
       DATE-WRITTEN.  0001-01-01.
      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *.MC  @BA0001    LGAMA   04/09/03 SE CONTROLA ERROR EN CALL'S    *
      *.MC  @BAD00002  E.MEJIA 09-09-03 SE AGREGA CIERRE DE CURSOR     *
      *                                 CUANDO HAY ERROR EN RUTINA     *
      *   @EMZG003GP  EMARQUEZ 08/10/03 MEJORAS AL TRATAMIENTO DE      *
      *                                 ERRORES                        *
      *                                                                *
      *.JG  @BA0004    JGONZAL 15-04-05 SE LE ENVIA LA BANDERA DE EL   *
      *                                 TIPO DE CAMBIDE DIVISA 'T'     *
      *                                 (TRASFERENCIA)                 *
      *     @BA0005    ACRUZ   04-10-05 AGREGA CODIGO DE ERROR DE      *
      *                                 RETORNO                        *
      * @BA0006       ACRUZ    14/12/05 MANEJO DE BLOQUEO DE TABLA     *
      * @BA0007     MCALVILLO  28/12/05 QUITAR LOG                     *
      * @BA0008     VCORTESM   23/04/07 MANEJO DE CODIGOS DE RETORNO   *
      * @BA0009     ACRUZC     11/09/07 SE REEMPLAZA CODIGO CURRENTIME *
      ******************************************************************
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
      *
       OBJECT-COMPUTER. IBM-370.
      *
       SPECIAL-NAMES.
      *
           DECIMAL-POINT IS COMMA.
      *
      ******************************************************************
      *                       DATA DIVISION                            *
      ******************************************************************
       DATA DIVISION.
      *
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
            COPY QAWCSQL.
       01  CA-QA8CARD                      PIC X(7)    VALUE 'QA8CARD'.
       01  CA-TC9C2810                     PIC X(8)    VALUE 'TC9C2810'.
      *
       01  VA-TXT-STT-WS.
           05 CA-TXT-STT-WS                PIC X(41)   VALUE
             '*** START WORKING STORAGE GP7C0690 ***'.
      *
           COPY QCWCL20.
      *
       01  VA-CONSTANTS.
      *
           05 CA-COD-RTN-OK                PIC XX      VALUE '00'.
           05 CA-COD-RTN-20                PIC XX      VALUE '20'.
      *@BA0005 - I
           05 CA-COD-RTN-30                PIC XX      VALUE '30'.
      *@BA0005 - F
           05 CA-COD-RTN-99                PIC XX      VALUE '99'.
           05 CA-COD-RTN-90                PIC XX      VALUE '90'.
      *
           05 CA-GPB0069                   PIC X(7)    VALUE 'GPB0069'.
           05 CA-GAE0244                   PIC X(7)    VALUE 'GAE0244'.

           05 CA-GPB0070                   PIC X(7)    VALUE 'GPB0070'.
           05 CA-GPB0071                   PIC X(7)    VALUE 'GPB0071'.
           05 CA-GPB0003                   PIC X(7)    VALUE 'GPB0003'.
           05 CA-GPB0107                   PIC X(7)    VALUE 'GPB0107'.
           05 CA-GP7C0690                  PIC X(8)    VALUE 'GP7C0690'.
      *
           05 CA-TBL-21                    PIC X(8)    VALUE 'GPDT021'.
      *@BA0009
          05 CA-PUNTO                     PIC X(1)   VALUE '.'.
          05 CA-DOSP                      PIC X(1)   VALUE ':'.
          05 CA-GUION                     PIC X(1)   VALUE  '-'.
      *@BA0009

      *
           05  CN-OPT-1                    PIC 9     COMP-3
                                   VALUE 1.
           05  CN-OPT-2                    PIC 9     COMP-3
                                   VALUE 2.
           05  CN-OPT-3                    PIC 9     COMP-3
                                   VALUE 3.
           05  CN-OPT-4                    PIC 9     COMP-3
                                   VALUE 4.
           05  CN-5                PIC 9  COMP-3 VALUE 5.
      *@BA0008-INI
           05  WS-TOT-UPD          PIC 9  COMP-3.
           05  WS-CONTADOR         PIC S9(07) COMP-3.
      *@BA0008-FIN
      *
           05  VN-NULL                     PIC S9(4) COMP
                                   VALUE 0.
           05  VN-NULL1                    PIC S9(4) COMP
                                   VALUE 0.
      *.MC.S @BA0001DGP
           05 CN-CONTROL-ERR               PIC 9(2)    VALUE 98.
      *.MC.E @BA0001DGP
      *
      *@BA0009
       01 WS-TMP-STP                        PIC x(26).
       01 WS-VARPRU.
          05 VA-FECHA.
            10 VN-YY4                       PIC 9(4).
            10 FILLER                       PIC x(1) value '-'.
            10 VN-MM                        PIC 9(2).
            10 FILLER                       PIC x(1) value '-'.
            10 VN-DD                        PIC 9(2).
      *
          05 FILLER                       PIC x(1) value '-'.
          05 VA-HORA.
            10 VN-HORA                      PIC 9(2).
            10 FILLER                      PIC x(1) value '.'.
            10 VN-MIN                       PIC 9(2).
            10 FILLER                      PIC x(1) value '.'.
            10 VN-SEG                       PIC 9(2).
            10 FILLER                      PIC x(1) value '.'.
            10 VN-MS                        PIC 9(3).
            10 VN-MMS                       PIC 9(3).

       01 RANDOM-NUMBER                 PIC V999 VALUE ZERO.
       01 VN-RANDOM                        PIC 9(5) VALUE ZERO.
       01 WS-TMP-STP1                        PIC x(23).
      *@BA0009


       01  VA-SWITCHES.
           05 SW-OPN-CUR                   PIC X(1)    VALUE 'N'.
              88 SW-OPN-CUR-YES                        VALUE 'S'.
              88 SW-OPN-CUR-NO                         VALUE 'N'.
      *
           05 SW-UPD                       PIC X(1)    VALUE 'N'.
              88 SW-UPD-YES                            VALUE 'S'.
              88 SW-UPD-NO                             VALUE 'N'.
      *
       01  VA-WORK-VAR.
           05  VN-AMT-CTRL-TBL             PIC S9(12)V9(3)
                                   VALUE ZEROES.
           05  VN-FACTOR                   PIC S9(6)V9(6)
                                   VALUE ZEROES.
           05 VA-CTR-ETY           PIC S9(7) COMP-3.
           05 VA-CTR-REP           PIC 9 COMP-3.
      *
           EXEC SQL
             INCLUDE GPGT021
           END-EXEC.
      *
           EXEC SQL
             INCLUDE GPVC021
           END-EXEC.
      *
      *    EXEC SQL
      *         DECLARE GPDC0210 CURSOR FOR
      *          SELECT T021_CTR_ETY,
      *                 T021_AMT_ENTRY,
      *                 T021_PER_CONTROL,
      *                 T021_TYP_ETY_VALU,
      *                 T021_DAT_CTRL_TBL,
      *                 T021_FCC,
      *                 T021_FILL_1
      *            FROM GPDT021 with(index=AZOGP021)
      *         WHERE T021_ENT_ORIGIN = :V021-ENT-ORIGIN AND
      *               T021_NUM_SEQUENCE = :V021-NUM-SEQUENCE
      *           FOR UPDATE OF T021_CTR_ETY, T021_DAT_CTRL_TBL,
      *               T021_STP_LAST_MOD
      *    END-EXEC.
      *
       01  VA-TXT-END-WS.
           05 CA-TXT-END-WS                PIC X(41)   VALUE

             '***  END WORKING STORAGE GP7C0690 ***'.
      *** @EMZG003GP           08-10-2003       I
       01 VA-QGECABC.
            COPY QGECABC.
      *** @EMZG003GP           08-10-2003       F
      *
           COPY TCEC2810.
      *
           COPY QRECARD.
      *
      ******************************************************************
      *                      LINKAGE SECTION                           *
      ******************************************************************
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
      *
           COPY GPEC069.
          COPY QBEC999.
      *
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM R1-START-PROGRAM.
      *
           PERFORM R2-PROGRAM-PROCESS.
      *
           PERFORM R3-END-PROGRAM.
      *
      ******************************************************************
      *.PN R1-START-PROGRAM.                                           *
      ******************************************************************
       R1-START-PROGRAM.
      *
           INITIALIZE QAWCSQL.
           INITIALIZE SQLCA.
      *
           INITIALIZE GPVC021.
      *
           MOVE E069-COD-CONTROL TO V021-NUM-SEQUENCE.
           MOVE ZERO TO VA-CTR-REP
           MOVE QBEC999-COD-ENTITY TO V021-ENT-ORIGIN.
      
      *@BA0008-INI
           PERFORM C2-OBTENER-VALOR.
      *@BA0008-FIN
      *
      ******************************************************************
      *.PN R2-PROGRAM-PROCESS.                                         *
      ******************************************************************
       R2-PROGRAM-PROCESS.
      *
           IF  E069-COD-OPTION NOT = CN-OPT-1
           AND E069-COD-OPTION NOT = CN-OPT-2
           AND E069-COD-OPTION NOT = CN-OPT-3
           AND E069-COD-OPTION NOT = CN-OPT-4
              MOVE CA-COD-RTN-20 TO E069-COD-MOD-RTN
              MOVE CA-GPB0069 TO E069-TXT-MESSAGE
      *emz
              MOVE CA-GP7C0690       TO ABC-DES-PROG
              MOVE E069-COD-MOD-RTN  TO ABC-REFERENCE1(1:2)
              MOVE E069-TXT-MESSAGE  TO ABC-OBJECT-ERROR
              MOVE 'R2-PROGRAM-PROC' TO ABC-REFERENCE1(4:16)
      *       PERFORM 999999-ABEND
              PERFORM C99-EXIT

      *emz
              EXEC CICS
                 RETURN
              END-EXEC
           END-IF.
      *
           IF  E069-FCC NOT EQUAL QBEC999-COD-OCCCTRY
           AND E069-FCC NOT EQUAL QBEC999-COD-NRESFCC
           AND E069-FCC NOT EQUAL QBEC999-COD-RCC
           AND E069-FCC NOT EQUAL SPACES
               INITIALIZE E069-FCC
           END-IF
      *
      *@BA0006 - INI
      *    PERFORM C4-OPEN-CURSOR.
      *
      *    MOVE CA-TBL-21 TO E069-DES-DB2-TBL.
      *    PERFORM DB2CHECK.
      *
      *    PERFORM C5-FETCH-CURSOR.
      *
      *@BA0008-INI
      *    PERFORM UNTIL SW-UPD-YES OR VA-CTR-REP EQUAL CN-5
           PERFORM UNTIL SW-UPD-YES OR VA-CTR-REP EQUAL WS-TOT-UPD
      *@BA0008-FIN
           INITIALIZE SQLCODE

           EXEC SQL
                 SELECT T021_CTR_ETY,
                        T021_AMT_ENTRY,
                        T021_PER_CONTROL,
                        T021_TYP_ETY_VALU,
                        T021_DAT_CTRL_TBL,
                        T021_FCC,
                        T021_FILL_1
                 INTO   :V021-CTR-ETY,
                        :V021-AMT-ENTRY,
                        :V021-PER-CONTROL,
                        :V021-SW-TYP-ETY-VALU,
                        :V021-DAT-CTRL-TBL:VN-NULL,
                        :V021-FCC,
                        :V021-FILL-1:VN-NULL1
                   FROM GPDT021 with(index=AZOGP021,nolock)
                WHERE T021_ENT_ORIGIN = :V021-ENT-ORIGIN AND
                      T021_NUM_SEQUENCE = :V021-NUM-SEQUENCE
           END-EXEC

           ADD 1 TO VA-CTR-REP

           SET SW-OPN-CUR-YES TO TRUE

           MOVE SQLCODE TO SQL-VALUES

      *    PERFORM DB2CHECK.

      *@BA0006 - FIN

      *@BA0005 - I
           IF SQL-88-OK
               IF V021-FCC EQUAL SPACES AND V021-SW-TYP-ETY-VALU = 'I'
                   MOVE CA-COD-RTN-30 TO E069-COD-MOD-RTN
                   MOVE 'GPB2129'   TO E069-TXT-MESSAGE
      *A.PR.S @BAD00002 ERMG
      *            PERFORM C6-CLOSE-CURSOR
                   SET SW-OPN-CUR-NO TO TRUE
      *A.PR.E
                   PERFORM C99-EXIT
               END-IF
               IF V021-SW-TYP-ETY-VALU = 'I' AND E069-FCC = SPACES
                   MOVE CA-COD-RTN-30 TO E069-COD-MOD-RTN
                   MOVE 'GPB2130'   TO E069-TXT-MESSAGE
      *A.PR.S @BAD00002 ERMG
      *            PERFORM C6-CLOSE-CURSOR
                   SET SW-OPN-CUR-NO TO TRUE
      *A.PR.E
                   PERFORM C99-EXIT
               END-IF
           ELSE
      *        PERFORM C6-CLOSE-CURSOR
               SET SW-OPN-CUR-NO TO TRUE
               PERFORM DB2CHECK
               MOVE CA-COD-RTN-99           TO E069-COD-MOD-RTN
      *@BA0008-INI         
      *        MOVE SQLERRM                 TO E069-TXT-DTA-SQL
               MOVE 'SELECT GPDT012'        TO E069-TXT-DTA-SQL
      *@BA0008-INI
               MOVE SQLCODE                 TO E069-SQLCODE
               PERFORM C99-EXIT
           END-IF
      *@BA0005 - F

           EVALUATE E069-COD-OPTION
            WHEN CN-OPT-1
            WHEN CN-OPT-3
      * @BA0007 -INI
      *       IF SQL-88-OK
                 IF NOT V021-SW-ETY-COUNTER
                    MOVE CA-COD-RTN-20 TO E069-COD-MOD-RTN
                    MOVE CA-GPB0070 TO E069-TXT-MESSAGE
                    IF SW-OPN-CUR-YES
      *emz
                    MOVE CA-GP7C0690       TO ABC-DES-PROG
                    MOVE E069-COD-MOD-RTN  TO ABC-REFERENCE1(1:2)
                    MOVE E069-TXT-MESSAGE  TO ABC-OBJECT-ERROR
                    MOVE 'R2-CLOSE CURSOR' TO ABC-REFERENCE1(4:16)
      *             PERFORM 999999-ABEND
      *emz
      *                PERFORM C6-CLOSE-CURSOR
                       SET SW-OPN-CUR-NO TO TRUE
      *                PERFORM DB2CHECK
                       PERFORM C99-EXIT
                    END-IF
                 ELSE
                    PERFORM C1-INFORM-OUTPUT
                    PERFORM C2-WRITE-COUNTER
                 END-IF
      *       ELSE
      *          MOVE CA-COD-RTN-20 TO E069-COD-MOD-RTN
      *          MOVE CA-GPB0070 TO E069-TXT-MESSAGE
      *          IF SW-OPN-CUR-YES
      *emz
      *          MOVE CA-GP7C0690       TO ABC-DES-PROG
      *          MOVE E069-COD-MOD-RTN  TO ABC-REFERENCE1(1:2)
      *          MOVE E069-TXT-MESSAGE  TO ABC-OBJECT-ERROR
      *          MOVE 'R2-CLOSE CURSOR' TO ABC-REFERENCE1(4:16)
      *
      *          PERFORM 999999-ABEND
      *          PERFORM C99-EXIT
      *
      *emz
      *             PERFORM C6-CLOSE-CURSOR
      *             SET SW-OPN-CUR-NO TO TRUE
      *             PERFORM DB2CHECK
      *          END-IF
      *          PERFORM C99-EXIT
      *       END-IF
      * @BA0007 - FIN

            WHEN 2
              IF SQL-88-OK
                 IF V021-AMT-ENTRY = 0
                    PERFORM C1-INFORM-OUTPUT
                    SET SW-UPD-YES TO TRUE
                 ELSE
                    IF E069-FCC = (SPACES OR LOW-VALUES OR
                                        QBEC999-COD-NRESFCC)
                       MOVE QBEC999-COD-OCCCTRY    TO E069-FCC
                    END-IF
                    IF E069-FCC = V021-FCC
                       PERFORM C1-INFORM-OUTPUT
                       SET SW-UPD-YES TO TRUE
                    ELSE
                       PERFORM C0-AMOUNT-CONVERSION
                       SET SW-UPD-YES TO TRUE
                    END-IF
                 END-IF
              ELSE
                 MOVE CA-COD-RTN-20 TO E069-COD-MOD-RTN
                 MOVE CA-GPB0070 TO E069-TXT-MESSAGE
                 IF SW-OPN-CUR-YES
      *emz
                 MOVE CA-GP7C0690       TO ABC-DES-PROG
                 MOVE E069-COD-MOD-RTN  TO ABC-REFERENCE1(1:2)
                 MOVE E069-TXT-MESSAGE  TO ABC-OBJECT-ERROR
                 MOVE 'R2-CLOSE CURSOR' TO ABC-REFERENCE1(4:16)
      *          PERFORM 999999-ABEND
                 PERFORM C99-EXIT

      *emz
      *             PERFORM C6-CLOSE-CURSOR
                    SET SW-OPN-CUR-NO TO TRUE
      *             PERFORM DB2CHECK
                 END-IF
                 PERFORM C99-EXIT
              END-IF

            WHEN 4
              IF SQL-88-OK
                 IF NOT V021-SW-ETY-DAT
                    MOVE CA-COD-RTN-20 TO E069-COD-MOD-RTN
                    MOVE CA-GPB0071 TO E069-TXT-MESSAGE
                    IF SW-OPN-CUR-YES
      *emz
                    MOVE CA-GP7C0690       TO ABC-DES-PROG
                    MOVE E069-COD-MOD-RTN  TO ABC-REFERENCE1(1:2)
                    MOVE E069-TXT-MESSAGE  TO ABC-OBJECT-ERROR
                    MOVE 'R2-CLOSE CURSOR' TO ABC-REFERENCE1(4:16)
      *             PERFORM 999999-ABEND
                    PERFORM C99-EXIT
      *emz
      *                PERFORM C6-CLOSE-CURSOR
                       SET SW-OPN-CUR-NO TO TRUE
      *                PERFORM DB2CHECK
                    END-IF
                    PERFORM C99-EXIT
                 ELSE
                    PERFORM C3-WRITE-DATE
                    SET SW-UPD-YES TO TRUE
                 END-IF

              ELSE
                 MOVE CA-COD-RTN-20 TO E069-COD-MOD-RTN
                 MOVE CA-GPB0070 TO E069-TXT-MESSAGE
                 IF SW-OPN-CUR-YES
      *emz
                   MOVE CA-GP7C0690       TO ABC-DES-PROG
                   MOVE E069-COD-MOD-RTN  TO ABC-REFERENCE1(1:2)
                   MOVE E069-TXT-MESSAGE  TO ABC-OBJECT-ERROR
                   MOVE 'R2-CLOSE CURSOR' TO ABC-REFERENCE1(4:16)
                   PERFORM C99-EXIT
      *            PERFORM 999999-ABEND
      *emz
      *            PERFORM C6-CLOSE-CURSOR
                   SET SW-OPN-CUR-NO TO TRUE
      *            PERFORM DB2CHECK
                 END-IF
                 PERFORM C99-EXIT
              END-IF
           END-EVALUATE
           END-PERFORM.

           SET SW-OPN-CUR-NO TO TRUE
      *
      *    PERFORM C6-CLOSE-CURSOR.
      *@BA0008-INI
      *     IF VA-CTR-REP EQUAL CN-5
            IF VA-CTR-REP EQUAL WS-TOT-UPD
      *@BA0008-FIN
               MOVE CA-COD-RTN-90 TO E069-COD-MOD-RTN
               MOVE CA-GAE0244 TO E069-TXT-MESSAGE
               PERFORM C99-EXIT
           END-IF.
      *
      *    PERFORM DB2CHECK.
      *
      ******************************************************************
      *.PN R3-END-PROGRAM.                                             *
      ******************************************************************
       R3-END-PROGRAM.
      *
           MOVE CA-COD-RTN-OK TO E069-COD-MOD-RTN.
      *A.PR.S @BAD00002 ERMG
      *    PERFORM C6-CLOSE-CURSOR
      *A.PR.E
           EXEC CICS
              RETURN
           END-EXEC.
      *
      ******************************************************************
      *.PN R4-END-AB.                                                  *
      ******************************************************************
       R4-END-AB.
      *
      *A.PR.S @BAD00002 ERMG
      *    PERFORM C6-CLOSE-CURSOR
      *A.PR.E
           EXEC CICS
              RETURN
           END-EXEC.
      *
      ******************************************************************
      *.PN C0-AMOUNT-CONVERSION.                                       *
      ******************************************************************
       C0-AMOUNT-CONVERSION.
      *
           INITIALIZE TCEC2810.
      *
           MOVE '1'                       TO TCEC2810-OPTION.
           MOVE QBEC999-COD-OCCCTRY       TO TCEC2810-COD-FCCSWIFT.
           MOVE QBEC999-LNG-DATA          TO TCEC2810-FLG-FCCB
      *.JG.E @BA0004
           MOVE 'T'                       TO TCEC2810-FLG-FCCB.
      *.JG.S @BA0004
      *
           CALL CA-TC9C2810 USING TCEC2810
                                      QBEC999
      *.MC.S @BA0001DGP
             ON EXCEPTION
               MOVE CN-CONTROL-ERR TO TCEC2810-COD-RETURN
           END-CALL
      *.MC.E @BA0001DGP
      *
           EVALUATE  TCEC2810-COD-RETURN
               WHEN '00'
                   MOVE TCEC2810-TB-FCC-FIXEXCHRATE(1)  TO VN-FACTOR
               WHEN '20'
                   MOVE TCEC2810-COD-RETURN     TO E069-COD-MOD-RTN
                   MOVE 'TCDT2810'        TO E069-DES-DB2-TBL
                   MOVE TCEC2810-SQLCODE      TO E069-SQLCODE
      *EM
                   MOVE 'GP7C0690'       TO ABC-DES-PROG
                   MOVE E069-DES-DB2-TBL TO ABC-OBJECT-ERROR
                   MOVE E069-SQLCODE     TO ABC-SQLCODE
                   MOVE E069-COD-MOD-RTN TO ABC-REFERENCE1
      *            PERFORM 999999-ABEND

      *EM
                   PERFORM R4-END-AB
               WHEN '99'
                   MOVE TCEC2810-COD-RETURN     TO E069-COD-MOD-RTN
                   MOVE 'TCDT2810'        TO E069-DES-DB2-TBL
                   MOVE TCEC2810-SQLCODE      TO E069-SQLCODE
      *EM
                    MOVE 'GP7C0690'       TO ABC-DES-PROG
                    MOVE E069-DES-DB2-TBL TO ABC-OBJECT-ERROR
                    MOVE E069-SQLCODE     TO ABC-SQLCODE
                    MOVE E069-COD-MOD-RTN TO ABC-REFERENCE1
      *             PERFORM 999999-ABEND
                    PERFORM C99-EXIT
      *EM
                   PERFORM R4-END-AB
               WHEN OTHER
                   MOVE TCEC2810-COD-RETURN     TO E069-COD-MOD-RTN
      *EM
                   MOVE 'GP7C0690'       TO ABC-DES-PROG
                   MOVE E069-COD-MOD-RTN TO ABC-REFERENCE1(1:2)
                  MOVE 'CO-AMOUNT-CONV' TO ABC-REFERENCE1(4:16)
      *            PERFORM 999999-ABEND
                   PERFORM C99-EXIT
      *EM
                   PERFORM R4-END-AB
           END-EVALUATE.
      *
           IF  E069-FCC = QBEC999-COD-OCCCTRY
           AND V021-FCC = QBEC999-COD-RCC
               COMPUTE VN-AMT-CTRL-TBL = V021-AMT-ENTRY * VN-FACTOR
               INITIALIZE  QRECARD
               MOVE  VN-AMT-CTRL-TBL           TO WARD-QRAMOUNT
               MOVE  QBEC999-COD-OCCCTRY            TO WARD-QRCURREN
               MOVE  'A'                   TO WARD-QRFLGHND
               CALL CA-QA8CARD   USING QRECARD
                                      QBEC999
      *.MC.S @BA0001DGP
             ON EXCEPTION
               MOVE CN-CONTROL-ERR TO WARD-QRCRETUR
           END-CALL
      *.MC.E @BA0001DGP
      *
               EVALUATE  WARD-QRCRETUR
                   WHEN '00'
                       PERFORM C1-INFORM-OUTPUT-CONV
                   WHEN OTHER
                       MOVE WARD-QRCRETUR TO E069-COD-MOD-RTN
      *EM
                       MOVE 'GP7C0690'       TO ABC-DES-PROG
                       MOVE E069-COD-MOD-RTN TO ABC-REFERENCE1(1:2)
                       MOVE 'CO-AMOUNT-CONV' TO ABC-REFERENCE1(4:16)
      *                PERFORM 999999-ABEND
                       PERFORM C99-EXIT
      *EM
                       PERFORM R4-END-AB
               END-EVALUATE
           END-IF
      *
           IF  E069-FCC = QBEC999-COD-RCC
           AND V021-FCC = QBEC999-COD-OCCCTRY
               COMPUTE VN-AMT-CTRL-TBL = V021-AMT-ENTRY / VN-FACTOR
               INITIALIZE  QRECARD
               MOVE  VN-AMT-CTRL-TBL           TO WARD-QRAMOUNT
               MOVE  QBEC999-COD-RCC                TO WARD-QRCURREN
               MOVE  'A'                   TO WARD-QRFLGHND
               CALL CA-QA8CARD   USING QRECARD
                                      QBEC999
      *.MC.S @BA0001DGP
             ON EXCEPTION
               MOVE CN-CONTROL-ERR TO WARD-QRCRETUR
           END-CALL
      *.MC.E @BA0001DGP
      *
               EVALUATE  WARD-QRCRETUR
                   WHEN '00'
                       PERFORM C1-INFORM-OUTPUT-CONV
                   WHEN OTHER
                       MOVE WARD-QRCRETUR TO E069-COD-MOD-RTN
      *EM
                       MOVE 'GP7C0690'       TO ABC-DES-PROG
                       MOVE E069-COD-MOD-RTN TO ABC-REFERENCE1(1:2)
                       MOVE 'CO-AMOUNT-CONV' TO ABC-REFERENCE1(4:16)
      *                PERFORM 999999-ABEND
                       PERFORM C99-EXIT
      *EM
                       PERFORM R4-END-AB
               END-EVALUATE
           END-IF.
      *
      ******************************************************************
      *.PN C1-INFORM-OUTPUT.                                           *
      ******************************************************************
       C1-INFORM-OUTPUT.
      *
           INITIALIZE E069-DTA-OUT-GPEC069.
      *
           MOVE V021-CTR-ETY TO E069-NUM-CTR-ETY
                                VA-CTR-ETY
           MOVE V021-AMT-ENTRY TO E069-AMT-ENTRY.
           MOVE V021-PER-CONTROL TO E069-PER-CONTROL.
           MOVE V021-DAT-CTRL-TBL TO E069-DAT-CTRL-TBL.
           MOVE V021-FILL-1 TO E069-FILL1.
      
      *@BA0008-INI
      ******************************************************************
      *C2-OBTENER-VALOR.                                               *
      * SE OBTIENE EL CODIGO PARA NUMERO DE VECES EN EL BUCLE          *
      ******************************************************************      
       C2-OBTENER-VALOR.
           
           EXEC SQL
              SELECT T021_CTR_ETY
                INTO :WS-CONTADOR
              FROM GPDT021 with(index=AZOGP021,nolock)
              WHERE T021_ENT_ORIGIN = :QBEC999-COD-ENTITY AND
                  T021_NUM_SEQUENCE = '434'
           END-EXEC  

           MOVE SQLCODE TO SQL-VALUES                             

           EVALUATE TRUE                                          
               WHEN SQL-88-OK
                   MOVE WS-CONTADOR          TO WS-TOT-UPD
               WHEN OTHER                                         
                MOVE CA-COD-RTN-99           TO E069-COD-MOD-RTN  
                MOVE 'SELECT COD 434'        TO E069-TXT-DTA-SQL        
                MOVE SQLCODE                 TO E069-SQLCODE
                PERFORM C99-EXIT
            END-EVALUATE.                                    
      *@BA0008-FIN
      *
      ******************************************************************
      *.PN C1-INFORM-OUTPUT-CONV.                                      *
      ******************************************************************
       C1-INFORM-OUTPUT-CONV.
      *
           INITIALIZE E069-DTA-OUT-GPEC069.
      *
           MOVE V021-CTR-ETY TO E069-NUM-CTR-ETY.
           MOVE WARD-QRRNDAMT TO E069-AMT-ENTRY.
           MOVE V021-PER-CONTROL TO E069-PER-CONTROL.
           MOVE V021-DAT-CTRL-TBL TO E069-DAT-CTRL-TBL.
      *
      ******************************************************************
      *.PN C2-WRITE-COUNTER.                                           *
      ******************************************************************
       C2-WRITE-COUNTER.
      *
           IF E069-COD-OPTION = CN-OPT-1
              ADD 1 TO V021-CTR-ETY
           ELSE
              ADD 2 TO V021-CTR-ETY
           END-IF.
      *
      *@BA0009
           PERFORM 8000-TMP-STP.
      *@BA0009

           EXEC SQL
                UPDATE GPDT021
                SET T021_CTR_ETY = :V021-CTR-ETY,
                    T021_STP_LAST_MOD = :V021-STP-LAST-MOD
                WHERE T021_ENT_ORIGIN = :V021-ENT-ORIGIN AND
                      T021_NUM_SEQUENCE = :V021-NUM-SEQUENCE AND
                      T021_CTR_ETY = :VA-CTR-ETY
           END-EXEC.
      *
      *    PERFORM DB2CHECK
           MOVE SQLCODE TO SQL-VALUES

      *@BA0005 - I
           EVALUATE TRUE
               WHEN SQL-88-OK
      *@BA0006 - INI
                   SET SW-UPD-YES TO TRUE
               WHEN  SQL-88-NOT-FOUND
                   SET SW-UPD-NO TO TRUE
               WHEN OTHER
                MOVE CA-COD-RTN-99           TO E069-COD-MOD-RTN
      *@BA0008-INI          
      *         MOVE SQLERRM                 TO E069-TXT-DTA-SQL
                MOVE 'UPDATE GPDT012'        TO E069-TXT-DTA-SQL
      *@BA0008-FIN
                MOVE SQLCODE                 TO E069-SQLCODE
                PERFORM C99-EXIT
            END-EVALUATE
      *@BA0006 - FIN
           .
      *@BA0005 - F
      *
      ******************************************************************
      *.PN C3-WRITE-DATE.                                              *
      ******************************************************************
       C3-WRITE-DATE.
      *
           MOVE E069-DAT-CTRL-TBL TO V021-DAT-CTRL-TBL.
      *
           EXEC SQL
                UPDATE GPDT021
                SET T021_DAT_CTRL_TBL = :V021-DAT-CTRL-TBL,
                    T021_STP_LAST_MOD = CURRENT TIMESTAMP
      *@BA0006 - INI
                WHERE T021_ENT_ORIGIN = :V021-ENT-ORIGIN AND
                      T021_NUM_SEQUENCE = :V021-NUM-SEQUENCE
      *         WHERE CURRENT OF GPDC0210
      *@BA0006 - FIN

           END-EXEC.
      *
           PERFORM DB2CHECK.
      *
      ******************************************************************
      *.PN C4-OPEN-CURSOR.                                             *
      ******************************************************************
      *C4-OPEN-CURSOR.
      *
      *          MOVE QBEC999-COD-ENTITY TO V021-ENT-ORIGIN
      *    EXEC SQL
      *         OPEN GPDC0210
      *    END-EXEC.
      *
      *    SET SW-OPN-CUR-YES TO TRUE.
      *
      ******************************************************************
      *.PN C5-FETCH-CURSOR.                                            *
      ******************************************************************
      *C5-FETCH-CURSOR.
      *
      *    EXEC SQL
      *         FETCH GPDC0210 INTO
      *               :V021-CTR-ETY, :V021-AMT-ENTRY,
      *               :V021-PER-CONTROL, :V021-SW-TYP-ETY-VALU,
      *               :V021-DAT-CTRL-TBL:VN-NULL, :V021-FCC,
      *               :V021-FILL-1:VN-NULL1
      *    END-EXEC.
      *
      ******************************************************************
      *.PN C6-CLOSE-CURSOR.                                            *
      ******************************************************************
      *C6-CLOSE-CURSOR.
      *
      *    EXEC SQL
      *         CLOSE GPDC0210
      *    END-EXEC.
      *
      *    SET SW-OPN-CUR-NO TO TRUE.
      *
      ******************************************************************
      *.PN C99-EXIT.                                                   *
      ******************************************************************
       C99-EXIT.
      *
      *A.PR.S @BAD00002 ERMG
      *    PERFORM C6-CLOSE-CURSOR
      *A.PR.E
           EXEC CICS
              RETURN
           END-EXEC.
      *
      ******************************************************************
      *.PN DB2CHECK.                                                   *
      ******************************************************************
       DB2CHECK.
      *
           MOVE SQLCODE TO E069-SQLCODE.
           MOVE SQLERRM TO E069-COD-SQL-MSG.
      *
           MOVE E069-SQLCODE TO SQL-VALUES
           IF SQL-88-DEADLOCK-TIMEOUT
              MOVE CA-GPB0107 TO E069-TXT-MESSAGE
              MOVE CA-COD-RTN-99 TO E069-COD-MOD-RTN
      *@BA0008-INI
              MOVE 'DEADLOCK'    TO E069-TXT-DTA-SQL
      *@BA0008-FIN
      *A.PR.S @BAD00002 ERMG
      *       PERFORM C6-CLOSE-CURSOR
      *A.PR.E
              EXEC CICS
                  RETURN
              END-EXEC
           END-IF.
      *
           IF SQLWARN0 NOT EQUAL SPACES
              MOVE SQLWARN TO DB2-RETURN-CDE
              MOVE 'GAE0867' TO E069-TXT-MESSAGE
              MOVE CA-COD-RTN-99 TO E069-COD-MOD-RTN
              IF SQLWARN1 NOT EQUAL SPACES
                 MOVE 'WARNING1' TO E069-TXT-DTA-SQL
              ELSE
                 IF SQLWARN2 NOT EQUAL SPACES
                    MOVE 'WARNING2' TO E069-TXT-DTA-SQL
                 ELSE
                    IF SQLWARN3 NOT EQUAL SPACES
                       MOVE 'WARNING3' TO E069-TXT-DTA-SQL
                    ELSE
                       IF SQLWARN4 NOT EQUAL SPACES
                          MOVE 'WARNING4' TO E069-TXT-DTA-SQL
                       ELSE
                          IF SQLWARN5 NOT EQUAL SPACES
                             MOVE 'WARNING5' TO E069-TXT-DTA-SQL
                          ELSE
                             IF SQLWARN6 NOT EQUAL SPACES
                                MOVE 'WARNING6' TO E069-TXT-DTA-SQL
                             ELSE
                                MOVE 'WARNING7' TO E069-TXT-DTA-SQL.
      *
           IF SQLWARN0 NOT EQUAL SPACES
      *A.PR.S @BAD00002 ERMG
      *       PERFORM C6-CLOSE-CURSOR
      *A.PR.E
              EXEC CICS
                  RETURN
              END-EXEC.
      *
             MOVE SQLCODE TO SQL-VALUES
             IF SQL-88-OK
                OR SQL-88-NOT-FOUND
                OR SQL-88-DUPLICATE-VALUES CONTINUE
             ELSE
                MOVE CA-COD-RTN-99 TO E069-COD-MOD-RTN
                MOVE SQLCODE TO DB2-ABEND-NUM
                MOVE DB2-ABEND TO E069-TXT-MESSAGE
      *A.PR.S @BAD00002 ERMG
      *         PERFORM C6-CLOSE-CURSOR
      *A.PR.E
                EXEC CICS
                    RETURN
                END-EXEC.
      *
      ******************************************************************
       8000-TMP-STP.
      ******************************************************************

          EXEC SQL
             SELECT(GETDATE())
             INTO :WS-TMP-STP1
          END-EXEC.

           MOVE SQLCODE TO SQL-VALUES
           IF NOT SQL-88-OK
              PERFORM 999999-ABEND
           END-IF.

          INSPECT WS-TMP-STP1 REPLACING ALL CA-DOSP BY CA-PUNTO
          INSPECT WS-TMP-STP1 REPLACING ALL SPACES BY CA-GUION
          MOVE   WS-TMP-STP1 TO WS-VARPRU
          COMPUTE VN-RANDOM = (VN-MIN * 60)+
                              (VN-SEG * 100 )+
                               (VN-HORA * 60) +
                               (VN-MS * 10)
          COMPUTE RANDOM-NUMBER = FUNCTION RANDOM  (VN-RANDOM)
      *   DISPLAY RANDOM-NUMBER
          MOVE WS-TMP-STP1 TO WS-TMP-STP(1:23)
          MOVE RANDOM-NUMBER  TO WS-TMP-STP(24:3)
          MOVE WS-TMP-STP                   TO V021-STP-LAST-MOD
          .

      ******************************************************************
      *.PN DB2CHECK-EXIT.                                              *
      ******************************************************************
       DB2CHECK-EXIT.
      *
           EXIT.
      *
      ******************************************************************
      ***@EMZG003GP     999999-ABEND.           07-10-2003             *
      ******************************************************************
       999999-ABEND.
      *
           MOVE 'S'     TO ABC-ABEND.
      *
           EXEC CICS
               LINK PROGRAM('QG1CABC')
               COMMAREA(QGECABC)
           END-EXEC.
      *
