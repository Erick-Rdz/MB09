      ******************************************************************
      *MP9C0009: RUTINA QUE REALIZA LA CONSULTA DE LOS MOVIMIENTOS DE  *
      *          TARJETA POR CUENTA Y OPERACION Y TOKEN Q2             *
      *          DISEÑADA PARA EL AREA DE CUENTAS LINEA.               *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    MP9C0009.
      *
       AUTHOR.        ISAAC CASTELLANOS V
      *
       DATE-WRITTEN.  2021-10-28.
      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *     @MP0001    JLO   02-04-2022 Se cambia consulta de MCDT803. *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SOURCE-COMPUTER.
      *
           IBM-3090.
       OBJECT-COMPUTER.
      *
           IBM-3090.
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
      ******************************************************************
      *CONSTANTES                                                      *
      ******************************************************************
       01  CONSTANTS.
           05  CA-PROGRAM                 PIC X(08)   VALUE 'MP9C0009'.
           05  CN-CAJERO                  PIC  X(02) VALUE '01'.
           05  CA-VISA                    PIC X(04)   VALUE 'VISA'.
           05  CA-CARGO-REC               PIC X(16)
                                          VALUE 'CARGO RECURRENTE'.
      *
      ******************************************************************
      *VARIABLES                                                       *
      ******************************************************************
       01  WORKIN-VARIABLES.
           05  VA-VARIABLES.
      *
               10  VA-ATM-OPERA.
                   15  VA-ATM-OPERA-ENT    PIC 9(11).
                   15  VA-ATM-OPERA-DEC    PIC  9(02).
               10  VA-DTA-DEB-OPE.
                   15  VN-DEB-AMT-OPE      PIC S9(11)V9(2).
      *
           05 VA-NUM-OPE-ALFA              PIC ZZZZZZZZ9.
           05 VA-CAMPOS                    PIC S9(02)  VALUE ZEROS.
           05 VA-CAMPO-CAL                 PIC S9(02)  VALUE ZEROS.
      *
      ******************************************************************
      *COMUNICATION AREA                                               *
      ******************************************************************
      *
      *MPWC012: COPY CON CONSTANTES.
       COPY MPWC012.
      *COPY PARA  ERRORES DE SQL SERVER
       01  VA-QGECABC-01.
           COPY QGECABC.
      *
      *COPY DE LA SWA
       COPY QGECFEC.
      *COPY CODIGOS DE RETORNO DE SQL
       COPY QAWCSQL.
      *
       01  VA-MCWC9400.
           COPY MCWC9400.
      *
      ******************************************************************
      *COMUNICATION SQL                                                *
      ******************************************************************
      *
       COPY TCWC0100.
      *
       EXEC SQL
         INCLUDE SQLCA
       END-EXEC.
      *
       EXEC SQL
         INCLUDE MCGT043
       END-EXEC.
      *
       EXEC SQL
         INCLUDE MCGT343
       END-EXEC.
      *
       EXEC SQL
         INCLUDE MCGT801
       END-EXEC.
      *
       EXEC SQL
         INCLUDE MCGT802
       END-EXEC.
      *
       EXEC SQL
         INCLUDE MCGT803
       END-EXEC.
      ******************************************************************
      *                      LINKAGE SECTION                           *
      ******************************************************************
       LINKAGE SECTION.
      *
       01 VA-MPWC0009.
          COPY MPEC0009.
          COPY QBEC999.
      *
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING VA-MPWC0009.
      *
      *
           PERFORM 10000-START
      *
           PERFORM 20000-PROCESS
      *
           PERFORM 30000-END
            .
      *
      ******************************************************************
      *.PN 10000-START.                                                *
      ******************************************************************
       10000-START.
      *
           INITIALIZE QAWCSQL
                      DCLMCDT043
                      VA-QGECABC-01
      *
           MOVE SPACES                      TO E109-COD-RETURN
           PERFORM 10100-VALIDA-ENTRADA
      *
       EXIT
             .
      *
      ******************************************************************
      *10100-VALIDA-ENTRADA                                            *
      ******************************************************************
       10100-VALIDA-ENTRADA.
      *
           IF E109-CUENTA = SPACES OR LOW-VALUES
              MOVE W012-20CONS-A1          TO E109-COD-RETURN
              MOVE 'E109-CUENTA NO INFORMADO'
                                           TO E109-DES-ERR
              PERFORM 30000-END
           END-IF

           IF E109-NUM-OPE = SPACES OR LOW-VALUES
              MOVE W012-20CONS-A1          TO E109-COD-RETURN
              MOVE 'E109-NUM-OPE NO INFORMADO'
                                           TO E109-DES-ERR
              PERFORM 30000-END
           END-IF
           .
      ******************************************************************
      *.PN 20000-PROCESS.                                              *
      ******************************************************************
       20000-PROCESS.
      *
           PERFORM 52000-PROCESA-MCDC0043  
           MOVE W012-00CONS-A1             TO E109-COD-RETURN
      *
       EXIT
             .
      *
      ******************************************************************
      *52000-PROCESA-MCDC0043                                          *
      ******************************************************************
       52000-PROCESA-MCDC0043.
      *
           INITIALIZE VA-NUM-OPE-ALFA
                      VA-CAMPOS
                      VA-CAMPO-CAL
      *
           MOVE E109-CUENTA(01:04)               TO T043-ENT-ACC
           MOVE E109-CUENTA(05:04)               TO T043-BRN-ACC
           MOVE E109-CUENTA(09:02)               TO T043-TYP-ACC
           MOVE E109-CUENTA(11:08)               TO T043-ACC
           MOVE E109-NUM-OPE                     TO T043-NUM-OPE-2
      *QUITA ESPACIOS A LA IZQUIERDA
           MOVE E109-NUM-OPE     TO VA-NUM-OPE-ALFA
           INSPECT VA-NUM-OPE-ALFA TALLYING VA-CAMPOS
           FOR LEADING SPACE
           COMPUTE VA-CAMPO-CAL = 9 - VA-CAMPOS
           ADD 1  TO VA-CAMPOS
           MOVE VA-NUM-OPE-ALFA(VA-CAMPOS:VA-CAMPO-CAL)
                           TO T043-NUM-OPE-2
           EXEC SQL
           SELECT  TOP 1
                   T043_NUM_BIN_CRD  ,
                   T043_NUM_CARD     ,
                   T043_ENT_CONTRACT ,
                   T043_BRN_CONTRACT ,
                   T043_TYP_CONTRACT ,
                   T043_NUM_CONTRACT ,
                   T043_ENT_ORIGIN   ,
                   T043_TXT_DIG_30   ,
                   T043_DES_TOWN     ,
                   T043_FLG_ORI_DEST ,
                   T043_DAT_OPERATION,
                   T043_TIM_OPERATION,
                   T043_COD_OPERATION,
                   T043_AMT_OPERATION,
                   T043_FCC_CONTRACT ,
                   T043_NUM_OPERATION,
                   T043_NUM_DEB_OPE  ,
                   T043_NUM_AUT      ,
                   T043_DAT_ACCT     ,
                   T043_FCC_OPERATION,
                   T043_NUM_BSS      ,
                   T043_COD_BSS_ACT  ,
                   T043_NUM_REFERENCE,
                   T043_COD_COUNTRY  ,
                   T043_EXCHRT       ,
                   T043_STP
              INTO
                   :T043-NUM-BIN-CRD  ,
                   :T043-NUM-CARD     ,
                   :T043-ENT-CONTRACT ,
                   :T043-BRN-CONTRACT ,
                   :T043-TYP-CONTRACT ,
                   :T043-NUM-CONTRACT ,
                   :T043-ENT-ORIGIN   ,
                   :T043-TXT-DIG-30   ,
                   :T043-DES-TOWN     ,
                   :T043-FLG-ORI-DEST ,
                   :T043-DAT-OPERATION,
                   :T043-TIM-OPERATION,
                   :T043-COD-OPERATION,
                   :T043-AMT-OPERATION,
                   :T043-FCC-CONTRACT ,
                   :T043-NUM-OPERATION,
                   :T043-NUM-DEB-OPE  ,
                   :T043-NUM-AUT      ,
                   :T043-DAT-ACCT     ,
                   :T043-FCC-OPERATION,
                   :T043-NUM-BSS      ,
                   :T043-COD-BSS-ACT  ,
                   :T043-NUM-REFERENCE,
                   :T043-COD-COUNTRY  ,
                   :T043-EXCHRT       ,
                   :T043-STP
                FROM
                     MCDT043 with (nolock)
           WHERE T043_ACC           = :T043-ACC
             AND T043_BRN_ACC       = :T043-BRN-ACC
             AND T043_TYP_ACC       = :T043-TYP-ACC
             AND T043_ENT_ACC       = :T043-ENT-ACC
             AND T043_NUM_OPE_2     = :T043-NUM-OPE-2
           ORDER BY T043_STP DESC
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
               WHEN SQL-88-OK
                    PERFORM 52000-DATOS-MCDT043
                    PERFORM 54000-VALIDA-RECURRENTE-801
               WHEN SQL-88-NOT-FOUND
                    MOVE W012-10CONS-A1             TO E109-COD-RETURN
                    MOVE 'E109-NUM-OPE NO EXISTE'
                                                    TO E109-DES-ERR
                    PERFORM 30000-END
               WHEN SQL-88-SEVERAL
                    MOVE W012-30CONS-A1             TO E109-COD-RETURN
                    PERFORM 30000-END
               WHEN OTHER
                   MOVE W012-99CONS-A1     TO E109-COD-RETURN
                   MOVE 'SELECT MCDT043'   TO E109-DES-ERR
                   MOVE SQLCODE            TO E109-SQLCODE
                   PERFORM 30000-END
           END-EVALUATE
            .
      *
      ******************************************************************
      *52000-DATOS-MCDT043                                             *
      ******************************************************************
       52000-DATOS-MCDT043.
      *
           EVALUATE T043-COD-OPERATION
              WHEN '00'
                  MOVE 'COMPRA '        TO E109-TIPO-OPE
              WHEN '01'
                  MOVE 'RETIRO '        TO E109-TIPO-OPE
              WHEN '15'
              WHEN '20'
              WHEN '24'
                  MOVE 'DEVOLUC'        TO E109-TIPO-OPE
              WHEN '30'
                  MOVE 'CONSULT'        TO E109-TIPO-OPE
              WHEN '60'
                  MOVE 'CARGO V'        TO E109-TIPO-OPE
              WHEN '08'
                  MOVE 'COMISIO'        TO E109-TIPO-OPE
              WHEN '10'
                  MOVE 'IVA COM'        TO E109-TIPO-OPE
              WHEN '09'
                  MOVE 'OT COMI'        TO E109-TIPO-OPE
              WHEN '70'
                  MOVE 'ABONO V'        TO E109-TIPO-OPE
              WHEN '71'
                  MOVE 'AB ACLA'        TO E109-TIPO-OPE
              WHEN '65'
                  MOVE 'VTA.GEN'        TO E109-TIPO-OPE
              WHEN '96'
                  MOVE 'CMB.NIP'        TO E109-TIPO-OPE
              WHEN '11'
                  MOVE 'INTERES'        TO E109-TIPO-OPE
              WHEN '12'
                  MOVE 'IVAINT'         TO E109-TIPO-OPE
           END-EVALUATE
      *
           MOVE  T043-AMT-OPERATION     TO VN-DEB-AMT-OPE
           MOVE  VN-DEB-AMT-OPE         TO VA-ATM-OPERA
      *
           IF T043-COD-OPERATION EQUAL TO CN-CAJERO
             IF T043-TXT-DIG-30(1:3) = 'CP-' OR '   '
      * CAJEROS PROPIOS
                IF T043-TXT-DIG-30(1:3) = 'CP-'
                   MOVE T043-TXT-DIG-30(4:6)
                                        TO W9400-CAJERO
                                           E109-NUM-REFER(13:6)
      *CAJEROS AJENOS
                ELSE
                   MOVE T043-TXT-DIG-30(25:6)
                                        TO W9400-CAJERO
                                           E109-NUM-REFER(13:6)
                END-IF
                IF T043-ENT-ORIGIN  EQUAL CA-VISA
                   MOVE 'ATM Extr'   TO E109-NUM-REFER(1:11)
                ELSE
                   IF VA-ATM-OPERA-DEC  NOT EQUAL ZEROES
                      MOVE 'ATM Extr'   TO E109-NUM-REFER(1:11)
                   ELSE
                     PERFORM 9400-OBTEN-BANCO
                     MOVE W9400-NOM-SAL      TO E109-NUM-REFER(1:11)
                   END-IF
                END-IF
      * OPERACIONES CON FORMATO INICIAL
             ELSE
                MOVE SPACES             TO E109-NUM-REFER(13:6)
                MOVE T043-TXT-DIG-30(1:11)
                                        TO E109-NUM-REFER(1:11)
             END-IF
             MOVE '-'                   TO E109-NUM-REFER(12:1)
             MOVE '-'                   TO E109-NUM-REFER(19:1)
             MOVE T043-NUM-OPERATION(4:6) TO E109-NUM-REFER(20:6)
           ELSE
             MOVE T043-NUM-REFERENCE      TO E109-NUM-REFER
           END-IF
      *
           MOVE T043-NUM-DEB-OPE    TO   E109-NUM-OPE-DEB
           MOVE T043-NUM-AUT        TO   E109-NUM-AUTO
           MOVE T043-FCC-OPERATION  TO   E109-COD-FCC
           MOVE T043-COD-BSS-ACT    TO   E109-COD-ACT
           MOVE T043-NUM-BSS        TO   E109-NUM-NEGOC
           MOVE T043-FLG-ORI-DEST   TO   E109-OPE-ORI
           MOVE SPACES              TO   E109-NUM-DECLA
           MOVE SPACES              TO   E109-NUM-TRANS
           MOVE T043-NUM-BIN-CRD    TO   E109-NUM-CARD(1:6)
           MOVE T043-NUM-CARD       TO   E109-NUM-CARD(7:13)
            .

      ******************************************************************
      *.PN 9400-OBTEN-BANCO.                                           *
      ******************************************************************
       9400-OBTEN-BANCO.
      *
           SET SW-NO-EXISTE-BCO               TO TRUE
           MOVE 1                             TO WS9400-CONT
           PERFORM UNTIL(SW-EXISTE-BCO OR WS9400-CONT > WS9400-NUM-MAX)
             IF W9400-TB-NEMOTEC(WS9400-CONT) EQUAL W9400-NEMOTEC-ENT
                MOVE W9400-TB-CODIGO(WS9400-CONT) TO W9400-COD-SAL
                MOVE W9400-TB-EXTEN(WS9400-CONT)  TO W9400-EXT-SAL
                MOVE W9400-TB-NOMBRE(WS9400-CONT) TO W9400-NOM-SAL
                SET SW-EXISTE-BCO                 TO TRUE
             END-IF
             ADD +1  TO WS9400-CONT
             IF WS9400-CONT > WS9400-NUM-MAX AND SW-NO-EXISTE-BCO
                MOVE 'XXX'                    TO W9400-COD-SAL
                MOVE '999'                    TO W9400-EXT-SAL
                MOVE 'SIN DESC.   '           TO W9400-NOM-SAL
             END-IF
           END-PERFORM
            .
      *
      ******************************************************************
      *54000-VALIDA-RECURRENTE-801                                     *
      ******************************************************************
       54000-VALIDA-RECURRENTE-801.
      *
           EXEC SQL
           SELECT  TOP 1
                    T801_TKN_Q2
           INTO    :T801-TKN-Q2
           FROM MCDT801 with (nolock)
           WHERE T801_NUM_CARD = :T043-NUM-CARD
           AND   T801_NUM_BIN_CRD = :T043-NUM-BIN-CRD
           AND   T801_DAT_OPERATION = :T043-DAT-OPERATION
           AND   T801_TIM_OPERATION = :T043-TIM-OPERATION
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
               WHEN SQL-88-OK
                    IF T801-TKN-Q2 = '02'
                       MOVE CA-CARGO-REC            TO E109-RECURRENTE
                    END-IF
               WHEN SQL-88-NOT-FOUND
                    PERFORM 55000-VALIDA-RECURRENTE-802
               WHEN SQL-88-SEVERAL
                    MOVE W012-30CONS-A1             TO E109-COD-RETURN
                    PERFORM 30000-END
               WHEN OTHER
                   MOVE W012-99CONS-A1     TO E109-COD-RETURN
                   MOVE 'SELECT MCDT801'   TO E109-DES-ERR
                   MOVE SQLCODE            TO E109-SQLCODE
                   PERFORM 30000-END
           END-EVALUATE
            .
      ******************************************************************
      *55000-VALIDA-RECURRENTE-802                                     *
      ******************************************************************
       55000-VALIDA-RECURRENTE-802.
      *
           EXEC SQL
           SELECT  TOP 1
                    T802_TKN_Q2
           INTO    :T802-TKN-Q2
           FROM MCDT802 with (nolock)
           WHERE T802_NUM_CARD = :T043-NUM-CARD
           AND   T802_NUM_BIN_CRD = :T043-NUM-BIN-CRD
           AND   T802_DAT_OPERATION = :T043-DAT-OPERATION
           AND   T802_TIM_OPERATION = :T043-TIM-OPERATION
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
               WHEN SQL-88-OK
                    IF T802-TKN-Q2 = '02'
                       MOVE CA-CARGO-REC            TO E109-RECURRENTE
                    END-IF
               WHEN SQL-88-NOT-FOUND
                    PERFORM 56000-VALIDA-RECURRENTE-803
               WHEN SQL-88-SEVERAL
                    MOVE W012-30CONS-A1             TO E109-COD-RETURN
                    PERFORM 30000-END
               WHEN OTHER
                   MOVE W012-99CONS-A1     TO E109-COD-RETURN
                   MOVE 'SELECT MCDT802'   TO E109-DES-ERR
                   MOVE SQLCODE            TO E109-SQLCODE
                   PERFORM 30000-END
           END-EVALUATE
            .
      ******************************************************************
      *56000-VALIDA-RECURRENTE-803                                     *
      ******************************************************************
       56000-VALIDA-RECURRENTE-803.
      *
      *MP0001-I
           EXEC SQL
           SELECT  TOP 1
                    T803_TKN_Q2
           INTO    :T803-TKN-Q2
           FROM MCDT803 with (nolock)
           WHERE T803_NUM_CARD      = :T043-NUM-CARD
           AND   T803_NUM_BIN_CRD   = :T043-NUM-BIN-CRD
           AND   T803_DAT_OPERATION = :T043-DAT-OPERATION
           AND   T803_TIM_OPERATION = :T043-TIM-OPERATION
           AND   T803_NUM_AUT       = :T043-NUM-AUT
           END-EXEC
      *SE QUITAN CAMPOS EN LA CONSULTA
      *     
      *MP0001-F
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
               WHEN SQL-88-OK
                    IF T803-TKN-Q2 = '02'
                       MOVE CA-CARGO-REC            TO E109-RECURRENTE
                    END-IF
               WHEN SQL-88-NOT-FOUND
                    CONTINUE
               WHEN SQL-88-SEVERAL
                    MOVE W012-30CONS-A1             TO E109-COD-RETURN
                    PERFORM 30000-END
               WHEN OTHER
                   MOVE W012-99CONS-A1     TO E109-COD-RETURN
                   MOVE 'SELECT MCDT803'   TO E109-DES-ERR
                   MOVE SQLCODE            TO E109-SQLCODE
                   PERFORM 30000-END
           END-EVALUATE
            .
      ******************************************************************
      *30000-END.                                                      *
      ******************************************************************
       30000-END.
      *
           GOBACK
            .
      *