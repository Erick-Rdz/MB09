      * MB7C0110: ALTA GEOLOCALIZACION
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    MB7C0110.
      *
       AUTHOR.        LCR(BAZDG).
      *
       DATE-WRITTEN.  2017-10-27.
      *
      *****************************************************************
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *     @MB00001   GICE    24/07/19 SE ELIMINA LLAMADO A WINAPI POR*
      *                                 NO USAR DLL'S                  *
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
      *@MB00001-I
      * CALL-CONVENTION 74 is winapi.
      *@MB00001-F
      ******************************************************************
      *                       DATA DIVISION                            *
      ******************************************************************
       DATA DIVISION.
      *
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *---------------------    C O P Y S    --------------------------*
      *
         COPY QAWCSQL.
         EXEC SQL INCLUDE SQLCA   END-EXEC.
         EXEC SQL INCLUDE PEGT100 END-EXEC.
      *
      *-------------------  C O N S T A N T E S  ----------------------*
       01  CT-CONSTANTES.
      *@MB00001-I
      *    05 CT-1                       PIC X(01) VALUE '1'.
      *    05 CA-ABEND                   PIC X(01) VALUE 'S'.
      *    05 CT-P                       PIC X(01) VALUE 'P'.
      *@MB00001-F
           05 CT-10                      PIC X(02) VALUE '10'.
      *@MB00001-I
      *    05 CT-20                      PIC X(02) VALUE '20'.
      *    05 CT-30                      PIC X(02) VALUE '30'.
      *    05 CT-40                      PIC X(02) VALUE '40'.
      *    05 CT-50                      PIC X(02) VALUE '50'.
      *@MB00001-F
           05 CT-00                      PIC X(02) VALUE '00'.
           05 CT-99                      PIC X(02) VALUE '99'.
      *@MB00001-I
      *    05 CA-QR4CDB0                 PIC X(07) VALUE 'QR4CDB0'.
      *    05 CA-PROGRAM                 PIC X(08) VALUE 'MB7C0047'.
      *@MB00001-F
      *-----------------  M E N S A J E S   E R R O R   ---------------*
       01 MS-MENSAJES.
      *
          05 MS-MSN-1                    PIC X(20) VALUE
                                          'CLIENTE NO INFORMADO'.
          05 MS-MSN-2                    PIC X(20) VALUE
                                          'CODIGO DE OPERACION'.
          05 MS-MSN-3                    PIC X(25) VALUE
                                          'COORDENADA NO INFORMADA'.
          05 MS-MSN-4                    PIC X(21) VALUE
                                          'ERROR EN ALTA PEDT100'.
      *
      *---------------------  V A R I A B L E S -----------------------*
       01 VA-VARIABLES.
          05 VA-FECHA.
             10 VA-YY-DATE                  PIC 9(02) VALUE ZEROES.
             10 VA-YY2-DATE                 PIC 9(02) VALUE ZEROES.
             10 VA-GUION1-DATE              PIC X(01) VALUE '-'.
             10 VA-MM-DATE                  PIC 9(02) VALUE ZEROES.
             10 VA-GUION2-DATE              PIC X(01) VALUE '-'.
             10 VA-DD-DATE                  PIC 9(02) VALUE ZEROES.
      *
          05 VA-FEC-SIS.
             10 VA-FSIST-A                  PIC X(02) VALUE SPACES.
             10 VA-FSIST-M                  PIC X(02) VALUE SPACES.
             10 VA-FSIST-D                  PIC X(02) VALUE SPACES.
      *
      ******************************************************************
      *LINKAGE SECTION.
      ******************************************************************
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
          COPY MBEC0100.
      *
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
      *
           PERFORM 2000-PROCESO
      *
           PERFORM 3000-FIN-PROCESO.
      *
      ******************************************************************
      *1000-INICIO.                                                    *
      ******************************************************************
       1000-INICIO.
      *
           PERFORM 1100-VALIDA-ENTRADA
           .
      *
      ******************************************************************
      *1100-VALIDA-ENTRADA:   VALIDA QUE LOS CAMPOS DE ENTRADA ESTEN   *
      *                       INFORMADOS.                              *
      ******************************************************************
       1100-VALIDA-ENTRADA.
      *
           IF PR-GEO-CLIENTE = SPACES OR LOW-VALUES
              MOVE CT-10              TO PR-GEO-ERROR
              MOVE MS-MSN-1           TO PR-GEO-AVISO
              PERFORM 3000-FIN-PROCESO
           END-IF
      *
           IF PR-GEO-CODOPER  = SPACES OR LOW-VALUES
              MOVE CT-10              TO PR-GEO-ERROR
              MOVE MS-MSN-2           TO PR-GEO-AVISO
              PERFORM 3000-FIN-PROCESO
           END-IF

           IF PR-GEO-LATITUD = SPACES OR
              PR-GEO-LATITUD = ZEROES
              MOVE CT-10              TO PR-GEO-ERROR
              MOVE MS-MSN-3           TO PR-GEO-AVISO
              PERFORM 3000-FIN-PROCESO
           END-IF

           IF PR-GEO-LONGITUD = SPACES OR
              PR-GEO-LONGITUD = ZEROES
              MOVE CT-10              TO PR-GEO-ERROR
              MOVE MS-MSN-3           TO PR-GEO-AVISO
              PERFORM 3000-FIN-PROCESO
           END-IF
      *
          IF PR-GEO-TYPAPPL EQUAL TO LOW-VALUES OR HIGH-VALUES OR SPACES
              MOVE 'D002'             TO PR-GEO-TYPAPPL
          END-IF
           IF PR-GEO-NUMREF EQUAL TO LOW-VALUES OR HIGH-VALUES
              MOVE SPACE              TO PR-GEO-NUMREF
           END-IF
           IF PR-GEO-CHAR2  EQUAL TO LOW-VALUES OR HIGH-VALUES
              MOVE SPACE              TO PR-GEO-CHAR2
           END-IF
           IF PR-GEO-FLG    EQUAL TO LOW-VALUES OR HIGH-VALUES
              MOVE SPACE              TO PR-GEO-FLG
           END-IF
           IF PR-GEO-FLG2   EQUAL TO LOW-VALUES OR HIGH-VALUES
              MOVE SPACE              TO PR-GEO-FLG2
           END-IF
           IF PR-GEO-MONTO  EQUAL TO LOW-VALUES OR HIGH-VALUES
              MOVE ZEROES             TO PR-GEO-MONTO
           END-IF
           IF PR-GEO-MONTO2 EQUAL TO LOW-VALUES OR HIGH-VALUES
              MOVE ZEROES             TO PR-GEO-MONTO2
           END-IF
      *
           ACCEPT VA-FEC-SIS FROM DATE

           MOVE '20'                   TO VA-YY-DATE
           MOVE VA-FSIST-A             TO VA-YY2-DATE
           MOVE VA-FSIST-M             TO VA-MM-DATE
           MOVE VA-FSIST-D             TO VA-DD-DATE
      *
        .
      *
      ******************************************************************
      *2000-PROCESO.                                                   *
      ******************************************************************
       2000-PROCESO.
      *
           INITIALIZE DCLPEDT100
      *
           MOVE '0127'                       TO T100-ENT
           MOVE PR-GEO-CLIENTE               TO T100-NUM-CUS
           MOVE VA-FECHA                     TO T100-DAT-REG
           MOVE PR-GEO-CODOPER               TO T100-TYP-OPE
           MOVE PR-GEO-TYPAPPL               TO T100-TYP-APPLI
           MOVE PR-GEO-NUMOPER               TO T100-NUM-OPE
           MOVE PR-GEO-LATITUD               TO T100-LATITUDE
           MOVE PR-GEO-LONGITUD              TO T100-LONGITUDE
           MOVE PR-GEO-NUMREF                TO T100-NUM-REF
           MOVE PR-GEO-CEL                   TO T100-CEL-NUM
           MOVE PR-GEO-CUENTA                TO T100-CHAR-FREE1
           MOVE PR-GEO-CHAR2                 TO T100-CHAR-FREE2
           MOVE PR-GEO-FLG                   TO T100-FLG-FREE
           MOVE PR-GEO-FLG2                  TO T100-FLG-FREE2
           MOVE PR-GEO-MONTO                 TO T100-AMT-FREE1
           MOVE PR-GEO-MONTO2                TO T100-AMT-FREE2
           MOVE PR-GEO-CENTRO                TO T100-CEN-LASTMOD
           MOVE PR-GEO-USUARIO               TO T100-USER-LASTMOD
           MOVE PR-GEO-TERMINAL              TO T100-TRM-LASTMOD
      *
           EXEC SQL
               INSERT INTO PEDT100
                  (T100_ENT         ,
                   T100_NUM_CUS     ,
                   T100_DAT_REG     ,
                   T100_TYP_OPE     ,
                   T100_TYP_APPLI   ,
                   T100_NUM_OPE     ,
                   T100_LATITUDE    ,
                   T100_LONGITUDE   ,
                   T100_NUM_REF     ,
                   T100_CEL_NUM     ,
                   T100_FLG_FREE    ,
                   T100_FLG_FREE2   ,
                   T100_CHAR_FREE1  ,
                   T100_CHAR_FREE2  ,
                   T100_AMT_FREE1   ,
                   T100_AMT_FREE2   ,
                   T100_CEN_LASTMOD ,
                   T100_USER_LASTMOD,
                   T100_TRM_LASTMOD ,
                   T100_STP_LASTMOD )
               VALUES
                  (:T100-ENT         ,
                   :T100-NUM-CUS     ,
                   :T100-DAT-REG     ,
                   :T100-TYP-OPE     ,
                   :T100-TYP-APPLI   ,
                   :T100-NUM-OPE     ,
                   :T100-LATITUDE    ,
                   :T100-LONGITUDE   ,
                   :T100-NUM-REF     ,
                   :T100-CEL-NUM     ,
                   :T100-FLG-FREE    ,
                   :T100-FLG-FREE2   ,
                   :T100-CHAR-FREE1  ,
                   :T100-CHAR-FREE2  ,
                   :T100-AMT-FREE1   ,
                   :T100-AMT-FREE2   ,
                   :T100-CEN-LASTMOD ,
                   :T100-USER-LASTMOD,
                   :T100-TRM-LASTMOD ,
                   CURRENT TIMESTAMP)
           END-EXEC
      *
           MOVE SQLCODE                      TO SQL-VALUES
      *
           EVALUATE TRUE
            WHEN SQL-88-OK
                 MOVE CT-00                  TO PR-GEO-AVISO
                 MOVE CT-00                  TO PR-GEO-ERROR
            WHEN OTHER
                 MOVE CT-99                     TO PR-GEO-AVISO
                 MOVE MS-MSN-4                  TO PR-GEO-ERROR
                 PERFORM 3000-FIN-PROCESO
           END-EVALUATE
           .
      ******************************************************************
      *3000-FIN-PROCESO.
      ******************************************************************
       3000-FIN-PROCESO.

              GOBACK.

      ******************************************************************
      ***************            Fin Programa            ***************
      *****************************************************************
