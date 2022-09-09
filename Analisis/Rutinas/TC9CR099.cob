       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. TC9CR099.
       AUTHOR.     ALNOVA.
      *
      *****************************************************************
      *                                                               *
      * APLICACION: TABLAS CORPORATIVAS                               *
      *                                                               *
      * DESCRIPCION:                                                  *
      * CON ESTE MODULO SE ACCEDE A AREAS DE MEMORIA PARA LEER O      *
      * CARGAR DATOS DE TCDT099 EN W2K.                               *
      *                                                               *
      * EN CICS, SIMPLEMENTE ACCEDE A SQL (NO SE ATIENDE OPCION 0)    *
      *                                                               *
      * OPCIONES:                                                     *
      *                                                               *
      *          - 0:  SE ACTUALIZA EL ÁREA DE MEMORIA                *
      *          - 1:  SE OBTIENE UNA SOLA OCURRENCIA CORRESPONDIENTE *
      *                A LA CLAVE INFORMADA.                          *
      *****************************************************************
      *                LOG DE MODIFICACIONES                          *
      *****************************************************************
      *     CÓDIGO   USUARIO FECHA    DESCRIPCIÓN                     *
      *     -------- ------- -------- ------------------------------- *
      *   @TCDT001   JSL      24-10-12  SE AGREGA ACCESO A TABLA      *
      *                                 TCDT099                       *
      *****************************************************************

      *****************************************************************
      *                ENVIRONMENT DIVISION                           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-4381.
       OBJECT-COMPUTER. IBM-4381.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *****************************************************************
      *                   DATA DIVISION                               *
      *****************************************************************
       DATA DIVISION.
      *****************************************************************
      *               WORKING STORAGE SECTION                         *
      *****************************************************************
       WORKING-STORAGE SECTION.
      *
       COPY QAWCSQL.

       COPY QAWCCO2C.

      *
       01 QAECREAM-01.
          COPY QAECREAM.
      *
       01 QAECLOAD-01.
          COPY QAECLOAD.
      *
      ******************************************************************
      *                                                                *
      *       WORKA CORRESPONDIENTE A ERRORES DB2                      *
      *                                                                *
      ******************************************************************
      *
       COPY QCWCL20.
      *
      ******************************************************************
      *                                                                *
      *       WORKA DE LA TABLA DE DB2: TCDT099                        *
      *                                                                *
      ******************************************************************
      *
      *******  TABLA DB2  ********
      *
           EXEC SQL
             INCLUDE TCGT099
           END-EXEC.
      *
       01 W-VARIABLES.
          05 W-NUM-REG              PIC 9(9).
          05 W-TB-TAB-REC.
             10 W-KEY.
                15 W-COD-ENTITY     PIC X(4).
                15 W-LNG-DATA       PIC X(1).
             10 W-DES-ENTITY        PIC X(40).
             10 W-COD               PIC X(2).
             10 W-ENT-NAC           PIC X(4).
             10 W-DES-NAC           PIC X(40).
             10 W-COD-NATCC         PIC X(3).
             10 W-DES-NATCC         PIC X(20).
             10 W-SDE-NATCC         PIC X(3).
             10 W-COD-NATCCSHORT    PIC X.
             10 W-COD-OCCCTRY       PIC X(3).
             10 W-DES-OCC           PIC X(20).
             10 W-SDE-OCC           PIC X(3).
             10 W-COD-OCCSHORT      PIC X.
             10 W-COD-RCC           PIC X(3).
             10 W-DES-RCC           PIC X(20).
             10 W-COD-RCCSHORT      PIC X.
             10 W-COD-NRESFCC       PIC X(3).
             10 W-DES-NRESFCC       PIC X(20).
             10 W-COD-NATCTRY       PIC X(3).
             10 W-DES-NATCTRY       PIC X(40).
             10 W-FLG-EURCTRY       PIC X.
             10 W-EXCHANGE          PIC X.
             10 W-LNG-OFDATA        PIC X.
             10 W-LASTMODUSER       PIC X(8).
             10 W-LASTMODTRM        PIC X(4).
             10 W-DAT-LASTMOD       PIC X(26).
             10 W-DES-ENTABR        PIC X(10).
             10 W-FLG-OFCACC        PIC X(1).
             10 W-FILLER            PIC X     VALUE LOW-VALUES.
      *
      *****************************************************************
      *               CONSTANTES                                      *
      *****************************************************************
       01 CONSTANTS.
          05 CN-0                   PIC S9(5) COMP-3 VALUE ZERO.
          05 CN-1                   PIC S9(5) COMP-3 VALUE 1.
          05 CN-10                  PIC S9(5) COMP-3 VALUE 10.
          05 CN-30                  PIC S9(5) COMP-3 VALUE 30.
          05 CN-40                  PIC S9(5) COMP-3 VALUE 40.
          05 CA-1                   PIC X            VALUE '1'.
          05 CA-3                   PIC X            VALUE '3'.
          05 CA-4                   PIC X            VALUE '4'.
          05 CA-00                  PIC XX           VALUE '00'.
          05 CA-10                  PIC XX           VALUE '10'.
          05 CA-80                  PIC XX           VALUE '80'.
          05 CA-QA6CREAD            PIC X(8)         VALUE 'QA6CREAD'.
          05 CA-QA6CLOAD            PIC X(8)         VALUE 'QA6CLOAD'.
          05 CA-TCDT099             PIC X(8)         VALUE 'TCDT099 '.
      *@TCDT001.I
       01  CA-WR-EVENT-LOGMP             PIC X(20)
                                         VALUE 'WriteEventLogDin.dll'.

       01 VA-MSG                      PIC X(500)  VALUE SPACES.
      *@MC00003 I. 22-12-03 variables para envio de mensaje al eventview
       01 VA-TITULO                     PIC X(15).
       01 VA-TIPO                       PIC X(01).
      *
       01 VA-MENSAJE.
          10 FILLER                     PIC X(10)
                                VALUE 'TC9CR099- '.
          10 VA-MSG-EVENT               PIC X(35)

                                VALUE ' LECTURA  BD VALIDA CARGA MEM '.
          10 FILLER                     PIC X(09)
          10 VA-COD-ERROR               PIC X(2)    VALUE SPACES.
          10 FILLER                     PIC X(7).
       01 VA-CICS                       PIC X(4)    VALUE SPACES.
      *@TCDT001.F
      *
      *****************************************************************
      *               FLAGS AND SWITCHES                              *
      *****************************************************************
      *
       01 SW-SWITCHES.
          05 SW-OPENCURSOR          PIC X.
             88 SW-OCUR-YES                          VALUE 'Y'.
             88 SW-OCUR-NO                           VALUE 'N'.
          05 SW-END-TABLE           PIC X.
             88 SW-END-TABLE-NO                      VALUE 'N'.
             88 SW-END-TABLE-YES                     VALUE 'Y'.
          05 SW-LOAD-COD            PIC X.
             88 SW-LOAD-COD-OK                       VALUE 'Y'.
             88 SW-LOAD-COD-NOT-OK                   VALUE 'N'.
          05 SW-OPTION              PIC X.
             88 SW-OPTION-READ                       VALUE '1'.
             88 SW-OPTION-LOAD                       VALUE '2'.
      *
      ******************************************************************
      *                                                                *
      *       DECLARACION DE CURSORES                                  *
      *                                                                *
      ******************************************************************
            EXEC SQL
              DECLARE TCDC0990 CURSOR FOR
               SELECT COD_ENTITY
                    , LNG_DATA
                    , DES_ENTITY
                    , COD_SHORT
                    , COD_NB
                    , DES_NB
                    , COD_NATCC
                    , DES_NATCC
                    , SDE_NATCC
                    , COD_NATCCSHORT
                    , COD_OCCCTRY
                    , DES_OCC
                    , SDE_OCC
                    , COD_OCCSHORT
                    , COD_RCC
                    , DES_RCC
                    , COD_RCCSHORT
                    , COD_NRESFCC
                    , DES_NRESFCC
                    , COD_NATCTRY
                    , DES_NATCTRY
                    , FLG_EURCTRY
                    , EXCHANGE
                    , LNG_OFDATA
                    , LASTMODUSER
                    , LASTMODTRM
                    , DAT_LASTMOD
                    , DES_ENTABR
                    , FLG_OFCACC
                 FROM TCDT099 with (nolock)
                ORDER BY COD_ENTITY
                       , LNG_DATA
                FOR READ ONLY
            END-EXEC.
      *                                                                *
      *
      *****************************************************************
      *                 LINKAGE SECTION                               *
      *****************************************************************
      *
       LINKAGE SECTION.

       COPY DFHEIBLK.

       01 QAECEAREA-01.
          COPY QAECAREA.

       COPY TCECR099.
      *
      *****************************************************************
      *                 PROCEDURE DIVISION                            *
      *****************************************************************
       PROCEDURE DIVISION USING DFHEIBLK QAECEAREA-01 TCECR099.
      *
      *****************************************************************
      *.PN               1000-MAINLINE                                *
      *****************************************************************
       1000-MAINLINE.

           PERFORM 2000-START

           PERFORM 3000-PROCESS

           GOBACK.
      *
      *****************************************************************
      *.PN                2000-START                                  *
      *****************************************************************
       2000-START.

           INITIALIZE QAWCSQL
           SET  SW-OCUR-NO          TO TRUE
           MOVE CN-0                TO EAREA-COD-RETURN
           MOVE EAREA-TYP-PROC-AREA TO SW-OPTION
           .
      *
      *****************************************************************
      *.PN          3000-PROCESS                                      *
      *****************************************************************
       3000-PROCESS.

           EVALUATE TRUE
             WHEN SW-OPTION-READ
               PERFORM 3100-READ
             WHEN SW-OPTION-LOAD
               PERFORM 3200-LOAD-MEM-AREA
             WHEN OTHER
               MOVE CN-30         TO EAREA-COD-RETURN
               GOBACK
           END-EVALUATE
           .
      *
      *****************************************************************
      *.PN          3100-READ                                    *
      *****************************************************************
       3100-READ.

           INITIALIZE TCECR099-OUTPUT
                      QAECREAM
           MOVE CA-00                  TO TCECR099-COD-RETURN

           MOVE EAREA-AREA-NM          TO EREAM-MEMO-NM
           MOVE TCECR099-COD-REQOPTION TO EREAM-OPTION
           MOVE TCECR099-KEY           TO EREAM-RECORD

           PERFORM 3111-CALL-READ-ROUTINE

           IF EREAM-COD-RETURN = CN-30
             PERFORM 3200-LOAD-MEM-AREA
             IF SW-LOAD-COD-OK
               PERFORM 3111-CALL-READ-ROUTINE
             END-IF
           END-IF

           EVALUATE EREAM-COD-RETURN
             WHEN ZERO
               MOVE EREAM-RECORD       TO TCECR099-TB-TAB-REC
               MOVE CN-1               TO TCECR099-NUM-RREC
             WHEN CN-10
               MOVE CA-10              TO TCECR099-COD-RETURN
               MOVE CA-TCDT099         TO TCECR099-DES-ABLE
             WHEN OTHER
               EVALUATE TCECR099-COD-REQOPTION
                 WHEN CA-1
                   PERFORM 5000-ACCESS-TABLE-1
                 WHEN CA-3
                   PERFORM 5000-ACCESS-TABLE-3
                 WHEN CA-4
                   PERFORM 5000-ACCESS-TABLE-4
                 WHEN OTHER
                   MOVE CN-30          TO EAREA-COD-RETURN
                   GOBACK
               END-EVALUATE
           END-EVALUATE.
      *
      ******************************************************************
      *.PN              3111-CALL-READ-ROUTINE
      ******************************************************************
       3111-CALL-READ-ROUTINE.

           CALL CA-QA6CREAD USING DFHEIBLK QAECREAM
             ON EXCEPTION
               MOVE CN-40 TO EREAM-COD-RETURN
           END-CALL
           .
      ******************************************************************
      *.PN              3200-LOAD-MEM-AREA
      *****************************************************************
       3200-LOAD-MEM-AREA.

           SET  SW-LOAD-COD-OK      TO TRUE
           SET  SW-END-TABLE-NO     TO TRUE

           MOVE EAREA-TYP-PROC-AREA TO ELOAD-OPTION
           MOVE EAREA-AREA-NM       TO ELOAD-MEMO-AREA
           MOVE ZERO                TO ELOAD-ORD-REC

           PERFORM 3210-MEMORY-LENGTH

           IF SW-LOAD-COD-OK
             PERFORM 3220-SELECT-RECORDS
           END-IF

           IF SW-LOAD-COD-NOT-OK AND SW-OPTION-LOAD
             MOVE CN-30             TO EAREA-COD-RETURN
           END-IF.
      *
      ******************************************************************
      *.PN              3210-MEMORY-LENGTH
      *****************************************************************
       3210-MEMORY-LENGTH.

           EXEC SQL
             SELECT COUNT(1)
               INTO :W-NUM-REG
               FROM TCDT099 with (NOLOCK)
           END-EXEC

           MOVE SQLCODE                  TO SQL-VALUES

           IF SQL-88-OK
             MOVE W-NUM-REG              TO ELOAD-NUM-RECORD
             MOVE LENGTH OF W-KEY        TO ELOAD-LTH-KEY
             MOVE LENGTH OF W-TB-TAB-REC TO ELOAD-LTH-REC
           ELSE
             PERFORM 5230-ERROR-DB2-LOAD
           END-IF
            .
      *
      ******************************************************************
      *.PN              3220-SELECT-RECORDS
      *****************************************************************
       3220-SELECT-RECORDS.

           EXEC SQL
             OPEN TCDC0990
           END-EXEC

           MOVE SQLCODE        TO SQL-VALUES

           EVALUATE TRUE
             WHEN SQL-88-OK
               SET SW-OCUR-YES TO TRUE
             WHEN OTHER
               PERFORM 5230-ERROR-DB2-LOAD
           END-EVALUATE

           PERFORM 3221-FETCH-RECORDS-TCDV0100
             UNTIL SW-END-TABLE-YES

           IF SW-OCUR-YES
             PERFORM 3222-CLOSE-CURSOR-LOAD
           END-IF
           .
      *
      ******************************************************************
      *.PN             3221-FETCH-RECORDS-TCDV0100
      ******************************************************************
       3221-FETCH-RECORDS-TCDV0100.
      *
           EXEC SQL
             FETCH TCDC0990
              INTO :W-COD-ENTITY
                 , :W-LNG-DATA
                 , :W-DES-ENTITY
                 , :W-COD
                 , :W-ENT-NAC
                 , :W-DES-NAC
                 , :W-COD-NATCC
                 , :W-DES-NATCC
                 , :W-SDE-NATCC
                 , :W-COD-NATCCSHORT
                 , :W-COD-OCCCTRY
                 , :W-DES-OCC
                 , :W-SDE-OCC
                 , :W-COD-OCCSHORT
                 , :W-COD-RCC
                 , :W-DES-RCC
                 , :W-COD-RCCSHORT
                 , :W-COD-NRESFCC
                 , :W-DES-NRESFCC
                 , :W-COD-NATCTRY
                 , :W-DES-NATCTRY
                 , :W-FLG-EURCTRY
                 , :W-EXCHANGE
                 , :W-LNG-OFDATA
                 , :W-LASTMODUSER
                 , :W-LASTMODTRM
                 , :W-DAT-LASTMOD
                 , :W-DES-ENTABR
                 , :W-FLG-OFCACC
           END-EXEC

           MOVE SQLCODE             TO SQL-VALUES

           EVALUATE TRUE
             WHEN SQL-88-OK
               PERFORM 32211-CALL-LOAD
             WHEN SQL-88-NOT-FOUND
               SET SW-END-TABLE-YES TO TRUE
             WHEN OTHER
               PERFORM 5230-ERROR-DB2-LOAD
           END-EVALUATE.
      *
      ******************************************************************
      *.PN          32211-CALL-LOAD
      ******************************************************************
       32211-CALL-LOAD.
      *
              ADD 1                      TO ELOAD-ORD-REC
      *
              MOVE W-TB-TAB-REC          TO ELOAD-RECORD
      *
              CALL CA-QA6CLOAD USING DFHEIBLK QAECLOAD
                ON EXCEPTION
                  SET SW-LOAD-COD-NOT-OK TO TRUE
                  SET SW-END-TABLE-YES   TO TRUE
              END-CALL

              IF ELOAD-COD-RETURN NOT = ZERO
                SET SW-LOAD-COD-NOT-OK   TO TRUE
                SET SW-END-TABLE-YES     TO TRUE
              END-IF
              .
      *
      ******************************************************************
      *.PN          3222-CLOSE-CURSOR-LOAD
      ******************************************************************
       3222-CLOSE-CURSOR-LOAD.
      *
           EXEC SQL
             CLOSE TCDC0990
           END-EXEC
      *
           MOVE SQLCODE       TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
               SET SW-OCUR-NO TO TRUE
             WHEN OTHER
               PERFORM 5230-ERROR-DB2-LOAD
           END-EVALUATE.
      *
      *****************************************************************
      *.PN       5230-ERROR-DB2-LOAD                                  *
      *****************************************************************
       5230-ERROR-DB2-LOAD.
      *
           IF SW-OCUR-YES
             PERFORM 3222-CLOSE-CURSOR-LOAD
           END-IF

           IF SW-OPTION-READ
             SET SW-LOAD-COD-NOT-OK TO TRUE
             SET SW-END-TABLE-YES   TO TRUE
           ELSE
             MOVE CN-30             TO EAREA-COD-RETURN
             GOBACK
           END-IF
           .
      *
      *****************************************************************
      *.PN          5000-ACCESS-TABLE-1                               *
      *****************************************************************
       5000-ACCESS-TABLE-1.
      *
           EXEC SQL
             SELECT COD_ENTITY
                  , LNG_DATA
                  , DES_ENTITY
                  , COD_SHORT
                  , COD_NB
                  , DES_NB
                  , COD_NATCC
                  , DES_NATCC
                  , SDE_NATCC
                  , COD_NATCCSHORT
                  , COD_OCCCTRY
                  , DES_OCC
                  , SDE_OCC
                  , COD_OCCSHORT
                  , COD_RCC
                  , DES_RCC
                  , COD_RCCSHORT
                  , COD_NRESFCC
                  , DES_NRESFCC
                  , COD_NATCTRY
                  , DES_NATCTRY
                  , FLG_EURCTRY
                  , EXCHANGE
                  , LNG_OFDATA
                  , LASTMODUSER
                  , LASTMODTRM
                  , DAT_LASTMOD
                  , DES_ENTABR
                  , FLG_OFCACC
               INTO :W-COD-ENTITY
                  , :W-LNG-DATA
                  , :W-DES-ENTITY
                  , :W-COD
                  , :W-ENT-NAC
                  , :W-DES-NAC
                  , :W-COD-NATCC
                  , :W-DES-NATCC
                  , :W-SDE-NATCC
                  , :W-COD-NATCCSHORT
                  , :W-COD-OCCCTRY
                  , :W-DES-OCC
                  , :W-SDE-OCC
                  , :W-COD-OCCSHORT
                  , :W-COD-RCC
                  , :W-DES-RCC
                  , :W-COD-RCCSHORT
                  , :W-COD-NRESFCC
                  , :W-DES-NRESFCC
                  , :W-COD-NATCTRY
                  , :W-DES-NATCTRY
                  , :W-FLG-EURCTRY
                  , :W-EXCHANGE
                  , :W-LNG-OFDATA
                  , :W-LASTMODUSER
                  , :W-LASTMODTRM
                  , :W-DAT-LASTMOD
                  , :W-DES-ENTABR
                  , :W-FLG-OFCACC
               FROM TCDT099 with (nolock)
              WHERE COD_ENTITY = :TCECR099-COD-ENT-INP
           END-EXEC
      *
           PERFORM DB2CHECK
      *
           IF SQL-88-OK OR SQL-88-SEVERAL
             MOVE W-TB-TAB-REC          TO TCECR099-TB-TAB-REC
             MOVE CA-00                 TO TCECR099-COD-RETURN
           ELSE
             MOVE SQLCODE               TO TCECR099-SQLCODE
             MOVE SQLERRM               TO TCECR099-DTA-SQLERRM
             MOVE CA-TCDT099            TO TCECR099-DES-ABLE
             IF SQL-88-NOT-FOUND
               MOVE CA-10               TO TCECR099-COD-RETURN
             ELSE
               MOVE CA-80               TO TCECR099-COD-RETURN
             END-IF
             GOBACK
           END-IF
           .
      *
      *****************************************************************
      *.PN          5000-ACCESS-TABLE-3                               *
      *****************************************************************
       5000-ACCESS-TABLE-3.
      *
           EXEC SQL
             SELECT COD_ENTITY
                  , LNG_DATA
                  , DES_ENTITY
                  , COD_SHORT
                  , COD_NB
                  , DES_NB
                  , COD_NATCC
                  , DES_NATCC
                  , SDE_NATCC
                  , COD_NATCCSHORT
                  , COD_OCCCTRY
                  , DES_OCC
                  , SDE_OCC
                  , COD_OCCSHORT
                  , COD_RCC
                  , DES_RCC
                  , COD_RCCSHORT
                  , COD_NRESFCC
                  , DES_NRESFCC
                  , COD_NATCTRY
                  , DES_NATCTRY
                  , FLG_EURCTRY
                  , EXCHANGE
                  , LNG_OFDATA
                  , LASTMODUSER
                  , LASTMODTRM
                  , DAT_LASTMOD
                  , DES_ENTABR
                  , FLG_OFCACC
               INTO :W-COD-ENTITY
                  , :W-LNG-DATA
                  , :W-DES-ENTITY
                  , :W-COD
                  , :W-ENT-NAC
                  , :W-DES-NAC
                  , :W-COD-NATCC
                  , :W-DES-NATCC
                  , :W-SDE-NATCC
                  , :W-COD-NATCCSHORT
                  , :W-COD-OCCCTRY
                  , :W-DES-OCC
                  , :W-SDE-OCC
                  , :W-COD-OCCSHORT
                  , :W-COD-RCC
                  , :W-DES-RCC
                  , :W-COD-RCCSHORT
                  , :W-COD-NRESFCC
                  , :W-DES-NRESFCC
                  , :W-COD-NATCTRY
                  , :W-DES-NATCTRY
                  , :W-FLG-EURCTRY
                  , :W-EXCHANGE
                  , :W-LNG-OFDATA
                  , :W-LASTMODUSER
                  , :W-LASTMODTRM
                  , :W-DAT-LASTMOD
                  , :W-DES-ENTABR
                  , :W-FLG-OFCACC
               FROM TCDT099 with (nolock)
              WHERE (    COD_ENTITY  = :TCECR099-COD-ENT-INP
                     AND LNG_DATA    > :TCECR099-LNG-INP    )
                 OR      COD_ENTITY  > :TCECR099-COD-ENT-INP
               ORDER BY COD_ENTITY
                      , LNG_DATA
           END-EXEC
      *
           PERFORM DB2CHECK
      *
           IF SQL-88-OK OR SQL-88-SEVERAL
             MOVE CA-00                 TO TCECR099-COD-RETURN
             MOVE W-TB-TAB-REC          TO TCECR099-TB-TAB-REC
           ELSE
             MOVE SQLCODE               TO TCECR099-SQLCODE
             MOVE SQLERRM               TO TCECR099-DTA-SQLERRM
             MOVE CA-TCDT099            TO TCECR099-DES-ABLE
             IF SQL-88-NOT-FOUND
               MOVE CA-10               TO TCECR099-COD-RETURN
             ELSE
               MOVE CA-80               TO TCECR099-COD-RETURN
             END-IF
             GOBACK
           END-IF
           .
      *
      *****************************************************************
      *.PN          5000-ACCESS-TABLE-4                               *
      *****************************************************************
       5000-ACCESS-TABLE-4.
      *
           EXEC SQL
             SELECT COD_ENTITY
                  , LNG_DATA
                  , DES_ENTITY
                  , COD_SHORT
                  , COD_NB
                  , DES_NB
                  , COD_NATCC
                  , DES_NATCC
                  , SDE_NATCC
                  , COD_NATCCSHORT
                  , COD_OCCCTRY
                  , DES_OCC
                  , SDE_OCC
                  , COD_OCCSHORT
                  , COD_RCC
                  , DES_RCC
                  , COD_RCCSHORT
                  , COD_NRESFCC
                  , DES_NRESFCC
                  , COD_NATCTRY
                  , DES_NATCTRY
                  , FLG_EURCTRY
                  , EXCHANGE
                  , LNG_OFDATA
                  , LASTMODUSER
                  , LASTMODTRM
                  , DAT_LASTMOD
                  , DES_ENTABR
                  , FLG_OFCACC
               INTO :W-COD-ENTITY
                  , :W-LNG-DATA
                  , :W-DES-ENTITY
                  , :W-COD
                  , :W-ENT-NAC
                  , :W-DES-NAC
                  , :W-COD-NATCC
                  , :W-DES-NATCC
                  , :W-SDE-NATCC
                  , :W-COD-NATCCSHORT
                  , :W-COD-OCCCTRY
                  , :W-DES-OCC
                  , :W-SDE-OCC
                  , :W-COD-OCCSHORT
                  , :W-COD-RCC
                  , :W-DES-RCC
                  , :W-COD-RCCSHORT
                  , :W-COD-NRESFCC
                  , :W-DES-NRESFCC
                  , :W-COD-NATCTRY
                  , :W-DES-NATCTRY
                  , :W-FLG-EURCTRY
                  , :W-EXCHANGE
                  , :W-LNG-OFDATA
                  , :W-LASTMODUSER
                  , :W-LASTMODTRM
                  , :W-DAT-LASTMOD
                  , :W-DES-ENTABR
                  , :W-FLG-OFCACC
               FROM TCDT099 with (nolock)
              WHERE (    COD_ENTITY  = :TCECR099-COD-ENT-INP
                     AND LNG_DATA   >= :TCECR099-LNG-INP    )
                 OR      COD_ENTITY  > :TCECR099-COD-ENT-INP
               ORDER BY COD_ENTITY
                      , LNG_DATA
           END-EXEC
      *
           PERFORM DB2CHECK
      *
           IF SQL-88-OK OR SQL-88-SEVERAL
             MOVE CA-00                 TO TCECR099-COD-RETURN
             MOVE W-TB-TAB-REC          TO TCECR099-TB-TAB-REC
           ELSE
             MOVE SQLCODE               TO TCECR099-SQLCODE
             MOVE SQLERRM               TO TCECR099-DTA-SQLERRM
             MOVE CA-TCDT099            TO TCECR099-DES-ABLE
             IF SQL-88-NOT-FOUND
               MOVE CA-10               TO TCECR099-COD-RETURN
             ELSE
               MOVE CA-80               TO TCECR099-COD-RETURN
             END-IF
             GOBACK
           END-IF
           .
      *
      ******************************************************************
      *.PN 2600-LLAMA-EVENT-LOG.                                       *
      ******************************************************************
       2600-LLAMA-EVENT-LOG.
      *
           MOVE 'Medios de Pago '     TO VA-TITULO
      *
           MOVE  '99'                 TO VA-COD-ERROR
      *
           CALL CA-WR-EVENT-LOGMP  USING
                   BY REFERENCE VA-TIPO
                   BY REFERENCE VA-TITULO
                   BY REFERENCE VA-MENSAJE
                   ON EXCEPTION
                      MOVE WCO2C-CICS-PGMIDERR         TO EIBRESP
                      MOVE WCO2C-CICS-LINK             TO EIBFN
                      MOVE CA-WR-EVENT-LOGMP           TO EIBRSRCE
           END-CALL
      *
           IF EIBRESP NOT EQUAL WCO2C-CICS-NORMAL
              MOVE ZEROES                       TO EIBRESP
           END-IF.
      *
      *****************************************************************
      *.PN   DB2CHECK                                                 *
      *****************************************************************
        DB2CHECK.

           IF SQLWARN0 NOT EQUAL SPACES
             MOVE SQLWARN                TO TCECR099-SQLCODE
             MOVE CA-80                  TO TCECR099-COD-RETURN
             IF SQLWARN1 NOT = SPACES
               MOVE 'WARNING1'           TO TCECR099-DTA-SQLERRM
             ELSE
               IF SQLWARN2 NOT = SPACES
                 MOVE 'WARNING2'         TO TCECR099-DTA-SQLERRM
               ELSE
                 IF SQLWARN3 NOT = SPACES
                   MOVE 'WARNING3'       TO TCECR099-DTA-SQLERRM
                 ELSE
                   IF SQLWARN4 NOT = SPACES
                     MOVE 'WARNING4'     TO TCECR099-DTA-SQLERRM
                   ELSE
                     IF SQLWARN5 NOT = SPACES
                       MOVE 'WARNING5'   TO TCECR099-DTA-SQLERRM
                     ELSE
                       IF SQLWARN6 NOT = SPACES
                         MOVE 'WARNING6' TO TCECR099-DTA-SQLERRM
                       ELSE
                         MOVE 'WARNING7' TO TCECR099-DTA-SQLERRM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF.
      *
           MOVE SQLCODE TO SQL-VALUES.
      *