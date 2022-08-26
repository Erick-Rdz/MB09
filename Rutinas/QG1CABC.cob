      ******************************************************************
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
      *.MC.S ALINFQA125-77
      **.MC.S ALINFQA125-35
      **PROGRAM-ID. QG1CABC.
      * PROGRAM-ID. QG1CABC RECURSIVE.
      **.MC.E ALINFQA125-35
       PROGRAM-ID. QG1CABC.
      *.MC.E ALINFQA125-77
       AUTHOR. ALNOVA TECHNOLOGIES CORPORATION.
       DATE-WRITTEN. 02/10/2000.

      ******************************************************************
      *B.OR.S
      *                     QG1CABC
      *   QG1CABC: MODULE FOR ABNORMAL ENDINGS
      *
      *           DESCRIPTION
      *
      *   THIS PROGRAM WILL BE CALLED WHEN UNEXPECTED ERROR HAPPENS
      *   IN A CALL TO CICS OR DB2, AND THE ERROR MUST BE MANAGED BY
      *   THE PROGRAMS THEMSELVES.
      *
      *   FIRSTLY, IT WILL CALL TO THE PROGRAM QG1CLOG (SYSTEM LOG
      *   MANAGEMENT), TO REGISTER THE INCIDENCE IN THE SYSTEM LOG
      *
      *   FINALLY, IF THE PROGRAM WHICH CALLED INDICATED  THE ABEND
      *   VARIABLE TO YES, IT WILL PRODUCE AN ABEND WITH THE
      *   APPROPRIATE CODE. OTHERWISE, IT WILL RETURN THE CONTROL TO THE
      *   PROGRAM WHICH CALLED.
      *
      *     COMMAREA PARAMETERS
      *     -------------------------
      *
      *    NAME            I/O             DESCRIPTION
      *   --------        -----  -----------------------------------
      *    ABEND            I    ABEND INDICATOR (YES OR NOT)
      *    PROGRAMA         I    NAME OF THE PROGRAM WHICH CALLS THIS
      *                          MODULE
      *    REFERENCIA       I    TYPE OF ERROR INDICATOR FOR POSSIBLE
      *                          SOLUTIONS
      *    OBJETO-ERRONEO   I    NAME OF THE WRONG OBJECT IN CASE OF
      *                          DB2 ERROR
      *    SQLCODE          I    SQLCODE IN CASE OF DB2 ERROR
      *    SQLERRM          I    SQLERRM IN CASE OF DB2 ERROR
      *    EIBFN            I    EIBFN   IN CASE OF CICS ERROR
      *    EIBRSRCE         I    EIBRSRCE IN CASE OF CICS ERROR
      *    EIBRCODE         I    EIBRCODE IN CASE OF CICS ERROR
      *    EIBRESP1         I    EIBRESP IN CASE OF CICS ERROR
      *    EIBRESP2         I    EIBRESP2 IN CASE OF CICS ERROR
      *
      *B.OR.E
      *A.OR.S
      *                     QG1CABC
      *   QG1CABC: MODULO DE GESTION DE ERRORES
      *           DESCRIPCION
      *
      *   ESTE PROGRAMA SERA LLAMADO CUANDO OCURRE UN ERROR INEXPERADO
      *   EN UNA LLAMADA A CICS O DB2, Y EL ERROR DEBE ESTAR GESTIONADO
      *   POR LOS PROGRAMAS.
      *
      *   PRIMERAMENTE LLAMA AL PROGRAM QG1CLOG (GESTION DE LOG DEL
      *   SISTEMA), PARA REGISTRAR LA INCIDENCIA EN EL LOG DEL SISTEMA.
      *
      *   FINALMENTE, SI EL PROGRAMA QUE LO LLAMO, INDICO LA VARIABLE DE
      *   ABEND A SI, PRODUCIRA UN ABEND CON EL CODIGO APROPIADO.
      *   EN OTRO CASO, DEVOLVERA EL CONTROL AL PROGRAMA QUE LO LLAMO.
      *
      *     PARAMETROS DE LA COMMAREA
      *     -----------------------------
      *
      *    NOMBRE        I/O              DESCRIPCION
      *   --------      -----    ---------------------------------
      *    ABEND            I    INDICADOR DE ABENDAR (SI O NO)
      *    PROGRAMA         I    NOMBRE DEL PROGRAMA QUE LLAMA AL MODULO
      *    REFERENCIA       I    INDICADOR DEL TIPO DE ERROR PARA
      *                          POSIBLES SOLUCIONES
      *    OBJETO-ERRONEO   I    NOMBRE DEL OBJETO ERRONEO EN CASO DE
      *                          ERROR DB2
      *    SQLCODE          I    SQLCODE EN CASO DE ERROR DB2
      *    SQLERRM          I    SQLERRM EN CASO DE ERROR DB2
      *    EIBFN            I    EIBFN EN CASO DE ERROR CICS
      *    EIBRSRCE         I    EIBRSRCE EN CASO DE ERROR CICS
      *    EIBRCODE         I    EIBRCODE EN CASO DE ERROR CICS
      *    EIBRESP1         I    EIBRESP1 EN CASO DE ERROR CICS
      *    EIBRESP2         I    EIBRESP2 EN CASO DE ERROR CICS
      *
      *A.OR.E
      ******************************************************************
      *        M O D I F I C A T I O N S    L O G                      *
      *        L O G    D E    M O D I F I C A C I O N E S             *
      ******************************************************************
      *   CODE          AUTHOR   DATE          DESCRIPTION
      *   CODIGO        AUTOR    FECHA         DESCRIPCION
      * ------- ------- ---------------------------------------------- *
      ******************************************************************
      *B.MD ALINFQA115  USCDB94 2001-11-28 MULTIENTITY ADAPTATION
      *B.MD ALINFQA121  USCDB64 2001-11-31 COMMIT MANAGEMENT
      *B.MD ALINFQA122  USCDBC5 2002-01-09 CALL ON EXCEPTION USE
      *B.MD ALINFQA125-35 USCDB64 2002-05-10 ADDITION OF RECURSIVE
      *B.MD ALINFQA125-37 USCDBA9 2002-05-14 MC ERRORS MANAGEMENT
      *B.MD ALINFQA125-39 UPXDB24 2002-05-19 CALL TO QG6CTUT
      *B.MD ALINFQA117-11 USCDB64 2002-06-24 START WITHOUT M800
      *B.MD ALINFQA125-77 ALNOVA 2002-08-13 RECURSIVE MANAGMENT ON W2K
      ******************************************************************
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER.
           UNIX-SOLARIS2-6.
       OBJECT-COMPUTER.
           UNIX-SOLARIS2-6.
       SPECIAL-NAMES.
      *.MC.S @PX0012D00 18-09-2002
      *    DECIMAL-POINT IS COMMA.
      *.MC.E @PX0012D00 18-09-2002
      *
      ******************************************************************
      *                  DATA DIVISION                                 *
      ******************************************************************
       DATA DIVISION.
      *
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************

       WORKING-STORAGE SECTION.



      *
      *B.PR COPY FOR CICS VALUES.
      *A.PR COPY PARA VALORES DE CICS.
       COPY QAWCCO2C.

       01 VA-QAECCOR1-01.
          COPY QAECCOR1.

       01 VA-QGECLOG1-01.
          COPY QGECLOG1.

       COPY QRECHEX.

       COPY QAECTS1C.

       COPY QACCCTS.


       01 VA-QCECATL-01.
       COPY QCECATL.

      *.MC.S ALINFQA115
      *   COPY QCECSQ1.
          COPY QGECFEC.
      *.MC.E ALINFQA115

       COPY QAECSQ2.

       COPY QAECMEMC.

       COPY QGECTUT.
      *
      *B.PR ***********  CONSTANTS   ***********************************
      *A.PR ***********  CONSTANTES  ***********************************
      *
       01 VA-CONSTANTS.
          05 CA-FREE-MAIN                  PIC X(7)    VALUE 'QA6CFRE'.
      *.MC.S ALINFQA115.
          05 CA-QG9CSWA0                   PIC X(08)   VALUE 'QG9CSWA0'.
      *.MC.E ALINFQA115.
          05 CA-ERR-ROUTINE                PIC X(7)    VALUE 'QG1CLOG'.
          05 CA-HXDM-RUT                   PIC X(7)    VALUE 'QR8CHEX'.
          05 CA-QA7CSQ1                    PIC X(7)    VALUE 'QA7CSQ1'.
          05 CA-QA7CSQ2                    PIC X(7)    VALUE 'QA7CSQ2'.
      *.MC.S ALINFQA114
          05 CA-QA6CCOR1                   PIC X(8)    VALUE 'QA6CCOR1'.
          05 CA-QCE0030                    PIC X(7)    VALUE 'QCE0030'.
          05 CA-QCE0004                    PIC X(7)    VALUE 'QCE0004'.
      *.MC.E ALINFQA114
          05 CA-QC2CATL                    PIC X(7)    VALUE 'QC2CATL'.
          05 CA-WRITEQ2                    PIC X(7)    VALUE 'QA6CWR2'.
          05 CA-DELETEQ2                   PIC X(7)    VALUE 'QA6CDL2'.
          05 CA-QA7CCTS                    PIC X(7)    VALUE 'QA7CCTS'.
          05 CA-GETM                       PIC X(7)    VALUE 'QA6CGTM'.
          05 CA-QG6CTUT                    PIC X(7)    VALUE 'QG6CTUT'.
      *.MC.S.ALINFQA125-37
          05 CA-QA7CROL                    PIC X(7)    VALUE 'QA7CROL'.
          05 CA-NO                         PIC X(1)    VALUE 'N'.
          05 CA-I                          PIC X(1)    VALUE 'I'.
          05 CA-O                          PIC X(1)    VALUE 'O'.
      *.MC.E.ALINFQA125-37
          05 CA-YES                        PIC X(1)    VALUE 'S'.
          05 CA-AT                         PIC X(1)    VALUE '@'.
      *
          05 CA-ABCODE-APP                 PIC X(4)    VALUE 'QCPG'.
          05 CA-ABCODE-QGAB                PIC X(4)    VALUE 'QGAB'.
      *
          05 CA-TBL-SCR                    PIC X(3)    VALUE 'SCR'.
      *.MC.S ALINFQA115
      *   05 C-TABLE-SWA               PIC X(3) VALUE 'SWA'.
      *.MC.E ALINFQA115
          05 VA-SPACE                      PIC X(1)    VALUE SPACES.
          05 CA-ASTERISKS                  PIC X(10)   VALUE ALL '*'.
      *
          05 CA-ABEND-CPTR                 PIC X(20)
                                   VALUE '@QM700023'.
      *
          05 CA-ABC                        PIC X(04)   VALUE '+ABC'.
          05 CA-GTS                        PIC X(04)   VALUE '+GTS'.
          05 CA-CTS                        PIC X(04)   VALUE '+CTS'.
      *.MC.S ALINFQA114
          05 CA-COE                        PIC X(4)    VALUE '+COE'.
          05 CA-COA                        PIC X(4)    VALUE '+COA'.
      *.MC.S ALINFQA114
          05 CA-OUT                        PIC X(03)   VALUE 'OUT'.
          05 CA-DELETE                     PIC X(01)   VALUE 'D'.
          05 CA-DAT-DEFAULT                PIC X(10)
                                   VALUE '0001-01-01'.
          05 CA-TIM-DEFAULT                PIC X(08)   VALUE '00:00:00'.

      *B.PR **********  SWITCHES  **************************************
      *A.PR **********  SWITCHES  **************************************
      *
       01 VA-SWITCHES.
          05 SW-TYP-ERR                    PIC X(1)    VALUE SPACES.
             88 SW-ERR-APPLICATION                     VALUE 'A'.
             88 SW-ERR-CICS                            VALUE 'C'.
             88 SW-ERR-DB2                             VALUE 'D'.

      *
      *B.PR ****  OTHER AREAS OF THE WORKING STORAGE SECTION  **********
      *A.PR ****  OTRAS AREAS DE LA WORKING STORAGE ********************
      *
       01 VA-VARIABLES.
          05 VA-AUXILIARY                  PIC X(01).
          05 VA-CICS-MSG.
             10 FILLER                     PIC X(18)
                                   VALUE 'ERROR CICS - PGM:'.
             10 VA-MSG-PGM1                PIC X(08)   VALUE SPACES.
             10 FILLER                     PIC X(11)
                                   VALUE '. EIBRESP:'.
             10 VN-MSG-EIBRESP             PIC 9(02)   VALUE ZEROES.
             10 FILLER                     PIC X(12)
                                   VALUE '. EIBRSRCE:'.
             10 VA-MSG-EIBRSRCE            PIC X(08)   VALUE SPACES.
             10 FILLER                     PIC X(09)   VALUE '. EIBFN:'.
             10 VA-MSG-EIBFN               PIC X(19)   VALUE SPACES.
             10 FILLER                     PIC X(12)
                                   VALUE '. EIBRESP2:'.
             10 VA-MSG-EIBRESP2            PIC ZZZZZZZZ
                                   VALUE ZEROES.
             10 FILLER                     PIC X(17)   VALUE SPACES.
             10 FILLER                     PIC X(06)   VALUE '.REF:'.
             10 VA-MSG-REF1                PIC X(20)   VALUE SPACES.
      *
          05 VA-MSG-DB2.
             10 FILLER                     PIC X(18)
                                   VALUE 'ERROR DB2  - PGM:'.
             10 VA-MSG-PGM2                PIC X(08)   VALUE SPACES.
             10 FILLER                     PIC X(07)   VALUE '.CODE:'.
             10 VA-SQLCODE-MSG             PIC X(04)   VALUE SPACES.
             10 FILLER                     PIC X(10)
                                   VALUE '.OBJ.ERR:'.
             10 VA-ERR-OBJ-MSG             PIC X(08)   VALUE SPACES.
             10 FILLER                     PIC X(07)   VALUE '.ERRM:'.
             10 VA-SQLERRM-MSG             PIC X(62)   VALUE SPACES.
             10 FILLER                     PIC X(06)   VALUE '.REF:'.
             10 VA-MSG-REF2                PIC X(20)   VALUE SPACES.
      *
      *B.PR.S
      *  IN CASE OF ABEND CAPTURED BY THE ARCHITECTURE THIS FIELD WILL
      *  BE USED TO PUT THE ABEND CODE WHICH HAS BEEN CAPTURED.
      *B.PR.E
      *A.PR.S
      *  EN CASO DE ABEND DETECTADO POR LA ARQUITECTURA, ESTE CAMPO
      *  SE UTILIZA PARA GUARDAR EL CODIGO DE ABEND CAPTURADO.
      *A.PR.E
      *
          05 VA-APP-MSG.
             10 FILLER                     PIC X(18)
                                   VALUE 'ERROR APL. - PGM:'.
             10 VA-MSG-PGM3                PIC X(08)   VALUE SPACES.
             10 FILLER                     PIC X(07)   VALUE SPACES.
             10 VA-MSG-APP-INF-3           PIC X(08)   VALUE SPACES.
      *      10 FILLER             PIC X(83) VALUE SPACES.
             10 FILLER                     PIC X(10)
                                   VALUE '.OBJ.ERR:'.
             10 VA-ERR-MSG-OBJ-3           PIC X(8)    VALUE SPACES.
             10 FILLER                     PIC X(65)   VALUE SPACES.
             10 FILLER                     PIC X(06)   VALUE '.REF:'.
             10 VA-MSG-REF3                PIC X(20)   VALUE SPACES.
      *
          05 VA-ABCODE                     PIC  X(04)  VALUE SPACES.
          05 VN-SQLCODE                    PIC S9(04)  VALUE ZEROES.

          05 VA-SQLCODE                    PIC -999    VALUE ZEROES.
          05 VA-SQLCODE-AL  REDEFINES VA-SQLCODE
                                           PIC X(04).

          05 VN-EIBRESP1                   PIC S9(02)  VALUE ZEROES.
          05 VN-EIBRESP1-Z                 PIC  9(02)  VALUE ZEROES.
          05 VN-EIBRESP2                   PIC S9(15) COMP-3
                                   VALUE ZEROES.
          05 VA-ABCODE-DB2.
             10 FILLER                     PIC X(01)   VALUE 'Q'.
             10 VN-ABCODE-SQL              PIC 9(03)   VALUE ZEROES.
          05 VA-ABCODE-CICS.
             10 FILLER                     PIC X(02)   VALUE 'QC'.
             10 VN-ABCODE-EIB              PIC 9(02)   VALUE ZEROES.

          05 VN-LTH-GETMAIN-N1             PIC S9(9) COMP
                                   VALUE ZERO.
          05 VA-PNT-GETMAIN              POINTER.

          05 CA-NUM-TASK                   PIC X(7)    VALUE ALL '0'.
          05 CN-NUM-TASK9  REDEFINES CA-NUM-TASK
                                           PIC 9(7).

          05 VA-WARNING                    PIC X(93)   VALUE SPACES.
      *
          05 VA-ERR-WARN-CICS.
             10 VA-HEA-WARN-C.
                15 VA-WARN-EIBTRNID-C      PIC X(04)   VALUE SPACES.
                15 FILLER                  PIC X(05)   VALUE '00000'.
                15 FILLER                  PIC X(01)   VALUE 'I'.
                15 FILLER                  PIC X(01)   VALUE 'C'.
             10 VA-DTA-WARN-C.
                15 FILLER                  PIC X(01)   VALUE 'A'.
                15 FILLER                  PIC X(17)
                                   VALUE 'ABEND CICS, PRG:'.
                15 VA-WARN-PGM-C           PIC X(08)   VALUE SPACES.
                15 FILLER                  PIC X(06)   VALUE ', FN:'.
                15 VA-WARN-EIBFN           PIC X(19)   VALUE SPACES.
                15 FILLER                  PIC X(08)   VALUE ', SRCE:'.
                15 VA-WARN-EIBRSRCE        PIC X(08)   VALUE SPACES.
                15 FILLER                  PIC X(08)   VALUE ', RESP:'.
                15 VN-WARN-EIBRESP         PIC 9(02)   VALUE ZEROS.
                15 FILLER                  PIC X(02)   VALUE SPACES.
                15 FILLER                  PIC X(01)   VALUE X'FA'.
                15 FILLER                  PIC X(01)   VALUE X'FB'.
             10 VA-WARN-QUEUE-C.
                15 FILLER                  PIC X(01)   VALUE X'FE'.

          05 VA-ERR-DB2-WARN.
             10 VA-HEA-WARN-D.
                15 VA-WARN-EIBTRNID-D      PIC X(04)   VALUE SPACES.
                15 FILLER                  PIC X(05)   VALUE '00000'.
                15 FILLER                  PIC X(01)   VALUE 'I'.
                15 FILLER                  PIC X(01)   VALUE 'C'.
             10 VA-DTA-WARN-D.
                15 FILLER                  PIC X(01)   VALUE 'A'.
                15 FILLER                  PIC X(16)
                                   VALUE 'ABEND DB2, PRG:'.
                15 VA-WARN-PGM-D           PIC X(08)   VALUE SPACES.
                15 FILLER                  PIC X(06)   VALUE ', RC:'.
                15 VA-WARN-SQLCODE         PIC X(04)   VALUE SPACES.
                15 FILLER                  PIC X(07)   VALUE ', OBJ:'.
                15 VA-ERR-WARN-OBJ         PIC X(08)   VALUE SPACES.
                15 FILLER                  PIC X(07)   VALUE ', REF:'.
                15 VA-WARN-REF             PIC X(20)   VALUE ZEROS.
                15 FILLER                  PIC X(02)   VALUE SPACES.
                15 FILLER                  PIC X(01)   VALUE X'FA'.
                15 FILLER                  PIC X(01)   VALUE X'FB'.
             10 VA-WARN-QUE-D.
                15 FILLER                  PIC X(01)   VALUE X'FE'.
      *
          05 VA-ERR-WARN-APP.
             10 VA-HEA-WARN-E.
                15 VA-WARN-EIBTRNID-E      PIC X(04)   VALUE SPACES.
                15 FILLER                  PIC X(05)   VALUE '00000'.
                15 FILLER                  PIC X(01)   VALUE 'I'.
                15 FILLER                  PIC X(01)   VALUE 'C'.
             10 VA-DTA-WARN-E.
                15 FILLER                  PIC X(01)   VALUE 'A'.
                15 FILLER                  PIC X(17)
                                   VALUE 'ABEND APL., PRG:'.
                15 VA-WARN-PGM-E           PIC X(08)   VALUE SPACES.
                15 FILLER                  PIC X(05)   VALUE SPACES.
                15 FILLER                  PIC X(19)   VALUE SPACES.
                15 FILLER                  PIC X(07)   VALUE ', REF:'.
                15 VA-WARN-REF-E           PIC X(20)   VALUE SPACES.
                15 FILLER                  PIC X(02)   VALUE SPACES.
                15 FILLER                  PIC X(01)   VALUE X'FA'.
                15 FILLER                  PIC X(01)   VALUE X'FB'.
             10 VA-WARN-QUE-E.
                15 FILLER                  PIC X(01)   VALUE X'FE'.
      *
          05 VA-REFERENCE                  PIC X(20)   VALUE SPACES.
      *
          05 VA-TS.
             10 VA-TS-PRFX                 PIC X(04).
             10 VA-TS-SFF                  PIC X(04).
      *
          05 VN-LTH-GETMAIN                PIC S9(9) COMP
                                   VALUE ZEROS.
      *

      *.MC.S ALINFQA125-77
       LOCAL-STORAGE SECTION.
      *.MC.E ALINFQA125-77

      ******************************************************************
      *                LINKAGE SECTION                                 *
      ******************************************************************
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
      *.CO QGECABC
           COPY QGECABC.
      *
      *.MC.S ALINFQA115
      *COPY QACCSWAM.
      *.MC.E ALINFQA115
      *
       COPY QACCOUT.
      *
      ******************************************************************
      *                PROCEDURE DIVISION                              *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 100000-START
      *
           PERFORM 200000-PROCESS
      *
           PERFORM 300000-END.
      *
      ******************************************************************
      *.PN                 100000-START
      *B.PR.S
      *    THE VARIABLES WHICH WILL BE SENT ARE INITIALIZED, APART FROM
      *    THOSE NECESSARY FOR THE HEXADECIMAL-DECIMAL CONVERSION
      *B.PR.E
      *A.PR.S
      *    SE INICIALIZAN LAS VARIABLES A ENVIAR, JUNTO CON AQUELLAS
      *    NECESARIAS PARA LA CONVERSION HEXADECIMAL-DECIMAL.
      *A.PR.E
      ******************************************************************
       100000-START.
      *
           MOVE WCO2C-CICS-NORMAL          TO EIBRESP
           MOVE WCO2C-CICS-EIBRCODE-OK TO EIBRCODE

           MOVE SPACES     TO VA-ABCODE
           INITIALIZE      QGECLOG1
           MOVE SPACES     TO LOG1-ORIGIN
           MOVE SPACES     TO LOG1-MESSAGE
           MOVE SPACES     TO HEX-CODERR
           MOVE SPACES     TO HEX-DECIFIELD
           MOVE SPACES     TO HEX-HXDMFIELD
           MOVE ZEROES     TO HEX-MSGLENGT.
      *
      ******************************************************************
      *.PN                 200000-PROCESS
      *B.PR.S
      *   FIELDS TO BE SENT ARE MOVED, COMMON TO BOTH TYPES OF ERROR,
      *   TO THE OUTPUT VARIABLES (COMMAREA FORMAT).
      *   CONNECTION WITH QG1CLOG, TO WHICH THE OUTPUT MESSAGES ARE SENT
      *
      *   IF THE ABC-REFERENCIA FIELD IS A MULTI-LANGUAGE LITERAL (SO
      *   ITS FIRST CHARACTER IS @) THE PARAGRAPH WHERE THAT LITERAL IS
      *   DECODED, IS CALLED, UNLESS IF THE PROGRAM THAT HAS CALLED TO
      *   THIS PROGRAM (QG1CABC) IS THE ONE THAT PERFORMS THAT OPERATION
      *   (THAT IS, THE QC2CATL). THIS EXCEPTION IS DONE TO AVOID A
      *   LOOP.
      *B.PR.E
      *A.PR.S
      *   SE MUEVEN LOS CAMPOS A SER ENVIADOS, TANTO LOS TIPOS DE ERROR
      *   COMO LAS VARIABLES DE SALIDA.
      *
      *   SI EL CAMPO ABC-REFERENCIA ES UN LITERAL MULTI-LENGUAJE (SU
      *   PRIMER CARACTER ES @), SE LLAMA AL PARRAFO QUE DECODIFICA EL
      *   LITERAL, A MENOS QUE EL PROGRAMA QUE HA LLAMADO A ESTE
      *   PROGRAMA (QG1CABC), SEA EL QUE EJECUTA ESTA OPERACION
      *   (QC2CATL). ESTA EXCEPCION EVITA UN POSIBLE BUCLE.
      *A.PR.E
      ******************************************************************
       200000-PROCESS.
      *
           MOVE ABC-DES-PROG      TO LOG1-ORIGIN
      *
           MOVE ABC-REFERENCE1    TO VA-REFERENCE
      *.MC.S ALINFQA125-39
           INITIALIZE QGECTUT

      *.MC.S ALINFQA117-11
      **.MC.S.ALINFQA125-37
      *    IF ABC-ABEND EQUAL C-NO OR C-YES
      **.MC.E.ALINFQA125-37
      *.MC.E ALINFQA117-11
           SET  TUT-SW-OPE-REA            TO TRUE
           CALL CA-QG6CTUT USING DFHEIBLK VA-AUXILIARY QGECTUT
             ON EXCEPTION
                 MOVE WCO2C-CICS-LINK           TO EIBFN
                 MOVE WCO2C-CICS-PGMIDERR       TO EIBRESP
                 MOVE CA-QG6CTUT           TO EIBRSRCE
           END-CALL
      *.MC.S ALINFQA117-11
      **.MC.S.ALINFQA125-37
      *    END-IF
      **.MC.E.ALINFQA125-37
      *.MC.E ALINFQA117-11

           IF EIBRESP NOT EQUAL WCO2C-CICS-NORMAL
      *.MC.S ALINFQA117-11
             IF ABC-ABEND EQUAL CA-NO OR CA-YES
      *.MC.E ALINFQA117-11
                 MOVE CA-YES                     TO ABC-ABEND
      *.MC.S ALINFQA117-11
             ELSE
                 MOVE WCO2C-CICS-NORMAL               TO EIBRESP
             END-IF
      *.MC.E ALINFQA117-11
           END-IF
      *.MC.E ALINFQA125-39
      *
           IF ABC-DES-PROG EQUAL CA-QC2CATL  OR
              ABC-DES-PROG EQUAL CA-QA7CSQ1
              CONTINUE
           ELSE
              IF ABC-REFERENCE1(1:1) EQUAL CA-AT
                 PERFORM 210000-TRANSLATE-LITERAL
              END-IF
           END-IF
      *
      *.MC.S ALINFQA125-39
      *    INITIALIZE QGECTUT
      *.MC.E ALINFQA125-39
      *.MC.S.ALINFQA125-37
      *.MC.S ALINFQA125-39
      *    IF ABC-ABEND EQUAL C-NO OR C-YES
      *.MC.E ALINFQA125-39
      *.MC.E.ALINFQA125-37
      *.MC.S ALINFQA125-39
      *       SET  TUT-88-OP-READ            TO TRUE
      *       CALL C-QG6CTUT USING DFHEIBLK W-AUX QGECTUT
      *.MC.E ALINFQA125-39
      *.MC.S ALINFQA122
      *.MC.S ALINFQA125-39
      *          ON EXCEPTION
      *             MOVE CICS-LINK           TO EIBFN
      *             MOVE CICS-PGMIDERR       TO EIBRESP
      *             MOVE C-QG6CTUT           TO EIBRSRCE
      *       END-CALL
      *.MC.E ALINFQA125-39
      *.MC.E ALINFQA122
      *.MC.S.ALINFQA125-37
      *.MC.S ALINFQA125-39
      *    END-IF
      *.MC.E ALINFQA125-39
      *.MC.E.ALINFQA125-37
      *
      *.MC.S ALINFQA125-39
      *    IF EIBRESP NOT EQUAL CICS-NORMAL
      *       MOVE C-YES                     TO ABC-ABEND
      *    END-IF
      *.MC.E ALINFQA125-39
      *
           IF RED-SQLCODE EQUAL LOW-VALUES
              IF RED-EIBRESP1 EQUAL LOW-VALUES
                 SET SW-ERR-APPLICATION TO TRUE
                 MOVE ZEROS TO RED-SQLCODE RED-EIBRESP1
              ELSE
                 SET SW-ERR-CICS        TO TRUE
                 MOVE ZEROS TO RED-SQLCODE
              END-IF
           ELSE
              SET SW-ERR-DB2            TO TRUE
              MOVE ZEROS TO RED-EIBRESP1
           END-IF
      *
           EVALUATE TRUE
              WHEN SW-ERR-APPLICATION
                 PERFORM 220000-APPLICATION-ERROR
                 MOVE VA-ERR-WARN-APP     TO VA-WARNING
              WHEN SW-ERR-CICS
                 PERFORM 230000-CICS-ERROR
                 MOVE VA-ERR-WARN-CICS      TO VA-WARNING
              WHEN SW-ERR-DB2
                 PERFORM 240000-DB2-ERROR
                 MOVE VA-ERR-DB2-WARN       TO VA-WARNING
           END-EVALUATE
      *
      *.MC.S ALINFQA121
      *    IF TUT-88-SYNCPOINT-PERMITIDO
      *       CALL C-ERROR-ROUTINE USING DFHEIBLK QGECLOG1
      *       EVALUATE EIBRESP
      *          WHEN CICS-NORMAL
      *             CONTINUE
      *          WHEN OTHER
      *             MOVE C-ABCODE-QGAB   TO W-ABCODE
      *       END-EVALUATE
      *    ELSE
      *       CALL C-ERROR-ROUTINE USING DFHEIBLK QGECLOG1
      *       EVALUATE EIBRESP
      *          WHEN CICS-NORMAL
      *             CONTINUE
      *          WHEN OTHER
      *             MOVE C-ABCODE-QGAB   TO W-ABCODE
      *       END-EVALUATE
      *    END-IF.
           CALL CA-ERR-ROUTINE USING DFHEIBLK QGECLOG1
      *.MC.S ALINFQA122
              ON EXCEPTION
                 MOVE WCO2C-CICS-LINK       TO EIBFN
                 MOVE WCO2C-CICS-PGMIDERR   TO EIBRESP
                 MOVE CA-ERR-ROUTINE TO EIBRSRCE
           END-CALL
      *.MC.E ALINFQA122
           EVALUATE EIBRESP
               WHEN WCO2C-CICS-NORMAL
                   CONTINUE
               WHEN OTHER
                   MOVE CA-ABCODE-QGAB   TO VA-ABCODE
           END-EVALUATE.
      *.MC.E ALINFQA121
      *
      ******************************************************************
      *.PN                 210000-TRANSLATE-LITERAL
      *B.PR.S
      *   IF THE APPLICATION PROGRAM HAS INFORMED ON THE ABC-REFERENCIA
      *   FIELD A MULTI-LANGUAGE LITERAL (THE FIRST CHARACTER OF THE
      *   FIELD WILL BE AN @), THE LITERAL IS DECODED AND SET TO THE
      *   SYSTEM LANGUAGE. THIS PARAGRAPH PERFORMS THE PROCESS OF
      *   OBTAINING THE SYSTEM LANGUAGE FROM THE QADTSWA TABLE AND CALLS
      *   TO THE ROUTINE THAT PERFORMS THE DECODING.
      *B.PR.E
      *A.PR.S
      *   SI EL PROGRAMA DE APLICACION HA INFORMADO EN EL CAMPO
      *   ABC-REFERENCIA UN LITERAL MULTI-LENGUAJE (EL PRIMER CARACTER
      *   DEL CAMPO ES EL CARACTER @), EL LITERAL ES DECODIFICADO Y
      *   ESTABLECIDO AL LENGUAJE DEL SISTEMA. ESTE PARRAFO SE OCUPA
      *   DEL PROCESSO DE OBTENER EL LENGUAJE DEL SISTEMA MEDIANTE LA
      *   TABLA QADTSWA, Y LLAMAR  A LA RUTINA QUE DESARROLLA LA
      *   DECODIFICACION.
      *A.PR.E
      ******************************************************************
       210000-TRANSLATE-LITERAL.
      *
      *.MC.S ALINFQA115
      *    INITIALIZE QAECSQ1
      *    MOVE C-TABLE-SWA      TO SQ1-TABLA
      *    MOVE C-10-ASTERISKS   TO SQ1-CLAVE
      *    CALL C-QA7CSQ1 USING DFHEIBLK QAECSQ1
      *
      *    EVALUATE SQ1-CODERR
      *        WHEN SPACES
      *            SET ADDRESS OF QACCSWAM         TO SQ1-PTR-TAB
      *            MOVE SWAM-IDIOMA-SIST           TO ATL-IDIOMA
      *            INITIALIZE QAECMEMC
      *            SET CICS-ACTION-FREEMAIN        TO TRUE
      *            SET CICS-MEM-PTR                TO SQ1-PTR-TAB
      *            MOVE LENGTH OF QACCSWAM         TO CICS-MEM-LENGTH
      *            CALL C-FREEMAIN USING DFHEIBLK W-AUX QAECMEMC
      *            IF EIBRESP NOT EQUAL CICS-NORMAL
      *               MOVE C-YES                   TO ABC-ABEND
      *            END-IF
      *        WHEN OTHER
      *            MOVE SQ1-SQLCODE            TO W-SQLCODE
      *            MOVE W-SQLCODE              TO W-Z-SQLCODE
      *            MOVE W-Z-SQLCODE-ALFA       TO W-ABCODE-DB2
      *            MOVE W-ABCODE-DB2           TO W-ABCODE
      *            MOVE SPACES                 TO ATL-IDIOMA
      *    END-EVALUATE
      *

      *.MC.S ALINFQA125-39
           MOVE TUT-ENT-COD            TO FEC-ENT-RTV
      *.MC.E ALINFQA125-39
           CALL CA-QG9CSWA0     USING QGECFEC
      *.MC.S ALINFQA122
              ON EXCEPTION
                 MOVE CA-ABCODE-QGAB    TO VA-ABCODE
                 MOVE ZEROS            TO FEC-RETURN
           END-CALL
      *.MC.E ALINFQA122

           EVALUATE FEC-RETURN
           WHEN ZEROS
               CONTINUE
           WHEN OTHER
               MOVE FEC-SQLCODE        TO VN-SQLCODE
               MOVE VN-SQLCODE          TO VA-SQLCODE
               MOVE VA-SQLCODE-AL   TO VA-ABCODE-DB2
               MOVE VA-ABCODE-DB2       TO VA-ABCODE
           END-EVALUATE
      *.MC.E ALINQA115
           INITIALIZE QCECATL
           MOVE ABC-REFERENCE1(2:8)    TO ATL-KEY-8
      *.MC.S ALINFQA115
           MOVE FEC-SYSTEM-LANG        TO ATL-LANGUA
      *    MOVE SWAM-IDIOMA-SIST       TO ATL-IDIOMA
      *.MC.E ALINFQA115
      *
           CALL CA-QC2CATL  USING DFHEIBLK VA-QCECATL-01
      *.MC.S ALINFQA122
              ON EXCEPTION
                 MOVE WCO2C-CICS-LINK        TO EIBFN
                 MOVE WCO2C-CICS-PGMIDERR    TO EIBRESP
                 MOVE CA-QC2CATL        TO EIBRSRCE
           END-CALL
      *.MC.E ALINFQA122
      *
           EVALUATE EIBRESP
               WHEN WCO2C-CICS-NORMAL
                  IF ATL-ERRCOD EQUAL SPACES
                     MOVE ATL-TXT-LNG(1:20)    TO ABC-REFERENCE1
                  ELSE
                     MOVE SPACES               TO ABC-REFERENCE1
                  END-IF
               WHEN OTHER
                  MOVE CA-ABCODE-QGAB           TO VA-ABCODE
           END-EVALUATE.
      *
      ******************************************************************
      *.PN                 220000-APPLICATION-ERROR
      *B.PR.S
      *   FORMAT OF THE OUTPUT MESSAGE IN THE CASE OF APPLICATION ERROR
      *B.PR.E
      *A.PR.S
      *   FORMATEA EL MENSAJE DE SALIDA EN EL CASO DE ERROR DE
      *   APLICACION.
      *A.PR.E
      ******************************************************************
       220000-APPLICATION-ERROR.
      *
           MOVE TUT-TRANSACTION          TO VA-WARN-EIBTRNID-E
           MOVE ABC-DES-PROG             TO VA-MSG-PGM3
                                            VA-WARN-PGM-E
           MOVE ABC-EIBRSRCE             TO VA-MSG-APP-INF-3
                                            VA-WARN-REF-E
           MOVE ABC-REFERENCE1           TO VA-MSG-REF3
           MOVE ABC-OBJECT-ERROR         TO VA-ERR-MSG-OBJ-3
           MOVE VA-APP-MSG          TO LOG1-MESSAGE.
      *
      ******************************************************************
      *.PN                 230000-CICS-ERROR
      *B.PR.S
      *   THE ERROR MESSAGE FOR THE CODE EIBFN IS ESTABLISHED, AS WELL
      *   AS THE OTHER VALUES TO SEND IN CASE OF CICS ERROR. THE OUTPUT
      *   CICS ERROR MESSAGE IS FORMATTED.
      *   THE CONVERSION TO DECIMAL CHARACTERS OF THE CODE EIBFN IS
      *   DONE, IN ORDER TO APPEAR IN THE OUTPUT MESSAGE.
      *B.PR.E
      *A.PR.S
      *   EL MENSAJE DE ERROR PARA LA VARIABLE EIBFN ES ESTABLECIDO,
      *   AL IGUAL QUE LOS OTROS VALORES A ENVIAR, EN CASO DE ERROR
      *   DE CICS. EL MENSAJE DE SALIDA DE ERROR DE CICS ES FORMATEADO.
      *   SE REALIZA LA CONVERSION A CARACTERES DECIMALES DEL CODIGO
      *   DE EIBFN, PARA QUE APAREZCA EN EL MENSAJE DE SALIDA.
      *A.PR.E
      ******************************************************************
       230000-CICS-ERROR.
      *
           EVALUATE ABC-EIBFN
              WHEN (X'0204')
                 MOVE 'HANDLE CONDITION'             TO ABC-EIBFN
              WHEN (X'0208')
                 MOVE 'ASSIGN'                       TO VA-MSG-EIBFN
              WHEN (X'0402')
                 MOVE 'RECEIVE'                      TO VA-MSG-EIBFN
              WHEN (X'0404')
                 MOVE 'SEND'                         TO VA-MSG-EIBFN
              WHEN (X'040C')
                 MOVE 'WAIT TERMINAL'                TO VA-MSG-EIBFN
              WHEN (X'0602')
                 MOVE 'READ'                         TO VA-MSG-EIBFN
              WHEN (X'0604')
                 MOVE 'WRITE'                        TO VA-MSG-EIBFN
              WHEN (X'0606')
                 MOVE 'REWRITE'                      TO VA-MSG-EIBFN
              WHEN (X'0608')
                 MOVE 'DELETE'                       TO VA-MSG-EIBFN
              WHEN (X'060A')
                 MOVE 'UNLOCK'                       TO VA-MSG-EIBFN
              WHEN (X'060C')
                 MOVE 'STARTBR'                      TO VA-MSG-EIBFN
              WHEN (X'060E')
                 MOVE 'READNEXT'                     TO VA-MSG-EIBFN
              WHEN (X'0610')
                 MOVE 'READPREV'                     TO VA-MSG-EIBFN
              WHEN (X'0612')
                 MOVE 'ENDBR'                        TO VA-MSG-EIBFN
              WHEN (X'0614')
                 MOVE 'RESETBR'                      TO VA-MSG-EIBFN
              WHEN (X'0802')
                 MOVE 'WRITEQ TD'                    TO VA-MSG-EIBFN
              WHEN (X'0804')
                 MOVE 'READQ TD'                     TO VA-MSG-EIBFN
              WHEN (X'0806')
                 MOVE 'DELETEQ TD'                   TO VA-MSG-EIBFN
              WHEN (X'0A02')
                 MOVE 'WRITEQ TS'                    TO VA-MSG-EIBFN
              WHEN (X'0A04')
                 MOVE 'READQ TS'                     TO VA-MSG-EIBFN
              WHEN (X'0A06')
                 MOVE 'DELETEQ TS'                   TO VA-MSG-EIBFN
              WHEN (X'0C02')
                 MOVE 'GETMAIN'                      TO VA-MSG-EIBFN
              WHEN (X'0C04')
                 MOVE 'FREEMAIN'                     TO VA-MSG-EIBFN
              WHEN (X'0E02')
                 MOVE 'LINK'                         TO VA-MSG-EIBFN
              WHEN (X'0E04')
                 MOVE 'XCTL'                         TO VA-MSG-EIBFN
              WHEN (X'0E06')
                 MOVE 'LOAD'                         TO VA-MSG-EIBFN
              WHEN (X'0E08')
                 MOVE 'RETURN'                       TO VA-MSG-EIBFN
              WHEN (X'0E0C')
                 MOVE 'ABEND'                        TO VA-MSG-EIBFN
              WHEN (X'1002')
                 MOVE 'ASKTIME'                      TO VA-MSG-EIBFN
              WHEN (X'1004')
                 MOVE 'DELAY'                        TO VA-MSG-EIBFN
              WHEN (X'1008')
                 MOVE 'START'                        TO VA-MSG-EIBFN
              WHEN (X'100A')
                 MOVE 'RETRIEVE'                     TO VA-MSG-EIBFN
              WHEN (X'100C')
                 MOVE 'CANCEL'                       TO VA-MSG-EIBFN
              WHEN (X'1202')
                 MOVE 'WAIT EVENT'                   TO VA-MSG-EIBFN
              WHEN (X'1204')
                 MOVE 'ENQ'                          TO VA-MSG-EIBFN
              WHEN (X'1206')
                 MOVE 'DEQ'                          TO VA-MSG-EIBFN
              WHEN (X'1602')
                 MOVE 'SYNCPOINT'                    TO VA-MSG-EIBFN
              WHEN (X'1802')
                 MOVE 'RECEIVE MAP'                  TO VA-MSG-EIBFN
              WHEN (X'1804')
                 MOVE 'SEND MAP'                     TO VA-MSG-EIBFN
              WHEN (X'1806')
                 MOVE 'SEND TEXT'                    TO VA-MSG-EIBFN
              WHEN (X'1608')
                 MOVE 'SEND PAGE'                    TO VA-MSG-EIBFN
              WHEN (X'4A02')
                 MOVE 'ASKTIME ABSTIME'              TO VA-MSG-EIBFN
              WHEN (X'4A04')
                 MOVE 'FORMATTIME'                   TO VA-MSG-EIBFN
              WHEN OTHER
                 MOVE 2               TO  HEX-MSGLENGT
                 MOVE ABC-EIBFN       TO  HEX-DECIFIELD
                 CALL CA-HXDM-RUT USING QRECHEX
      *.MC.S ALINFQA122
                    ON EXCEPTION
                       MOVE HEX-DECIFIELD   TO HEX-HXDMFIELD
                 END-CALL
      *.MC.E ALINFQA122
                 MOVE HEX-HXDMFIELD   TO  VA-MSG-EIBFN
           END-EVALUATE

           MOVE ABC-DES-PROG             TO VA-MSG-PGM1
                                            VA-WARN-PGM-C
           MOVE TUT-TRANSACTION          TO VA-WARN-EIBTRNID-C
           MOVE VA-MSG-EIBFN              TO VA-WARN-EIBFN
           MOVE ABC-EIBRSRCE             TO VA-MSG-EIBRSRCE
                                            VA-WARN-EIBRSRCE
           MOVE ABC-EIBRESP1             TO VN-EIBRESP1
           MOVE VN-EIBRESP1               TO VN-EIBRESP1-Z
           MOVE VN-EIBRESP1-Z             TO VN-MSG-EIBRESP
                                            VN-WARN-EIBRESP
           MOVE ABC-EIBRESP2             TO VN-EIBRESP2
           MOVE VN-EIBRESP2        TO VA-MSG-EIBRESP2
           MOVE ABC-REFERENCE1           TO VA-MSG-REF1
           MOVE VA-CICS-MSG           TO LOG1-MESSAGE.
      *
      ******************************************************************
      *.PN                 240000-DB2-ERROR
      *B.PR.S
      *   FORMAT OF THE OUTPUT MESSAGE IN THE CASE OF DB2 ERROR.
      *B.PR.E
      *A.PR.S
      *   FORMATEA EL MENSAJE DE SALIDA EN EL CASO DE ERROR DB2.
      *A.PR.E
      ******************************************************************
       240000-DB2-ERROR.
      *
           MOVE TUT-TRANSACTION          TO VA-WARN-EIBTRNID-D
           MOVE ABC-DES-PROG             TO VA-MSG-PGM2
                                            VA-WARN-PGM-D
           MOVE ABC-OBJECT-ERROR         TO VA-ERR-OBJ-MSG
                                            VA-ERR-WARN-OBJ
           MOVE ABC-SQLCODE              TO VN-SQLCODE
           MOVE VN-SQLCODE                TO VA-SQLCODE
           MOVE VA-SQLCODE-AL         TO VA-SQLCODE-MSG
                                            VA-WARN-SQLCODE
           MOVE ABC-SQLERRM              TO VA-SQLERRM-MSG
           MOVE ABC-REFERENCE1           TO VA-MSG-REF2
                                            VA-WARN-REF
           MOVE VA-MSG-DB2            TO LOG1-MESSAGE.
      *
      ******************************************************************
      *.PN                     300000-END
      *B.PR.S
      *  THE ABEND CODE FOR THE OUTPUT IS FORMATTED, IF ABC-ABEND IS
      *  POSITIVE. IF NOT, THE CONTROL IS RETURNED TO THE PROGRAM THAT
      *  CALLED.
      *B.PR.E
      *A.PR.S
      *  SE FORMATEA EL CODIGO DE ABEND PARA LA SALIDA, SI ABC-ABEND
      *  ES POSITIVO. SI NO, SE DEVUELVE EL CONTROL AL PROGRAMA QUE LO
      *  LLAMO.
      *A.PR.E
      ******************************************************************
       300000-END.
      *
           IF VA-ABCODE EQUAL SPACES
               EVALUATE TRUE
                  WHEN SW-ERR-APPLICATION
                     IF VA-REFERENCE   NOT EQUAL CA-ABEND-CPTR
                       MOVE CA-ABCODE-APP TO VA-ABCODE
                     ELSE
                       MOVE ABC-EIBRSRCE    TO VA-ABCODE
                     END-IF
                  WHEN SW-ERR-CICS
                     MOVE VN-EIBRESP1-Z  TO VN-ABCODE-EIB
                     MOVE VA-ABCODE-CICS TO VA-ABCODE
                  WHEN SW-ERR-DB2
                    MOVE VA-SQLCODE   TO VN-ABCODE-SQL
                    MOVE VA-ABCODE-DB2  TO VA-ABCODE
               END-EVALUATE
           END-IF
      *
      *.MC.S ALINFQA121
      *    PERFORM 310000-CREATE-ABC-TS-QUEUE
      *.MC.S.ALINFQA125-37
           IF ABC-ABEND EQUAL CA-NO OR CA-YES
      *.MC.E.ALINFQA125-37
              IF (TUT-TERMINAL  EQUAL SPACES
                 OR TUT-TERMINAL  EQUAL LOW-VALUES)
                 MOVE WCO2C-CICS-ABENDERR TO EIBRESP
                 GOBACK
              END-IF
              PERFORM 310000-NOTIFY-ABEND
              IF ABC-ABEND EQUAL CA-YES
                 PERFORM 320000-DELETE-TS-QUEUE
                 MOVE WCO2C-CICS-ABENDERR      TO EIBRESP
                 GOBACK
              ELSE
                 MOVE WCO2C-CICS-NORMAL        TO EIBRESP
                 GOBACK
              END-IF
      *.MC.S.ALINFQA125-37
           END-IF
           IF ABC-ABEND EQUAL CA-I OR CA-O
              CALL CA-QA7CROL USING DFHEIBLK VA-AUXILIARY
                 ON EXCEPTION
                    MOVE WCO2C-CICS-PGMIDERR        TO EIBRESP
                    MOVE WCO2C-CICS-LINK            TO EIBFN
                    MOVE CA-QA7CROL            TO EIBRSRCE
              END-CALL

              EVALUATE EIBRESP
                 WHEN WCO2C-CICS-NORMAL
                    CONTINUE
                 WHEN OTHER
                    CALL CA-ERR-ROUTINE USING DFHEIBLK QGECLOG1
                       ON EXCEPTION
                          MOVE WCO2C-CICS-LINK       TO EIBFN
                          MOVE WCO2C-CICS-PGMIDERR   TO EIBRESP
                          MOVE CA-ERR-ROUTINE TO EIBRSRCE
                    END-CALL
              END-EVALUATE
              IF ABC-ABEND EQUAL CA-I
                 STOP RUN
              ELSE
                 GOBACK
              END-IF
           END-IF.
      *.MC.E.ALINFQA125-37
      *.MC.E ALINFQA121
      *
      *.MC.S.ALINFQA125-37
      *    IF ABC-ABEND EQUAL C-YES
      *       PERFORM 320000-DELETE-TS-QUEUE
      *       MOVE CICS-ABENDERR      TO EIBRESP
      *       GOBACK
      *    ELSE
      *       MOVE CICS-NORMAL        TO EIBRESP
      *       GOBACK
      *    END-IF.
      *.MC.E.ALINFQA125-37
      *
      *.MC.S ALINFQA121 - PARRAFO-NUEVO
      ******************************************************************
      *.PN            310000-NOTIFY-ABEND                              *
      ******************************************************************
       310000-NOTIFY-ABEND.
      *
           IF ABC-DES-PROG NOT EQUAL CA-QG6CTUT
               SET TUT-SW-OPE-UPDAT        TO TRUE
               SET TUT-SW-ROLLBACK         TO TRUE
               CALL CA-QG6CTUT USING DFHEIBLK VA-AUXILIARY QGECTUT
      *.MC.S ALINFQA122
                  ON EXCEPTION
                     MOVE WCO2C-CICS-LINK       TO EIBFN
                     MOVE WCO2C-CICS-PGMIDERR   TO EIBRESP
                     MOVE CA-QG6CTUT       TO EIBRSRCE
               END-CALL
      *.MC.E ALINFQA122
               IF EIBRESP NOT EQUAL WCO2C-CICS-NORMAL
                   MOVE CA-YES              TO ABC-ABEND
               END-IF
           END-IF.
      *.MC.E ALINFQA121 - PARRAFO NUEVO FIN

      *.MC.S ALINFQA121
      ******************************************************************
      *.PN            310000-CREATE-ABC-TS-QUEUE                       *
      *B.PR.S
      *  THIS PARAGRAPH CREATES THE TS QUEUE +ABCTTTT, NECCESARY TO
      *  DELETE THE +DC AND +TOT QUEUES AND DETECT IN THE ARCHITECTURE
      *  MODULES THAT AN ABEND HAS OCCURED.
      *B.PR.E
      *A.PR.S
      *  ESTE PARRAFO CREA LA COLA TS +ABCTTTT, NECESARIA PARA BORRAR
      *  LAS COLAS +DC Y +TOT Y DETECTAR EN LOS MODULOS DE ARQUITECTURA
      *  QUE HA OCURRIDO UN ABEND.
      *A.PR.E
      ******************************************************************
      *310000-CREATE-ABC-TS-QUEUE.
      *
      *    MOVE TUT-TERMINAL           TO W-TS-SUFFIX
      *
      *    IF ABC-PROGRAMA EQUAL C-WRITEQ2
      *       CONTINUE
      *    ELSE
      *       IF ABC-PROGRAMA EQUAL C-DELETEQ2
      *          INITIALIZE QAECTS1C
      *          MOVE C-ABC                  TO W-TS-PREFIX
      *          MOVE W-TS                   TO CICS-QUEUE
      *          MOVE +1                     TO CICS-ITEM
      *          SET CICS-88-REWRITE-YES     TO TRUE
      *          MOVE LENGTH OF W-ABCODE     TO CICS-LENGTH
      *          CALL C-WRITEQ2  USING DFHEIBLK W-AUX QAECTS1C W-ABCODE
      *
      *          EVALUATE EIBRESP
      *             WHEN (CICS-NORMAL)
      *                CONTINUE
      *             WHEN (CICS-QIDERR)
      *                INITIALIZE QAECTS1C
      *                MOVE C-ABC                  TO W-TS-PREFIX
      *                MOVE W-TS                   TO CICS-QUEUE
      *                MOVE 0                      TO CICS-ITEM
      *                SET CICS-88-REWRITE-NO      TO TRUE
      *                MOVE LENGTH OF W-ABCODE     TO CICS-LENGTH
      *                CALL C-WRITEQ2  USING DFHEIBLK W-AUX QAECTS1C
      *                                      W-ABCODE
      *                EVALUATE EIBRESP
      *                   WHEN (CICS-NORMAL)
      *                      CONTINUE
      *                   WHEN OTHER
      *                      MOVE C-YES      TO ABC-ABEND
      *                END-EVALUATE
      *             WHEN OTHER
      *                MOVE C-YES            TO CICS-ABENDERR
      *          END-EVALUATE
      *       ELSE
      *          INITIALIZE QAECTS1C
      *          MOVE C-ABC                  TO W-TS-PREFIX
      *          MOVE W-TS                   TO CICS-QUEUE
      *          CALL C-DELETEQ2 USING DFHEIBLK W-AUX QAECTS1C
      *
      *          EVALUATE EIBRESP
      *             WHEN (CICS-NORMAL)
      *                CONTINUE
      *             WHEN (CICS-QIDERR)
      *                CONTINUE
      *             WHEN OTHER
      *                MOVE C-YES            TO ABC-ABEND
      *          END-EVALUATE
      *
      *          INITIALIZE QAECTS1C
      *          MOVE C-ABC                  TO W-TS-PREFIX
      *          MOVE W-TS                   TO CICS-QUEUE
      *          MOVE 0                      TO CICS-ITEM
      *          SET CICS-88-REWRITE-NO      TO TRUE
      *          MOVE LENGTH OF W-ABCODE     TO CICS-LENGTH
      *          MOVE 0                      TO CICS-NUMITEMS
      *          CALL C-WRITEQ2  USING DFHEIBLK W-AUX QAECTS1C W-ABCODE
      *       END-IF
      *    END-IF.
      *.MC.E ALINFQA121
      *
      ******************************************************************
      *.PN            320000-DELETE-TS-QUEUE
      *B.PR.S
      *     ALL THE POSSIBLE TS ARE DELETED.
      *       - PAGING QUEUES (+GTS,+CTS)
      *       - +OUT: IS STORED ON QADTOUT. INSTEAD OF DELETING, IT IS
      *         UPDATED WITH NON-SIGNIFICANT VALUES.
      *B.PR.E
      *A.PR.S
      *     SE BORRAN TODAS LAS COLAS TS.
      *       - COLAS DE PAGINACION (+GTS, +CTS)
      *       - +OUT: SE ALMACENA SOBRE LA TABLA QADTOUT. EN VEZ DE
      *         BORRARLA, SE ALMACENA CON VALORES INSIGNIFICANTES.
      *A.PR.E
      ******************************************************************
       320000-DELETE-TS-QUEUE.
      *
           IF ABC-DES-PROG EQUAL CA-DELETEQ2
              CONTINUE
           ELSE
              INITIALIZE QAECTS1C
      *.MC.S ALINFQA121
              MOVE TUT-TERMINAL           TO VA-TS-SFF
      *.MC.E ALINFQA121
              MOVE CA-GTS                  TO VA-TS-PRFX
              MOVE VA-TS                   TO ETSIC-CICS-QUEUE
              CALL CA-DELETEQ2 USING DFHEIBLK VA-AUXILIARY QAECTS1C
      *.MC.S ALINFQA122
                 ON EXCEPTION
                    MOVE WCO2C-CICS-LINK       TO EIBFN
                    MOVE WCO2C-CICS-PGMIDERR   TO EIBRESP
                    MOVE CA-DELETEQ2      TO EIBRSRCE
              END-CALL
      *.MC.E ALINFQA122
      *
              EVALUATE EIBRESP
                 WHEN (WCO2C-CICS-NORMAL)
                    CONTINUE
                 WHEN (WCO2C-CICS-QIDERR)
                    CONTINUE
                 WHEN OTHER
                    MOVE CA-YES            TO ABC-ABEND
              END-EVALUATE


              MOVE CA-CTS                  TO VA-TS-PRFX
              MOVE VA-TS                   TO CCTS-NAME
              CALL CA-QA7CCTS USING DFHEIBLK  QACCCTS CA-DELETE
      *.MC.S ALINFQA122
                 ON EXCEPTION
                    MOVE WCO2C-CICS-LINK       TO EIBFN
                    MOVE WCO2C-CICS-PGMIDERR   TO EIBRESP
                    MOVE CA-QA7CCTS       TO EIBRSRCE
              END-CALL
      *.MC.E ALINFQA122
      *
              EVALUATE EIBRESP
                 WHEN (WCO2C-CICS-NORMAL)
                    CONTINUE
                 WHEN (WCO2C-CICS-QIDERR)
                    CONTINUE
                 WHEN OTHER
                    MOVE CA-YES            TO ABC-ABEND
              END-EVALUATE
           END-IF

           IF ABC-DES-PROG EQUAL CA-QA7CSQ2
              CONTINUE
           ELSE
              PERFORM 321000-GETMAIN-OUT
      *
              INITIALIZE QACCOUT
              MOVE VA-TS-SFF            TO COUT-TERMINAL
                                             ESQ2-KEY
              MOVE SPACES                 TO COUT-DTA-C
              MOVE ZEROES                 TO COUT-DTA-L
              MOVE CA-DAT-DEFAULT         TO COUT-DAT-USE
              MOVE CA-TIM-DEFAULT         TO COUT-TIM-USE
              MOVE CA-OUT                  TO ESQ2-TBL
              SET ESQ2-TBL-PNT             TO ADDRESS OF QACCOUT
              CALL CA-QA7CSQ2 USING DFHEIBLK QAECSQ2
      *.MC.S ALINFQA122
                 ON EXCEPTION
                    MOVE CA-YES           TO ABC-ABEND
              END-CALL
      *.MC.E ALINFQA122
      *
              INITIALIZE QAECMEMC
              SET EMEMC-SW-ACT-FREEMAIN    TO TRUE
              SET EMEMC-CICS-MEMO-PNT TO ADDRESS OF QACCOUT
              MOVE LENGTH OF QACCOUT      TO EMEMC-CICS-MEMO-LTH
      *
              CALL CA-FREE-MAIN USING DFHEIBLK VA-AUXILIARY QAECMEMC
      *.MC.S ALINFQA122
                 ON EXCEPTION
                    MOVE WCO2C-CICS-LINK       TO EIBFN
                    MOVE WCO2C-CICS-PGMIDERR   TO EIBRESP
                    MOVE CA-FREE-MAIN      TO EIBRSRCE
              END-CALL
      *.MC.E ALINFQA122
      *
              IF EIBRESP NOT EQUAL WCO2C-CICS-NORMAL
                 MOVE CA-YES               TO ABC-ABEND
              END-IF
           END-IF

      *.MC.S ALINFQA114
           IF ABC-DES-PROG EQUAL CA-QA6CCOR1
              CONTINUE
           ELSE
              MOVE TUT-TERMINAL             TO VA-TS-SFF
              MOVE CA-COE                    TO VA-TS-PRFX
              PERFORM 320500-DELETE-TS-QAECCOR1
              MOVE CA-COA                    TO VA-TS-PRFX
              PERFORM 320500-DELETE-TS-QAECCOR1
           END-IF.
      *
      ******************************************************************
      *.PN              320500-DELETE-TS-QAECCOR1
      ******************************************************************
       320500-DELETE-TS-QAECCOR1.

           INITIALIZE QAECCOR1
           MOVE CA-DELETE            TO ECOR1-OPTION
           MOVE VA-TS                TO ECOR1-KEY
           CALL CA-QA6CCOR1          USING DFHEIBLK QAECCOR1
              ON EXCEPTION
                 MOVE WCO2C-CICS-PGMIDERR TO EIBRESP
                 MOVE WCO2C-CICS-LINK     TO EIBFN
                 MOVE CA-QA6CCOR1    TO EIBRSRCE
           END-CALL

           EVALUATE ECOR1-COD-ERR
              WHEN SPACES
              WHEN (CA-QCE0030)
              WHEN (CA-QCE0004)
                 MOVE SPACES           TO ECOR1-PRFX-QUE-ID
              WHEN OTHER
                 MOVE CA-YES            TO ABC-ABEND
           END-EVALUATE.
      *.MC.E ALINFQA114
      *
      ******************************************************************
      *.PN              321000-GETMAIN-OUT
      ******************************************************************
       321000-GETMAIN-OUT.
      *
           MOVE LENGTH OF QACCOUT                 TO VN-LTH-GETMAIN
      *
           SET EMEMC-SW-ACT-NO-INI-GETMAIN          TO TRUE
           MOVE VN-LTH-GETMAIN                     TO
               EMEMC-CICS-MEMO-LTH
           SET EMEMC-SW-LTH-1              TO TRUE
      *
           CALL CA-GETM USING DFHEIBLK VA-AUXILIARY QAECMEMC
      *.MC.S ALINFQA122
              ON EXCEPTION
                 MOVE WCO2C-CICS-LINK       TO EIBFN
                 MOVE WCO2C-CICS-PGMIDERR   TO EIBRESP
                 MOVE CA-GETM          TO EIBRSRCE
           END-CALL
      *.MC.E ALINFQA122
      *
           EVALUATE EIBRESP
              WHEN (WCO2C-CICS-NORMAL)
                 CONTINUE
              WHEN OTHER
                 MOVE CA-YES                       TO ABC-ABEND
           END-EVALUATE
      *
           SET ADDRESS OF QACCOUT                 TO
               EMEMC-CICS-MEMO-PNT.
      *
      *
      ******************************************************************
      *B.PR                    END OF PROGRAM                          *
      *A.PR                  FINAL DEL PROGRAMA                        *
      ******************************************************************
