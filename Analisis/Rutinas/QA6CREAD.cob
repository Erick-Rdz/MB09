      ******************************************************************
      * WINDOWS 2000  MIGRATION - SQLCODE FOR SQLSERVER                *
      *                                                                *
      * ANALYZED PROGRAM. CONVERSION CODE IS MARKED WITH THIS TEXT     *
      * PSCW0001-SDF --------> SDF CHANGE - START CHANGE IN PROCEDURE  *
      * FSCW0001-SDF --------> SDF CHANGE - END CHANGE IN PROCEDURE    *
      *                                                                *
      * PSCW0002-SDF---------> SDF CHANGE - START CHANGE IN WORKING    *
      * FSCW0002-SDF---------> SDF CHANGE - END CHANGE IN WORKING      *
      *                                                                *
      ******************************************************************
      *****************************************************************
      *                  IDENTIFICATION DIVISION                      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QA7CREAD IS INITIAL.
       DATE-WRITTEN. 11/02/91.
       DATE-COMPILED. 12/02/91.
      *****************************************************************
      *****************************************************************
      ******************************************************************
      * LOG DE MODIFICACIONES                                          *
      *----------------------------------------------------------------*
      * ADTC002D                                                       *
      * PROYECTO  FECHA      COMENTARIO                                *
      *---------- ---------- ------------------------------------------*
      *---------- ---------- ------------------------------------------*
      ******************************************************************
      *                                                               *
      *****************************************************************
      *****************************************************************
      *                ENVIRONMENT DIVISION                           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-4381.
       OBJECT-COMPUTER. IBM-4381.
       SPECIAL-NAMES.
      *.MC.S @PX0012D00 18-09-2002
      *    DECIMAL-POINT IS COMMA.
      *.MC.E @PX0012D00 18-09-2002
      *****************************************************************
      *                   DATA DIVISION                               *
      *****************************************************************
       DATA DIVISION.
      *****************************************************************
      *               WORKING STORAGE SECTION                         *
      *****************************************************************
       WORKING-STORAGE SECTION.
      *****************************************************************
      *              WORKA DE CAMPOS VARIABLES                        *
      *****************************************************************
      *
       01  VA-DLLP-PNT                PROCEDURE-POINTER.
       01  VN-ADR-MEMO-NUM                 PIC S9(9)  COMP-5.
       01  VN-ADR-MEMO-NUM-N1    REDEFINES VN-ADR-MEMO-NUM USAGE
           POINTER.

       01 VA-SWITCH.
          05 SW-TBL-END                    PIC X(1)    VALUE SPACES.
             88 SW-END-TBL-NO                          VALUE 'T'.
             88 SW-END-TBL-YES                         VALUE 'N'.
          05 SW-RETURN                     PIC 9(5)  COMP-5.
             88 SW-RET-OK                              VALUE 0.
             88 SW-RET-REC-NO-FND                      VALUE 10.
             88 SW-RET-NO-MEMO                         VALUE 30.
             88 SW-RET-UPDATING                        VALUE 40.

       01 VA-CONSTANT.
          05 CA-YES                        PIC X(01)   VALUE 'S'.
          05 CA-SPACE                      PIC X(01)   VALUE ' '.
          05 CA-QG1CABC                    PIC X(7)    VALUE 'QG1CABC'.
          05 CA-RD-MEMO                    PIC X(10)
                                   VALUE 'READMEMORY'.
          05 CA-STATUS.
             10 CA-READ                    PIC X       VALUE 'R'.
             10 CA-UPDATING                PIC X       VALUE 'U'.
             10 CA-DELETE                  PIC X       VALUE 'D'.
             10 CA-PEND-DEL                PIC X       VALUE 'P'.
          05 CA-OPTION.
             10 CA-OPT-1                   PIC X       VALUE '1'.
             10 CA-OPT-2                   PIC X       VALUE '2'.
             10 CA-OPT-3                   PIC X       VALUE '3'.
             10 CA-OPT-4                   PIC X       VALUE '4'.
             10 CA-OPT-5                   PIC X       VALUE '5'.
          05 CN-TIM-OUT                    PIC 9(5)    VALUE 10000.
          05 CA-COD-RETURN.
             10 CN-OK                      PIC 9(2)    VALUE 0.
             10 CN-REC-NO-FND              PIC 9(2)    VALUE 10.
             10 CN-TIM-OUT-UPD             PIC 9(2)    VALUE 20.
             10 VN-RCRT-AREA               PIC 9(2)    VALUE 30.
             10 CN-ERR-READING             PIC 9(2)    VALUE 40.

       01 VA-VARIABLE.
          05  VA-REFERENCE                 PIC X(20)   VALUE SPACES.
          05  VN-COUNTER                   PIC 9(5)  COMP-5.
          05  VA-MEMORY.
              10 VA-TBL-NAME               PIC X(10).
              10 VA-MEMO-NM                PIC X(12).
              10 VA-MEMO-OPT               PIC X(3).
              10 VA-MEMO-SBOPT             PIC X(3).
              10 VA-NUM-REC-MEMO           PIC X(5).
              10 VA-NUM-REC-MEMO-AUX       PIC X(2).
              10 VA-MEMO-ST                PIC X(4).
              10 VA-KEY-RECORD.
                 15 VA-KAY                 PIC X(50).
                 15 FILLER                 PIC X(2)    VALUE LOW-VALUES.
              10 VA-RECORD                 PIC X(20000).
              10 VA-MEMO-RLT               PIC X(50).
          05 VA-MEMO-LTH                   PIC X(20).

      *****************************************************************
      *              WORKA DE COPYS                                   *
      *****************************************************************
      *
       01 VA-QGECABC-01.
            COPY QGECABC.
      *
          COPY QAWCCO2C.

      *****************************************************************
      *              WORKA DE DCLGEN                                  *
      *****************************************************************
       LINKAGE SECTION.

      *    COPY DFHEIBLK.
       01 DFHCOMMAREA.
           COPY QAECREAM.

       PROCEDURE DIVISION.

           PERFORM 1000-START
           PERFORM 2000-PROCESS
           PERFORM 3000-END.

      ******************************************************************
      *
      *I---------------------------------------------------------------*
      *I                   INIT
      *E---------------------------------------------------------------*
      *E                   INICIO
      *****************************************************************
       1000-START.

           INITIALIZE VA-VARIABLE.

      ******************************************************************
      *I---------------------------------------------------------------*
      *I                   PROCESS
      *E---------------------------------------------------------------*
      *E                   PROCESO
      *****************************************************************
       2000-PROCESS.

           MOVE CA-OPT-1                  TO  VA-MEMO-OPT
           MOVE EREAM-MEMO-NM          TO  VA-MEMO-NM
                                               VA-TBL-NAME

           MOVE EREAM-RECORD               TO  VA-RECORD
           MOVE EREAM-OPTION               TO  VA-MEMO-SBOPT
           MOVE EREAM-NUM-RECORD          TO  VA-NUM-REC-MEMO

           PERFORM 2100-READ-RECORD

           PERFORM 2150-EVALUATE-RESULT.


      ******************************************************************
      *I---------------------------------------------------------------*
      *I                   2200-READ-RECORD
      * llamada al modulo en c++ de b£squeda binaria del registro. Para
      * que este m¢dulo realize la b£squeda en funci¢n de la clave se
      * deber  llamar con la opci¢n 1.
      * La opci¢n 1 de busqueda binaria contempla diferentes opciones:
      *       Sub-opcion 1: Devuelve el registro correspondiente a la
      *                     la clave informada.
      *       Sub-opcion 2: Devuelve los 'n' registros a partir de la
      *                     clave informada. Si la clave no est  infor
      *                     mada, se devolver n a partir del primer
      *                     registro de la tabla
      *       Sub-opci¢n 3: Devuelve el registro siguiente a la clave
      *                     informada
      *       Sub-opci¢n 4: Devuelve una sola ocurrencia correspondiente
      *                     a la clave informada, sino existe, se
      *                     devuelve la siguiente
      *****************************************************************
       2100-READ-RECORD.

           INSPECT VA-MEMO-NM      REPLACING ALL SPACES BY LOW-VALUES
           INSPECT VA-MEMO-OPT    REPLACING ALL SPACES BY LOW-VALUES
           INSPECT VA-TBL-NAME       REPLACING ALL SPACES BY LOW-VALUES
           INSPECT VA-MEMO-SBOPT REPLACING ALL SPACES BY LOW-VALUES
           INSPECT VA-NUM-REC-MEMO   REPLACING ALL SPACES BY LOW-VALUES
           INSPECT VA-MEMO-ST    REPLACING ALL SPACES BY LOW-VALUES

           SET VA-DLLP-PNT TO ENTRY 'READMEMORY.DLL'

           CALL CA-RD-MEMO USING
                  BY REFERENCE VA-TBL-NAME
                  BY REFERENCE VA-MEMO-NM
                  BY REFERENCE VA-MEMO-OPT
                  BY REFERENCE VA-MEMO-SBOPT
                  BY REFERENCE VA-NUM-REC-MEMO
                  BY REFERENCE VA-MEMO-ST
                  BY REFERENCE VA-KEY-RECORD
                  BY REFERENCE VA-RECORD
                  BY REFERENCE VA-MEMO-RLT
                  RETURNING SW-RETURN
           END-CALL

           INSPECT VA-NUM-REC-MEMO REPLACING ALL LOW-VALUES BY SPACES
           INSPECT VA-MEMO-ST REPLACING ALL LOW-VALUES BY SPACES.



      ******************************************************************
      *I---------------------------------------------------------------*
      *I                   2150-EVALUATE-RESULT
      *****************************************************************
       2150-EVALUATE-RESULT.

           EVALUATE TRUE
               WHEN SW-RET-OK
                  MOVE CN-OK                 TO EREAM-COD-RETURN
                  IF VA-NUM-REC-MEMO(2:1) EQUAL SPACES
                   MOVE VA-NUM-REC-MEMO(1:1) TO VA-NUM-REC-MEMO-AUX(2:1)
                   MOVE ZEROES               TO VA-NUM-REC-MEMO-AUX(1:1)
                  ELSE
                   MOVE VA-NUM-REC-MEMO      TO VA-NUM-REC-MEMO-AUX
                  END-IF
                  MOVE VA-NUM-REC-MEMO-AUX   TO EREAM-NUM-RECORD
                  MOVE VA-RECORD             TO EREAM-RECORD
               WHEN SW-RET-REC-NO-FND
                  MOVE CN-REC-NO-FND      TO EREAM-COD-RETURN
               WHEN SW-RET-NO-MEMO
      *           MOVE VN-RCRT-AREA      TO EREAM-COD-RETURN
                  MOVE CN-ERR-READING    TO EREAM-COD-RETURN
               WHEN SW-RET-UPDATING
                  PERFORM 2200-EVALUA-STATE
               WHEN OTHER
                  MOVE CN-ERR-READING      TO EREAM-COD-RETURN
                  GOBACK
           END-EVALUATE.

      ******************************************************************
      *I---------------------------------------------------------------*
      *I                   2200-EVALUA-STATE
      *****************************************************************
       2200-EVALUA-STATE.

           EVALUATE TRUE
             WHEN VA-MEMO-ST = CA-DELETE
                  MOVE VN-RCRT-AREA        TO EREAM-COD-RETURN
                  GOBACK
             WHEN VA-MEMO-ST = CA-PEND-DEL
                  MOVE VN-RCRT-AREA        TO EREAM-COD-RETURN
                  GOBACK
             WHEN VA-MEMO-ST = CA-UPDATING
                  INITIALIZE VN-COUNTER
                  PERFORM 2150-WAIT
                     UNTIL VA-MEMO-ST = CA-READ     OR
                           VA-MEMO-ST = CA-DELETE   OR
                           VA-MEMO-ST = CA-PEND-DEL OR
                           VN-COUNTER      =  CN-TIM-OUT

                  IF VN-COUNTER = CN-TIM-OUT
                     MOVE CN-TIM-OUT-UPD      TO EREAM-COD-RETURN
                     GOBACK
                  ELSE
                     IF VA-MEMO-ST = CA-DELETE OR
                        VA-MEMO-ST = CA-PEND-DEL
                        MOVE VN-RCRT-AREA      TO EREAM-COD-RETURN
                        GOBACK
                     ELSE
                       MOVE EREAM-MEMO-NM     TO VA-TBL-NAME
                                                     VA-MEMO-NM
                       MOVE EREAM-RECORD          TO VA-RECORD
                       PERFORM 2100-READ-RECORD
                       PERFORM 2150-EVALUATE-RESULT
                     END-IF
                  END-IF
             WHEN VA-MEMO-ST = CA-READ
                  MOVE EREAM-MEMO-NM          TO VA-TBL-NAME
                                                    VA-MEMO-NM
                  MOVE EREAM-RECORD               TO VA-RECORD
                  PERFORM 2100-READ-RECORD
                  PERFORM 2150-EVALUATE-RESULT
             WHEN OTHER
                  MOVE CN-ERR-READING      TO EREAM-COD-RETURN
                  GOBACK
           END-EVALUATE.

      ******************************************************************
      *I---------------------------------------------------------------*
      *I                   2150-WAIT
      *****************************************************************
       2150-WAIT.

           ADD 1 TO VN-COUNTER

           MOVE CA-OPT-5                  TO  VA-MEMO-OPT
           MOVE EREAM-MEMO-NM          TO  VA-MEMO-NM
                                               VA-TBL-NAME
           PERFORM 2100-READ-RECORD.

      ******************************************************************
      *                   3000-END.
      *I---------------------------------------------------------------*
      *I                 CALLS TO DISCONNECT ROUTINE
      *E---------------------------------------------------------------*
      *E                 LLAMA A LA RUTINA DE DESCONEXION
      *****************************************************************
       3000-END.

           GOBACK.

      ******************************************************************
      *                     999999-ERROR-EIBRESP
      *I---------------------------------------------------------------*
      *I CALLS QG1CABC PROGRAM TO ABEND.
      *-----------------------------------------------------------------
      *E LLAMA AL PROGRAMA QG1CABC PARA ABENDAR.
      ******************************************************************
       999999-ERROR-EIBRESP.
      *
           INITIALIZE QGECABC
           MOVE VA-REFERENCE                      TO ABC-REFERENCE1
           MOVE CA-YES                            TO ABC-ABEND
           MOVE 'READTAB'                        TO ABC-DES-PROG
           MOVE EIBFN                            TO ABC-EIBFN
           MOVE EIBRCODE                         TO ABC-EIBRCODE
           MOVE EIBRSRCE                         TO ABC-EIBRSRCE
           MOVE EIBRESP                          TO ABC-EIBRESP1
           MOVE EIBRESP2                         TO ABC-EIBRESP2
      *
           CALL CA-QG1CABC USING DFHEIBLK QGECABC
      *.MC.S ALINFQA122
              ON EXCEPTION
                 MOVE WCO2C-CICS-ABENDERR TO EIBRESP
           END-CALL
      *.MC.E ALINFQA122
           IF EIBRESP = WCO2C-CICS-ABENDERR
              GOBACK
           END-IF.
      *
