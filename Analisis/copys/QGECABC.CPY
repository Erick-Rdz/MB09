      ******************************************************************
      *A.OR.S
      *  QGECABC: COMMAREA DE EMPLEO DEL MODULO PARA ABENDAR TAREAS
      *A.OR.E
      *B.OR QGECABC: COMMAREA OF USING THE MODULE TO ABEND TASKS
      ******************************************************************
      ******************************************************************
      *A.MF QGECABC                   Qgecabc                          *
      *A.IF ABEND                     Indicador de si se proceder  a   *
      *A/IF                           abendar o no                     *
      *A.IF DES-PROG                  Nombre del programa que          *
      *A/IF                           solicita el abend                *
      *A.IF REFERENCE1                Datos para facilitar la          *
      *A/IF                           resolucion del abend             *
      *A.IF OBJECT-ERROR              Objeto erroneo en caso de error  *
      *A/IF                           db2                              *
      *A.IF SQLCODE                   Sqlcode en caso de error db2     *
      *A.IF SQLCODE                   Sqlcode en caso de error db2     *
      *A.IF SQLCODE                   Sqlcode en caso de error db2     *
      *A.IF SQLERRM                   Sqlerrm en caso de error db2     *
      *A.IF EIBFN                     Eibfn en caso de error cics      *
      *A.IF EIBRSRCE                  Eibrsrce en caso de error cics   *
      *A.IF EIBRCODE                  Eibrcode en caso de error cics   *
      *A.IF EIBRESP1                  Eibresp1 en caso de error cics   *
      *A.IF EIBRESP1                  Eibresp1 en caso de error cics   *
      *A.IF EIBRESP1                  Eibresp1 en caso de error cics   *
      *A.IF EIBRESP2                  Eibresp2 en caso de error cics   *
      ******************************************************************
      ******************************************************************
      *B.MF QGECABC                   Qgecabc                          *
      *B.IF ABEND                     Flag of abend (or not) process   *
      *B.IF DES-PROG                  Program name in which abend is   *
      *B/IF                           required                         *
      *B.IF REFERENCE1                Data to resolve abend            *
      *B.IF OBJECT-ERROR              Wrong object in case of db2      *
      *B/IF                           error                            *
      *B.IF SQLCODE                   Sqlcode in case of db2 error     *
      *B.IF SQLCODE                   Sqlcode in case of db2 error     *
      *B.IF SQLCODE                   Sqlcode in case of db2 error     *
      *B.IF SQLERRM                   Sqlcode in case of db2 error     *
      *B.IF EIBFN                     Eibfn in case of cics error      *
      *B.IF EIBRSRCE                  Eibrsrce in case of cics error   *
      *B.IF EIBRCODE                  Eibrcode in case of cics error   *
      *B.IF EIBRESP1                  Eibresp1 in case of cics error   *
      *B.IF EIBRESP1                  Eibresp1 in case of cics error   *
      *B.IF EIBRESP1                  Eibresp2 in case of cics error   *
      *B.IF EIBRESP2                  Eibresp2 in case of cics error   *
      ******************************************************************

             02 QGECABC.
                 05 ABC-ABEND              PIC X(1).
                 05 ABC-DES-PROG           PIC X(8).
                 05 ABC-REFERENCE1         PIC X(20).
                 05 ABC-OBJECT-ERROR       PIC X(8).
                 05 ABC-SQLCODE            PIC S9(9) COMP.
                 05 ABCR-SQLCODE REDEFINES ABC-SQLCODE.
                    10 RED-SQLCODE         PIC X(4).
                 05 ABC-SQLERRM            PIC X(70).
                 05 ABC-EIBFN              PIC X(2).
                 05 ABC-EIBRSRCE           PIC X(8).
                 05 ABC-EIBRCODE           PIC X(6).
                 05 ABC-EIBRESP1           PIC S9(9) COMP.
                 05 ABCR-EIBRESP1 REDEFINES ABC-EIBRESP1.
                    10 RED-EIBRESP1        PIC X(4).
                 05 ABC-EIBRESP2           PIC S9(9) COMP.
      *$.
             02 QGECABC-RE REDEFINES QGECABC.
                 05 FILLER                 PIC X(1).
                 05 ABC-PROGRAMA           PIC X(8).
                 05 ABC-REFERENCIA         PIC X(20).
                 05 ABC-OBJETO-ERROR       PIC X(8).
                 05 FILLER-1               PIC S9(9) COMP.
                 05 FILLER-2 REDEFINES FILLER-1.
                    10 FILLER              PIC X(4).
                 05 FILLER-3               PIC X(70).
                 05 FILLER-4               PIC X(2).
                 05 FILLER-5               PIC X(8).
                 05 FILLER-6               PIC X(6).
                 05 FILLER-7               PIC S9(9) COMP.
                 05 FILLER-8 REDEFINES FILLER-7.
                    10 FILLER              PIC X(4).
                 05 FILLER-9               PIC S9(9) COMP.
