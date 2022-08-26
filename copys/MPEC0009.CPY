      * MPEC0009 :  COMMAREA MP9C0009                                  *
      ******************************************************************
      *                     MODIFICATIONS LOG.                         *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *                                                                *                     
      ******************************************************************
      *B.IN                                                            *
      *B.MF MPEC0009                  COMMUNICATION AREA WITH ROUTINE  *
      *B.IF CUENTA                    NUMERO DE CUENTA                 *
      *B.IF NUM-OPE                   NUMERO DE OPERACION              *
      *B.OUT                                                           *
      *B.OF NUM-OPE                   NUMERO DE OPERACION              *
      *B.OF TIPO-OPE                  TIPO DE OPERACION                *
      *B.OF NUM-AUTO                  NUMERO DE AUTORIZACION           *
      *B.OF COD-FCC                   CODIGO FCC                       *
      *B.OF NUM-REF                   NUMERO DE REFERENCIA             *
      *B.OF COD-ACT                   CODIGO DE ACTIVIDAD              *
      *B.OF NUM-NEGOC                 NUMERO DE NEGOCIO                *
      *B.OF OPE-ORI                   OPERACION ORIGEN                 *
      *B.OF NUM-DECLA                 NUMERO DE DECLARACION            *
      *B.OF NUM-TRANS                 NUMERO DE TRANSACCION            *
      *B.OF NUM-CARD                  NUMERO DE TARJETA                *
      *B.OF SQLCODE                   SQL RETURN CODE                  *
      *B.OF SQLERRM                   DB2 SQLERRM                      *
      *B.OF TXT-30-DIG                30 DIGITS LITERAL                *
      *B.OF COD-RETURN                RETURN CODE                      *
      *                                 '00' OK                        *
      *                                 '10' NO ENCONTRADO             *
      *                                 '20' ERROR EN DATOS DE ENTRADA *
      *                                 '30' MAS DE UN REGISTRO        *
      *                                 '99' ERROR DE DB2              *
      *B.OF DES-ERR                   DESCRIPCI�N DEL ERROR            *
      *B.OF SQLCODE                   CODIGO DE RETORNO DE ERROR SQL.  *
      *B.OF RECURRENTE                SE INFORMA SI ES CARGO RECURRENTE*
      ******************************************************************
       02  MPEC009.
      *
         03 MPEC009-ENTRADA.
           05  E109-CUENTA                 PIC X(18).
           05  E109-NUM-OPE                PIC 9(9).
      *
         03 MPEC0009-DATA.
           05  E109-NUM-OPE-DEB            PIC X(05).
           05  E109-TIPO-OPE               PIC X(07).
           05  E109-NUM-AUTO               PIC X(06).
           05  E109-COD-FCC                PIC X(03).
           05  E109-NUM-REFER              PIC X(25).
           05  E109-COD-ACT                PIC X(04).
           05  E109-NUM-NEGOC              PIC X(10).
           05  E109-OPE-ORI                PIC X(02).
           05  E109-NUM-DECLA              PIC X(04).
           05  E109-NUM-TRANS              PIC X(06).
           05  E109-NUM-CARD               PIC X(16).
           05  E109-RECURRENTE             PIC X(16).
      *
         03 MPEC0009-SALIDA.
           05  E109-COD-RETURN             PIC X(02).
           05  E109-DES-ERR                PIC X(45).
           05  E109-SQLCODE                PIC S9(09) COMP.

      *
