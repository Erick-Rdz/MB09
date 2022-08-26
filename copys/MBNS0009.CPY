      * MBNS0009:  COPY DEL FORMATO DE SALIDA PARA LA TRX MB09.        *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      * CODE    AUTOR  FECHA       DESCRIPCION                         *
      * ------- ------ ----------- ----------------------------------- *
      * @BAZ000   AAG  24-01-2017  -PASEA A PRODUCCION XXXXXXXXXXXXXXX *
      * @BAZ001   IMM  02-11-2016  SE AGREGA BANDERA PARA INDICAR SI EL*
      *                            MOVIMIENTO ES DE NOMINA.            *
      ******************************************************************
      * MBNS0009       NOMBRE DEL COPY DE SALIDA  PARA LA TRX MB09.    *
      * FILLER         ESPACIO RESERVADO PARA DATOS DE LA ARQUITECTURA.*
      * FECHM01  (S)   FECHA DE MOV                                    *
      * HORAM01  (S)   HORA DE MOV                                     *
      * DESCM01  (S)   DESCRIP. DE MOV                                 *
      * IMPTM01  (S)   IMPORTE DEL MOV                                 *
      * NOPEM01  (S)   NUM. OPERAC. MOV                                *
      * CONTREG  (S)   CONTADOR DE REGISTROS                           *
      * ULTLLAV  (S)   ULTIMA LLAVE                                    *
      * INDPAGI  (S)   INDICADOR DE PAGINACION                         *
      * INDNOM   (S)   INDICADOR DE MOVIMIENTO SI ES NOMINA            *
      * (S)=SALIDA                                                     *
      ******************************************************************
       01 MBNS0009.
      *
          05 S009-FILLER                   PIC X(12).
      *
          05 S009-FECHM01-L                PIC S9(4) COMP.
          05 S009-FECHM01-A                PIC X(01).
          05 S009-FECHM01                  PIC X(10).
      *
          05 S009-HORAM01-L                PIC S9(4) COMP.
          05 S009-HORAM01-A                PIC X(01).
          05 S009-HORAM01                  PIC X(04).
      *
          05 S009-DESCM01-L                PIC S9(4) COMP.
          05 S009-DESCM01-A                PIC X(01).
          05 S009-DESCM01                  PIC X(30).
      *
          05 S009-IMPTM01-L                PIC S9(4) COMP.
          05 S009-IMPTM01-A                PIC X(01).
          05 S009-IMPTM01                  PIC X(16).
      *
          05 S009-NOPEM01-L                PIC S9(4) COMP.
          05 S009-NOPEM01-A                PIC X(01).
          05 S009-NOPEM01                  PIC X(09).
      *
          05 S009-CONTREG-L                PIC S9(4) COMP.
          05 S009-CONTREG-A                PIC X(01).
          05 S009-CONTREG                  PIC X(03).
      *
          05 S009-DATPAG-L                PIC S9(4) COMP.
          05 S009-DATPAG-A                PIC X(01).
          05 S009-DATPAG.
            10 S009-ULTLLAV                PIC X(20).
            10 S009-TIPCONS                PIC X(02).
      *
          05 S009-INDPAGI-L                PIC S9(4) COMP.
          05 S009-INDPAGI-A                PIC X(01).
          05 S009-INDPAGI                  PIC X(01).
      *
          05 S009-TIPOPER-L                PIC S9(4) COMP.
          05 S009-TIPOPER-A                PIC X(01).
          05 S009-TIPOPER                  PIC X(03).
      *
      *@BAZ001-->INI
          05 S009-INDNOM-L                 PIC S9(4) COMP.
          05 S009-INDNOM-A                 PIC X(01).
          05 S009-INDNOM                   PIC X(01).
      *@BAZ001<--FIN
      *
