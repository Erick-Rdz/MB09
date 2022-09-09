      * MBNS3009:  COPY DEL FORMATO DE SALIDA PARA LA TRX MB09.        *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      * CODE    AUTOR  FECHA       DESCRIPCION                         *
      * ------- ------ ----------- ----------------------------------- *
      * @BAZ001   AAG  31-03-2017  AGREGAR CAMPO BENEFIC EN COPY3      *
      ******************************************************************
      *                                                                *
      * STATUS   (S)   ESTATUS DE ACLARACION                           *
      * FOLIO    (S)   DFOLIO DE ACLARACION                            *
      * BENEFIC  (S)   BENEFICIARIO (inf.complementaria)               *
      *                                                                *
      ******************************************************************
      *
       01 MBNS3009.
      *
          05 S209-FILLER                   PIC X(12).
      *
          05 S309-STATUS-L                PIC S9(4) COMP.
          05 S309-STATUS-A                PIC X(01).
          05 S309-STATUS                  PIC X(02).
      *
          05 S309-FOLIO-L                 PIC S9(4) COMP.
          05 S309-FOLIO-A                 PIC X(01).
          05 S309-FOLIO                   PIC X(15).
      *@BAZ001.I
          05 S309-BENEFIC-L               PIC S9(4) COMP.
          05 S309-BENEFIC-A               PIC X(01).
          05 S309-BENEFIC                 PIC X(50).
      *@BAZ001.F
