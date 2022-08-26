      * MBNS2009:  COPY DEL FORMATO DE SALIDA PARA LA TRX MB09.        *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      * CODE    AUTOR  FECHA       DESCRIPCION                         *
      * ------- ------ ----------- ----------------------------------- *
      * @BAZ000   AAG  24-01-2017  -PASE A PRODUCCION XXXXXXXXXXXXXXXX *
      ******************************************************************
      *                                                                *
      * CONCEPT  (S)   CONCEPTO DE OPERACIÓN                           *
      * DESCOPE  (S)   DESCRIPCION DE OPERACIÓN                        *
      * FOTO     (S)   FOTOGRAFÍA DEL USUARIO                          *
      * IDOPER   (S)   ID DE OPERACIÓN                                 *
      *                                                                *
      ******************************************************************
      *
       01 MBNS2009.
      *
          05 S209-FILLER                   PIC X(12).
      *
          05 S209-CONCEPT-L                PIC S9(4) COMP.
          05 S209-CONCEPT-A                PIC X(01).
          05 S209-CONCEPT                  PIC X(50).
      *
          05 S209-DESCOPE-L                PIC S9(4) COMP.
          05 S209-DESCOPE-A                PIC X(01).
          05 S209-DESCOPE                  PIC X(50).
      *
          05 S209-FOTO-L                   PIC S9(4) COMP.
          05 S209-FOTO-A                   PIC X(01).
          05 S209-FOTO                     PIC X(30).
      *
          05 S209-IDOPER-L                 PIC S9(4) COMP.
          05 S209-IDOPER-A                 PIC X(01).
          05 S209-IDOPER                   PIC X(03).
      *
