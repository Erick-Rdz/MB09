      *MBNS5009:  COPY DEL FORMATO DE SALIDA PARA LA TRX MB09.         *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      * CODE    AUTOR  FECHA       DESCRIPCION                         *
      * ------- ------ ----------- ----------------------------------- *
      ******************************************************************
      * MBNS5009       NOMBRE DEL COPY DE SALIDA  PARA LA TRX MB09.    *
      * FILLER         ESPACIO RESERVADO PARA DATOS DE LA ARQUITECTURA.*
      * ALIASPA  (S)   RAZON SOCIAL PARA                               *
      * NOMBPA   (S)   NOMBRE PARA                                     *
      * CTAPARA  (S)   CUENTA PARA                                     *
      * BANCOPA  (S)   BANCO PARA                                      *
      * ALIASDE  (S)   RAZON SOCIAL DE ENVIO                           *
      * NOMBDE   (S)   NOMBRE DE                                       *
      * CTADE    (S)   CUENTA DE                                       *
      * REFEOPE  (S)   REFERENCIA DE OPERACION                         *
      * OPERADO  (S)   OPERADORA                                       *
      ******************************************************************
       01 MBNS5009.
      *
          05 S509-FILLER                   PIC X(12).
      *                                                                         
          05 S509-ALIASPA-L                PIC S9(4) COMP.
          05 S509-ALIASPA-A                PIC X(01).
          05 S509-ALIASPA                  PIC X(50).
      *
          05 S509-NOMBPA-L                 PIC S9(4) COMP.
          05 S509-NOMBPA-A                 PIC X(01).
          05 S509-NOMBPA                   PIC X(60).
      *
          05 S509-CTAPARA-L                PIC S9(4) COMP.
          05 S509-CTAPARA-A                PIC X(01).
          05 S509-CTAPARA                  PIC X(20).
      *
          05 S509-BANCOPA-L                PIC S9(4) COMP.
          05 S509-BANCOPA-A                PIC X(01).
          05 S509-BANCOPA                  PIC X(40).
      *
          05 S509-ALIASDE-L                PIC S9(4) COMP.
          05 S509-ALIASDE-A                PIC X(01).
          05 S509-ALIASDE                  PIC X(50).
      *
          05 S509-NOMBDE-L                 PIC S9(4) COMP.
          05 S509-NOMBDE-A                 PIC X(01).
          05 S509-NOMBDE                   PIC X(60).
      *
          05 S509-CTADE-L                  PIC S9(4) COMP.
          05 S509-CTADE-A                  PIC X(01).
          05 S509-CTADE                    PIC X(20).
      *
          05 S509-REFEOPE-L                PIC S9(4) COMP.
          05 S509-REFEOPE-A                PIC X(01).
          05 S509-REFEOPE                  PIC X(20).
      *
          05 S509-OPERADO-L                PIC S9(4) COMP.
          05 S509-OPERADO-A                PIC X(01).
          05 S509-OPERADO                  PIC X(10).
      *
