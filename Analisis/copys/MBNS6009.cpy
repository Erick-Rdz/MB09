      *MBNS6009:  COPY DEL FORMATO DE SALIDA PARA LA TRX MB09 SOBRES   *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      * CODE    AUTOR  FECHA       DESCRIPCION                         *
      * ------- ------ ----------- ----------------------------------- *
      * ------- ------ ----------- ----------------------------------- *
      ******************************************************************
      * DATOS SOBRES Y ALCANCIA                                        *
      * MBNS6009       NOMBRE DEL COPY DE SALIDA  PARA LA TRX MB09.    *
      * FILLER         ESPACIO RESERVADO PARA DATOS DE LA ARQUITECTURA.*
      * MONSOB   (S)   MONTO TOTAL DE SOBRES                           *
      * MONALC   (S)   MONTO TOTAL DE ALCANCIA                         *
      * COMSOB   (S)   COMENTARIO DE SOBRE                             *
      * COMALC   (S)   COMENTARIO DE ALCANCIA                          *
      * INDSOB   (S)   INDICADOR DE SOBRE                              *
      * INDALC   (S)   INDICADOR DE ALCANCIA                           *
      * SALSOB   (S)   SALDO DE SOBRE                                  *
      * SALALC   (S)   SALDO DE ALCANCIA.                              *
      ******************************************************************
       01 MBNS6009.
      *
          05 S609-FILLER                   PIC X(12).
      *                                                                         
          05 S609-MONSOB-L                 PIC S9(4) COMP.
          05 S609-MONSOB-A                 PIC X(01).
          05 S609-MONSOB                   PIC S9(13)V9(02).
      *
          05 S609-MONALC-L                 PIC S9(4) COMP.
          05 S609-MONALC-A                 PIC X(01).
          05 S609-MONALC                   PIC S9(13)V9(02).
      *
          05 S609-COMSOB-L                 PIC S9(4) COMP.
          05 S609-COMSOB-A                 PIC X(01).
          05 S609-COMSOB                   PIC X(30).
      *
          05 S609-COMALC-L                 PIC S9(4) COMP.
          05 S609-COMALC-A                 PIC X(01).
          05 S609-COMALC                   PIC X(30).
      *
          05 S609-INDSOB-L                 PIC S9(4) COMP.
          05 S609-INDSOB-A                 PIC X(01).
          05 S609-INDSOB                   PIC X(01).
      *
          05 S609-INDALC-L                 PIC S9(4) COMP.
          05 S609-INDALC-A                 PIC X(01).
          05 S609-INDALC                   PIC X(01).
      *
          05 S609-SALSOB-L                 PIC S9(4) COMP.
          05 S609-SALSOB-A                 PIC X(01).
          05 S609-SALSOB                   PIC S9(13)V9(02).
      *
          05 S609-SALALC-L                 PIC S9(4) COMP.
          05 S609-SALALC-A                 PIC X(01).
          05 S609-SALALC                   PIC S9(13)V9(02).
      *
      *
