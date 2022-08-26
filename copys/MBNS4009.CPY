      * MBNS4009:  COPY DEL FORMATO DE SALIDA PARA LA TRX MB09.        *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      * CODE    AUTOR  FECHA       DESCRIPCION                         *
      * ------- ------ ----------- ----------------------------------- *
      ******************************************************************
      *                                                                *
      * IDCODI   (S)  IDENTIFICADOR DE CODI                            *
      * NOMORD   (S)  NOMBRE DE ORDENANTE                              *
      * IDLIBRE  (S)  IDENTIFICADOR LIBRE                              *
      * TXTLIBR  (S)  TEXTO LIBRE)                                     *
      * NUMLIBR  (S)  NUMERO LIBRE                                     *
      *                                                               **
      ******************************************************************
      *
       01 MBNS4009.
      *
          05 S409-FILLER                   PIC X(12).
      *
          05 S409-IDCODI-L                PIC S9(4) COMP.
          05 S409-IDCODI-A                PIC X(01).
          05 S409-IDCODI                  PIC X(02).
      *
          05 S409-NOMORD-L               PIC S9(4) COMP.
          05 S409-NOMORD-A               PIC X(01).
          05 S409-NOMORD                 PIC X(60).
      *
          05 S409-IDLIBRE-L               PIC S9(4) COMP.
          05 S409-IDLIBRE-A               PIC X(01).
          05 S409-IDLIBRE                 PIC X(02).
      *
          05 S409-TXTLIBR-L               PIC S9(4) COMP.
          05 S409-TXTLIBR-A               PIC X(01).
          05 S409-TXTLIBR                 PIC X(20).
      *
          05 S409-NUMLIBR-L               PIC S9(4) COMP.
          05 S409-NUMLIBR-A               PIC X(01).
          05 S409-NUMLIBR                 PIC 9(10).

