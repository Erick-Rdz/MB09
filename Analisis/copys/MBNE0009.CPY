      * MBNE0009:  COPY DEL FORMATO DE ENTRADA PARA LA TRX MB09.       *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      * CODE    AUTOR  FECHA       DESCRIPCION                         *
      * ------- ------ ----------- ----------------------------------- *
      * @BAZ000   AAG  24-01-2017  -PASE A PRODUCCION XXXXXXXXXXXXXXXX *
      ******************************************************************
      * MBNE0009       NOMBRE DEL COPY DE ENTRADA PARA LA TRX MB09.    *
      * FILLER         ESPACIO RESERVADO PARA DATOS DE LA ARQUITECTURA.*
      * BDMID    (O)   BDMID
      * NUMTARJ  (O)   NUMERO DE TARJETA                               *
      * NUMCUEN  (O)   NUMERO DE CUENTA                                *
      * FECHINI  (O)   FECHA INICIO                                    *
      * FECHFIN  (O)   FECHA FIN                                       *
      * NUMECEL  (O)   NUMERO DE CELULAR                               *
      * IDCELUL  (O)   ID DE CELULAR                                   *
      * LATITUD  (O)   LATITUD                                         *
      * LONGITD  (O)   LONGITUD                                        *
      * CONTREG  (O)   CONTADOR DE REG.                                *
      * ULTLLAV  (O)   ULTIMA LLAVE.                                   *
      * PRMCONA  (O)   PARAM. CONSULTA ANT                             *
      *                                                                *
      * (R)=REQUERIDO=OBLIGATORIO  (O)=OPCIONAL                        *
      ******************************************************************
      *
       01 MBNE0009.
      *
          05 E009-FILLER                   PIC X(12).
      *
          05 E009-BDMID-L                  PIC S9(4) COMP.
          05 E009-BDMID-A                  PIC X(01).
          05 E009-BDMID                    PIC X(40).
      *
          05 E009-NUMTARJ-L                PIC S9(4) COMP.
          05 E009-NUMTARJ-A                PIC X(01).
          05 E009-NUMTARJ                  PIC X(16).
      *
          05 E009-NUMCUEN-L                PIC S9(4) COMP.
          05 E009-NUMCUEN-A                PIC X(01).
          05 E009-NUMCUEN                  PIC X(14).
      *
          05 E009-FECHINI-L                PIC S9(4) COMP.
          05 E009-FECHINI-A                PIC X(01).
          05 E009-FECHINI                  PIC X(10).
      *
          05 E009-FECHFIN-L                PIC S9(4) COMP.
          05 E009-FECHFIN-A                PIC X(01).
          05 E009-FECHFIN                  PIC X(10).
      *
          05 E009-NUMECEL-L                PIC S9(4) COMP.
          05 E009-NUMECEL-A                PIC X(01).
          05 E009-NUMECEL                  PIC X(15).
      *
          05 E009-IDCELUL-L                PIC S9(4) COMP.
          05 E009-IDCELUL-A                PIC X(01).
          05 E009-IDCELUL                  PIC X(13).
      *
          05 E009-LATITUD-L                PIC S9(4) COMP.
          05 E009-LATITUD-A                PIC X(01).
          05 E009-LATITUD                  PIC X(11).
      *
          05 E009-LONGITD-L                PIC S9(4) COMP.
          05 E009-LONGITD-A                PIC X(01).
          05 E009-LONGITD                  PIC X(11).
      *
          05 E009-CONTREG-L                PIC S9(4) COMP.
          05 E009-CONTREG-A                PIC X(01).
          05 E009-CONTREG                  PIC X(03).
      *
          05 E009-DATPAG-L                 PIC S9(4) COMP.
          05 E009-DATPAG-A                 PIC X(01).
          05 E009-DATPAG.
            10 E009-ULTLLAV                PIC X(20).
            10 E009-TIPCONS                PIC X(02).
      *
