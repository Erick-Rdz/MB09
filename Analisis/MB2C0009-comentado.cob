      ******************************************************************
      *  MB2C0009: CONSULTA DE MOVIMIENTOS POR NUMERO DE CUENTA O POR  *
      *  NUMERO DE TARJETA                                             *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                      **
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   MB2C0009.
       AUTHOR.       JULIO CESAR GARCIA SAN JUAN.
       DATE-WRITTEN. 12/09/2016.
      ******************************************************************
      *        L O G    D E   M O D I F I C A C I O N E S              *
      ******************************************************************
      *  CODE   AUTOR    FECHA    DESCRIPCION                          *
      * ------- ----- ----------- ------------------------------------ *
      *  DECLARE DECLARE @BAZ001   IMM  26-10-2016  SE VALIDA MOVIMIENTO PARA INFORMAR  *
      *                            SI DE NOMINA S009-INDNOM..          *
      * ------- ----- ----------- ------------------------------------ *
      *  MCV001 185940 24-11-2016 SE MODIFICA FORMATO DE HORA PARA LA  *
      *                           TABLA BGDT089.                      **
      *  MCV002 GICE   30/12/2016 SE CAMBIA DESCRIPCI�N A C�DIGOS DE   *
      *                           OPERACI�N                            *
      *  MCV003 GICE   02/01/2017 SE CAMBIA DECLARACI�N DE VARIABLES   *
      *  DECLARE DECLARE @BAZ005 AAG    16/01/2017 MOD.DESCRIPCIONES SEGUNDAS LINEAS    *
      * LCR-INI2 LCR    01/03/17   SE ANEXAN COPY DE SALIDA CON FOLIO Y*
      *                                                      ESTATUS   *
      *  DECLARE DECLARE @BAZ007 AAG    10/04/2017 MOD.DESCRIPCIONES COD.CREDITOS       *
      *  DECLARE DECLARE @BAZ008 AAG    02/05/2017 AGREGAR CAMPO BENEFIC EN COPY3.      *
      *  DECLARE DECLARE @BAZ009 AAG    30/06/2017 OBT.DATOS DIRECTOS P/TRANSFER COD.213*
      *  DECLARE DECLARE @BAZ010 AAG    26/09/2017 OBT.DATOS DE TC PARA DEVOLUCION SPEI *
      *  DECLARE DECLARE @BAZ011 AAG    19/10/2017 VAL.TEL_CEL DE LONG.MAYOR 10 DIGITOS.*
      * LCR-INI2  LCR  NOV 2017   SE ANEXA RUTINA DE GEOLOCALIZACION   *
      * LCR-INI3 LCR   DIC 2017   SE CORRIGUE LA PALABRA EXCENTO       *
      * LCR-INI4 LCR   ENE 2018   SE CAMBIA MENSAJE COMPRA GUARD GO    *
      *  DECLARE DECLARE @BAZ015 IMM    01/03/2018 INFORMAR MTCN PARA MULTIMARCAS Y DEX.*
      *  DECLARE DECLARE @BAZ016 IMM    15/03/2018 INFORMAR DESCRIPCION WALLET          *
      *  DECLARE DECLARE @BAZ017 IMM    22/06/2018 ESCRIBIR FORMATO DE SALIDA 2 Y 3 PARA*
      *                           MOVIMIENTOS CON TARJETA.             *
      *  DECLARE DECLARE @BAZ018 IMM    04/09/2018 AGREGAR DESCRIPCION PARA CODIGO T53  *
      *                           PROMOCION BILLETAZO                  *
      *  DECLARE DECLARE @BAZ019 MLR    21/03/2019 AGREGAR DESCRIPCION PARA CODIGO V06  *
      *  DECLARE DECLARE @BAZ020 RLV    29/04/2019 RECOMPILACI�N MODF.SECUENCIA MBDT010 *
      *  DECLARE DECLARE @BAZ021 RSC    29/07/2019 SE AGREGAN LAS FUNCKEY 03 CONS CTA   *
      *                           WALLET Y 04 CONS RET CUENTA WALLET   *
      *                           SE VALIDAN EN LA MBDT036             *
      *                           SE ATIENDE OBSERVACION DE BD         *
      *                                                                *
      *  DECLARE DECLARE @BAZ022 GGS    30/09/2019 SE CORRIGE PARA MOSTRAR LA CLAVE     *
      *                           INTERBANCARIA                        *
      *  DECLARE DECLARE @BAZ023 LLV    09/10/2019 SE AGREGA FORMATO DE SALIDA4 PARA    *
      *                           IDENTIFICAR SI EL MOVTO ES UN CODI   *
      *                           TRASPASO O SPEI                      *
      *  DECLARE DECLARE @BAZ024 RSC    15/10/2019 SE MODIFICAN CAMPOS RECEPCION SPEI   *
      *                           TOIMII; Y PAGOS CON QR TOMIIN        *
      *  DECLARE DECLARE @BAZ025 LLV    16/10/2019 SE AGREGA FORMATO DE SALIDA4 PARA    *
      *                           MOVIMIENTOS SPEI                     *
      *  DECLARE DECLARE @BAZ026 FMD    30/01/2020 SE AGREGA DESCRICION PARA CODIGOS    *
      *                           L67 Y Z00 PARA INVERSION AZTECA MAS  *
      *  DECLARE DECLARE @BAZ027 IASJ   22/01/2020 SE AGREGA COPY DE SALIA 5 PARA ENVIO *
      *                           DE INFROMACION ADICIONAL DE          *
      *                           MOVIMIENTOS WALLET                   *
      *  DECLARE DECLARE @BAZ028 FMD    12/02/2020 SE AGREGA DESCRICION PARA EL CODIGO  *
      *                           L70 PARA INVERSION AZTECA MAS Y SE   *
      *                           COMENTA SE MUESTRE EL NO. OPERACION  *
      *  DECLARE DECLARE @BAZ029 YYGO   20/03/2020 SE AGREGA VALIDACION GUARDADITO KIDS *
      *                           SE VALIDA DESDE BDMID LETRA 'K'.     *
      *  DECLARE DECLARE @BAZ030 AISP   15/04/2020 CAMBIO DE NUMERO DE REFERENCIA AL    *
      *                           CONSULTAR LOS DOLARES DEL CLIENTE    *
      *  DECLARE DECLARE @BAZ031 FYZB   16/04/2020 SE CAMBIA INFORMACION DE SALIDA      *
      *                           PARA RETENCIONES DE ALCANC�A         *
      *  DECLARE DECLARE @BAZ032 RSC    21/04/2020 SE ELIMINA CONSULTA A GADT001        *
      *  DECLARE DECLARE @BAZ033 IASJ   04/05/2020 SE AGREGAN CODIGOS DE OPERACION EN   *
      *                           DEPOSITOS Y RETIROS EN CORRESPONSALES*
      *                           CON TX B601; B501 CON COD OPER W50;  *
      *                           Q54; 000; T27                        *
      *  DECLARE DECLARE @BAZ034 IASJ   08/06/2020 CORRECION DE NOMBRE CON N/A;         *
      *                           SE LIMPIA CONCEPTO EN PAGOS Y COBROS *
      *                           CON QR SE AGREGA FLUJO CODI MBWB     *
      *  DECLARE DECLARE @BAZ035 FMD    05/06/2020 SE MODIFICA LA DESCRIPCION PARA LOS  *
      *                           CODIGOS L67; L70 Y Z00 DE INVERSION  *
      *                           AZTECA MAS A MERCADO DE DINERO.      *
      *  DECLARE DECLARE @BAZ036 MVMJ   JUL 2020   SE CONSULTA DOBLES LINEAS PARA MOVS  *
      *                           DAPP (Z25 - Z26); .                    *
      *  DECLARE DECLARE @BAZ037 JCGM   12/06/2020 SE AGREGA PROCEDURE STORED PARA      *
      *                           REDUCIR EL NUMERO DE ACCESOS A BD    *
      *  DECLARE DECLARE @BAZ038 CEPL   24/06/2020 MODIFICACION PARA IDENTIFICAR LOS    *
      *                           MOVIMIENTOS QUE FUERON HECHOS CON    *
      *                           LA TARJETA DIGITAL                   *
      *  DECLARE DECLARE @BAZ039 MAR    20/07/2020 MODIFICACION PARA IDENTIFICAR LAS    *
      *                           RETENCIONES QUE FUERON HECHOS CON    *
      *                           LA TARJETA DIGITAL                   *
      *  DECLARE DECLARE @BAZ040 IASJ   27/07/2020 MODIFICA PARA NOTIFICACION DE DEPOSI-*
      *                           TO DE REFERENTES Y REFERIDOS COD OPER*
      *                           U50 TX MBW9.                         *
      *  DECLARE DECLARE @BAZ041 IASJ   20/08/2020 SE MODIFICA DESCRIPCION DE ENVIO SPEI*
      *                          SE MAPEA B520 PARA ENVIOS POR SUCURSA *
      *  DECLARE DECLARE @BAZ042 MAR   09/09/2020  SE CAMBIA EL TIPO DE CAMPO DE N�MERO *
      *                           DE MOV A ALFAN�MERICO                *
      *  DECLARE DECLARE @BAZ043 GGS   14/09/2020  SE MODIFICA LA DESCRIPCION PARA LOS  *
      *                           MOVIMIENTOS DE APERTURA DE SOCIO PLUS*
      *                           ADEMAS SE OMITE LA APERTURA DEL CURSOR
      *                           BGDC0089 DEBIDO A QUE NO SE OCUPA Y  *
      *                           POR OBSERVACIONES DE BD              *
      *  DECLARE DECLARE @BAZ044 IASJ   17/09/2020 CAMBIO DE DESCRIPCION EN RETENCION   *
      *                           EN DEPOSITO SIN TARJETA; SE AGREGA   *
      *                           TICKET PARA PROMO 2 PESOS            *
      *                                                                *
      *  DECLARE DECLARE @BAZ045 GGS    20/10/2020 SE MODIFICA LA DESCRIPCI�N PARA      *
      *                           MOSTRAR LOS MOVIMIENTOS DE FIAR PLUS *
      *  DECLARE DECLARE @BAZ046 JML    26/10/2020 ACCESO A LA MCDT279 CUANDO NO SE     *
      *                           ENCUENTRA EL BIN DE AMERICAN EXPRESS *
      *                                                                *
      *  DECLARE DECLARE @BAZ047 GGS    31/10/2020 SE MODIFICA LA DESCRIPCION PARA SOBRES
      *                           PARA LA CONSULTA DE RETENCIONES      *
      *  DECLARE DECLARE @BAZ048 YYGO   30-11-2020 SE AGREGA CONSULTA A TABLA FEDT004   *
      *                           PARA LA CONSULTA DE CODOPER 217      *
      *  DECLARE DECLARE @BAZ049 IASJ   07-12-2020 SE AGREGAN LOS MOVIMIENTOS DE        *
      *                           RETIRO Y ABONO PARA COMPRA DE BOLETOS*
      *                           METRO
      *  DECLARE DECLARE @BAZ050 LLV    08-02-2021 SE AGREGAN NUMERO DE CUENTA EN SALIDA4
      *                           Y SE AGREGA VALIDACION DE TX MBL7    *
      *  DECLARE DECLARE @BAZ051 EPS    12-04-2021 SE ENMASCARA EL NUMERO DE TARJETA;   *
      *                           CUANDO EL MOVTO CORREPSONDE A PAGO   *
      *                           TDC CON CODIGO DE OPERACI�N R81      *
      *  DECLARE DECLARE @BAZ052 YYGO   06-04-2021 SUMATORIA DE SOBRES DE LA CTA        *
      *                           SUMATORIA DE ALCANCIA DE LA CTA      *
      *                           TOTAL DE SOBRES Y TOTAL DE ALCANCIA  *
      *  DECLARE DECLARE @BAZ053 JCGM   03-05-2021 SE AGREGA BANDERA CR PARA MOVTOS     *
      *                           RECURRENTES Y 1 PARA MOVTOS DE       *
      *                           DOMICILIACI�N.                       *
      *  DECLARE DECLARE @BAZ054 JCGM   10-05-2021 SE CAMBIA EL CONCEPTO EN MOVTOS DE   *
      *                           SEGUROS                              *
      *  DECLARE DECLARE @BAZ055 YYGO   09-07-2021 CONSULTA CLIENTE TUTOR GUARDADITO KIDS
      *  DECLARE DECLARE @BAZ056 IASJ   14-07-2021 AGREGA FLUJO SAPP                    *
      *  DECLARE DECLARE @BAZ057  JAA   25/08/2021 SE INFORMA ID DE OPERACI�N PARA RETE-*
      *                           NCIONES DE ENVIO A CELULAR. Y EL     *
      *                           CONCEPTO PARA CARGOS RECURRENTES.    *
      *  DECLARE DECLARE @BAZ058  JAA   31/08/2021 SE CAMBIA LEYENDA DE LA CONSTANTE    *
      *                           CA-COPEBAZ 'RENTA PELICULAS BAZ'.    *
      *  DECLARE DECLARE @BAZ059  IASJ  25/10/2021 SE AGREGA CODOPER AG0 Y SE CORRIGE   *
      *                           DESCRIPCION DE OPERACIONES DE QR.    *
      *  DECLARE DECLARE @BAZ060  IASJ  25/10/2021 SE INFORMA NUMERO DE AUTORIZACION    *
      *                           EN VENTA Y COMPRA DE TIEMPO AIRE     *
      ** DECLARE DECLARE @BAZ061  PGR   29/09/2021 SE AGREGA DESCRIPCION PARA ENVIOS WU *
      *  DECLARE DECLARE @BAZ062  MVMJ  08/11/2021 SE CAMBIAN CODIGOS DE OPERACION PARA *
      *                           LA TRANSACCION MBS4.                 *
      *  DECLARE DECLARE @BAZ063  JCGM  18/01/2022 INFORMAR HH:MM:SS EN S009-HORAM01 Y  *
      *                           AGREGAR RUTINA DE IDENTIFICACION DE  *
      *                           CARGOS RECURRENTES Y DATOS DEL MISMO
      *  DECLARE DECLARE @BAZ065  IASJ  13/01/2022 DESCRIPCION TRASPASOS SAPP           *
      *  DECLARE DECLARE @BAZ066  LSLV  23/02/2022 AGREGAR NUEVA DESCRIPCION CUANDO LA  *
      *                           TX SEA B925 Y COD OPER = 160         *
      *  DECLARE DECLARE @BAZ067  FFR   15/03/2022 SE AGREGAN CONCEPTOS DELIVERY SAPP   *
      *  DECLARE DECLARE @BAZ068  YYGO  31/03/2022 SE ADECUA DESCRIPCION RETIRO QR OTROS*
      *  DECLARE DECLARE @BAZ069  GFV   01/04/2022 SE AGRREGAN DESCRIPCIONES DE CODIGOS *
      *  DECLARE DECLARE @BAZ070  MAR   18/04/2022 MEJORAS PARA REDUCIR TIEMPOS ALTOS   *
      *  DECLARE DECLARE @BAZ071  JFCS  02/05/2022 SE CORRIGE DESCRIPCION POR COMPRA TA *
      *                           Y POR PAGO A COMERCIO; SE AGREGA     *
      *                           CONSULTA A MBDT258                   *
      *  DECLARE DECLARE @BAZ072 IASJ   18/05/2022 AGREGA VALIDACION DE CARGO PARA CODI *
      *  DECLARE DECLARE @BAZ073  PGR   22/06/2022 SE AGREGA DESCRIPCION PARA COD G89  **
      *  DECLARE DECLARE @BAZ074  JOM   25/07/2022 SE AGREGA DESCRIPCION PARA ALCANCIA  *
      *                           CON CODIGOS AG0; AF0; Z87; Z88; AD1  *
      ******************************************************************
      ******************************************************************
      *              ENVIRONMENT DIVISION                              *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-3090.
       OBJECT-COMPUTER.
           IBM-3090.
      *SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.
      ******************************************************************
      *                  DATA DIVISION                                 *
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
            COPY QAWCSQL.
      *
       01  VA-TCEC9900.
           COPY TCEC9900.
       01  CA-QBEC999-01.
           COPY QBEC999.
      *LCR-INI2
       01 MB7C0100-01.
          COPY MBEC0100.
      *LCR-FIN
      *
      * DECLARE DECLARE @BAZ063-INI
       01 VA-MPWC0009.
          COPY MPEC0009.
      * DECLARE DECLARE @BAZ063-FIN

       01 VA-QGECABC.
           COPY QGECABC.
      *
           COPY BGVC071.
      *-- Copy de Salida
           COPY MBNS0009.
      *
           COPY MBNS2009.
      *LCR-INI2
           COPY MBNS3009.
      *LCR-FIN2
      * DECLARE DECLARE @BAZ023.I
           COPY MBNS4009.
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
           COPY MBNS5009.

      * DECLARE DECLARE @BAZ027-F
      * DECLARE DECLARE @BAZ052-I
           COPY MBNS6009.

      * DECLARE DECLARE @BAZ052-F
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.
      * DECLARE DECLARE @BAZ033-I
           EXEC SQL
             INCLUDE BGGT148
           END-EXEC.
      * DECLARE DECLARE @BAZ033-F
      *
           EXEC SQL
             INCLUDE BAGV0010
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT071
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT089
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT606
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT140
           END-EXEC.
      *
           EXEC SQL
             INCLUDE GAGV0011
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BLGT002
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MCGT010
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MCGT028
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MCGT043
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MCGT114
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MCGT403
           END-EXEC.
      *
           EXEC SQL
             INCLUDE PEGT100
           END-EXEC.
      *
           EXEC SQL
             INCLUDE TCGT010
           END-EXEC.
      *
           EXEC SQL
             INCLUDE TCGV0403
           END-EXEC.
      * DECLARE DECLARE @BAZ.I
           EXEC SQL
             INCLUDE CGGT002
           END-EXEC.
      *
           EXEC SQL
             INCLUDE DMGT001
           END-EXEC.
      *
           EXEC SQL
             INCLUDE DMGT003
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT235
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT041
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGVC041
           END-EXEC.
      *
           EXEC SQL
             INCLUDE PEGT008
           END-EXEC.
      *
           EXEC SQL
             INCLUDE PEGT001
           END-EXEC.
      *
           EXEC SQL
             INCLUDE WPGT003
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGVC111
           END-EXEC.
      *
      *    EXEC SQL
      *      INCLUDE BGGT111
      *    END-EXEC.
      *
           EXEC SQL
             INCLUDE TCGV0811
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MBGT010
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MCGT097
           END-EXEC.
      *
           EXEC SQL
             INCLUDE MCGT011
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT600
           END-EXEC.
      *
           EXEC SQL
             INCLUDE BGGT607
           END-EXEC.
      * DECLARE DECLARE @BAZ005C.I
      *    COPY FEVC0040.
           EXEC SQL
             INCLUDE FEVC0040
           END-EXEC.
      * DECLARE DECLARE @BAZ005C.F
      * DECLARE DECLARE @BAZ021-INI
           EXEC SQL
             INCLUDE MBGT036
           END-EXEC.
      * DECLARE DECLARE @BAZ021-FIN
      * DECLARE DECLARE @BAZ046-I
           EXEC SQL
             INCLUDE MCGT279
           END-EXEC.
      * DECLARE DECLARE @BAZ046-F
      *
      * DECLARE DECLARE @BAZ052-I
           EXEC SQL
             INCLUDE MBGT039
           END-EXEC.
      * DECLARE DECLARE @BAZ052-F
      * DECLARE DECLARE @BAZ060-I
       EXEC SQL
            INCLUDE MBGT255
       END-EXEC.
      * DECLARE DECLARE @BAZ071-INI
       EXEC SQL
            INCLUDE MBGT258
       END-EXEC.
      * DECLARE DECLARE @BAZ071-FIN
      * DECLARE DECLARE @BAZ060-F
       01 CA-BGNC477.
           COPY BGNC477.
       01 VA-WPWC0010-01.
          COPY WPWC0010.
       01 VA-FENC1000.
          COPY FENC1000.
       01 VA-TCEC0810.
          COPY TCEC0810.
          03 VA-QBEC999-TCEC0810   PIC X(50); .
       01 VA-TCEC0820.
          COPY TCEC0820.
          05 VA-QBEC999-TCEC0820   PIC X(50); .
      * DECLARE DECLARE @BAZ.F
      *SWITCHES
       01 WS-SWITCHES.
      * DECLARE DECLARE @BAZ027-I
        05 WSS-SEG-CONSULTA        PIC X(01);  VALUE 'N'.
         88 WSS-SEGCON-SI                    VALUE 'S'.
         88 WSS-SEGCON-NO                    VALUE 'N'.
      * DECLARE DECLARE @BAZ027-F
        05 WSS-RSEGM               PIC X(01); .
         88 WSS-RSEGM-SI                     VALUE 'S'.
         88 WSS-RSEGM-NO                     VALUE 'N'.
        05 WSS-LTYLG               PIC X(01); .
         88 WSS-LTYLG-SI                     VALUE 'S'.
         88 WSS-LTYLG-NO                     VALUE 'N'.
        05 WSS-FIN-CURSOR1         PIC X(01); .
         88 WSS-FIN-S1                       VALUE 'S'.
         88 WSS-FIN-N1                       VALUE 'N'.
        05 WSS-FIN-CURSOR2         PIC X(01); .
         88 WSS-FIN-S2                       VALUE 'S'.
         88 WSS-FIN-N2                       VALUE 'N'.
        05 WSS-FIN-CURSOR3         PIC X(01); .
         88 WSS-FIN-S3                       VALUE 'S'.
         88 WSS-FIN-N3                       VALUE 'N'.
      * DECLARE DECLARE @BAZ029-INI
        05 WSS-CTA-GKIDS           PIC X(01); .
         88 WSS-CTA-SI                       VALUE 'S'.
         88 WSS-CTA-NO                       VALUE 'N'.
      * DECLARE DECLARE @BAZ029-FIN
        05 WSS-SIGNO               PIC X(01); .
         88 WSS-MENOS                        VALUE '-'.
         88 WSS-MAS                          VALUE '+'.
        05 WSS-PAGINACION          PIC X(01);  VALUE 'N'.
         88 WSS-SI-PAGINA                    VALUE 'S'.
         88 WSS-NO-PAGINA                    VALUE 'N'.
        05 WSS-TIPO-CONSULTA       PIC X(02); .
         88 WSS-TARJETA                      VALUE 'TA'.
         88 WSS-CUENTA                       VALUE 'CU'.
      * DECLARE DECLARE @BAZ070-I
         88 WSS-CUENTA-710                   VALUE 'C7'.
      * DECLARE DECLARE @BAZ070-F
         88 WSS-RET-TRJ                      VALUE 'RT'.
         88 WSS-RET-CTA                      VALUE 'RC'.
        05 WSS-FIN-PERFORM         PIC X(01);  VALUE 'N'.
         88 SW-FIN-OK                        VALUE 'S'.
         88 SW-FIN-NO                        VALUE 'N'.
      * DECLARE DECLARE @BAZ021-INI
        05 WSS-CONS-ORIGEN         PIC X(01);  VALUE SPACE.
         88 SW-WALLET                        VALUE 'W'.
         88 SW-BAZ                           VALUE 'B'.
        05 WSS-OPE-WALLET          PIC X(01);  VALUE SPACE.
         88 SW-OPE-WALLET-OK                     VALUE 'Y'.
         88 SW-OPE-WALLET-NK                     VALUE 'N'.

      * DECLARE DECLARE @BAZ021-FIN
      * DECLARE DECLARE @BAZ038 - INI
          05 SW-PARAM-VIRTUAL              PIC X(01);   VALUE '0'.
             88 SW-88-PARAM-V-OK                      VALUE '1'.
             88 SW-88-PARAM-V-NOK                     VALUE '0'.
          05 SW-FIN-BINES-VIRT             PIC X(01);   VALUE 'N'.
             88 SW-88-FIN-BIN-S                       VALUE 'S'.
             88 SW-88-FIN-BIN-N                       VALUE 'N'.
          05 SW-MOVTO-FOUND                PIC X(01);   VALUE 'N'.
             88 SW-88-MOV-FOU-S                       VALUE 'S'.
             88 SW-88-MOV-FOU-N                       VALUE 'N'.
      * DECLARE DECLARE @BAZ038 - FIN
      * DECLARE DECLARE @BAZ.I
        05 WSS-FINEDIT-AMT         PIC X(01);  VALUE 'N'.
         88 SW-FINEDIT-Y                     VALUE 'S'.
         88 SW-FINEDIT-N                     VALUE 'N'.
      * DECLARE DECLARE @BAZ.F
       01 WS-SWITCH.
        05 WSS-COD-OPERACION       PIC X(03);  VALUE SPACES.
      * DECLARE DECLARE @BAZ033-I
         88 SW-W50                           VALUE 'W50'.
         88 SW-T27                           VALUE 'T27'.
         88 SW-C75                           VALUE 'C75'.
      * DECLARE DECLARE @BAZ033-F
      * ABONO DE N�MINA
         88 SW-907                           VALUE '907'.
      * VENTA DE DOLARES
         88 SW-129                           VALUE '129'.
      * BONIFICACI�N POR PRIMERA COMPRA
         88 SW-S39                           VALUE 'S39'.
      * CARGO AUTOM�TICO
         88 SW-CARGO-AUT                     VALUE 'A24' '708' '707'
                                                   '579' '082'.
      * CARGOS VARIOS EXEMPLEADOS
         88 SW-H73                           VALUE 'H73'.
      * CHEQUE DE CAJA
         88 SW-534                           VALUE '534'.
      * CHEQUE DEVUELTO
         88 SW-170                           VALUE '170'.
      * CHEQUE SALVO BUEN COBRO
         88 SW-036                           VALUE '036'.
      * COBRO DE DINERO EXPRESS
         88 SW-S06                           VALUE 'S06'.
      * COMISI�N CHEQUE DE CAJA
         88 SW-731                           VALUE '731'.
      * COMISI�N CHEQUE SIN FONDOS
         88 SW-442                           VALUE '442'.
      * COMISI�N CONSULTA EN CAJERO
         88 SW-078                           VALUE '078'.
      * COMISI�N PAGO DE SERVICIOS
         88 SW-COM-PAGO-SER                  VALUE 'G25' '379'.
      * COMISI�N POR ENVIO DE DINERO(TEF); 
         88 SW-155                           VALUE '155'.
      * COMISI�N POR RETIRO
         88 SW-S10                           VALUE 'S10'.
         88 SW-013                           VALUE '013'.
      * COMISI�N POR TRASPASOS MASIVOS
         88 SW-550                           VALUE '550'.
      * COMISI�N PROTECCI�N DE CHEQUE
         88 SW-465                           VALUE '465'.
      * COMISI�N SPEI
         88 SW-216                           VALUE '216'.
      * COMPRA CON TARJETA
         88 SW-114                           VALUE '114'.
      * COMPRA DE DOLARES
         88 SW-130                           VALUE '130'.
      * COMPRA DE MONEDAS DE PLATA
         88 SW-A94                           VALUE 'A94'.
      * DEPOSITO
         88 SW-DEPOSITO                      VALUE '756' '416'.
      * DEPOSITO CHEQUE DE CAJA
         88 SW-533                           VALUE '533'.
      * DEPOSITO DE INVERSI�N
         88 SW-A36                           VALUE 'A36'.
      * DEPOSITO MONEDAS DE PLATA
         88 SW-350                           VALUE '350'.
      * DEP�SITO DE PRESTAMO
         88 SW-DEP-PREST                     VALUE 'P80' 'N95' 'N29'
                                                   'F23' 'S79' 'R82'
                                                   'J06' 'G10'.
      * DEP�SITO DE PRESTAMO GRUPAL
         88 SW-DEP-GRUP                      VALUE 'S87' 'L83' 'H08'.
      * DEPOSITO DE TERCEROS
         88 SW-160                           VALUE '160'.
      * DEPOSITO DE TERCEROS (TEF); 
         88 SW-215                           VALUE '215'.
      * DEPOSITO EN EFECTIVO
         88 SW-Q54                           VALUE 'Q54'.
         88 SW-J15                           VALUE 'J15'.
         88 SW-000                           VALUE '000'.
      * DEPOSITO POR VENTA
         88 SW-229                           VALUE '229'.
      * DEPOSITO PRESTA PRENDA
         88 SW-M52                           VALUE 'M52'.
      * DEVOLUCI�N COMISI�N
         88 SW-R20                           VALUE 'R20'.
      * DECOLUCI�N DE CHEQUE
         88 SW-874                           VALUE '874'.
      * DECOLUCI�N DE ENVIO DE DINERO (TEF); 
         88 SW-283                           VALUE '283'.
      * DEVOLUCI�N DE SPEI
         88 SW-DEV-SPEI                      VALUE '217' '214'.
      * DEVOLUCI�N POR VENTA
         88 SW-869                           VALUE '869'.
      * ENV�O DE DINERO (DEX); 
         88 SW-S08                           VALUE 'S08'.
      * ENVIO DE DINERO A TERCEROS
         88 SW-169                           VALUE '169'.
      * ENVIO DE DINERO A TERCEROS (TEF); 
         88 SW-152                           VALUE '152'.
      * FONDEO DE CHEQUE CON INFINITE
         88 SW-A85                           VALUE 'A85'.
      * INVERSI�N A PAGO FIJO
         88 SW-M78                           VALUE 'M78'.
      * INVERSI�N EN MERCADO DE DINERO
         88 SW-L67                           VALUE 'L67'.
      * IVA DE COMISI�N
         88 SW-IVA-COMISION                  VALUE 'S12' 'N75' 'M31'
                                                   'F71' '383' '193'
                                                   '192' 'R05'.
      * PAGO A TERCEROS
         88 SW-548                           VALUE '548'.
      * PAGO DE CHEQUE
         88 SW-PAGO-CHEQUE                   VALUE '875' '439' '438'.
      * PAGO DE INTERESES PLAZO
         88 SW-PAGO-INT                      VALUE 'H99' '972';'182'.
      * PAGO DE INTERES CRECIENTE
         88 SW-PAGO-INT2                     VALUE '884';'282';'882'.
      * PAGO DE INTERESES EXENTO DE ISR
         88 SW-880                           VALUE '880'.
      * PAGO DE PRESTAMO
         88 SW-PAGO-PREST                    VALUE 'M97' 'D23' 'C42'
                                                   '020'.
      * PAGO DE SERVICIO
         88 SW-PAGO-SERV                     VALUE 'Q95' 'G50' 'I24'
                                                   'G46' 'G16' 'G07'.
      * PAGO PRESTA PRENDA
         88 SW-PAGO-PP                       VALUE 'J73' 'J71'.
      * PAGO A TARJETA DE CR�DITO
         88 SW-PAGO-TC                       VALUE 'R81' '826'.
         88 SW-R80                           VALUE 'R80'.
         88 SW-U05                           VALUE 'U05'.
      * PRESTAMO DE N�MINA
         88 SW-PREST-NOM                     VALUE 'S20' 'H49'.
      * DEPOSITO DE PRESTAMO
         88 SW-R96                           VALUE 'R96'.
      * DEPOSITO POR PUNTOS DE LEALTAD
         88 SW-S34                           VALUE 'S34'.
      * PAGO DE PRESTAMO
         88 SW-D25                           VALUE 'D25'.
      * RENOVACI�N DE INVERSI�N
         88 SW-RENOV-INV                     VALUE '923' '664' '178'.
      * RETIRO A CUENTA
         88 SW-417                           VALUE '417'.
      * RETIRO DE EFECTIVO
         88 SW-RET-EFECT                     VALUE 'Q57' 'J14' '001'.
      * RETIRO DE MARCADO DE DINERO
         88 SW-L70                           VALUE 'L70'.
      * RETIRO DE MONEDAS DE PLATA
         88 SW-363                           VALUE '363'.
      * PAGO DE PRESTAMO
         88 SW-R97                           VALUE 'R97'.
      * RETIRO EN CAJERO BANCO AZTECA
         88 SW-111                           VALUE '111'.
      * RETIRO EN CAJERO DE OTRO BANCO
         88 SW-106                           VALUE '106'.
      * SEGURO DE AUTOMOVIL
         88 SW-A28                           VALUE 'A28'.
      * SPEI ENVIADO
         88 SW-212                           VALUE '212'.
      * SPEI RECIBIDO
         88 SW-213                           VALUE '213'.
      * TRASPASO A TERCEROS
         88 SW-549                           VALUE '549'.
      * Vencimiento de inversion
         88 SW-A35                           VALUE 'A35'.
      * Venta de Monedas de Plata
         88 SW-A95                           VALUE 'A95'.
      * DECLARE DECLARE @BAZ060-I Env�o Dinero Express MTCN/IVA MB20yMB90(cargo); 
         88 SW-T96                           VALUE 'T96'.
      * DECLARE DECLARE @BAZ060-F
      * Env�o Dinero Express MTCN/IVA MB20yMB90(cargo); 
         88 SW-S56                           VALUE 'S56'.
      * Comisi�n MTCN MB20yMB90 (cargo); 
         88 SW-S58                           VALUE 'S58'.
      * Compra Tiempo Aire
         88 SW-T05                           VALUE 'T05'.
      * Pago Facil
         88 SW-T60                           VALUE 'T60'.
      * Cobro Facil
         88 SW-T59                           VALUE 'T59'.
      * MN Guardadito GO
         88 SW-787                           VALUE '787'.
      * MN Guardadito GO
         88 SW-786                           VALUE '786'.
      * Dolares Guardadito GO
         88 SW-T64                           VALUE 'T64'.
      * Dolares Guardadito GO
         88 SW-T63                           VALUE 'T63'.
      * Retiro ATM
         88 SW-T31                           VALUE 'T31'.
      * MTCN MB48 (abono); 
         88 SW-S59                           VALUE 'S59'.
      * MTCN MB48 (cargo); 
         88 SW-S60                           VALUE 'S60'.
      * Env�o Dinero Express MTCN/IVA MB20yMB90 (abono); 
         88 SW-S55                           VALUE 'S55'.
      * Comisi�n MTCN MB20yMB90 (abono); 
         88 SW-S57                           VALUE 'S57'.
      * Impuesto retenido a cuenta Plazo
         88 SW-683                           VALUE '683'.
      *LCR-INI-DEPOSITO CREDITO NOMINA
         88 SW-S02                           VALUE 'S02'.
      *LCR-FIN
      * Apertura de cuenta
         88 SW-656                           VALUE '656'.
      * Deposito PagoQR3
         88 SW-T13                           VALUE 'T13'.
      * Retiro PagoQR3
         88 SW-T15                           VALUE 'T15'.
      * Cargo por pago de Cr�dito
         88 SW-T39                           VALUE 'T39'.
         88 SW-U08                           VALUE 'U08'.
      * Prestamo personal (cr�dito); 
         88 SW-P48                           VALUE 'P48'.
         88 SW-P50                           VALUE 'P50'.
      * Compras con Guardadito Go
         88 SW-U79                           VALUE 'U79'.
         88 SW-U80                           VALUE 'U80'.
      * Deposito Promocion BazDig
         88 SW-U50                           VALUE 'U50'.
      * Deposito (al cancelar pedido de compra QR ekt); 
         88 SW-U36                           VALUE 'U36'.
      * DECLARE DECLARE @BAZ018-->INI
      * Promocion Billetazo.
         88 SW-T53                           VALUE 'T53'.
      * DECLARE DECLARE @BAZ018<--FIN
      *BAZ019-I
         88 SW-V06                           VALUE 'V06'.
      *BAZ019-F
      * DECLARE DECLARE @BAZ021-INI
         88 SW-SPC                           VALUE '   '.
      * DECLARE DECLARE @BAZ021-FIN
      * DECLARE DECLARE @BAZ026-INI
      * INVERSION AZTECA MAS
         88 SW-Z00                           VALUE 'Z00'.
      * DAPP DEPOSITO
         88 SW-Z25                           VALUE 'Z25'.
      * DAPP CARGO
         88 SW-Z26                           VALUE 'Z26'.
      * DECLARE DECLARE @BAZ026-FIN
      * DECLARE DECLARE @BAZ049-I
         88 SW-Z51                           VALUE 'Z51'.
      * DECLARE DECLARE @BAZ049-F
      * DECLARE DECLARE @BAZ056-I SUPER APP
      *SURTIMIENTO CREDITO BAZ
         88 SW-Z83                           VALUE 'Z83'.
         88 SW-Z84                           VALUE 'Z84'.
      *PAGO CREDITO
         88 SW-Z85                           VALUE 'Z85'.
         88 SW-Z86                           VALUE 'Z86'.
      *ENVIO CUENTA BAZ
         88 SW-Z87                           VALUE 'Z87'.
         88 SW-Z88                           VALUE 'Z88'.
      *PAGO SERVICIO
         88 SW-Z89                           VALUE 'Z89'.
         88 SW-Z90                           VALUE 'Z90'.
      *VENTA TIEMPO AIRE
         88 SW-Z91                           VALUE 'Z91'.
         88 SW-Z92                           VALUE 'Z92'.
      *ENVIO CELULAR BAZ
         88 SW-Z93                           VALUE 'Z93'.
         88 SW-Z94                           VALUE 'Z94'.
      *PAGO QR BAZ
         88 SW-Z95                           VALUE 'Z95'.
         88 SW-Z96                           VALUE 'Z96'.
      *COBRO QR BAZ
         88 SW-Z97                           VALUE 'Z97'.
         88 SW-Z98                           VALUE 'Z98'.
      *TRASPASO CUENTA CHAT BAZ
         88 SW-Z99                           VALUE 'Z99'.
         88 SW-AA0                           VALUE 'AA0'.
      *COMPRA PELICULAS BAZ
         88 SW-AA1                           VALUE 'AA1'.
         88 SW-AA2                           VALUE 'AA2'.
      *TRASPASO DONATIVO BAZ
         88 SW-AA3                           VALUE 'AA3'.
         88 SW-AA4                           VALUE 'AA4'.
      *COMPRA INTERNET BAZ
         88 SW-AA5                           VALUE 'AA5'.
         88 SW-AA6                           VALUE 'AA6'.
      *RECARGA DE SALDO BAZ
         88 SW-AA7                           VALUE 'AA7'.
         88 SW-AA8                           VALUE 'AA8'.
      *RETIRO BAZ
         88 SW-AA9                           VALUE 'AA9'.
         88 SW-AB0                           VALUE 'AB0'.
      *COMPRA TIEMPO AIRE
         88 SW-AB1                           VALUE 'AB1'.
         88 SW-AB2                           VALUE 'AB2'.
      *COMPRA TARJETAS PREPAGO
         88 SW-AB3                           VALUE 'AB3'.
         88 SW-AB4                           VALUE 'AB4'.
      *VENTA TARJETAS PREPAGO
         88 SW-AB5                           VALUE 'AB5'.
         88 SW-AB6                           VALUE 'AB6'.
      *PAGO CREDITO OTROS BAZ
         88 SW-AB7                           VALUE 'AB7'.
         88 SW-AB8                           VALUE 'AB8'.
      *PAGO SERVICIOS OTROSPA
         88 SW-AB9                           VALUE 'AB9'.
         88 SW-AC0                           VALUE 'AC0'.
      *SURTIMIENTO CREDITO
         88 SW-AD2                           VALUE 'AD2'.
         88 SW-AD3                           VALUE 'AD3'.
      *COMISION PAGO SERVICIOS
         88 SW-AD4                           VALUE 'AD4'.
         88 SW-AD5                           VALUE 'AD5'.
      *COMISION TIEMPO AIRE
         88 SW-AD6                           VALUE 'AD6'.
         88 SW-AD7                           VALUE 'AD7'.
      *DEPOSITO  TDC SUPERAPP
         88 SW-AE0                           VALUE 'AE0'.
      *RETIRO  TDC SUPERAPP
         88 SW-AE1                           VALUE 'AE1'.
      *PAGO CREDITO OTROS
         88 SW-AE2                           VALUE 'AE2'.
         88 SW-AE3                           VALUE 'AE3'.
      *RECARGA TARJETA BAZ
      * DECLARE DECLARE @BAZ059-I
         88 SW-AG0                           VALUE 'AG0'.
      * DECLARE DECLARE @BAZ059-F
         88 SW-AF0                           VALUE 'AF0'.
         88 SW-AF1                           VALUE 'AF1'.
      * DECLARE DECLARE @BAZ065-I
      *ABONO TRASPASO
         88 SW-AI2                           VALUE 'AI2'.
      *CARGO TRASPASO
         88 SW-AI3                           VALUE 'AI3'.
         88 SW-AQ1                           VALUE 'AQ1'.
         88 SW-AQ2                           VALUE 'AQ2'.
         88 SW-AQ3                           VALUE 'AQ3'.
         88 SW-AQ4                           VALUE 'AQ4'.
         88 SW-AQ5                           VALUE 'AQ5'.
         88 SW-AQ6                           VALUE 'AQ6'.
      * DECLARE DECLARE @BAZ065-F
      * DECLARE DECLARE @BAZ069-INI
      *TRASPASO POR SERVICIO
         88 SW-AV5                           VALUE 'AV5'.
         88 SW-AV6                           VALUE 'AV6'.
      * DECLARE DECLARE @BAZ069-FIN
      * DECLARE DECLARE @BAZ073-I
         88 SW-G89                           VALUE 'G89'.
      * DECLARE DECLARE @BAZ073-F
      * DECLARE DECLARE @BAZ074-I
         88 SW-AD1                           VALUE 'AD1'. 
      * DECLARE DECLARE @BAZ074-F
      *
        05 CA-1156                       PIC X(04);    VALUE '1156'.
        05 CA-54                         PIC X(02);    VALUE '54'.
      * DECLARE DECLARE @BAZ056-F
      * DECLARE DECLARE @BAZ022-INI
        05 SW-PORTABILIDAD         PIC X(01);  VALUE 'N'.
         88 SW-PORTANOM-Y                    VALUE 'S'.
         88 SW-PORTANOM-N                    VALUE 'N'.
      * DECLARE DECLARE @BAZ022-FIN
      * DECLARE DECLARE @BAZ056-I
         01 SW-SAPP                       PIC X(002);  VALUE '10'.
            88 SW-OK-SAPP                     VALUE '00'.
            88 SW-NOK-SAPP                    VALUE '10'.
      * DECLARE DECLARE @BAZ070-I
         01 SW-BAN-TBL                     PIC X(003);  VALUE '071'.
            88 SW-BAN071                      VALUE '071'.
            88 SW-BAN710                      VALUE '710'.
      * DECLARE DECLARE @BAZ070-F
      * DECLARE DECLARE @BAZ056-F
      * DECLARE DECLARE @BAZ066.I
         01 SW-B925                       PIC X(001);  VALUE 'N'.
            88 SW-B925-SI                     VALUE 'S'.
            88 SW-B925-NO                     VALUE 'N'.
      * DECLARE DECLARE @BAZ066.F
      * DECLARE DECLARE @BAZ070.I
         01 SW-VALIDA-SP                  PIC X(001);  VALUE 'N'.
            88 SW-PRENDE-JSON                 VALUE 'S'.
            88 SW-APAGA-JSON                  VALUE 'N'.
      * DECLARE DECLARE @BAZ070.F
      ****************  CONSTANTES  ************************************
       01 WS-CONSTANTES.
      *
          05 WSCN-1                    PIC S9(1);   VALUE +1.
          05 WSCN-5                    PIC S9(1);   VALUE +5.
          05 WSCN-20                   PIC S9(2);   VALUE +20.
      * DECLARE DECLARE @BAZ034-I
          05 CA-NA                     PIC X(03);   VALUE 'N/A'.
      * DECLARE DECLARE @BAZ034-F
          05 CN-99                     PIC 9(02);   VALUE 99.
          05 CA-FETCH                  PIC X(05);   VALUE 'FETCH'.
          05 CA-SELECT                 PIC X(06);   VALUE 'SELECT'.
          05 CA-INSERT                 PIC X(06);   VALUE 'INSERT'.
          05 CA-DC1                    PIC X(04);   VALUE '+DC1'.
          05 CA-E                      PIC X(01);   VALUE 'E'.
          05 CA-N                      PIC X(01);   VALUE 'N'.
          05 CA-S                      PIC X(01);   VALUE 'S'.
          05 CA-P                      PIC X(01);   VALUE 'P'.
          05 CA-1                      PIC X(01);   VALUE '1'.
          05 CA-00                     PIC X(02);   VALUE '00'.
          05 CA-01                     PIC X(02);   VALUE '01'.
          05 CA-02                     PIC X(02);   VALUE '02'.
          05 CA-CU                     PIC X(02);   VALUE 'CU'.
      * DECLARE DECLARE @BAZ070-I
          05 CA-C7                     PIC X(02);   VALUE 'C7'.
          05 CA-BRACKETS               PIC X(02);   VALUE '{}'.
      * DECLARE DECLARE @BAZ070-F
          05 CA-TA                     PIC X(02);   VALUE 'TA'.
          05 CA-RC                     PIC X(02);   VALUE 'RC'.
          05 CA-RT                     PIC X(02);   VALUE 'RT'.
          05 CA-D002                   PIC X(04);   VALUE 'D002'.
          05 CA-E003                   PIC X(04);   VALUE 'E003'.
          05 CA-0127                   PIC X(04);   VALUE '0127'.
          05 CA-0140                   PIC X(04);   VALUE '0140'.
          05 CA-V06                    PIC X(03);   VALUE 'V06'.
      * DECLARE DECLARE @BAZ070-I
          05 CA-71                     PIC X(03);   VALUE '071'.
          05 CA-710                    PIC X(03);   VALUE '710'.
      * DECLARE DECLARE @BAZ070-F
      *BAZ053-INI
          05 CT-CR                     PIC X(02);   VALUE 'CR'.
      *BAZ053-FIN
      * DECLARE DECLARE @BAZ027-I
          05 CA-LIMITE                 PIC 9(02);   VALUE 15.
          05 CA-0                      PIC S9(1);   VALUE +0.
          05 CA-10                     PIC X(02);   VALUE '10'.
          05 CA-03                     PIC X(02);   VALUE '03'.
          05 CA-40                     PIC X(02);   VALUE '40'.
          05 CA-217                    PIC X(03);   VALUE '217'.
          05 CA-G07                    PIC X(03);   VALUE 'G07'.
          05 CA-G16                    PIC X(03);   VALUE 'G16'.
      * DECLARE DECLARE @BAZ050.I
          05 CA-Z36                    PIC X(03);   VALUE 'Z36'.
          05 CA-MBL7                   PIC X(04);   VALUE 'MBL7'.
      * DECLARE DECLARE @BAZ050.F
      * DECLARE DECLARE @BAZ066.I
          05 CA-160                    PIC X(03);   VALUE '160'.
          05 CA-B925                   PIC X(04);   VALUE 'B925'.
      * DECLARE DECLARE @BAZ066.F
      *
          05 CA-MBW5                   PIC X(04);   VALUE 'MBW5'.
          05 CA-MBW9                   PIC X(04);   VALUE 'MBW9'.
          05 CA-B601                   PIC X(04);   VALUE 'B601'.
          05 CA-B501                   PIC X(04);   VALUE 'B501'.
          05 CA-0500                   PIC X(04);   VALUE '0500'.
          05 CA-0600                   PIC X(04);   VALUE '0600'.
          05 CA-0700                   PIC X(04);   VALUE '0700'.
          05 CA-B043                   PIC X(04);   VALUE 'B043'.
          05 CA-F648                   PIC X(04);   VALUE 'F648'.
      * DECLARE DECLARE @BAZ056-I
          05 CA-FS64                   PIC X(04);   VALUE 'FS64'.
      * DECLARE DECLARE @BAZ056-F
          05 CA-MBW4                   PIC X(04);   VALUE 'MBW4'.
      * DECLARE DECLARE @BAZ049-I
          05 CA-MBWE                   PIC X(04);   VALUE 'MBWE'.
      * DECLARE DECLARE @BAZ049-F
      *BAZ044-I
          05 CA-BATC                   PIC X(04);   VALUE 'BATC'.
      *BAZ044-F
      * DECLARE DECLARE @BAZ034-I
          05 CA-MBWB                   PIC X(04);   VALUE 'MBWB'.
      * DECLARE DECLARE @BAZ034-F
      * DECLARE DECLARE @BAZ060-I
          05 CA-MBWI                   PIC X(04);   VALUE 'MBWI'.
      * DECLARE DECLARE @BAZ060-F
      * DECLARE DECLARE @BAZ041-I
          05 CA-B520                   PIC X(04);   VALUE 'B520'.
          05 CA-7760                   PIC X(04);   VALUE '7760'.
      * DECLARE DECLARE @BAZ041-F
          05 CA-MB03                   PIC X(04);   VALUE 'MB03'.
          05 CA-BS03                   PIC X(04);   VALUE 'BS03'.
          05 CA-MBW8                   PIC X(04);   VALUE 'MBW8'.
          05 CA-TOMIIN                 PIC X(06);   VALUE 'Tomiin'.
          05 CA-MB4C0100               PIC X(08);   VALUE 'MB4C0100'.
          05 CA-BAZ                    PIC X(12);   VALUE 'Banco Azteca'.
      * DECLARE DECLARE @BAZ044-I
      *   05 CA-RET-ATM                PIC X(13);   VALUE 'Retiro en ATM'.
          05 CA-RET-ATM                PIC X(18);   VALUE
                                                   'Retiro sin tarjeta'.
          05 CA-BON-DOS-P              PIC X(29);   VALUE
                                        'Bonificacion Promo de 2 pesos'.
      * DECLARE DECLARE @BAZ044-F
          05 CA-DEP-BANCO              PIC X(17);   VALUE
                                                    'Dep�sito en banco'.
          05 CA-ENV-ATM-TOMIIN         PIC X(18);   VALUE
                                                   'Envio a ATM Tomiin'.
          05 CA-RET-ATM-TOMIIN         PIC X(23);   VALUE
                                              'Retiraste dinero de ATM'.
          05 CA-DEP-EFECTIVO           PIC X(24);   VALUE
                                             'Dep�sito de efectivo en '.
          05 CA-RET-VEN-TOMIIN         PIC X(30);   VALUE
                                       'Retiraste dinero de ventanilla'.
          05 CA-DEP-TELECOMM           PIC X(32);   VALUE
                                     'Dep�sito de efectivo en Telecomm'.
      * DECLARE DECLARE @BAZ049-I
          05 CA-ADEUDO-ENTR            PIC X(14);   VALUE
                                                       'ADEUDO ENTRADA'.
          05 CA-ADEUDO-COMI            PIC X(15);   VALUE
                                                      'ADEUDO COMISION'.
          05 CA-SERVICIO-TOM           PIC X(15);   VALUE
                                                      'Servicio Tomiin'.
          05 CA-COMPRA-ENTRADA         PIC X(18);   VALUE
                                                   'Compra de entradas'.
          05 CA-ENTRADA-REUTILIZ       PIC X(19);   VALUE
                                                  'Entrada reutilizada'.
          05 CA-ENTRADA-METRO          PIC X(21);   VALUE
                                                'Entrada al Metro CDMX'.
          05 CA-COMISION-COMPRA        PIC X(23);   VALUE
                                              'Comisi�n por compra de '.
          05 CA-COMISION-ENTRADA       PIC X(26);   VALUE
                                           'Comisi�n compra de entrada'.
          05 CA-COMI-REUT-ENTRA        PIC X(26);   VALUE
                                           'Comisi�n reutiliza entrada'.
          05 CA-ADEU-ENTMET            PIC X(37);   VALUE
                                'Adeudo entrada reutilizada metro CDMX'.
          05 CA-COMI-REUT-ENTMET       PIC X(40);   VALUE
                             'Comisi�n al reutilizar entradas al Metro'.
          05 CA-ENTRADA-REU-MET        PIC X(41);   VALUE
                            'Entrada reutilizada para acceder al Metro'.
          05 CA-COMI-ADEU-ENTMET       PIC X(46);   VALUE
                       'Adeudo comisi�n entrada reutilizada metro CDMX'.
      * DECLARE DECLARE @BAZ049-F
      * DECLARE DECLARE @BAZ050.I
          05 CA-COMP-GIFTCARD          PIC X(22);   VALUE
                                               'COMPRA TARJETA DIGITAL'.
      * DECLARE DECLARE @BAZ050.F
      * DECLARE DECLARE @BAZ066.I
          05 CA-DEPGO-BAZ-USA          PIC X(31);   VALUE
                                     'Dep�sito desde EUA a cuenta baz'.
      * DECLARE DECLARE @BAZ066.F
      * DECLARE DECLARE @BAZ027-F
          05 CA-QG1CABC                PIC X(07);   VALUE 'QG1CABC'.
      *LCR-INI2
          05 CT-MB7C0110               PIC X(08);  VALUE 'MB7C0110'.
      *LCR- FIN
          05 CA-BGDT071                PIC X(07);   VALUE 'BGDT071'.
          05 CA-BGDT089                PIC X(07);   VALUE 'BGDT089'.
          05 CA-BGDT710                PIC X(07);   VALUE 'BGDT710'.
          05 CA-MCDT010                PIC X(07);   VALUE 'MCDT010'.
          05 CA-MCDT028                PIC X(07);   VALUE 'MCDT028'.
          05 CA-MCDT043                PIC X(07);   VALUE 'MCDT043'.
          05 CA-MCDT114                PIC X(07);   VALUE 'MCDT114'.
          05 CA-MCDT403                PIC X(07);   VALUE 'MCDT403'.
      * DECLARE DECLARE @BAZ046-I
          05 CA-MCDT279                PIC X(07);   VALUE 'MCDT279'.
      * DECLARE DECLARE @BAZ046-F
      * DECLARE DECLARE @BAZ052-I
          05 CA-MBDT039                PIC X(07);   VALUE 'MBDT039'.

      * DECLARE DECLARE @BAZ052-F
      * DECLARE DECLARE @BAZ063-INI
          05 CA-MP9C0009               PIC X(08);   VALUE 'MP9C0009'.
          05 CA-114                    PIC X(03);   VALUE '114'.
      * DECLARE DECLARE @BAZ063-FIN
          05 CA-PEDT100                PIC X(07);   VALUE 'PEDT100'.
          05 CA-MB2C0009               PIC X(08);   VALUE 'MB2C0009'.
          05 CA-TC9C9900               PIC X(08);   VALUE 'TC9C9900'.
          05 CA-MAXLLAV                PIC X(20);   VALUE
                                            '9999-12-31 999999999'.
      * DECLARE DECLARE @BAZ070-I
          05 CA-FECHA-MAX              PIC X(19);  VALUE  '9999-12-31'.
      * DECLARE DECLARE @BAZ070-F
          05 CA-WRITEQQUEUE            PIC X(17);  VALUE
                                               'ERROR WRITEQ COLS'.
          05 CA-BAN-EMP-AZT            PIC X(24);  VALUE
                                        'Banca Empresarial Azteca'.
      * DECLARE DECLARE @BAZ.I
          05 CA-BS9C6101               PIC X(08);  VALUE 'BS9C6101'.
          05 CA-BG7C4770               PIC X(10);  VALUE 'BG7C4770'.
          05 CA-TC7C0820               PIC X(08);  VALUE 'TC7C0820'.
          05 CA-TC7C0810               PIC X(08);  VALUE 'TC7C0810'.
          05 COD-0497                  PIC X(04);  VALUE '0497'.
          05 FECHCOD-INVERTIDO         PIC X(10);  VALUE '2017-01-26'.
      * DECLARE DECLARE @BAZ.F
      * DECLARE DECLARE @BAZ031-I
          05 CA-COD-Y84                PIC X(03);  VALUE 'Y84'.
      * DECLARE DECLARE @BAZ031-F

      * DECLARE DECLARE @BAZ047-INI
          05 CA-COD-Z28                PIC X(03);  VALUE 'Z28'.
      * DECLARE DECLARE @BAZ047-FIN

      * DECLARE DECLARE @BAZ038 - I
          05 CA-MBT1                       PIC X(04);    VALUE 'MBT1'.
          05 CA-BINVIRTUAL                 PIC X(10);    VALUE
                                                           'BINVIRTUAL'.
          05 CA-FECHINV                    PIC X(10);    VALUE
                                                           '0001-01-01'.
      * DECLARE DECLARE @BAZ038 - F
      * DECLARE DECLARE @BAZ056-I
          05 CA-SUCRBAZ    PIC X(23);  VALUE 'SURTIMIENTO CREDITO BAZ'.
          05 CA-PACR       PIC X(12);  VALUE 'PAGO CREDITO'.
          05 CA-ENCTABAZ   PIC X(16);  VALUE 'ENVIO CUENTA BAZ'.
          05 CA-PASER      PIC X(13);  VALUE 'PAGO SERVICIO'.
          05 CA-VETIAI     PIC X(17);  VALUE 'VENTA TIEMPO AIRE'.
          05 CA-ENCELBAZ   PIC X(17);  VALUE 'ENVIO CELULAR BAZ'.
          05 CA-PAQRBAZ    PIC X(11);  VALUE 'PAGO QR BAZ'.
          05 CA-COQRBAZ    PIC X(12);  VALUE 'COBRO QR BAZ'.
          05 CA-TRCTACHBAZ PIC X(24);  VALUE 'TRASPASO CUENTA CHAT BAZ'.
      * DECLARE DECLARE @BAZ058-I
      *   05 CA-COPEBAZ    PIC X(20);  VALUE 'COMPRA PELICULAS BAZ'.
          05 CA-COPEBAZ    PIC X(20);  VALUE 'RENTA PELICULAS BAZ'.
      * DECLARE DECLARE @BAZ058-I
          05 CA-TRDOBAZ    PIC X(21);  VALUE 'TRASPASO DONATIVO BAZ'.
          05 CA-COMINTBAZ  PIC X(19);  VALUE 'COMPRA INTERNET BAZ'.
          05 CA-RECSALBAZ  PIC X(20);  VALUE 'RECARGA DE SALDO BAZ'.
          05 CA-RETIBAZ    PIC X(10);  VALUE 'RETIRO BAZ'.
          05 CA-COMPTIEAIR PIC X(18);  VALUE 'COMPRA TIEMPO AIRE'.
          05 CA-COMTARPRE  PIC X(23);  VALUE 'COMPRA TARJETAS PREPAGO'.
          05 CA-VENTARPRE  PIC X(22);  VALUE 'VENTA TARJETAS PREPAGO'.
          05 CA-PAGCREDOT  PIC X(22);  VALUE 'PAGO CREDITO OTROS BAZ'.
          05 CA-PAGSERVOT  PIC X(20);  VALUE 'PAGO SERVICIOS OTROS'.
          05 CA-SURCREDIT  PIC X(19);  VALUE 'SURTIMIENTO CREDITO'.
          05 CA-COMPAGSER  PIC X(23);  VALUE 'COMISION PAGO SERVICIOS'.
          05 CA-COMTIEAIR  PIC X(20);  VALUE 'COMISION TIEMPO AIRE'.
          05 CA-DEPTDCSUP  PIC X(22);  VALUE 'DEPOSITO  TDC SUPERAPP'.
          05 CA-RETTDCSUP  PIC X(20);  VALUE 'RETIRO  TDC SUPERAPP'.
          05 CA-PAGCREOTR  PIC X(18);  VALUE 'PAGO CREDITO OTROS'.
          05 CA-RECTARBAZ  PIC X(19);  VALUE 'RECARGA TARJETA BAZ'.
      * DECLARE DECLARE @BAZ056-F
      * DECLARE DECLARE @BAZ065-I
          05 CA-COMREDEXT  PIC X(20);  VALUE 'COMPRA RED EXTENDIDA'.
      * DECLARE DECLARE @BAZ067-I
      *   05 CA-COMMISENT  PIC X(19);  VALUE 'COMPRA MIS ENTREGAS'.
      *   05 CA-SERMISENT  PIC X(21);  VALUE 'SERVICIO MIS ENTREGAS'.
      *   05 CA-PROMISENT  PIC X(20);  VALUE 'PROPINA MIS ENTREGAS'.
          05 CA-COMMISENT  PIC X(11);  VALUE 'PAGO PEDIDO'.
          05 CA-SERMISENT  PIC X(14);  VALUE 'COSTO DE ENVIO'.
          05 CA-PROMISENT  PIC X(29); 
                           VALUE 'PROPINA POR SERVICIO DE ENVIO'.
      * DECLARE DECLARE @BAZ067-F
      * DECLARE DECLARE @BAZ065-F
      * DECLARE DECLARE @BAZ069-1
          05 CA-ENVPAREC   PIC X(22);  VALUE 'ENVIO PARA RECARGA BAZ'.
          05 CA-RECARBAZ   PIC X(11);  VALUE 'RECARGA BAZ'.
          05 CA-RECEPEFEC  PIC X(21);  VALUE 'RECEPCION DE EFECTIVO'.
          05 CA-TRASPSERV  PIC X(21);  VALUE 'TRASPASO POR SERVICIO'.
          05 CA-REEMENTEF  PIC X(33);  VALUE
                                   'REEMBOLSO POR ENTREGA DE EFECTIVO'.
      * DECLARE DECLARE @BAZ074-I
          05 CA-DEPALCGP   PIC X(26);  VALUE 'DEPOSITO A ALCANCIA GRUPAL'. 
      * DECLARE DECLARE @BAZ074-F
                             
      * DECLARE DECLARE @BAZ069-F
      *ACUMULADORES / CONTADORES
       01 WS-ACUMULADORES.
      * DECLARE DECLARE @BAZ065-I
          05 VN-CON2               PIC 9(03);  VALUE ZEROES.
      * DECLARE DECLARE @BAZ065-F
          05 VN-GRABADOS           PIC 9(03);  VALUE ZEROES.
          05 VN-CONTREG            PIC 9(03);  VALUE ZEROES.
          05 VN-LEIDOS             PIC 9(03);  VALUE ZEROES.
          05 VN-IND                PIC 9(03);  VALUE ZEROES.
          05 VN-IND1               PIC 9(03);  VALUE ZEROES.
          05 VN-IND2               PIC 9(03);  VALUE ZEROES.
          05 VN-CONT               PIC 9(03);  VALUE ZEROES.
          05 VN-CONQ               PIC 9(03);  VALUE ZEROES.
      * DECLARE DECLARE @BAZ034-I
          05 VN-SPC                PIC 9(03);  VALUE ZEROES.
      * DECLARE DECLARE @BAZ034-F
      * DECLARE DECLARE @BAZ051-I
          05 VN-AUX1               PIC 9(02);  VALUE ZEROES.
          05 VN-AUX2               PIC 9(02);  VALUE ZEROES.
          05 VN-AUX3               PIC 9(04);  VALUE ZEROES.
      * DECLARE DECLARE @BAZ051-F
      * DECLARE DECLARE @BAZ038 - INI
          05 WSV-CONT                   PIC 9(02); .
          05 WSV-CONT1                  PIC 9(02); .
          05 VN-MARCA-AUX               PIC X(30); .
          05 VN-CON                     PIC 9(03);       VALUE 0.
          05 VN-FIN-CADENA              PIC 9(03);       VALUE 0.
          05 VN-VAL-INI                 PIC 9(02);       VALUE 0.
      * DECLARE DECLARE @BAZ038 - FIN
      *VARIABLES
       01 WS-VARIABLES.
      * DECLARE DECLARE @BAZ030-I
          05  N100-NUM-REF-OPE  PIC    9(10); .
          05  V0011-NUM-REF-OPE  REDEFINES
                       N100-NUM-REF-OPE  PIC    9(10); .
      * DECLARE DECLARE @BAZ030-F
          05 VA-MBS41                 PIC X(04); .
          05 VA-MBS42                 PIC X(40); .
          05 VA-MBS43                 PIC X(04); .
          05 VA-MBS45                 PIC X(04); .
          05 VA-NOMBRESEG             PIC X(50); .
      * DECLARE DECLARE @BAZ063-INI
          05 VA-T071-TIM-OPERATION          PIC X(06);  VALUE SPACES.
          05 VA-CARGOS-REC.
               10 VA-S109-NUM-OPE-DEB       PIC X(05);  VALUE SPACES.
               10 VA-S109-TIPO-OPE          PIC X(07);  VALUE SPACES.
               10 VA-S109-NUM-AUTO          PIC X(06);  VALUE SPACES.
               10 VA-S109-COD-FCC           PIC X(03);  VALUE SPACES.
               10 VA-S109-NUM-REFER         PIC X(25);  VALUE SPACES.
               10 VA-S109-COD-ACT           PIC X(04);  VALUE SPACES.
               10 VA-S109-NUM-NEGOC         PIC X(10);  VALUE SPACES.
               10 VA-S109-OPE-ORI           PIC X(02);  VALUE SPACES.
               10 VA-S109-NUM-DECLA         PIC X(04);  VALUE SPACES.
               10 VA-S109-NUM-TRANS         PIC X(06);  VALUE SPACES.
               10 VA-S109-NUM-CARD          PIC X(16);  VALUE SPACES.
               10 VA-S109-RECURRENTE        PIC X(16);  VALUE SPACES.

      * DECLARE DECLARE @BAZ063-FIN
      * DECLARE DECLARE @BAZ027-I
          05 VA-CUENTA-AUX             PIC X(20);  VALUE SPACES.
          05 VA-ALIAS-AUX              PIC X(50);  VALUE SPACES.
          05 VA-NOMBRE-AUX             PIC X(60);  VALUE SPACES.
      * DECLARE DECLARE @BAZ044-I
          05 VA-DESC-AUX               PIC X(60);  VALUE SPACES.
      * DECLARE DECLARE @BAZ044-F
          05 VA-NAME-CUS-AUX           PIC X(60);  VALUE SPACES.
          05 VA-NOMBRE-COMPLE.
             10 VA-NOMBRES             PIC X(20);  VALUE SPACES.
             10 VA-APELLIDO-PAT        PIC X(20);  VALUE SPACES.
             10 VA-APELLIDO-MAT        PIC X(20);  VALUE SPACES.
      * DECLARE DECLARE @BAZ027-F
      *MCV003-I
          05 VN-AUX-TXT-R              PIC 9(03);  VALUE ZEROS.
          05 VN-AUX-TXT                REDEFINES VN-AUX-TXT-R
                                       PIC X(03);  VALUE SPACES.
      *MCV003-F
         05 VA-IND-PAGINA              PIC X(01);  VALUE SPACES.
         05 VA-FECHA-CALC.
           10 VA-YEAR-CALC             PIC X(04);  VALUE SPACES.
           10 FILLER                   PIC X(01);  VALUE '-'.
           10 VA-MES-CALC              PIC X(02);  VALUE SPACES.
           10 FILLER                   PIC X(01);  VALUE '-'.
           10 VA-DIA-CALC              PIC X(02);  VALUE SPACES.
         05 VN-FECHA-C9 REDEFINES VA-FECHA-CALC.
           10 VA-YEAR-C9               PIC 9(04); .
           10 FILLER                   PIC X(01); .
           10 VA-MES-C9                PIC 9(02); .
           10 FILLER                   PIC X(01); .
           10 VA-DIA-C9                PIC 9(02); .
      *
         05 VA-FECHA-SIST.
           10 VA-YEAR-SIST             PIC X(04);  VALUE SPACES.
           10 FILLER                   PIC X(01);  VALUE '-'.
           10 VA-MES-SIST              PIC X(02);  VALUE SPACES.
           10 FILLER                   PIC X(01);  VALUE '-'.
           10 VA-DIA-SIST              PIC X(02);  VALUE SPACES.
         05 VN-FECHA-S9 REDEFINES VA-FECHA-SIST.
           10 VN-YEAR-S9               PIC 9(04); .
           10 FILLER                   PIC X(01); .
           10 VN-MES-S9                PIC 9(02); .
           10 FILLER                   PIC X(01); .
           10 VN-DIA-S9                PIC 9(02); .
      *
         05 WSV-AUXSAL.
           10 WSV-AUX-FECHA            PIC X(10);  VALUE SPACES.
      * DECLARE DECLARE @BAZ063-INI
      *    10 WSV-AUX-HORA             PIC X(04);  VALUE SPACES.
           10 WSV-AUX-HORA             PIC X(06);  VALUE SPACES.
      * DECLARE DECLARE @BAZ063-FIN
           10 WSV-AUX-DESC             PIC X(31);  VALUE SPACES.
           10 WSV-AUX-IMPT             PIC +(12); 9.99 VALUE ZEROES.
           10 WSV-AUX-NUMOPE           PIC Z(08); 9 VALUE ZEROES.
           10 WSV-AUX-CLASIF           PIC X(03);  VALUE SPACES.
           10 VA-AUX-PAGINA.
             15 VA-AUX-LLAVE           PIC X(20);  VALUE SPACES.
             15 VA-AUX-TCONS           PIC X(02);  VALUE SPACES.
         05 VA-ENT-PAGINA.
             15 VA-ULT-LLAVE           PIC X(20);  VALUE SPACES.
             15 VA-TIP-CONS            PIC X(02);  VALUE SPACES.
         05 VA-AUX-POSICION            PIC X(11); .
         05 WSV-AUXENTP.
           10 WSV-AUX-LATITUD          PIC S9(03); V9(6);  VALUE ZEROES.
           10 WSV-AUX-LONGITD          PIC S9(03); V9(6);  VALUE ZEROES.
         05 WSV-AUXCTA.
           10 WSV-AUXCTA-ENT           PIC X(04);  VALUE SPACES.
           10 WSV-AUXCTA-CEN-REG       PIC X(04);  VALUE SPACES.
           10 WSV-AUXCTA-ACC           PIC X(10);  VALUE SPACES.
         05 VA-FECHA-OPERACION         PIC X(10);  VALUE SPACES.
         05 WSVA-NUMCTE                PIC X(08);  VALUE SPACES.
         05 VA-FETCH-LLAVE             PIC X(20);  VALUE SPACES.
      * DECLARE DECLARE @BAZ001-->INI
         05 VA-COD-MOV                 PIC X(03);  VALUE SPACES.
      * DECLARE DECLARE @BAZ001<--FIN
         05 VA-DESC-OPE                PIC X(50); .
         05 VA-NUM-CHEQUE              PIC X(09); .
         05 VA-NUM-CHEQUE-9            PIC 9(09); .
         05 VN-AMT-TOT                 PIC Z(12); 9.99.
         05 VA-DAT-VALUE               PIC X(10); .
         05 VA-FLG-FREE2               PIC X(01); .
      * DECLARE DECLARE @BAZ.I
         05 VA-DATOS-TC.
            10 VA-COD-TC               PIC X(04);  VALUE SPACES.
            10 VA-KEY-TC               PIC X(20);  VALUE SPACES.
         05 VA-BENEFIC                 PIC X(50);  VALUE SPACES.
         05 AUX-VA-COD-MOV             PIC X(03);  VALUE SPACES.
         05 AUX-PRODSPRD               PIC X(06);  VALUE SPACES.
         05 AUX-NUMCUS8-TRSP.
            10 AUX-NUMCUS8-DEB         PIC X(08);  VALUE SPACES.
            10 AUX-NOMCUS8-DEB         PIC X(40);  VALUE SPACES.
            10 AUX-NUMCUS8-CRE         PIC X(08);  VALUE SPACES.
            10 AUX-NOMCUS8-CRE         PIC X(40);  VALUE SPACES.
      *  05 AUX-TIPCAM-INTERF          PIC X(7);  VALUE ZEROS.
      *  05 AUX-TIPCAM-INTERF-R        REDEFINES AUX-TIPCAM-INTERF
      *                                          PIC S9(3); V9999.
         05 AUX-TIPCAM-INTERF          PIC X(5);  VALUE ZEROS.
         05 AUX-TIPCAM-INTERF-R        REDEFINES AUX-TIPCAM-INTERF
                                                 PIC S9(3); V99.
         05 AUX-SEQ-T04                PIC S9(09);  COMP-3.
         05 AUX-FEDT004.
            10 AUX-FESEQ               PIC S9(10); .
            10 AUX-FETIP               PIC X(01); .
            10 AUX-FETRX               PIC X(04); .
         05 AUX-TERMINAL               PIC X(04);  VALUE SPACES.
      * DECLARE DECLARE @BAZ023.I
         05 AUX-NETNAMEUPD             PIC X(08);  VALUE SPACES.
      * DECLARE DECLARE @BAZ023.F
         05 AUX-BIN                    PIC X(06);  VALUE SPACES.
         05 AUX-DESBANCO               PIC X(20);  VALUE SPACES.
         05 AUX-TIME043                PIC 9(06);  VALUE ZEROES.
         05 AUX-TIMESTP043             PIC X(16);  VALUE SPACES.
         05 AUX-CAJERO                 PIC X(30);  VALUE SPACES.
         05 AUX-X                      PIC 9(03);  VALUE ZEROS.
         05 AUX-Y                      PIC 9(03);  VALUE ZEROS.
         05 AUX-PASO                   PIC 9(01);  VALUE ZERO.
         05 AUX-TIP-CAMVTA             PIC 9(4); V9(6); .
         05 AUX-TIP-CAMCPRA            PIC 9(4); V9(6); .
         05 AUX-UNIDADES               PIC 9(12); V9(4); .
         05 AUX-MONTO-TIP              PIC Z(12); 9.99 VALUE ZEROES.
      *  05 AUX-MONTO-UNI              PIC Z(12); 9.9999 VALUE ZEROES.
         05 AUX-MONTO-UNI              PIC Z(12); 9.99   VALUE ZEROES.
         05 AUX-VALOR                  PIC X(16);  VALUE SPACES.
         05 AUX-UNIDA                  PIC X(20);  VALUE SPACES.
         05 AUX-CANAL071               PIC X(03);  VALUE SPACES.
         05 AUX-INTREF71               PIC X(15);  VALUE SPACES.
      * DECLARE DECLARE @BAZ016-->INI
         05 AUX-USERUPD                PIC X(08);  VALUES SPACES.
      * DECLARE DECLARE @BAZ016<--FIN
         05 AUX-CTECEL                 PIC X(08);  VALUE SPACES.
      * DECLARE DECLARE @BAZ011.I
      *  05 AUX-CEL                    PIC X(10);  VALUE SPACES.
         05 AUX-CEL                    PIC X(15);  VALUE SPACES.
      * DECLARE DECLARE @BAZ011.F
      * DECLARE DECLARE @BAZ015-->INI
         05 AUX-DESC                   PIC X(31);  VALUE SPACES.
      * DECLARE DECLARE @BAZ015<--FIN
      * DECLARE DECLARE @BAZ021-->INI
         05 AUX-DESC-W                 PIC X(31);  VALUE SPACES.
      * DECLARE DECLARE @BAZ021<--FIN
         05 AUX-CENTRO                 PIC X(04);  VALUE SPACES.
         05 AUX1-CTABEN                PIC X(20);  VALUE SPACES.
         05 AUX2-NUMCTA.
            10 AUX2-NUMCEN             PIC X(04);  VALUE SPACES.
            10 AUX2-NUMACC.
               15 AUX2-NUMPRD          PIC X(02);  VALUE SPACES.
               15 AUX2-NUMCAC          PIC X(08);  VALUE SPACES.
         05 AUX-NUMCUS8                PIC X(08);  VALUE SPACES.
         05 AUX-SURNAME                PIC X(20);  VALUE SPACES.
         05 AUX-SCDNAME                PIC X(20);  VALUE SPACES.
         05 AUX-NAME                   PIC X(20);  VALUE SPACES.
         05 AUX-NOMBRECTE              PIC X(40);  VALUE SPACES.
      * DECLARE DECLARE @BAZ023.I
         05 AUX-NOM-CTE                PIC X(60);  VALUE SPACES.
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
         05 AUX2-NOMBRECTE             PIC X(40);  VALUE SPACES.
      * DECLARE DECLARE @BAZ027-F
         05 AUX-CTA-INPUT.
            10 AUX-CTA-ENT             PIC X(04);  VALUE SPACES.
            10 AUX-CTA-CEN             PIC X(04);  VALUE SPACES.
            10 AUX-CTA-DIG1            PIC X(01);  VALUE SPACES.
            10 AUX-CTA-DIG2            PIC X(01);  VALUE SPACES.
            10 AUX-CTA-NUM             PIC X(10);  VALUE SPACES.
         05 AUX-AMT-COMP3              PIC S9(13); V99 USAGE COMP-3.
         05 AUX-AMT-COMP043            PIC 9(11); V999 USAGE COMP-3.
         05 AUX-DIAS                   PIC ZZ9 VALUE ZEROES.
         05 VA-REF                     PIC X(9);  VALUE ZEROS.
         05 VA-REF-R                   REDEFINES VA-REF
                                                 PIC S9(9);  COMP-3.
         05 AUX-DES169-TRASCEL.
            10 AUX-DES169-TRASCEL1     PIC X(04);  VALUE SPACES.
            10 AUX-DES169-TRASCEL2     PIC X(30);  VALUE SPACES.
            10 AUX-DES169-TRASCEL3     PIC X(01);  VALUE SPACES.
      * DECLARE DECLARE @BAZ011.I
      *     10 AUX-DES169-TRASCEL4     PIC X(11);  VALUE SPACES.
            10 AUX-DES169-TRASCEL4     PIC X(15);  VALUE SPACES.
      * DECLARE DECLARE @BAZ011.F
         05 AUX-DEST05-TRASCEL.
            10 AUX-DEST05-TRASCEL1     PIC X(12);  VALUE SPACES.
      * DECLARE DECLARE @BAZ011.I
      *     10 AUX-DEST05-TRASCEL2     PIC X(10);  VALUE SPACES.
            10 AUX-DEST05-TRASCEL2     PIC X(15);  VALUE SPACES.
      * DECLARE DECLARE @BAZ011.F
            10 AUX-DEST05-TRASCEL3     PIC X(12);  VALUE SPACES.
         05 AUX-DES-TRANS.
            10 AUX-DES-TRANS1          PIC X(20);  VALUE SPACES.
            10 AUX-DES-TRANS2          PIC X(30);  VALUE SPACES.
         05 AUX-DES-TRANF.
            10 AUX-DES-TRANF1          PIC X(20);  VALUE SPACES.
            10 AUX-DES-TRANF2          PIC X(23);  VALUE SPACES.
            10 AUX-DES-TRANF3          PIC X(07);  VALUE SPACES.
         05 AUX-DESINT-APERT.
            10 AUX-DESINT-APERT1       PIC X(36);  VALUE SPACES.
            10 AUX-DESINT-APERT2       PIC X(14);  VALUE SPACES.
         05 AUX-DESPAG-SERV.
            10 AUX-DESPAG-SERV1        PIC X(30);  VALUE SPACES.
            10 AUX-DESPAG-SERV2        PIC X(30);  VALUE SPACES.
         05 AUX-DESINT-RENEJE.
            10 AUX-DESINT-RENEJE1      PIC X(28);  VALUE SPACES.
            10 AUX-DESINT-RENEJE2      PIC X(14);  VALUE SPACES.
            10 AUX-DESINT-RENEJE3      PIC X(03);  VALUE SPACES.
            10 AUX-DESINT-RENEJE4      PIC X(04);  VALUE SPACES.
         05 AUX-DESINT-RENPZO.
            10 AUX-DESINT-RENPZO1      PIC X(12);  VALUE SPACES.
            10 AUX-DESINT-RENPZO2      PIC X(03);  VALUE SPACES.
            10 AUX-DESINT-RENPZO3      PIC X(04);  VALUE SPACES.
         05 AUX-DESTIP-CAMBIO.
            10 AUX-DESTIP-CAMBIO0      PIC X(03);  VALUE SPACES.
            10 AUX-DESTIP-CAMBIO1      PIC X(15);  VALUE SPACES.
            10 AUX-DESTIP-CAMBIOC      PIC X(01);  VALUE SPACES.
            10 AUX-DESTIP-CAMBIO2      PIC X(15);  VALUE SPACES.
      *     10 AUX-DESTIP-CAMBIO3      PIC X(10);  VALUE SPACES.
            10 AUX-DESTIP-CAMBIO3      PIC X(03);  VALUE SPACES.
         05 AUX-DESTIP2-CAMBIO.
            10 AUX-DESTIP2-CAMBIO1     PIC X(14);  VALUE SPACES.
            10 AUX-DESTIP2-CAMBIO2     PIC X(15);  VALUE SPACES.
         05 AUX-DES160-TRASTER.
            10 AUX-DES160-TRASTER1     PIC X(02);  VALUE SPACES.
            10 AUX-DES160-TRASTER2     PIC X(40);  VALUE SPACES.
            10 AUX-DES160-TRASTER3     PIC X(08);  VALUE SPACES.
         05 AUX-DES169-TRASTER.
            10 AUX-DES169-TRASTER1     PIC X(04);  VALUE SPACES.
            10 AUX-DES169-TRASTER2     PIC X(40);  VALUE SPACES.
            10 AUX-DES169-TRASTER3     PIC X(08);  VALUE SPACES.
         05 AUX-DESTDC-PAGO.
            10 AUX-DESTDC-PAGO1        PIC X(30);  VALUE SPACES.
            10 AUX-DESTDC-PAGO2        PIC X(08);  VALUE SPACES.
         05 AUX-DESCOD-R80.
            10 AUX-DESCOD-R801         PIC X(30);  VALUE SPACES.
            10 AUX-DESCOD-R802         PIC X(08);  VALUE SPACES.
         05 AUX-DESDEP-CHEQ.
            10 AUX-DESDEP-CHEQ1        PIC X(30);  VALUE SPACES.
            10 AUX-DESDEP-CHEQ2        PIC X(10);  VALUE SPACES.
         05 AUX-DEVSPEI.
            10 AUX-CLAV-DEVSPEI        PIC X(02);  VALUE SPACES.
            10 AUX-MOTIV-DEVSPEI       PIC X(40);  VALUE SPACES.
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ022-INI
         05 AUX-REFERENCIA-INTERB.
            10 AUX-NUM-REFERENCIA      PIC X(36); .
      * DECLARE DECLARE @BAZ022-FIN
      * DECLARE DECLARE @BAZ074-I
         05 VA-USER-089                PIC X(08);  VALUE SPACES.
      * DECLARE DECLARE @BAZ074-F
      * DECLARE DECLARE @BAZ005B.I
         05 WSE-ENTRADA.
            10 WSE-CTA-20.
               15 WSE-ENTACC           PIC X(04);  VALUE SPACES.
               15 WSE-CUENTA-18.
                  20 WSE-CENACC        PIC X(04);  VALUE SPACES.
                  20 WSE-DIG1          PIC X(01);  VALUE SPACES.
                  20 WSE-DIG2          PIC X(01);  VALUE SPACES.
                  20 WSE-CTA.
                     25 WSE-PROACC     PIC X(02);  VALUE SPACES.
                     25 WSE-CTAACC     PIC X(08);  VALUE SPACES.
            10 WSE-NUMCUS              PIC X(08);  VALUE SPACES.
            10 WSE-TARJETA.
               15 WSE-BIN              PIC X(06);  VALUE SPACES.
               15 WSE-CRD              PIC X(13);  VALUE SPACES.
      * DECLARE DECLARE @BAZ005B.F
         05 PR-PARAMETROS.
            10 PR-FECH-OPER            PIC X(10); .
            10 PR-MONTO                PIC S9(13); V99 COMP-3.
            10 PR-REFERENCIA           PIC X(30); .
            10 PR-CUENTA               PIC X(14); .
            10 PR-NUM-OPER             PIC S9(09);     COMP-3.
            10 PR-ERROR                PIC X(02); .
            10 PR-AVISO                PIC X(30); .
            10 PR-NOM-EMI              PIC X(30); .
      * DECLARE DECLARE @BAZ.F
      * DECLARE DECLARE @BAZ038 - INI
          05 VN-STRING                  PIC X(50); .
          05 VN-DES-TABLE               PIC X(250); .
          05 VN-NUMOPE                  PIC Z(09); .
          05 VN-NUM-OPERATION           PIC S999999999V USAGE COMP-3.
          05 VN-TARJETA-ACTUAL.
              10 VN-TJ-ACT-BIN          PIC X(06); .
              10 VN-TJ-ACT-CRD          PIC X(10); .
      *
          05 VN-TARJETA-ANTERIOR.
              10 VN-TJ-ANT-BIN          PIC X(06); .
              10 VN-TJ-ANT-CRD          PIC X(10); .
      * DECLARE DECLARE @BAZ038 - FIN
      * DECLARE DECLARE @BAZ039 - INI
          05 VA-DAT-ACCT                PIC X(10);  VALUE SPACES.
          05 AUX-NUM-BIN-CRD            PIC X(06);  VALUE SPACES.
          05 AUX-NUM-CARD               PIC X(10);  VALUE SPACES.
          05 AUX-TYP-CRD                PIC X(02);  VALUE SPACES.
      * DECLARE DECLARE @BAZ039 - FIN
      * DECLARE DECLARE @BAZ042 - I
          05 VA-SPACE                   PIC 9(03);  VALUE ZEROS.
          05 VA-STRING                  PIC 9(03);  VALUE ZEROS.
          05 WSV-CONT2                  PIC 9(03);  VALUE ZEROS.
      * DECLARE DECLARE @BAZ042 - F
      * DECLARE DECLARE @BAZ071-INI
         05 WSV-AUX-FOLOPE             PIC X(15);  VALUE SPACES.
         05 WSV-AUX-COMPTA.
           10 WSV-AUX-TA               PIC X(02);  VALUE SPACES.
           10 FILLER                   PIC X(01);  VALUE SPACES.
           10 WSV-AUX-CTACAR           PIC X(10);  VALUE SPACES.
           10 FILLER                   PIC X(01);  VALUE SPACES.
           10 WSV-AUX-TIPCOM           PIC X(15);  VALUE SPACES.
           10 FILLER                   PIC X(02);  VALUE SPACES.
      * DECLARE DECLARE @BAZ071-FIN
      *
       01 WS-VAR-WRITE.
         05  VA-DES-QUEUE.
           10 VA-NUM-OPERATION         PIC X(04); .
           10 VA-NUM-TERMINAL          PIC X(04); .
         05  VA-TS2.
           10 FILLER                   PIC X(04);    VALUE '+DC1'.
           10 VA-TSSFF2                PIC X(04);    VALUE SPACES.
         05  VA-TSCNT2.
           10 VA-DES-FORMAT2           PIC X(08);    VALUE SPACES.
           10 VA-FMTCNT2               PIC X(500);   VALUE SPACES.
         05  VN-TSLTH2                 PIC S9(4);    COMP VALUE +0.

      * DECLARE DECLARE @BAZ037-I
       01 VA-STORED.
          05 VA-ENT-IN                   PIC X(04); .
          05 VA-BDMID-IN                 PIC X(40); .
          05 VA-BRN-OPEN-IN              PIC X(40); .
          05 VA-COD-PROD-IN              PIC X(02); .
          05 VA-NUM-ACC-IN               PIC X(08); .
          05 VA-FECHA-IN                 PIC X(10); .
          05 VA-ULT-LLAVE-IN             PIC X(20); .
      * DECLARE DECLARE @BAZ070-I
          05 VA-BAN71                    PIC X(03); .
          05 VA-MOV-71                   PIC X(8000); .
          05 VA-MOV-710                  PIC X(8000); .
          05 VA-DATOS-403                PIC X(8000); .
          05 VA-DATOS-CTA                PIC X(8000); .
          05 VA-FECHA-ACCT               PIC X(10); .
      * DECLARE DECLARE @BAZ070-F
          05 VA-NUM-REG                  PIC 9(02);  VALUE ZEROS.
          05 I-REG                       PIC 9(02);  VALUE ZEROS.
          05 I-REG2                      PIC 9(02);  VALUE ZEROS.
          05 RESULTADO                   PIC S9(8);  COMP.
          05 VA-T041-WDRWBAL-ENT         PIC X(15); .
          05 VA-T041-WDRWBAL-DEC         PIC X(10); .
          05 VA-LONG-E                   PIC 9(2);   VALUE ZEROS.
          05 VA-LONG-D                   PIC 9(2);   VALUE ZEROS.
          05 VA-LONG-T                   PIC 9(2);   VALUE ZEROS.
          05 VA-LONG-DEC                 PIC 9(2);   VALUE ZEROS.
          05 VA-POS                      PIC 9(2);   VALUE ZEROS.
      * DECLARE DECLARE @BAZ070-I
          05 VA-CONTADOR                 PIC 9(02);  VALUE ZEROS.
      * DECLARE DECLARE @BAZ070-F
          05 VA-EMPIEZA                  PIC 9(2); .
          05 VA-ENTRADA-JUSTIF-DER       PIC X(17); .
          05 VA-SALIDA-JUSTIF-DER        PIC X(17); .
          05 VA-CUANTOS-MENOS            PIC 99.
      * DECLARE DECLARE @BAZ070-I
          05 WS-LLAVE                    PIC X(20); .
      *
       01  VA-TIEMPO-AUX.
           05 VA-H-INI-AUX.
              10 VA-INI-AUX-HM             PIC X(04);   VALUE SPACES.
              10 VA-INI-AUX-SM             PIC X(04);   VALUE SPACES.
           05 VA-H-FIN-AUX.
              10 VA-FIN-AUX-HM             PIC X(04);   VALUE SPACES.
              10 VA-FIN-AUX-SM             PIC X(04);   VALUE SPACES.
      *
       01 VA-REG.
          05 VA-REG01                    PIC X(4000); .
          05 VA-REG02                    PIC X(4000); .
          05 VA-REG03                    PIC X(4000); .
          05 VA-REG04                    PIC X(4000); .
          05 VA-REG05                    PIC X(4000); .
          05 VA-REG06                    PIC X(4000); .
          05 VA-REG07                    PIC X(4000); .
          05 VA-REG08                    PIC X(4000); .
          05 VA-REG09                    PIC X(4000); .
          05 VA-REG10                    PIC X(4000); .
          05 VA-REG11                    PIC X(4000); .
          05 VA-REG12                    PIC X(4000); .
          05 VA-REG13                    PIC X(4000); .
          05 VA-REG14                    PIC X(4000); .
          05 VA-REG15                    PIC X(4000); .
      * DECLARE DECLARE @BAZ070-F
       01 CT-STORED.
          05 CT-BGE0236                  PIC X(07);  VALUE 'BGE0236'.
          05 CT-15-9                     PIC 9(2);  VALUE 15.

       01 TB-STORED.
          05 TB-SAL-SP                   OCCURS 15 TIMES.
             10 TB-T403-NUM-BIN          PIC X(06);  VALUE SPACES.
             10 TB-T403-NUM-CRD          PIC X(10);  VALUE SPACES.
             10 TB-T403-NUM-CLTE         PIC X(08);  VALUE SPACES.
             10 TB-T403-NUM-CTA          PIC X(20);  VALUE SPACES.
             10 TB-T403-TEL-CEL          PIC X(15);  VALUE SPACES.
             10 TB-NUM-CUS               PIC X(08);  VALUE SPACES.
             10 TB-T041-CAC-DIG1         PIC X(01);  VALUE SPACES.
             10 TB-T041-CAC-DIG2         PIC X(01);  VALUE SPACES.
             10 TB-T041-COD-PRODUCT      PIC X(02);  VALUE SPACES.
             10 TB-T041-COD-SPROD        PIC X(04);  VALUE SPACES.
             10 TB-T041-CEN-ACCT         PIC X(04);  VALUE SPACES.
             10 TB-T041-FCC              PIC X(04);  VALUE SPACES.
             10 TB-T140-DES-TABLE        PIC X(250);  VALUE SPACES.
             10 TB-VA-FETCH-LLAVE        PIC X(20);  VALUE SPACES.

             10 TB-T071-NUM-OPERATION2   PIC X(09); .
             10 TB-T071-NUM-OPERATION    REDEFINES
                TB-T071-NUM-OPERATION2   PIC 9(09); .

             10 TB-T071-DAT-OPERATION    PIC X(10);  VALUE SPACES.
             10 TB-T071-DAT-VALUE        PIC X(10);  VALUE SPACES.
      * DECLARE DECLARE @BAZ063-INI
      *      10 TB-T071-TIM-OPERATION    PIC X(04);  VALUE SPACES.
             10 TB-T071-TIM-OPERATION    PIC X(06);  VALUE SPACES.
      * DECLARE DECLARE @BAZ063-FIN
             10 TB-T071-AMOUNT2          PIC X(15);  VALUE SPACES.
             10 TB-T071-AMOUNT           REDEFINES
                TB-T071-AMOUNT2          PIC S9(13); V99.


             10 TB-T071-CODE             PIC X(03);  VALUE SPACES.
             10 TB-T071-OBSERVATIONS     PIC X(31);  VALUE SPACES.
             10 TB-T071-COD-PRODUCT      PIC X(02);  VALUE SPACES.
             10 TB-T071-COD-SPROD        PIC X(04);  VALUE SPACES.
             10 TB-T071-FLG-FREE1        PIC X(03);  VALUE SPACES.
             10 TB-T071-USERUPD          PIC X(08);  VALUE SPACES.
             10 TB-T071-NTNMUPD          PIC X(08);  VALUE SPACES.
             10 TB-T100-BIGALP           PIC X(34);  VALUE SPACES.
             10 TB-T606-ACC              PIC X(16);  VALUE SPACES.

             10 TB-T606-AMOUNT2          PIC X(17);  VALUE SPACES.
             10 TB-T606-AMOUNT           REDEFINES
                TB-T606-AMOUNT2          PIC S9(15); V99.

             10 TB-T606-NUM-OPERATION2   PIC X(09);  VALUE SPACES.
             10 TB-T606-NUM-OPERATION    REDEFINES
                TB-T606-NUM-OPERATION2   PIC 9(09); .

             10 TB-T606-DESCRIPTION      PIC X(150);  VALUE SPACES.
             10 TB-T606-PATH             PIC X(250);  VALUE SPACES.

             10 TB-T606-GPS-LAT2         PIC X(17);  VALUE SPACES.
             10 TB-T606-GPS-LAT          REDEFINES
                TB-T606-GPS-LAT2         PIC S9(11); V9(6); .

             10 TB-T606-GPS-LONG2        PIC X(17);  VALUE SPACES.
             10 TB-T606-GPS-LONG         REDEFINES
                TB-T606-GPS-LONG2        PIC S9(11); V9(6); .


             10 TB-T606-DAT-OPERATION    PIC X(10);  VALUE SPACES.
             10 TB-T606-FLG-FREE1        PIC X(01);  VALUE SPACES.
             10 TB-T606-CHAR-FREE1       PIC X(30);  VALUE SPACES.
             10 TB-T071-INTREF           PIC X(15);  VALUE SPACES.
      *BAZ053-INI
             10 TB-T043-NUM-OPE-2        PIC X(21);  VALUE SPACES.
             10 TB-T803-ENT-ACC          PIC X(4);  VALUES SPACES.
      *BAZ053-FIN
      * DECLARE DECLARE @BAZ070-I
             10 TB-T071-DAT-ACCT         PIC X(10);  VALUE SPACES.
      * DECLARE DECLARE @BAZ070-F

          05 TB-SAL-SP-RET               OCCURS 15 TIMES.
             10 TB-RET-T403-NUM-BIN          PIC X(06);  VALUE SPACES.
             10 TB-RET-T403-NUM-CRD          PIC X(10);  VALUE SPACES.
             10 TB-RET-T403-NUM-CLTE         PIC X(08);  VALUE SPACES.
             10 TB-RET-T403-NUM-CTA          PIC X(20);  VALUE SPACES.
             10 TB-RET-T403-TEL-CEL          PIC X(15);  VALUE SPACES.
             10 TB-RET-NUM-CUS               PIC X(08);  VALUE SPACES.
             10 TB-RET-T041-CAC-DIG1         PIC X(01);  VALUE SPACES.
             10 TB-RET-T041-CAC-DIG2         PIC X(01);  VALUE SPACES.
             10 TB-RET-T041-COD-PRODUCT      PIC X(02);  VALUE SPACES.
             10 TB-RET-T041-COD-SPROD        PIC X(04);  VALUE SPACES.
             10 TB-RET-T041-CEN-ACCT         PIC X(04);  VALUE SPACES.
             10 TB-RET-T041-FCC              PIC X(04);  VALUE SPACES.
             10 TB-RET-T140-DES-TABLE        PIC X(250);  VALUE SPACES.
             10 TB-RET-VA-FETCH-LLAVE        PIC X(20);  VALUE SPACES.

             10 TB-T089-NUM-WHD2             PIC X(05); .
             10 TB-T089-NUM-WHD              REDEFINES
                TB-T089-NUM-WHD2             PIC 9(05); .

             10 TB-T089-DAT-REG              PIC X(10);  VALUE SPACES.
             10 TB-T089-TIM-REG              PIC X(08);  VALUE SPACES.

             10 TB-T089-AMT-ORIGIN2          PIC X(15);  VALUE SPACES.
             10 TB-T089-AMT-ORIGIN           REDEFINES
                TB-T089-AMT-ORIGIN2          PIC S9(13); V99.

             10 TB-T089-AMT-CURRENT2         PIC X(15);  VALUE SPACES.
             10 TB-T089-AMT-CURRENT          REDEFINES
                TB-T089-AMT-CURRENT2         PIC S9(13); V99.

             10 TB-T089-CODE                 PIC X(03);  VALUE SPACES.
             10 TB-T089-OBSERVATIONS         PIC X(40);  VALUE SPACES.

             10 TB-RET-T100-BIGALP           PIC X(34);  VALUE SPACES.
             10 TB-RET-T606-ACC              PIC X(16);  VALUE SPACES.

             10 TB-RET-T606-AMOUNT2          PIC X(17);  VALUE SPACES.
             10 TB-RET-T606-AMOUNT           REDEFINES
                TB-RET-T606-AMOUNT2          PIC S9(15); V99.

             10 TB-RET-T606-NUM-OPERATION2   PIC X(09);  VALUE SPACES.
             10 TB-RET-T606-NUM-OPERATION    REDEFINES
                TB-RET-T606-NUM-OPERATION2   PIC 9(09); .

             10 TB-RET-T606-DESCRIPTION      PIC X(150);  VALUE SPACES.
             10 TB-RET-T606-PATH             PIC X(250);  VALUE SPACES.

             10 TB-RET-T606-GPS-LAT2         PIC X(17);  VALUE SPACES.
             10 TB-RET-T606-GPS-LAT          REDEFINES
                TB-RET-T606-GPS-LAT2         PIC S9(11); V9(6); .

             10 TB-RET-T606-GPS-LONG2        PIC X(17);  VALUE SPACES.
             10 TB-RET-T606-GPS-LONG         REDEFINES
                TB-RET-T606-GPS-LONG2        PIC S9(11); V9(6); .


             10 TB-RET-T606-DAT-OPERATION    PIC X(10);  VALUE SPACES.
             10 TB-RET-T606-FLG-FREE1        PIC X(01);  VALUE SPACES.
             10 TB-RET-T606-CHAR-FREE1       PIC X(30);  VALUE SPACES.
      *
          05 VA-SAL                      PIC X(4000);  OCCURS 15 TIMES.
          05 VA-SAL-1                    PIC X(4000); .
          05 VA-SAL-2                    PIC X(4000); .
          05 VA-SAL-3                    PIC X(4000); .
          05 VA-SAL-4                    PIC X(4000); .
          05 VA-SAL-5                    PIC X(4000); .
          05 VA-SAL-6                    PIC X(4000); .
          05 VA-SAL-7                    PIC X(4000); .
          05 VA-SAL-8                    PIC X(4000); .
          05 VA-SAL-9                    PIC X(4000); .
          05 VA-SAL-10                   PIC X(4000); .
          05 VA-SAL-11                   PIC X(4000); .
          05 VA-SAL-12                   PIC X(4000); .
          05 VA-SAL-13                   PIC X(4000); .
          05 VA-SAL-14                   PIC X(4000); .
          05 VA-SAL-15                   PIC X(4000); .
      * DECLARE DECLARE @BAZ070-I

          01 VA-BUFFER-JSON                   PIC X(8000); .

          01 VA-BUFFER-JSON-SEP.
             05 VA-A.
                10 FILLER                   PIC X(04);  VALUE 'A":"'.
                10 VA-T071-DAT-OPERATION    PIC X(10); .
             05 VA-B.
                10 FILLER                   PIC X(04);  VALUE 'B":"'.
                10 VA-T071-NUM-OPERATION    PIC X(09); .
             05 VA-C.
                10 FILLER                   PIC X(04);  VALUE 'C":"'.
                10 VA-T071-DAT-VALUE        PIC X(10); .
             05 VA-D.
                10 FILLER                   PIC X(04);  VALUE 'D":"'.
                10 VA-T071-HORA             PIC X(06); .
             05 VA-E.
                10 FILLER                   PIC X(04);  VALUE 'E":"'.
                10 VA-T071-AMOUNT           PIC X(17); .
             05 VA-F.
                10 FILLER                   PIC X(04);  VALUE 'F":"'.
                10 VA-T071-CODE             PIC X(03); .
             05 VA-G.
                10 FILLER                   PIC X(04);  VALUE 'G":"'.
                10 VA-T071-OBSERVATIONS     PIC X(31); .
             05 VA-H.
                10 FILLER                   PIC X(04);  VALUE 'H":"'.
                10 VA-T071-COD-PRODUCT      PIC X(02); .
             05 VA-I.
                10 FILLER                   PIC X(04);  VALUE 'I":"'.
                10 VA-T071-COD-SPROD        PIC X(04); .
             05 VA-J.
                10 FILLER                   PIC X(04);  VALUE 'J":"'.
                10 VA-T071-FLG-FREE1        PIC X(03); .
             05 VA-K.
                10 FILLER                   PIC X(04);  VALUE 'K":"'.
                10 VA-T071-USERUPD          PIC X(08); .
             05 VA-L.
                10 FILLER                   PIC X(04);  VALUE 'L":"'.
                10 VA-T071-NTNMUPD          PIC X(08); .
             05 VA-M.
                10 FILLER                   PIC X(04);  VALUE 'M":"'.
                10 VA-T100-BIGALP           PIC X(34); .
             05 VA-N.
                10 FILLER                   PIC X(04);  VALUE 'N":"'.
                10 VA-T606-NUM-OPERATION    PIC X(09); .
             05 VA-O.
                10 FILLER                   PIC X(04);  VALUE 'O":"'.
                10 VA-T606-DESCRIPTION      PIC X(50); .
             05 VA-P.
                10 FILLER                   PIC X(04);  VALUE 'P":"'.
                10 VA-T606-PATH             PIC X(250); .
             05 VA-Q.
                10 FILLER                   PIC X(04);  VALUE 'Q":"'.
                10 VA-T606-FLG-FREE1        PIC X(01); .
             05 VA-R.
                10 FILLER                   PIC X(04);  VALUE 'R":"'.
                10 VA-T606-CHAR-FREE1       PIC X(30); .
             05 VA-S.
                10 FILLER                   PIC X(04);  VALUE 'S":"'.
                10 VA-T071-INTREF           PIC X(15); .
             05 VA-T.
                10 FILLER                   PIC X(04);  VALUE 'T":"'.
                10 VA-T071-DAT-ACCT         PIC X(10); .


          01 VA-DATOS-403-SEP.
             05 VA-T403-NUM-BIN-S.
                10 FILLER                   PIC X(17);  VALUE
                                                    '{"T403_NUM_BIN":"'.
                10 VA-T403-NUM-BIN          PIC X(06); .
      *
             05 VA-T403-NUM-CRD-S.
                10 FILLER                   PIC X(15);  VALUE
                                                      'T403_NUM_CRD":"'.
                10 VA-T403-NUM-CRD          PIC X(10); .
      *

             05 VA-T403-NUM-CLTE-S.
                10 FILLER                   PIC X(16);  VALUE
                                                     'T403_NUM_CLTE":"'.
                10 VA-T403-NUM-CLTE         PIC X(08); .
      *
             05 VA-T403-NUM-CTA-S.
                10 FILLER                   PIC X(16);  VALUE
                                                      'T403_NUM_CTA":"'.
                10 VA-T403-NUM-CTA          PIC X(20); .
      *
             05 VA-T403-TEL-CEL-S.
                10 FILLER                   PIC X(15);  VALUE
                                                      'T403_TEL_CEL":"'.
                10 VA-T403-TEL-CEL          PIC X(15); .
      *

          01 VA-DATOS-CTA-S.
             05 VA-NUM-CUS-S.
                10 FILLER                   PIC X(12);  VALUE
                                                      '{"NUM_CUS":"'.
                10 VA-NUM-CUS               PIC X(08); .
      *
             05 VA-T041-CAC-DIG1-S.
                10 FILLER                   PIC X(16);  VALUE
                                                     'T041_CAC_DIG1":"'.
                10 VA-T041-CAC-DIG1          PIC X(01); .
      *

             05 VA-T041-CA-DIG2-S.
                10 FILLER                   PIC X(16);  VALUE
                                                     'T041_CAC_DIG2":"'.
                10 VA-T041-CAC-DIG2         PIC X(01); .
      *
             05 VA-T041-COD-PRODUCT-S.
                10 FILLER                   PIC X(19);  VALUE
                                                  'T041_COD_PRODUCT":"'.
                10 VA-T041-COD-PRODUCT      PIC X(02); .
      *
             05 VA-T041-COD-SPROD-S.
                10 FILLER                   PIC X(17);  VALUE
                                                    'T041_COD_SPROD":"'.
                10 VA-T041-COD-SPROD        PIC X(04); .
      *
             05 VA-T041-CEN-ACCT-S.
                10 FILLER                   PIC X(16);  VALUE
                                                'T041_CEN_ACCT":"'.
                10 VA-T041-CEN-ACCT         PIC X(04); .
      *
             05 VA-T041-FCC-S.
                10 FILLER                   PIC X(11);  VALUE
                                                'T041_FCC":"'.
                10 VA-T041-FCC              PIC X(04); .
      *
             05 VA-T140-DES-TABLE-S.
                10 FILLER                   PIC X(18);  VALUE
                                                'T140_DES_TABLE":"'.
                10 VA-T140-DES-TABLE        PIC X(249); .

       01 VN-NULL-1                     PIC S9(4);  COMP.
       01 VN-NULL-2                     PIC S9(4);  COMP.
       01 VN-NULL-3                     PIC S9(4);  COMP.
       01 VN-NULL-4                     PIC S9(4);  COMP.
      * DECLARE DECLARE @BAZ070-F
      * DECLARE DECLARE @BAZ065-I
       01 VA-TCDT010-6631.
          05 VA-DESCSUBSI                  PIC X(30);    VALUE SPACES.
      * DECLARE DECLARE @BAZ065-F
      * DECLARE DECLARE @BAZ037-F
      ******************************************************************
      *                LINKAGE SECTION
      ******************************************************************
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
          COPY QGECCAA.
          COPY MBNE0009.
      *
      ******************************************************************
      *                    PROCEDURE DIVISION                          *
      ******************************************************************
       PROCEDURE DIVISION.
      *
      *     CALL "CBL_DEBUGBREAK".
           PERFORM 10000-INICIO.
           PERFORM 20000-PROCESO.
           PERFORM 30000-FIN.
      *
      ******************************************************************
      *                        1000-INICIO                             *
      ******************************************************************
       10000-INICIO.
      *
           INITIALIZE QAWCSQL
                      MBNS0009
                      BGVC071
                      TCGV0403
                      DCLBADV0010
                      DCLBGDT140
                      DCLBGDT606
                      DCLBGGT089
                      DCLBGGT071
                      DCLBGGT100
                      DCLGADV0011
                      DCLMCDT010
                      DCLMCDT028
                      DCLMCDT043
                      DCLMCGT403
                      WS-VARIABLES
      *
           SET ADDRESS OF MBNE0009       TO CAA-PNT-INPCPY
      *
           MOVE CAA-COD-TRA              TO VA-NUM-OPERATION            **VARIABLES DEL COMMAREA
           MOVE CAA-TERMINAL             TO VA-NUM-TERMINAL
                                            VA-TSSFF2
           MOVE SPACES                   TO CAA-SW-NEXTTRNS
      *
           SET CAA-SW-88-TERMINALACTI    TO TRUE                        **ESTAS VARIABLES ESTAN EN EL COMMAREA
           SET CAA-SW-ACC-OPERANO        TO TRUE
           SET CAA-SW-OPE-TYPEINQUIRY    TO TRUE
      *
           INSPECT E009-CONTREG                                         *ESTA VARIABLE ESTA EN EL COPY MBNE0009
                           REPLACING ALL LOW-VALUES BY SPACE
           INSPECT E009-NUMTARJ
                           REPLACING ALL LOW-VALUES BY SPACE
           INSPECT E009-NUMCUEN
                           REPLACING ALL LOW-VALUES BY SPACE
           INSPECT E009-NUMECEL
                           REPLACING ALL LOW-VALUES BY SPACE
           INSPECT E009-IDCELUL
                           REPLACING ALL LOW-VALUES BY SPACE
           INSPECT E009-LATITUD
                           REPLACING ALL LOW-VALUES BY SPACE
           INSPECT E009-LONGITD
                           REPLACING ALL LOW-VALUES BY SPACE
           INSPECT E009-DATPAG
                           REPLACING ALL '.' BY SPACE.
      * 
           PERFORM ACCESO-TC9C9900                                      Manda llamar a CA-TC9C9900 que llama a la rutina TC9CR099 y hace un select a la tabla TCDT099
      * DECLARE DECLARE @BAZ038 - I                                                      
           PERFORM CONSULTA-PARAM-DIGITALES                             ACCESO A LA TABLA DE PARAMETRIA PARA OBTENER LOS BINES DE TARJETAS VIRTUALES
    
      * DECLARE DECLARE @BAZ038 - F
      * DECLARE DECLARE @BAZ056-I
           IF CAA-CEN-ACCOUNT = CA-1156 AND                             VARIABLES DEL COMMAREA
              CAA-CHANN = CA-54
              PERFORM 1100-VAL-USUADIO-SAPP                             REALIZA VALIDACION DE USUARIO SAPP CON SELECT A TABLA MBDT140|
           END-IF
      * DECLARE DECLARE @BAZ056-F
           .
      * DECLARE DECLARE @BAZ056-I
      *
      ******************************************************************
       1100-VAL-USUADIO-SAPP.
      ******************************************************************
      *
           MOVE CAA-USERID           TO T140-KEY-TABLE
           MOVE 'SAPP'               TO T140-COD-TABLE
           MOVE 'E'                  TO T140-LANGUAGE
           MOVE CAA-ENTIDAD          TO T140-ENTITY
      *
           EXEC SQL
             SELECT T140_DES_TABLE
             INTO  :T140-DES-TABLE
             FROM MBDT140 with (nolock); 
             WHERE  T140_KEY_TABLE  =:T140-KEY-TABLE AND
                    T140_COD_TABLE  =:T140-COD-TABLE AND
                    T140_LANGUAGE   =:T140-LANGUAGE AND
                    T140_ENTITY     =:T140-ENTITY
           END-EXEC
      *
           MOVE SQLCODE                     TO SQL-VALUES
      *
           EVALUATE TRUE
               WHEN SQL-88-OK
                    SET SW-OK-SAPP          TO TRUE
               WHEN OTHER
                    SET SW-NOK-SAPP         TO TRUE
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ056-F
      *
      ******************************************************************
      *ACCESO-TC9C9900.
      ******************************************************************
       ACCESO-TC9C9900.
      *
           MOVE '3'                      TO TCEC9900-OPTION
           MOVE CAA-ENT-ACC              TO TCEC9900-KEY
           MOVE CAA-SW-LNG-TERM          TO TCEC9900-COD-LNGKEY
      *
           CALL CA-TC9C9900 USING VA-TCEC9900
      *
           EVALUATE TCEC9900-COD-RETURN
           WHEN 00
                MOVE TCEC9900-DATA-PARAM TO QBEC999-DATA-PARAM
           WHEN OTHER
               INITIALIZE QGECABC
               MOVE 'S'                  TO ABC-ABEND
               MOVE CA-TC9C9900          TO ABC-DES-PROG
               MOVE 'ERROR TC9C9900: '   TO ABC-REFERENCE1(1:16); 
               MOVE TCEC9900-COD-RETURN  TO ABC-REFERENCE1(17:1); 
               MOVE TCEC9900-DES-TABLE   TO ABC-OBJECT-ERROR
               PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      *
      * DECLARE DECLARE @BAZ038 - I
      *-----------------------------------------------------------------
      *CONSULTA-PARAM-DIGITALES.
      *     ACCESO A LA TABLA DE PARAMETRIA PARA OBTENER LOS BINES DE
      *     TARJETAS VIRTUALES
      *-----------------------------------------------------------------
       CONSULTA-PARAM-DIGITALES.
      *
           INITIALIZE                         DCLBGDT140
      *
           MOVE CA-MBT1                       TO T140-COD-TABLE
           MOVE CA-E                          TO T140-LANGUAGE
           MOVE CAA-ENT-ACC                   TO T140-ENTITY
           MOVE CA-BINVIRTUAL                 TO T140-KEY-TABLE
      *
           EXEC SQL
              SELECT  T140_DES_TABLE
                INTO :T140-DES-TABLE
                FROM MBDT140 with(nolock); 
               WHERE T140_KEY_TABLE = :T140-KEY-TABLE
                 AND T140_COD_TABLE = :T140-COD-TABLE
                 AND T140_LANGUAGE  = :T140-LANGUAGE
                 AND T140_ENTITY    = :T140-ENTITY
           END-EXEC
      *
           MOVE SQLCODE                     TO SQL-VALUES
      *
           IF SQL-88-OK
      *
               SET SW-88-PARAM-V-OK         TO TRUE
               MOVE ZEROS                   TO VN-CON
               MOVE T140-DES-TABLE          TO VN-DES-TABLE
      *
               INSPECT VN-DES-TABLE TALLYING VN-FIN-CADENA
                   FOR CHARACTERS BEFORE '&'
      *
               INSPECT VN-DES-TABLE TALLYING VN-CON
                   FOR CHARACTERS BEFORE '|'
      *
               MOVE VN-DES-TABLE(01:VN-CON);  TO VN-MARCA-AUX
               ADD  2                       TO VN-CON
               MOVE VN-CON                  TO VN-VAL-INI
           END-IF
           .
      *
      * DECLARE DECLARE @BAZ038 - F
      * DECLARE DECLARE @BAZ070-I
      ******************************************************************
      *                 22000-LLAMADO-SP                               *
      ******************************************************************
       22000-LLAMADO-SP.
      *
           EXEC SQL
             SELECT  T140_DES_TABLE
              INTO   :SW-VALIDA-SP
              FROM MBDT140 with (nolock); 
               WHERE T140_KEY_TABLE  = 'MB09'
                 AND T140_COD_TABLE  = 'JSON'
                 AND T140_ENTITY     = :CAA-ENT-ACC
                 AND T140_LANGUAGE   = 'E'
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES

           IF SQL-88-OK
              CONTINUE
           ELSE
              SET SW-APAGA-JSON TO TRUE
           END-IF
       .
      * DECLARE DECLARE @BAZ070-F
      ******************************************************************
      *                    20000-PROCESO                               *
      ******************************************************************
       20000-PROCESO.
      *
           PERFORM 21000-VALIDA-ENTRADA                                 * Realiza Validaciones Generales y de geolocalizacion *
           PERFORM 22000-CALCULA-FECHA                                  * SE RESTAN TRES MESES A LA FECHA EN CURSO.           *
      * DECLARE DECLARE @BAZ037-I
      * DECLARE DECLARE @BAZ070-I
           PERFORM 22000-LLAMADO-SP                                     Solo realiza Select a tabla MBDT140 
      *
           IF WSS-CUENTA AND SW-APAGA-JSON                              La variable SW-APAGA-JSON se apaga si no retorna en el paso anterior
              PERFORM 23000-EXTRAE-DATOS-X-STORED                       Ejecuta el procedimiento MB09_MB2CF119_v2                    
           ELSE
              IF SW-PRENDE-JSON  AND (WSS-CUENTA OR WSS-CUENTA-710); 
                 PERFORM 23000-EXTRAE-DATOS-X-SP-JSON                   Ejecuta el procedimiento MB09_MB2CF119_v5                    
              END-IF
           END-IF
      * DECLARE DECLARE @BAZ070-F
      *
           IF WSS-RET-CTA
               PERFORM 24000-EXTRAE-DATOS-X-STORED-RET                  Ejecuta el procedimiento MB09_MB2CF219
           END-IF
      * DECLARE DECLARE @BAZ037-F
      * DECLARE DECLARE @BAZ0005.I
      * DECLARE DECLARE @BAZ021-INI
           EVALUATE CAA-PRKEY
              WHEN '01'
              WHEN '02'
      * DECLARE DECLARE @BAZ056-I
      *        PERFORM VALIDA-RELACION-BDMID
               IF SW-NOK-SAPP
                  PERFORM VALIDA-RELACION-BDMID
               END-IF
               SET SW-BAZ      TO TRUE
      * DECLARE DECLARE @BAZ056-F
              WHEN '03'
              WHEN '04'
               PERFORM VALIDA-BDMID-MBDT036
               SET SW-WALLET   TO TRUE
           END-EVALUATE
      * DECLARE DECLARE @BAZ021-FIN
      * DECLARE DECLARE @BAZ0005.F
      *
           EVALUATE CAA-PRKEY
              WHEN  '01'
      * DECLARE DECLARE @BAZ021-INI
              WHEN  '03'
      * DECLARE DECLARE @BAZ021-FIN
                  IF WSS-TARJETA
      * DECLARE DECLARE @BAZ027-I
      *              PERFORM 23000-CONSULTA-TARJETA
                     CONTINUE
      * DECLARE DECLARE @BAZ027-F
                  ELSE
      * DECLARE DECLARE @BAZ070-I
                     IF WSS-CUENTA  OR   WSS-CUENTA-710
      * DECLARE DECLARE @BAZ070-I
                        PERFORM 25000-CONSULTA-CUENTA                   REALIZA FETCH A LA TABLA BGDT071 PARA CONSULTAR LAS OPERACIONES DEL DIA EN CURSO; DE UNA CUENTA.

                     ELSE
                        MOVE 'MCE0128'          TO CAA-COD-ERROR
                        PERFORM 30000-FIN
                     END-IF
                  END-IF

              WHEN  '02'
      * DECLARE DECLARE @BAZ021-INI
              WHEN  '04'
      * DECLARE DECLARE @BAZ021-FIN
                    IF WSS-RET-TRJ
      * DECLARE DECLARE @BAZ027-I
      *                PERFORM 24000-RETENCION-TARJETA
                       CONTINUE
      * DECLARE DECLARE @BAZ027-F
                    ELSE
                        IF WSS-RET-CTA
                            PERFORM 26000-RETENCION-CUENTA              REALIZA FETCH A LA TABLA BGDT089 PARA CONSULTAR LAS RETENCIONES DE UNA CUENTA.  
     
      * DECLARE DECLARE @BAZ052-I
                            PERFORM  23300-SUMA-SOBRES                  REALIZA SELECT SUMA DE SALDO Y COUNT DE ID_CTA_META a tabla MBDT039
                            PERFORM  23400-SUMA-ALCANCIA                REALIZA SELECT SUMA DE SALDO Y COUNT DE ID_CTA_META a tabla MBDT039
                            PERFORM  66669-WRITE-SALIDA6
      * DECLARE DECLARE @BAZ052-F
                        ELSE
                            MOVE 'MCE0128'          TO CAA-COD-ERROR
                            PERFORM 30000-FIN
                        END-IF
                    END-IF
              WHEN OTHER
                 MOVE 'MCE0128'          TO CAA-COD-ERROR
                 PERFORM 30000-FIN
           END-EVALUATE
           PERFORM 23200-INSERT-PEDT100                                 Llama a rutina MB7C0110 que realiza insert a tabla PDT100 con los datos de todo el usuario y la geolocalizacion
           .
      *
      * DECLARE DECLARE @BAZ037-I
      * DECLARE DECLARE @BAZ070-I
      ******************************************************************
      *21100-PRENDE-TABLA
      ******************************************************************
       21100-PRENDE-TABLA.
      *
          EVALUATE TRUE
              WHEN WSS-CUENTA
                   MOVE CA-71            TO VA-BAN71
              WHEN WSS-CUENTA-710
                   MOVE CA-710           TO VA-BAN71
              WHEN OTHER
                   CONTINUE
          END-EVALUATE
        .
      ******************************************************************
      *23000-EXTRAE-DATOS-X-SP-JSON
      ******************************************************************
       23000-EXTRAE-DATOS-X-SP-JSON.
      *
           INITIALIZE TB-STORED
                      VA-STORED
           PERFORM 21100-PRENDE-TABLA
           MOVE CAA-ENT-ACC                TO VA-ENT-IN
           MOVE E009-BDMID                 TO VA-BDMID-IN
           MOVE E009-NUMCUEN(1:4);           TO VA-BRN-OPEN-IN
           MOVE E009-NUMCUEN(5:2);           TO VA-COD-PROD-IN
           MOVE E009-NUMCUEN(7:8);           TO VA-NUM-ACC-IN
           MOVE VA-FECHA-CALC              TO VA-FECHA-IN
           MOVE VA-ULT-LLAVE               TO VA-ULT-LLAVE-IN
      *
           MOVE VA-ULT-LLAVE(1:10);          TO VA-FECHA-ACCT
      *
           IF   VA-FECHA-ACCT    =CA-FECHA-MAX
                MOVE CAA-FECHA-CONT2       TO VA-FECHA-ACCT
           END-IF
      *
           ACCEPT VA-H-INI-AUX     FROM TIME
           exec sql
               :RESULTADO = CALL MAZP.MAZP.MB09_MB2CF119_v5(
                                           :VA-ENT-IN       IN;
                                           :VA-BDMID-IN     IN;
                                           :VA-BRN-OPEN-IN  IN;
                                           :VA-COD-PROD-IN  IN;
                                           :VA-NUM-ACC-IN   IN;
                                           :VA-FECHA-IN     IN;
                                           :VA-ULT-LLAVE-IN IN;
                                           :VA-BAN71        IN;
                                           :VA-FECHA-ACCT   IN;
                                         :VA-MOV-71   :VN-NULL-1  
                                         :VA-MOV-710  :VN-NULL-2  
                                         :VA-DATOS-403:VN-NULL-3  
                                         :VA-DATOS-CTA:VN-NULL-4 OUTPUT
                                      ); 
          end-exec.
          ACCEPT VA-H-FIN-AUX    FROM TIME
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
      *
                  PERFORM  23100-CARGA-REGISTROS
      *
             WHEN OTHER
                  MOVE CT-BGE0236          TO CAA-COD-ERROR
                  MOVE SQLCODE             TO CAA-ERR-VARIA1
                  MOVE VA-BDMID-IN         TO CAA-ERR-VARIA2
                  PERFORM 30000-FIN
           END-EVALUATE
      *
           MOVE VA-INI-AUX-SM     TO CAA-TB-AUTH(01:4); 
           MOVE VA-FIN-AUX-SM     TO CAA-TB-AUTH(05:4); 
           .
      *
      ******************************************************************
      *23100-CARGA-REGISTROS
      *****************************************************************
       23100-CARGA-REGISTROS.
      *
           MOVE 0                          TO VA-CONTADOR

           UNSTRING VA-DATOS-403 DELIMITED BY '";"'
           INTO VA-T403-NUM-BIN-S     ;
                VA-T403-NUM-CRD-S     ;
                VA-T403-NUM-CLTE-S    ;
                VA-T403-NUM-CTA-S     ;
                VA-T403-TEL-CEL-S
           PERFORM 23100-SEPARA-MCDT403
      *
           UNSTRING VA-DATOS-CTA DELIMITED BY '";"'
           INTO VA-NUM-CUS-S          ;
                VA-T041-CAC-DIG1-S    ;
                VA-T041-CA-DIG2-S     ;
                VA-T041-COD-PRODUCT-S ;
                VA-T041-COD-SPROD-S   ;
                VA-T041-CEN-ACCT-S    ;
                VA-T041-FCC-S         ;
                VA-T140-DES-TABLE-S
           PERFORM 23100-SEPARA-CTA
      *
           IF  VA-MOV-71  NOT = SPACE       AND
               VA-MOV-71  NOT = LOW-VALUE   AND
               VA-MOV-71  NOT = CA-BRACKETS
                INITIALIZE         VA-REG
                SET SW-BAN071      TO TRUE
                UNSTRING VA-MOV-71  DELIMITED BY '"};{"'
                INTO VA-REG01      ;
                     VA-REG02      ;
                     VA-REG03      ;
                     VA-REG04      ;
                     VA-REG05      ;
                     VA-REG06      ;
                     VA-REG07      ;
                     VA-REG08      ;
                     VA-REG09      ;
                     VA-REG10      ;
                     VA-REG11      ;
                     VA-REG12      ;
                     VA-REG13      ;
                     VA-REG14      ;
                     VA-REG15
                 PERFORM 23100-SEPARA-JSON
           END-IF
      *
           IF  VA-MOV-710  NOT = SPACE       AND
               VA-MOV-710  NOT = LOW-VALUE   AND
               VA-MOV-710  NOT = CA-BRACKETS
                INITIALIZE         VA-REG
                SET SW-BAN710      TO TRUE
                UNSTRING VA-MOV-710 DELIMITED BY '"};{"'
                INTO VA-REG01      ;
                     VA-REG02      ;
                     VA-REG03      ;
                     VA-REG04      ;
                     VA-REG05      ;
                     VA-REG06      ;
                     VA-REG07      ;
                     VA-REG08      ;
                     VA-REG09      ;
                     VA-REG10      ;
                     VA-REG11      ;
                     VA-REG12      ;
                     VA-REG13      ;
                     VA-REG14      ;
                     VA-REG15
                 PERFORM 23100-SEPARA-JSON
           END-IF
       .
      ******************************************************************
      *23100-SEPARA-MCDT403
      *****************************************************************
       23100-SEPARA-MCDT403.
      *

           MOVE VA-T403-NUM-BIN      TO TB-T403-NUM-BIN (01); 
           MOVE VA-T403-NUM-CRD      TO TB-T403-NUM-CRD (01); 
           MOVE VA-T403-NUM-CLTE     TO TB-T403-NUM-CLTE(01); 
           MOVE VA-T403-NUM-CTA      TO TB-T403-NUM-CTA (01); 
           MOVE VA-T403-TEL-CEL      TO TB-T403-TEL-CEL (01); 
       .

      ******************************************************************
      *23100-SEPARA-CTA
      *****************************************************************
       23100-SEPARA-CTA.
      *
           MOVE VA-NUM-CUS           TO TB-NUM-CUS          (01); 
           MOVE VA-T041-CAC-DIG1     TO TB-T041-CAC-DIG1    (01); 
           MOVE VA-T041-CAC-DIG2     TO TB-T041-CAC-DIG2    (01); 
           MOVE VA-T041-COD-PRODUCT  TO TB-T041-COD-PRODUCT (01); 
           MOVE VA-T041-COD-SPROD    TO TB-T041-COD-SPROD   (01); 
           MOVE VA-T041-CEN-ACCT     TO TB-T041-CEN-ACCT    (01); 
           MOVE VA-T041-FCC          TO TB-T041-FCC         (01); 
           MOVE VA-T140-DES-TABLE    TO TB-T140-DES-TABLE   (01); 
       .
      ******************************************************************
      *23100-SEPARA-JSON
      *****************************************************************
       23100-SEPARA-JSON.

      *
           IF VA-REG01   NOT = SPACES   AND
              VA-REG01   NOT =LOW-VALUES
              MOVE VA-REG01(4:3996);  TO     VA-BUFFER-JSON
              ADD  1                TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF

      *
           IF VA-REG02   NOT = SPACES   AND
              VA-REG02   NOT =LOW-VALUES
              MOVE VA-REG02  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF

      *
           IF VA-REG03   NOT  = SPACES   AND
              VA-REG03   NOT =LOW-VALUES
              MOVE VA-REG03  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG04   NOT = SPACES    AND
              VA-REG04   NOT =LOW-VALUES
              MOVE VA-REG04  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG05   NOT = SPACES     AND
              VA-REG05   NOT =LOW-VALUES
              MOVE VA-REG05  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG06   NOT = SPACES     AND
              VA-REG06   NOT =LOW-VALUES
              MOVE VA-REG06  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG07   NOT = SPACES     AND
              VA-REG07   NOT =LOW-VALUES
              MOVE VA-REG07  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG08   NOT = SPACES     AND
              VA-REG08   NOT =LOW-VALUES
              MOVE VA-REG08  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG09   NOT = SPACES     AND
              VA-REG09   NOT =LOW-VALUES
              MOVE VA-REG09  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG10   NOT = SPACES     AND
              VA-REG10   NOT =LOW-VALUES
              MOVE VA-REG10  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG11   NOT = SPACES     AND
              VA-REG11   NOT =LOW-VALUES
              MOVE VA-REG11  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG12   NOT = SPACES     AND
              VA-REG12   NOT =LOW-VALUES
              MOVE VA-REG12  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG13   NOT = SPACES     AND
              VA-REG13   NOT =LOW-VALUES
              MOVE VA-REG13  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG14   NOT = SPACES     AND
              VA-REG14   NOT =LOW-VALUES
              MOVE VA-REG14  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
      *
           IF VA-REG15   NOT= SPACES      AND
              VA-REG15   NOT =LOW-VALUES

              MOVE VA-REG15  TO     VA-BUFFER-JSON
              ADD  1         TO     VA-CONTADOR
              PERFORM 23110-SEPARA-BUFFER
           END-IF
       .
      ******************************************************************
      *23110-SEPARA-BUFFER
      *****************************************************************
       23110-SEPARA-BUFFER.
           UNSTRING VA-BUFFER-JSON DELIMITED BY '";"'
           INTO VA-A      ;
                VA-B      ;
                VA-C      ;
                VA-D      ;
                VA-E      ;
                VA-F      ;
                VA-G      ;
                VA-H      ;
                VA-I      ;
                VA-J      ;
                VA-K      ;
                VA-L      ;
                VA-M      ;
                VA-N      ;
                VA-O      ;
                VA-P      ;
                VA-Q      ;
                VA-R      ;
                VA-S      ;
                VA-T


           MOVE   VA-T071-NUM-OPERATION
                                TO TB-T071-NUM-OPERATION2  (VA-CONTADOR); 
           MOVE   VA-T071-DAT-OPERATION
                                TO TB-T071-DAT-OPERATION   (VA-CONTADOR); 
           MOVE   VA-T071-DAT-VALUE
                                TO TB-T071-DAT-VALUE       (VA-CONTADOR); 
           MOVE   VA-T071-HORA
                                TO TB-T071-TIM-OPERATION   (VA-CONTADOR); 
           MOVE   VA-T071-AMOUNT
                                TO TB-T071-AMOUNT2         (VA-CONTADOR); 
           MOVE   VA-T071-CODE
                                TO TB-T071-CODE            (VA-CONTADOR); 
           MOVE   VA-T071-OBSERVATIONS
                                TO TB-T071-OBSERVATIONS    (VA-CONTADOR); 
           MOVE   VA-T071-COD-PRODUCT
                                TO TB-T071-COD-PRODUCT     (VA-CONTADOR); 
           MOVE   VA-T071-COD-SPROD
                                TO TB-T071-COD-SPROD       (VA-CONTADOR); 
           MOVE   VA-T071-FLG-FREE1
                                TO TB-T071-FLG-FREE1       (VA-CONTADOR); 
           MOVE   VA-T071-USERUPD
                                TO TB-T071-USERUPD         (VA-CONTADOR); 
           MOVE   VA-T071-NTNMUPD
                                TO TB-T071-NTNMUPD         (VA-CONTADOR); 
           MOVE   VA-T100-BIGALP
                                TO TB-T100-BIGALP          (VA-CONTADOR); 
           MOVE   VA-T606-NUM-OPERATION
                                TO TB-T606-NUM-OPERATION2  (VA-CONTADOR); 
           MOVE   VA-T606-DESCRIPTION
                                TO TB-T606-DESCRIPTION     (VA-CONTADOR); 
           MOVE   VA-T606-PATH
                                TO TB-T606-PATH            (VA-CONTADOR); 
           MOVE   VA-T606-FLG-FREE1
                                TO TB-T606-FLG-FREE1       (VA-CONTADOR); 
           MOVE   VA-T606-CHAR-FREE1
                                TO TB-T606-CHAR-FREE1      (VA-CONTADOR); 
           MOVE   VA-T071-INTREF
                                TO TB-T071-INTREF          (VA-CONTADOR); 
           MOVE   VA-T071-DAT-ACCT
                                TO TB-T071-DAT-ACCT        (VA-CONTADOR); 

      * --JUSTIFICA MONTOS A LA DERECHA
           MOVE TB-T071-NUM-OPERATION2(VA-CONTADOR); 
                                TO VA-ENTRADA-JUSTIF-DER
           MOVE 9               TO VA-LONG-T
           MOVE 0               TO VA-LONG-DEC
           PERFORM 100151-JUSTIFICAR-DERECHA
           MOVE VA-SALIDA-JUSTIF-DER
                                TO TB-T071-NUM-OPERATION2(VA-CONTADOR); 
      *
           MOVE TB-T071-AMOUNT2(VA-CONTADOR); 
                                TO VA-ENTRADA-JUSTIF-DER
           MOVE 15              TO VA-LONG-T
           MOVE 2               TO VA-LONG-DEC
           PERFORM 100151-JUSTIFICAR-DERECHA
           MOVE VA-SALIDA-JUSTIF-DER
                                TO TB-T071-AMOUNT2(VA-CONTADOR); 
      *
           MOVE TB-T606-NUM-OPERATION2(VA-CONTADOR); 
                                TO VA-ENTRADA-JUSTIF-DER
           MOVE 9               TO VA-LONG-T
           MOVE 0               TO VA-LONG-DEC
           PERFORM 100151-JUSTIFICAR-DERECHA
           MOVE VA-SALIDA-JUSTIF-DER
                                TO TB-T606-NUM-OPERATION2(VA-CONTADOR); 
      *
      *--FIN JUSTIFICA MONTOS A LA DERECHA

           MOVE   VA-T071-DAT-ACCT
                                TO WS-LLAVE(01:10); 
           MOVE   TB-T071-NUM-OPERATION2(VA-CONTADOR); 
                                TO WS-LLAVE(12:10); 
           MOVE   WS-LLAVE
                                TO TB-VA-FETCH-LLAVE       (VA-CONTADOR); 
       .
      * DECLARE DECLARE @BAZ070-F
      ******************************************************************
      *23000-EXTRAE-DATOS-X-STORED
      ******************************************************************
       23000-EXTRAE-DATOS-X-STORED.
           INITIALIZE TB-STORED
                      VA-STORED
      *
           MOVE CAA-ENT-ACC                TO VA-ENT-IN
           MOVE E009-BDMID                 TO VA-BDMID-IN
           MOVE E009-NUMCUEN(1:4);           TO VA-BRN-OPEN-IN
           MOVE E009-NUMCUEN(5:2);           TO VA-COD-PROD-IN
           MOVE E009-NUMCUEN(7:8);           TO VA-NUM-ACC-IN
           MOVE VA-FECHA-CALC              TO VA-FECHA-IN
           MOVE VA-ULT-LLAVE               TO VA-ULT-LLAVE-IN
      *
      *BAZ053-INI
      * Se cambia el stored MB09_MB2CF119 por MB09_MB2CF119_v2
      *BAZ053-FIN
           exec sql
               :RESULTADO = CALL MAZP.MAZP.MB09_MB2CF119_v2(
                                           :VA-ENT-IN IN;
                                           :VA-BDMID-IN IN;
                                           :VA-BRN-OPEN-IN IN;
                                           :VA-COD-PROD-IN IN;
                                           :VA-NUM-ACC-IN IN;
                                           :VA-FECHA-IN IN;
                                           :VA-ULT-LLAVE-IN IN;
                                         :VA-SAL-1  
                                         :VA-SAL-2  
                                         :VA-SAL-3  
                                         :VA-SAL-4  
                                         :VA-SAL-5  
                                         :VA-SAL-6  
                                         :VA-SAL-7  
                                         :VA-SAL-8  
                                         :VA-SAL-9  
                                         :VA-SAL-10  
                                         :VA-SAL-11  
                                         :VA-SAL-12  
                                         :VA-SAL-13  
                                         :VA-SAL-14  
                                         :VA-SAL-15 OUTPUT

                                      ); 
          end-exec.
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
      *
                  IF VN-CONTREG >= 15 AND
                     (VA-SAL-1             EQUAL SPACES OR
                      VA-SAL-1             EQUAL SPACES); 
      *
                     MOVE 'MCA0003'        TO CAA-COD-AVISO1
                     PERFORM 30000-FIN
                  END-IF

                  MOVE VA-SAL-1            TO VA-SAL(1); 
                  MOVE VA-SAL-2            TO VA-SAL(2); 
                  MOVE VA-SAL-3            TO VA-SAL(3); 
                  MOVE VA-SAL-4            TO VA-SAL(4); 
                  MOVE VA-SAL-5            TO VA-SAL(5); 
                  MOVE VA-SAL-6            TO VA-SAL(6); 
                  MOVE VA-SAL-7            TO VA-SAL(7); 
                  MOVE VA-SAL-8            TO VA-SAL(8); 
                  MOVE VA-SAL-9            TO VA-SAL(9); 
                  MOVE VA-SAL-10           TO VA-SAL(10); 
                  MOVE VA-SAL-11           TO VA-SAL(11); 
                  MOVE VA-SAL-12           TO VA-SAL(12); 
                  MOVE VA-SAL-13           TO VA-SAL(13); 
                  MOVE VA-SAL-14           TO VA-SAL(14); 
                  MOVE VA-SAL-15           TO VA-SAL(15); 
      *
                  MOVE CA-1                TO I-REG
                  PERFORM UNTIL I-REG>15
      *
                       UNSTRING VA-SAL(I-REG);  DELIMITED BY '| DECLARE DECLARE @'
                       INTO TB-T403-NUM-BIN(I-REG);        ;
                            TB-T403-NUM-CRD(I-REG);        ;
                            TB-T403-NUM-CLTE(I-REG);       ;
                            TB-T403-NUM-CTA(I-REG);        ;
                            TB-T403-TEL-CEL(I-REG);        ;
                            TB-NUM-CUS(I-REG);             ;
                            TB-T041-CAC-DIG1(I-REG);       ;
                            TB-T041-CAC-DIG2(I-REG);       ;
                            TB-T041-COD-PRODUCT(I-REG);    ;
                            TB-T041-COD-SPROD(I-REG);      ;
                            TB-T041-CEN-ACCT(I-REG);       ;
                            TB-T041-FCC(I-REG);            ;
                            TB-T140-DES-TABLE(I-REG);      ;
                            TB-VA-FETCH-LLAVE(I-REG);      ;
                            TB-T071-NUM-OPERATION2(I-REG);  ;
                            TB-T071-DAT-OPERATION(I-REG);  ;
                            TB-T071-DAT-VALUE(I-REG);      ;
                            TB-T071-TIM-OPERATION(I-REG);  ;
                            TB-T071-AMOUNT2(I-REG);         ;
                            TB-T071-CODE(I-REG);           ;
                            TB-T071-OBSERVATIONS(I-REG);   ;
                            TB-T071-COD-PRODUCT(I-REG);    ;
                            TB-T071-COD-SPROD(I-REG);      ;
                            TB-T071-FLG-FREE1(I-REG);      ;
                            TB-T071-USERUPD(I-REG);        ;
                            TB-T071-NTNMUPD(I-REG);        ;
                            TB-T100-BIGALP(I-REG);         ;
                            TB-T606-ACC(I-REG);            ;
                            TB-T606-AMOUNT2(I-REG);         ;
                            TB-T606-NUM-OPERATION2(I-REG);  ;
                            TB-T606-DESCRIPTION(I-REG);    ;
                            TB-T606-PATH(I-REG);           ;
                            TB-T606-GPS-LAT2(I-REG);        ;
                            TB-T606-GPS-LONG2(I-REG);       ;
                            TB-T606-DAT-OPERATION(I-REG);  ;
                            TB-T606-FLG-FREE1(I-REG);      ;
                            TB-T606-CHAR-FREE1(I-REG);     ;
                            TB-T071-INTREF(I-REG); 
      *BAZ053-INI
                           ;TB-T043-NUM-OPE-2(I-REG);      ;
                            TB-T803-ENT-ACC(I-REG); 
      *BAZ053-FIN
                       END-UNSTRING
      *
      * --JUSTIFICA MONTOS A LA DERECHA
                       MOVE TB-T071-NUM-OPERATION2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 9           TO VA-LONG-T
                       MOVE 0           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-T071-NUM-OPERATION2(I-REG); 
      *
                       MOVE TB-T071-AMOUNT2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 15          TO VA-LONG-T
                       MOVE 2           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-T071-AMOUNT2(I-REG); 
      *
                       MOVE TB-T606-AMOUNT2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 17          TO VA-LONG-T
                       MOVE 2           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-T606-AMOUNT2(I-REG); 
      *
                       MOVE TB-T606-NUM-OPERATION2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 9           TO VA-LONG-T
                       MOVE 0           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-T606-NUM-OPERATION2(I-REG); 
      *
                       MOVE TB-T606-GPS-LAT2(I-REG); 
                                           TO VA-ENTRADA-JUSTIF-DER
                       MOVE 17          TO VA-LONG-T
                       MOVE 6           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                           TO TB-T606-GPS-LAT2(I-REG); 
      *
                       MOVE TB-T606-GPS-LONG2(I-REG); 
                                           TO VA-ENTRADA-JUSTIF-DER
                       MOVE 17          TO VA-LONG-T
                       MOVE 6           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                          TO TB-T606-GPS-LONG2(I-REG); 
      *
      *--FIN JUSTIFICA MONTOS A LA DERECHA

                       IF TB-T403-TEL-CEL(I-REG);  EQUAL SPACES OR
                          TB-T403-TEL-CEL(I-REG);  EQUAL LOW-VALUES
      *
                          COMPUTE VA-NUM-REG = I-REG - 1
                          MOVE 15                TO I-REG
                       ELSE
                          ADD 1                  TO VA-NUM-REG
                       END-IF

                       COMPUTE I-REG = I-REG + 1
                  END-PERFORM

                  CONTINUE
             WHEN OTHER
                  MOVE CT-BGE0236          TO CAA-COD-ERROR
                  MOVE SQLCODE             TO CAA-ERR-VARIA1
                  MOVE VA-BDMID-IN         TO CAA-ERR-VARIA2
                  PERFORM 30000-FIN
           END-EVALUATE
           .
      ******************************************************************
      *24000-EXTRAE-DATOS-X-STORED-RET
      ******************************************************************
       24000-EXTRAE-DATOS-X-STORED-RET.
           INITIALIZE TB-STORED
                      VA-STORED
      *
           MOVE CAA-ENT-ACC                TO VA-ENT-IN
           MOVE E009-BDMID                 TO VA-BDMID-IN
           MOVE E009-NUMCUEN(1:4);           TO VA-BRN-OPEN-IN
           MOVE E009-NUMCUEN(5:2);           TO VA-COD-PROD-IN
           MOVE E009-NUMCUEN(7:8);           TO VA-NUM-ACC-IN
           MOVE VA-FECHA-CALC              TO VA-FECHA-IN
           MOVE VA-ULT-LLAVE               TO VA-ULT-LLAVE-IN
      *
           ACCEPT VA-H-INI-AUX     FROM TIME
           exec sql
               :RESULTADO = CALL MAZP.MAZP.MB09_MB2CF219(
                                           :VA-ENT-IN IN;
                                           :VA-BDMID-IN IN;
                                           :VA-BRN-OPEN-IN IN;
                                           :VA-COD-PROD-IN IN;
                                           :VA-NUM-ACC-IN IN;
                                           :VA-FECHA-IN IN;
                                           :VA-ULT-LLAVE-IN IN;
                                         :VA-SAL-1  
                                         :VA-SAL-2  
                                         :VA-SAL-3  
                                         :VA-SAL-4  
                                         :VA-SAL-5  
                                         :VA-SAL-6  
                                         :VA-SAL-7  
                                         :VA-SAL-8  
                                         :VA-SAL-9  
                                         :VA-SAL-10  
                                         :VA-SAL-11  
                                         :VA-SAL-12  
                                         :VA-SAL-13  
                                         :VA-SAL-14  
                                         :VA-SAL-15 OUTPUT

                                      ); 
          end-exec.
          ACCEPT VA-H-FIN-AUX    FROM TIME
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
      *
                  IF VN-CONTREG >= 15 AND
                     (VA-SAL-1             EQUAL SPACES OR
                      VA-SAL-1             EQUAL SPACES); 
      *
                     MOVE 'MCA0003'        TO CAA-COD-AVISO1
                     PERFORM 30000-FIN
                  END-IF

                  MOVE VA-SAL-1            TO VA-SAL(1); 
                  MOVE VA-SAL-2            TO VA-SAL(2); 
                  MOVE VA-SAL-3            TO VA-SAL(3); 
                  MOVE VA-SAL-4            TO VA-SAL(4); 
                  MOVE VA-SAL-5            TO VA-SAL(5); 
                  MOVE VA-SAL-6            TO VA-SAL(6); 
                  MOVE VA-SAL-7            TO VA-SAL(7); 
                  MOVE VA-SAL-8            TO VA-SAL(8); 
                  MOVE VA-SAL-9            TO VA-SAL(9); 
                  MOVE VA-SAL-10           TO VA-SAL(10); 
                  MOVE VA-SAL-11           TO VA-SAL(11); 
                  MOVE VA-SAL-12           TO VA-SAL(12); 
                  MOVE VA-SAL-13           TO VA-SAL(13); 
                  MOVE VA-SAL-14           TO VA-SAL(14); 
                  MOVE VA-SAL-15           TO VA-SAL(15); 
      *
                  MOVE CA-1                TO I-REG
                  PERFORM UNTIL I-REG>15
      *
                       UNSTRING VA-SAL(I-REG);  DELIMITED BY '| DECLARE DECLARE @'
                       INTO TB-RET-T403-NUM-BIN(I-REG);        ;
                            TB-RET-T403-NUM-CRD(I-REG);        ;
                            TB-RET-T403-NUM-CLTE(I-REG);       ;
                            TB-RET-T403-NUM-CTA(I-REG);        ;
                            TB-RET-T403-TEL-CEL(I-REG);        ;
                            TB-RET-NUM-CUS(I-REG);             ;
                            TB-RET-T041-CAC-DIG1(I-REG);       ;
                            TB-RET-T041-CAC-DIG2(I-REG);       ;
                            TB-RET-T041-COD-PRODUCT(I-REG);    ;
                            TB-RET-T041-COD-SPROD(I-REG);      ;
                            TB-RET-T041-CEN-ACCT(I-REG);       ;
                            TB-RET-T041-FCC(I-REG);            ;
                            TB-RET-T140-DES-TABLE(I-REG);      ;
                            TB-RET-VA-FETCH-LLAVE(I-REG);      ;
                            TB-T089-NUM-WHD2(I-REG);           ;
                            TB-T089-DAT-REG(I-REG);            ;
                            TB-T089-TIM-REG(I-REG);            ;
                            TB-T089-AMT-ORIGIN2(I-REG);        ;
                            TB-T089-AMT-CURRENT2(I-REG);       ;
                            TB-T089-CODE(I-REG);               ;
                            TB-T089-OBSERVATIONS(I-REG);       ;

                            TB-RET-T100-BIGALP(I-REG);         ;
                            TB-RET-T606-ACC(I-REG);            ;
                            TB-RET-T606-AMOUNT2(I-REG);        ;
                            TB-RET-T606-NUM-OPERATION2(I-REG); ;
                            TB-RET-T606-DESCRIPTION(I-REG);    ;
                            TB-RET-T606-PATH(I-REG);           ;
                            TB-RET-T606-GPS-LAT2(I-REG);       ;
                            TB-RET-T606-GPS-LONG2(I-REG);      ;
                            TB-RET-T606-DAT-OPERATION(I-REG);  ;
                            TB-RET-T606-FLG-FREE1(I-REG);      ;
                            TB-RET-T606-CHAR-FREE1(I-REG); 

                       END-UNSTRING
      *
      * --JUSTIFICA MONTOS A LA DERECHA
                       MOVE TB-T089-NUM-WHD2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 5           TO VA-LONG-T
                       MOVE 0           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-T089-NUM-WHD2(I-REG); 
      *
                       MOVE TB-T089-AMT-ORIGIN2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 15          TO VA-LONG-T
                       MOVE 2           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-T089-AMT-ORIGIN2(I-REG); 
      *
                       MOVE TB-T089-AMT-CURRENT2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 15          TO VA-LONG-T
                       MOVE 2           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-T089-AMT-CURRENT2(I-REG); 


                       MOVE TB-RET-T606-AMOUNT2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 17          TO VA-LONG-T
                       MOVE 2           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-RET-T606-AMOUNT2(I-REG); 
      *
                       MOVE TB-RET-T606-NUM-OPERATION2(I-REG); 
                                        TO VA-ENTRADA-JUSTIF-DER
                       MOVE 9           TO VA-LONG-T
                       MOVE 0           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                    TO TB-RET-T606-NUM-OPERATION2(I-REG); 
      *
                       MOVE TB-RET-T606-GPS-LAT2(I-REG); 
                                           TO VA-ENTRADA-JUSTIF-DER
                       MOVE 17          TO VA-LONG-T
                       MOVE 6           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                       TO TB-RET-T606-GPS-LAT2(I-REG); 
      *
                       MOVE TB-RET-T606-GPS-LONG2(I-REG); 
                                           TO VA-ENTRADA-JUSTIF-DER
                       MOVE 17          TO VA-LONG-T
                       MOVE 6           TO VA-LONG-DEC
                       PERFORM 100151-JUSTIFICAR-DERECHA
                       MOVE VA-SALIDA-JUSTIF-DER
                                        TO TB-RET-T606-GPS-LONG2(I-REG); 
      *
      *--FIN JUSTIFICA MONTOS A LA DERECHA

                       IF TB-RET-T403-TEL-CEL(I-REG);  EQUAL SPACES OR
                          TB-RET-T403-TEL-CEL(I-REG);  EQUAL LOW-VALUES
      *
                          COMPUTE VA-NUM-REG = I-REG - 1
                          MOVE 15                TO I-REG
                       ELSE
                          ADD 1                  TO VA-NUM-REG
                       END-IF

                       COMPUTE I-REG = I-REG + 1
                  END-PERFORM

                  MOVE CA-1                TO I-REG2
                  PERFORM UNTIL I-REG2>15
                    MOVE TB-RET-T403-NUM-BIN(I-REG2); 
                                           TO TB-T403-NUM-BIN(I-REG2); 
                    MOVE TB-RET-T403-NUM-CRD(I-REG2); 
                                           TO TB-T403-NUM-CRD(I-REG2); 
                    MOVE TB-RET-T403-NUM-CLTE(I-REG2); 
                                           TO TB-T403-NUM-CLTE(I-REG2); 
                    MOVE TB-RET-T403-NUM-CTA(I-REG2); 
                                           TO TB-T403-NUM-CTA(I-REG2); 
                    MOVE TB-RET-T403-TEL-CEL(I-REG2); 
                                           TO TB-T403-TEL-CEL(I-REG2); 
                    MOVE TB-RET-NUM-CUS(I-REG2); 
                                           TO TB-NUM-CUS(I-REG2); 
                    MOVE TB-RET-T041-CAC-DIG1(I-REG2); 
                                           TO TB-T041-CAC-DIG1(I-REG2); 
                    MOVE TB-RET-T041-CAC-DIG2(I-REG2); 
                                           TO TB-T041-CAC-DIG2(I-REG2); 
                    MOVE TB-RET-T041-COD-PRODUCT(I-REG2); 
                                          TO TB-T041-COD-PRODUCT(I-REG2); 
                    MOVE TB-RET-T041-COD-SPROD(I-REG2); 
                                           TO TB-T041-COD-SPROD(I-REG2); 
                    MOVE TB-RET-T041-CEN-ACCT(I-REG2); 
                                           TO TB-T041-CEN-ACCT(I-REG2); 
                    MOVE TB-RET-T041-FCC(I-REG2); 
                                           TO TB-T041-FCC(I-REG2); 
                    MOVE TB-RET-T140-DES-TABLE(I-REG2); 
                                           TO TB-T140-DES-TABLE(I-REG2); 
                    MOVE TB-RET-VA-FETCH-LLAVE(I-REG2); 
                                           TO TB-VA-FETCH-LLAVE(I-REG2); 
                    MOVE TB-RET-T100-BIGALP(I-REG2); 
                                           TO TB-T100-BIGALP(I-REG2); 
                    MOVE TB-RET-T606-ACC(I-REG2); 
                                           TO TB-T606-ACC(I-REG2); 
                    MOVE TB-RET-T606-AMOUNT2(I-REG2); 
                                           TO TB-T606-AMOUNT2(I-REG2); 
                    MOVE TB-RET-T606-NUM-OPERATION2(I-REG2); 
                                       TO TB-T606-NUM-OPERATION2(I-REG2); 
                    MOVE TB-RET-T606-DESCRIPTION(I-REG2); 
                                          TO TB-T606-DESCRIPTION(I-REG2); 
                    MOVE TB-RET-T606-PATH(I-REG2); 
                                           TO TB-T606-PATH(I-REG2); 
                    MOVE TB-RET-T606-GPS-LAT2(I-REG2); 
                                           TO TB-T606-GPS-LAT2(I-REG2); 
                    MOVE TB-RET-T606-GPS-LONG2(I-REG2); 
                                            TO TB-T606-GPS-LONG2(I-REG2); 
                    MOVE TB-RET-T606-DAT-OPERATION(I-REG2); 
                                        TO TB-T606-DAT-OPERATION(I-REG2); 
                    MOVE TB-RET-T606-FLG-FREE1(I-REG2); 
                                           TO TB-T606-FLG-FREE1(I-REG2); 
                    MOVE TB-RET-T606-CHAR-FREE1(I-REG2); 
                                           TO TB-T606-CHAR-FREE1(I-REG2); 

                       ADD 1               TO I-REG2
                  END-PERFORM

                  CONTINUE
             WHEN OTHER
                  MOVE CT-BGE0236          TO CAA-COD-ERROR
                  MOVE SQLCODE             TO CAA-ERR-VARIA1
                  MOVE VA-BDMID-IN         TO CAA-ERR-VARIA2
                  PERFORM 30000-FIN
           END-EVALUATE

           MOVE VA-INI-AUX-SM     TO CAA-TB-AUTH(01:4); 
           MOVE VA-FIN-AUX-SM     TO CAA-TB-AUTH(05:4); 
           .

      *
      ******************************************************************
      *100151-JUSTIFICAR-DERECHA.
      ******************************************************************
       100151-JUSTIFICAR-DERECHA.
              MOVE ZERO                    TO VA-LONG-E
              MOVE ZERO                    TO VA-LONG-D
              MOVE SPACES                  TO VA-SALIDA-JUSTIF-DER

              INSPECT VA-ENTRADA-JUSTIF-DER REPLACING
                                        ALL '.' BY ''

              UNSTRING VA-ENTRADA-JUSTIF-DER
                                            DELIMITED BY SPACE
                  INTO VA-T041-WDRWBAL-ENT;
                       VA-T041-WDRWBAL-DEC
              END-UNSTRING

              MOVE SPACES TO VA-ENTRADA-JUSTIF-DER

              INSPECT VA-T041-WDRWBAL-ENT
                      TALLYING VA-LONG-E
                      FOR CHARACTERS BEFORE INITIAL SPACE
      *
              INSPECT VA-T041-WDRWBAL-DEC
                      TALLYING VA-LONG-D
                      FOR CHARACTERS BEFORE INITIAL SPACE

                  COMPUTE VA-EMPIEZA = VA-LONG-T - VA-LONG-DEC
                                       - VA-LONG-E + 1

                  MOVE VA-T041-WDRWBAL-ENT
                     TO VA-SALIDA-JUSTIF-DER(VA-EMPIEZA:VA-LONG-E); 

                  COMPUTE VA-POS = VA-LONG-T - VA-LONG-DEC + 1

                  MOVE VA-T041-WDRWBAL-DEC(1:VA-LONG-D); 
                     TO VA-SALIDA-JUSTIF-DER(VA-POS:VA-LONG-DEC); 
                  .
      * DECLARE DECLARE @BAZ037-F


      ******************************************************************
      *                    21000-VALIDA-ENTRADA                        *
      ******************************************************************
       21000-VALIDA-ENTRADA.
      *
      * VALIDA QUE E009-CONTREG TENGA VALORES NUMÉRICOS O SE ENCUENTRE VACIÓ
           EVALUATE TRUE ALSO TRUE
             WHEN E009-CONTREG = SPACES ALSO E009-ULTLLAV = SPACES
                MOVE CA-MAXLLAV          TO VA-ULT-LLAVE
                MOVE ZEROES              TO VN-CONTREG
             WHEN E009-CONTREG > SPACES ALSO E009-ULTLLAV > SPACES
                MOVE E009-ULTLLAV        TO VA-ULT-LLAVE
                IF E009-CONTREG IS NOT NUMERIC
                   MOVE 'MCE0099'        TO CAA-COD-ERROR
                   PERFORM 30000-FIN
                ELSE
                   MOVE E009-CONTREG     TO VN-CONTREG
                END-IF
             WHEN E009-CONTREG = SPACES ALSO E009-ULTLLAV > SPACES
             WHEN E009-CONTREG > SPACES ALSO E009-ULTLLAV = SPACES
                MOVE 'MCE0129'           TO CAA-COD-ERROR
                PERFORM 30000-FIN
           END-EVALUATE
      *-------------------
      *    VALIDA QUE SE ENCUENTRE EL NÚMERO DE TARJETA (E009-NUMTARJ);  O 
      *    EL NÚMERO DE CUENTA (E009-NUMCUEN);  Y QUE SEAN VALORES NUMÉRICOS
           EVALUATE TRUE ALSO TRUE
             WHEN  E009-NUMTARJ= SPACES ALSO E009-NUMCUEN = SPACES
                MOVE 'MCE0813'           TO CAA-COD-ERROR
                PERFORM 30000-FIN
             WHEN E009-NUMTARJ > SPACES ALSO E009-NUMCUEN > SPACES
                MOVE 'MDE0025'           TO CAA-COD-ERROR
                PERFORM 30000-FIN
             WHEN E009-NUMTARJ > SPACES ALSO E009-NUMCUEN = SPACES
                IF E009-NUMTARJ IS NOT NUMERIC
                   MOVE 'MCE0344'        TO CAA-COD-ERROR
                   PERFORM 30000-FIN
                END-IF
                IF CAA-PRKEY = '01'
                   SET WSS-TARJETA          TO TRUE
                ELSE
                   SET WSS-RET-TRJ          TO TRUE
                END-IF
      * DECLARE DECLARE @BAZ021-INI
                IF CAA-PRKEY EQUAL '03'
                OR CAA-PRKEY EQUAL '04'
                   MOVE 'MCE0005'              TO CAA-SW-ERRCOD
                   MOVE 'CUENTA'               TO CAA-ERR-VARIA1
                   MOVE CAA-PRKEY              TO CAA-ERR-VARIA2
                   PERFORM 30000-FIN
                END-IF
      * DECLARE DECLARE @BAZ021-FIN
             WHEN E009-NUMTARJ = SPACES ALSO E009-NUMCUEN > SPACES
                IF E009-NUMCUEN IS NOT NUMERIC
                   MOVE 'MCE0099'        TO CAA-COD-ERROR
                   PERFORM 30000-FIN
                END-IF
                IF CAA-PRKEY = '01'
      * DECLARE DECLARE @BAZ021-INI
                OR CAA-PRKEY = '03'
      * DECLARE DECLARE @BAZ021-FIN
                   SET WSS-CUENTA          TO TRUE
                ELSE
                   SET WSS-RET-CTA         TO TRUE
                END-IF
           END-EVALUATE
      *-----------------------
      *    VALIDA QUE SÍ SE ENCUENTRA EL NÚMERO DE TELÉFONO (E009-NUMECEL); ;
      *    TAMBIÉN SU IDENTIFICADOR (E009-IDCELUL); 
           EVALUATE TRUE ALSO TRUE
             WHEN E009-NUMECEL = SPACES ALSO E009-IDCELUL = SPACES
                SET WSS-RSEGM-NO TO TRUE
             WHEN E009-NUMECEL > SPACES ALSO E009-IDCELUL > SPACES
                SET WSS-RSEGM-SI TO TRUE
             WHEN E009-NUMECEL = SPACES ALSO E009-IDCELUL > SPACES
             WHEN E009-NUMECEL > SPACES ALSO E009-IDCELUL = SPACES
                MOVE 'MCE0129'        TO CAA-COD-ERROR
                PERFORM 30000-FIN
           END-EVALUATE
      *------------------------------
      *    VALIDA QUE SE ENCUENTRE LA LATITUD (E009-LATITUD);  Y LONGITUD 
      *    (E009-LONGITD);  PARA LLAMAR AL PROCEDIMIENTO
           EVALUATE TRUE ALSO TRUE
             WHEN E009-LATITUD = SPACES ALSO E009-LONGITD > SPACES
             WHEN E009-LATITUD > SPACES ALSO E009-LONGITD = SPACES
                MOVE 'MCE0129'           TO CAA-COD-ERROR
                PERFORM 30000-FIN
             WHEN E009-LATITUD = SPACES ALSO E009-LONGITD = SPACES
                SET WSS-LTYLG-NO         TO TRUE
             WHEN E009-LATITUD > SPACES ALSO E009-LONGITD > SPACES
                MOVE E009-LATITUD        TO VA-AUX-POSICION
                PERFORM 21100-CAMPOS-POSICION
                MOVE E009-LONGITD        TO VA-AUX-POSICION
                PERFORM 21100-CAMPOS-POSICION
                SET WSS-LTYLG-SI         TO TRUE
           END-EVALUATE
      *
           IF E009-TIPCONS > SPACES
              MOVE E009-TIPCONS          TO WSS-TIPO-CONSULTA
           END-IF.
      *
      ******************************************************************
      *                    21100-CAMPOS-POSICION                       *
      *SE VALIDA QUE LOS CAMPOS RELACIONADOS CON LA POSICI�N SATELITAL *
      *TENGAN CARACTERES V�LIDOS.                                      *
      ******************************************************************
       21100-CAMPOS-POSICION.
      *
           INSPECT VA-AUX-POSICION TALLYING VN-IND1
                           FOR CHARACTERS BEFORE INITIAL '.'
      *
           MOVE VA-AUX-POSICION(1:1);    TO WSS-SIGNO
           MOVE VN-IND1                TO VN-IND2
      *
           IF WSS-MENOS OR WSS-MAS
              SUBTRACT 1 FROM VN-IND2
              IF VA-AUX-POSICION(2:VN-IND2);  IS NOT NUMERIC
                 MOVE 'MCE0258'     TO CAA-COD-ERROR
                 PERFORM 30000-FIN
              END-IF
           ELSE
              IF VA-AUX-POSICION(1:VN-IND2);  IS NOT NUMERIC
                 MOVE 'MCE0258'     TO CAA-COD-ERROR
                 PERFORM 30000-FIN
              END-IF
           END-IF
      *
           INSPECT VA-AUX-POSICION TALLYING VN-IND2
                           FOR CHARACTERS AFTER INITIAL '.'
                           REPLACING ALL SPACES BY ZEROS
      *
           ADD 2 TO VN-IND1
      *
           IF VA-AUX-POSICION(VN-IND1:VN-IND2);  IS NOT NUMERIC
                 MOVE 'MCE0258'     TO CAA-COD-ERROR
              PERFORM 30000-FIN
           END-IF
      *
           INITIALIZE VA-AUX-POSICION
                      VN-IND1
                      VN-IND2.
      *
      ******************************************************************
      *                    22000-CALCULA-FECHA                         *
      * SE RESTAN TRES MESES A LA FECHA EN CURSO.                      *
      ******************************************************************
       22000-CALCULA-FECHA.
      *
           MOVE CAA-2DAT-TRANSMI       TO VA-FECHA-SIST
           MOVE '01'                   TO VA-DIA-CALC
      *
           EVALUATE VA-MES-SIST
           WHEN '01'
             MOVE '10'                 TO VA-MES-CALC
             COMPUTE VA-YEAR-C9 = VN-YEAR-S9 - 1
           WHEN '02'
             MOVE '11'                 TO VA-MES-CALC
             COMPUTE VA-YEAR-C9 = VN-YEAR-S9 - 1
           WHEN '03'
             MOVE '12'                 TO VA-MES-CALC
             COMPUTE VA-YEAR-C9 = VN-YEAR-S9 - 1
           WHEN OTHER
             COMPUTE VA-MES-C9 = VN-MES-S9 - 3
             MOVE VA-YEAR-SIST         TO VA-YEAR-CALC
           END-EVALUATE.
      *
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ057-I
      ******************************************************************
      *                    23000-CONSULTA-TARJETA                      *
      * SE CONSULTAN LAS OPERACIONES DE UNA TARJETA.                   *
      ******************************************************************
      *23000-CONSULTA-TARJETA.
      *
      *    MOVE VA-FECHA-CALC          TO T043-DAT-OPERATION
      *    MOVE E009-NUMTARJ(1:6);       TO T043-NUM-BIN-CRD
      *    MOVE E009-NUMTARJ(7:10);      TO T043-NUM-CARD
      * DECLARE DECLARE @BAZ027-I
      *    PERFORM 23001-ABRE-MCDC0043
      *    PERFORM 23002-LEER-MCDC0043 UNTIL WSS-FIN-S1
      *                                OR VN-CONTREG = CN-99
      *    PERFORM 23003-CERR-MCDC0043
      * DECLARE DECLARE @BAZ027-F
      *    IF WSS-LTYLG-SI
      *       MOVE E009-NUMTARJ(1:6);    TO T403-NUM-BIN
      *       MOVE E009-NUMTARJ(7:10);   TO T403-NUM-CRD
      *       MOVE E009-NUMECEL        TO T403-TEL-CEL
      *       MOVE E009-IDCELUL        TO T403-ID-TEL-CEL
      *       PERFORM 23100-CONSULTA-MCDT403
      *       IF WSVA-NUMCTE > SPACES
      *          PERFORM 23200-INSERT-PEDT100
      *          CONTINUE
      *       END-IF
      *    END-IF
      *
      *    EVALUATE TRUE
      *    WHEN VN-CONTREG = 0
      *       MOVE 'MCE0004'        TO CAA-COD-ERROR
      *    WHEN VN-CONTREG = CN-99
      *    WHEN WSS-NO-PAGINA
      *       MOVE 'MCA0003'        TO CAA-COD-AVISO1
      *    WHEN WSS-SI-PAGINA
      *       MOVE 'MCA0022'        TO CAA-COD-AVISO1
      *    END-EVALUATE.
      *
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ057-F
      ******************************************************************
      *               23001-ABRE-MCDC0043                              *
      ******************************************************************
       23001-ABRE-MCDC0043.
      * DECLARE DECLARE @BAZ027-I
      *    EXEC SQL
      *       OPEN MCDC0043
      *    END-EXEC
      * DECLARE DECLARE @BAZ027-F
           MOVE SQLCODE       TO SQL-VALUES
      *
           IF NOT SQL-88-OK
              MOVE 'MDE0026'  TO CAA-COD-ERROR
              PERFORM 30000-FIN
           END-IF.
      *
      ******************************************************************
      *               23002-LEER-MCDC0043                              *
      *REALIZA FETCH A LA TABLA MCDT043 PARA CONSULTAR LAS OPERACIONES *
      *DE UNA TARJETA.                                                 *
      ******************************************************************
       23002-LEER-MCDC0043.
      * DECLARE DECLARE @BAZ027-I
      *    EXEC SQL
      *       FETCH MCDC0043
      *        INTO :VA-FETCH-LLAVE    ;
      *             :T043-NUM-DEB-OPE ;
      *             :VA-FECHA-OPERACION;
      *             :T043-TIM-OPERATION;
      *             :T043-AMT-OPERATION;
      *             :T043-TXT-DIG-30 ;
      *             :T043-COD-OPERATION
      *    END-EXEC
      * DECLARE DECLARE @BAZ027-F
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
                ADD WSCN-1   TO VN-LEIDOS
                IF VN-LEIDOS <=  CA-LIMITE
                  INITIALIZE WSV-AUXSAL
                  PERFORM 28888-TIPO-OPER
                  MOVE VA-FETCH-LLAVE     TO VA-AUX-LLAVE
                  MOVE T043-NUM-DEB-OPE   TO WSV-AUX-NUMOPE
                  MOVE VA-FECHA-OPERACION TO WSV-AUX-FECHA
                  MOVE T043-TIM-OPERATION TO WSV-AUX-HORA
      *           MOVE T043-AMT-OPERATION TO WSV-AUX-IMPT
                  MOVE T043-TXT-DIG-30    TO WSV-AUX-DESC
                  MOVE CA-TA              TO VA-AUX-TCONS
      * DECLARE DECLARE @BAZ007E.I
                  INSPECT WSV-AUX-DESC  REPLACING ALL 'CR ' BY '   '
      * DECLARE DECLARE @BAZ007E.F
                  IF VN-LEIDOS < CA-LIMITE
      * DECLARE DECLARE @BAZ017-->INI
                    MOVE T043-COD-OPERATION         TO VA-COD-MOV
      * DECLARE DECLARE @BAZ017<--FIN
                    PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
                    PERFORM 29990-MOVER-SALIDA-2
                    PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ023.I
                    PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F


                  END-IF
                ELSE
                  SET WSS-FIN-S1          TO TRUE
                  SET WSS-SI-PAGINA       TO TRUE
                  MOVE CA-S               TO VA-IND-PAGINA
      * DECLARE DECLARE @BAZ017-->INI
                    MOVE T043-COD-OPERATION         TO VA-COD-MOV
      * DECLARE DECLARE @BAZ017<--FIN
                  PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
                  PERFORM 29990-MOVER-SALIDA-2
                  PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ023.I
                  PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F

                END-IF
             WHEN SQL-88-NOT-FOUND
      *         MOVE CA-MAXLLAV           TO VA-ULT-LLAVE
      *         PERFORM 24000-RETENCION-TARJETA
                SET WSS-FIN-S1            TO TRUE
             WHEN OTHER
                MOVE CA-MCDT043           TO ABC-OBJECT-ERROR
                MOVE CA-FETCH             TO ABC-REFERENCE1
                MOVE SQLCODE              TO ABC-SQLCODE
                PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      *
      ******************************************************************
      *               23003-CERR-MCDC0043                              *
      ******************************************************************
       23003-CERR-MCDC0043.
      * DECLARE DECLARE @BAZ027-I
      *    EXEC SQL
      *       CLOSE MCDC0043
      *    END-EXEC
      * DECLARE DECLARE @BAZ027-F
           MOVE SQLCODE       TO SQL-VALUES
      *
           IF NOT SQL-88-OK
              MOVE 'MDE0027'  TO CAA-COD-ERROR
              PERFORM 30000-FIN
           END-IF.
      *
      ******************************************************************
      *                    23100-CONSULTA-MCDT403                      *
      ******************************************************************
       23100-CONSULTA-MCDT403.
      *
           IF WSS-RSEGM-SI
             EXEC SQL
               SELECT T403_TXT_LIB1   ;
                      T403_NUM_BIN    ;
                      T403_NUM_CRD    ;
                      T403_TEL_CEL    ;
                      T403_ID_TEL_CEL ;
                      T403_NUM_CLTE
                INTO :T403-TXT-LIB1   ;
                     :T403-NUM-BIN    ;
                     :T403-NUM-CRD    ;
                     :T403-TEL-CEL    ;
                     :T403-ID-TEL-CEL ;
                     :T403-NUM-CLTE
                FROM MCDT403 with (nolock); 
               WHERE T403_NUM_CRD    = :T403-NUM-CRD
                 AND T403_NUM_BIN    = :T403-NUM-BIN
                 AND T403_TEL_CEL    = :T403-TEL-CEL
                 AND T403_ID_TEL_CEL = :T403-ID-TEL-CEL
             END-EXEC
           ELSE
             EXEC SQL
               SELECT T403_TXT_LIB1   ;
                      T403_NUM_BIN    ;
                      T403_NUM_CRD    ;
                      T403_TEL_CEL    ;
                      T403_ID_TEL_CEL ;
                      T403_NUM_CLTE
                INTO :T403-TXT-LIB1   ;
                     :T403-NUM-BIN    ;
                     :T403-NUM-CRD    ;
                     :T403-TEL-CEL    ;
                     :T403-ID-TEL-CEL ;
                     :T403-NUM-CLTE
                FROM MCDT403 with (nolock); 
               WHERE T403_NUM_CRD    = :T403-NUM-CRD
                 AND T403_NUM_BIN    = :T403-NUM-BIN
             END-EXEC
           END-IF
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
            WHEN SQL-88-OK
               MOVE T403-NUM-CLTE      TO WSVA-NUMCTE
            WHEN SQL-88-NOT-FOUND
               CONTINUE
            WHEN OTHER
               MOVE CA-MCDT403         TO ABC-OBJECT-ERROR
               MOVE CA-SELECT          TO ABC-REFERENCE1
               MOVE SQLCODE            TO ABC-SQLCODE
               PERFORM 999999-DB2-ABEND
           END-EVALUATE.
      *
      ******************************************************************
      *              23200-INSERT-PEDT100                              *
      ******************************************************************
       23200-INSERT-PEDT100.
      *
      *    INITIALIZE DCLPEDT100
      *
      *    MOVE CAA-ENT-ACC      TO T100-ENT
      *    MOVE WSVA-NUMCTE      TO T100-NUM-CUS
      *    MOVE VA-FECHA-SIST    TO T100-DAT-REG
      *    MOVE CA-E003          TO T100-TYP-OPE
      *    MOVE CA-D002          TO T100-TYP-APPLI
      *    MOVE ZEROES           TO T100-NUM-OPE
      *    MOVE FUNCTION NUMVAL (E009-LATITUD); 
      *                          TO T100-LATITUDE
      *    MOVE FUNCTION NUMVAL (E009-LONGITD); 
      *                          TO T100-LONGITUDE
      *    MOVE SPACES           TO T100-NUM-REF
      *    MOVE E009-NUMECEL     TO T100-CEL-NUM
      *    MOVE SPACES           TO T100-FLG-FREE
      *                             VA-FLG-FREE2
      *                             T100-CHAR-FREE1
      *                             T100-CHAR-FREE2
      *    MOVE ZEROES           TO T100-AMT-FREE1
      *                             T100-AMT-FREE2
      *    MOVE CAA-CEN-ACCOUNT  TO T100-CEN-LASTMOD
      *    MOVE CAA-USERID       TO T100-USER-LASTMOD
      *    MOVE CAA-TERMINAL     TO T100-TRM-LASTMOD
      *
      *    EXEC SQL
      *        INSERT INTO PEDT100
      *           (T100_ENT         ;
      *            T100_NUM_CUS     ;
      *            T100_DAT_REG     ;
      *            T100_TYP_OPE     ;
      *            T100_TYP_APPLI   ;
      *            T100_NUM_OPE     ;
      *            T100_LATITUDE    ;
      *            T100_LONGITUDE   ;
      *            T100_NUM_REF     ;
      *            T100_CEL_NUM     ;
      *            T100_FLG_FREE    ;
      *            T100_FLG_FREE2   ;
      *            T100_CHAR_FREE1  ;
      *            T100_CHAR_FREE2  ;
      *            T100_AMT_FREE1   ;
      *            T100_AMT_FREE2   ;
      *            T100_CEN_LASTMOD ;
      *            T100_USER_LASTMOD;
      *            T100_TRM_LASTMOD ;
      *            T100_STP_LASTMOD ); 
      *        VALUES
      *           (:T100-ENT         ;
      *            :T100-NUM-CUS     ;
      *            :T100-DAT-REG     ;
      *            :T100-TYP-OPE     ;
      *            :T100-TYP-APPLI   ;
      *            :T100-NUM-OPE     ;
      *            :T100-LATITUDE    ;
      *            :T100-LONGITUDE   ;
      *            :T100-NUM-REF     ;
      *            :T100-CEL-NUM     ;
      *            :T100-FLG-FREE    ;
      *            :VA-FLG-FREE2;
      *            :T100-CHAR-FREE1  ;
      *            :T100-CHAR-FREE2  ;
      *            :T100-AMT-FREE1   ;
      *            :T100-AMT-FREE2   ;
      *            :T100-CEN-LASTMOD ;
      *            :T100-USER-LASTMOD;
      *            :T100-TRM-LASTMOD ;
      *            CURRENT TIMESTAMP); 
      *    END-EXEC
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *     WHEN SQL-88-OK
      *        CONTINUE
      *     WHEN OTHER
      *        MOVE CA-PEDT100         TO ABC-OBJECT-ERROR
      *        MOVE CA-INSERT          TO ABC-REFERENCE1
      *        MOVE SQLCODE            TO ABC-SQLCODE
      *        PERFORM 999999-DB2-ABEND
      *    END-EVALUATE.

           INITIALIZE MB7C0100-01
      *
           IF  E009-LONGITD   NOT EQUAL TO SPACES
           AND E009-LATITUD   NOT EQUAL TO SPACES

               MOVE WSVA-NUMCTE            TO PR-GEO-CLIENTE
               MOVE CAA-CODTRAN            TO PR-GEO-CODOPER
               MOVE FUNCTION NUMVAL (E009-LATITUD); 
                                           TO PR-GEO-LATITUD
               MOVE FUNCTION NUMVAL (E009-LONGITD); 
                                           TO PR-GEO-LONGITUD
               MOVE T403-TEL-CEL           TO PR-GEO-CEL
               MOVE E009-NUMCUEN           TO PR-GEO-CUENTA
               MOVE ZEROES                 TO PR-GEO-MONTO
               MOVE ZEROES                 TO PR-GEO-NUMOPER
               MOVE CAA-CEN-ACCOUNT        TO PR-GEO-CENTRO
               MOVE CAA-USERID             TO PR-GEO-USUARIO
               MOVE CAA-TERMINAL           TO PR-GEO-TERMINAL

               EXEC CICS
                   LINK PROGRAM (CT-MB7C0110); 
                   COMMAREA (MB7C0100-01); 
               END-EXEC
      *
           END-IF
           .
      * DECLARE DECLARE @BAZ052-I
      ******************************************************************
      *              23300-SUMA-SOBRES                                 *
      ******************************************************************
       23300-SUMA-SOBRES.
            INITIALIZE DCLMBGT039 MBNS6009

            MOVE  WSVA-NUMCTE        TO  T039-NUM-CLIENTE
            MOVE  VA-CUENTA-AUX      TO  T039-CTA-EJE

            EXEC SQL
              SELECT SUM(T039_SALDO); ;
                     COUNT(T039_ID_CTA_META); 
                 INTO :T039-SALDO;
                      :T039-ID-CTA-META
              FROM  MAZP.MBDT039 with(nolock); 
              WHERE T039_NUM_CLIENTE = :T039-NUM-CLIENTE
                AND T039_CTA_EJE     = :T039-CTA-EJE
                AND T039_ESTAT_CTA_META IN ('SA';'SP';'SV';'SS'); 
                AND T039_LOG_METAS   = 'SOBRES'
            END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
            WHEN SQL-88-OK
               MOVE T039-SALDO         TO S609-MONSOB
               MOVE T039-ID-CTA-META   TO S609-SALSOB
            WHEN SQL-88-NOT-FOUND
               CONTINUE
            WHEN OTHER
               MOVE CA-MBDT039         TO ABC-OBJECT-ERROR
               MOVE CA-SELECT          TO ABC-REFERENCE1
               MOVE SQLCODE            TO ABC-SQLCODE
               PERFORM 999999-DB2-ABEND
           END-EVALUATE.

      ******************************************************************
      *              23400-SUMA-ALCANCIA                               *
      ******************************************************************
       23400-SUMA-ALCANCIA.
            INITIALIZE DCLMBGT039

            MOVE  WSVA-NUMCTE        TO  T039-NUM-CLIENTE
            MOVE  VA-CUENTA-AUX      TO  T039-CTA-EJE

            EXEC SQL
              SELECT SUM(T039_SALDO); ;
                     COUNT(T039_ID_CTA_META); 
                 INTO :T039-SALDO;
                      :T039-ID-CTA-META
              FROM  MAZP.MBDT039 with(nolock); 
              WHERE T039_NUM_CLIENTE = :T039-NUM-CLIENTE
                AND T039_CTA_EJE     = :T039-CTA-EJE
                AND T039_ESTAT_CTA_META IN ('AC';'IN';'PA'); 
                AND T039_LOG_METAS   = 'ALCANCIA'
            END-EXEC


      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
            WHEN SQL-88-OK
               MOVE T039-SALDO         TO S609-MONALC
               MOVE T039-ID-CTA-META   TO S609-SALALC
            WHEN SQL-88-NOT-FOUND
               CONTINUE
            WHEN OTHER
               MOVE CA-MBDT039         TO ABC-OBJECT-ERROR
               MOVE CA-SELECT          TO ABC-REFERENCE1
               MOVE SQLCODE            TO ABC-SQLCODE
               PERFORM 999999-DB2-ABEND
           END-EVALUATE.
      * DECLARE DECLARE @BAZ052-F
      *
      *
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ057-I
      ******************************************************************
      *                   24000-RETENCION-TARJETA                      *
      * SE CONSULTAN LAS RETENCIONES DE UNA TARJETA.                   *
      ******************************************************************
      *24000-RETENCION-TARJETA.
      *
      *    PERFORM 24001-CONSULTA-MCDT114
      *
      *    MOVE T114-NUM-CON          TO T010-NUM-CONTRACT
      *    MOVE T114-BRN-CON          TO T010-BRN-CONTRACT
      *    MOVE T114-TYP-CON          TO T010-TYP-CONTRACT
      *    MOVE T114-ENT-CON          TO T010-ENT-CONTRACT
      *    MOVE VA-FECHA-CALC         TO T010-DAT-OPERATION
      * DECLARE DECLARE @BAZ027-I
      *    PERFORM 24002-ABRIR-MCDC0010
      *    PERFORM 24003-LEER-MCDC0010 UNTIL WSS-FIN-S2
      *                                OR VN-CONTREG = CN-99
      *    PERFORM 24004-CERRAR-MCDC0010
      * DECLARE DECLARE @BA027-F
      *    EVALUATE TRUE
      *      WHEN VN-CONTREG = 0
      *         MOVE 'MCE0004'        TO CAA-COD-ERROR
      *      WHEN VN-CONTREG = CN-99
      *      WHEN WSS-NO-PAGINA
      *         MOVE 'MCA0003'        TO CAA-COD-AVISO1
      *      WHEN WSS-SI-PAGINA
      *         MOVE 'MCA0022'        TO CAA-COD-AVISO1
      *    END-EVALUATE.
      *
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ057-F
      ******************************************************************
      *                    24001-CONSULTA-MCDT114                      *
      ******************************************************************
       24001-CONSULTA-MCDT114.
      *
           INITIALIZE DCLMCDT114
           MOVE E009-NUMTARJ(1:6);      TO T114-NUM-BINCRD
           MOVE E009-NUMTARJ(7:10);     TO T114-NUM-CRD
      *
           EXEC SQL
             SELECT T114_ENT_CON ;
                    T114_BRN_CON ;
                    T114_TYP_CON ;
                    T114_NUM_CON
              INTO :T114-ENT-CON ;
                   :T114-BRN-CON ;
                   :T114-TYP-CON ;
                   :T114-NUM-CON
              FROM MCDT114 with (nolock); 
             WHERE T114_NUM_CRD    = :T114-NUM-CRD
               AND T114_NUM_BINCRD = :T114-NUM-BINCRD
               AND T114_COD_CRDWHD = ' '
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
            WHEN SQL-88-OK
              CONTINUE
            WHEN SQL-88-NOT-FOUND
              MOVE 'MCE0107'          TO CAA-COD-ERROR
              PERFORM 30000-FIN
            WHEN OTHER
               MOVE CA-MCDT114        TO ABC-OBJECT-ERROR
               MOVE CA-FETCH          TO ABC-REFERENCE1
               MOVE SQLCODE           TO ABC-SQLCODE
               PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      *
      ******************************************************************
      *               24002-ABRIR-MCDC0010                             *
      ******************************************************************
       24002-ABRIR-MCDC0010.
      * DECLARE DECLARE @BAZ027-I
      *    EXEC SQL
      *       OPEN MCDC0010
      *    END-EXEC
      * DECLARE DECLARE @BAZ027-F
           MOVE SQLCODE       TO SQL-VALUES
      *
           IF NOT SQL-88-OK
              MOVE 'MDE0026'  TO CAA-COD-ERROR
              PERFORM 30000-FIN
           END-IF.
      *
      ******************************************************************
      *               24003-LEER-MCDC0010                              *
      * REALIZA FETCH A LA TABLA MCDT010 PARA CONSULTAR LAS RETENCIONES*
      * DE UNA TARJETA.                                                *
      ******************************************************************
       24003-LEER-MCDC0010.
      * DECLARE DECLARE @BAZ027-I
      *    EXEC SQL
      *       FETCH MCDC0010
      *        INTO :VA-FETCH-LLAVE    ;
      *             :T010-NUM-AUT      ;
      *             :VA-FECHA-OPERACION;
      *             :T010-TIM-OPERATION;
      *             :T010-AMT-OPERATION;
      *             :T010-TXT-DIG-30
      *    END-EXEC
      * DECLARE DECLARE @BAZ027-F
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
                ADD WSCN-1   TO VN-LEIDOS
                IF VN-LEIDOS <= CA-LIMITE
                  INITIALIZE WSV-AUXSAL
                  MOVE VA-FETCH-LLAVE     TO VA-AUX-LLAVE
                  MOVE T010-NUM-AUT       TO WSV-AUX-NUMOPE
                  MOVE VA-FECHA-OPERACION TO WSV-AUX-FECHA
                  MOVE T010-TIM-OPERATION TO WSV-AUX-HORA
                  MOVE T010-AMT-OPERATION TO WSV-AUX-IMPT
                  MOVE T010-TXT-DIG-30    TO WSV-AUX-DESC
                  MOVE CA-RT              TO VA-AUX-TCONS
      * DECLARE DECLARE @BAZ007E.I
                  INSPECT WSV-AUX-DESC  REPLACING ALL 'CR ' BY '   '
      * DECLARE DECLARE @BAZ007E.F
                  IF VN-LEIDOS < CA-LIMITE
                    PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
                    PERFORM 29990-MOVER-SALIDA-2
                    PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ023.I
                    PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
                  END-IF
                ELSE
                  SET WSS-FIN-S2        TO TRUE
                  SET WSS-SI-PAGINA     TO TRUE
                  MOVE CA-S             TO VA-IND-PAGINA
                  PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
                  PERFORM 29990-MOVER-SALIDA-2
                  PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ025.I
                  PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ025.F

                END-IF
             WHEN SQL-88-NOT-FOUND
                IF VN-LEIDOS = CA-LIMITE
                  PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
                  PERFORM 29990-MOVER-SALIDA-2
                  PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ023.I
                  PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F

                END-IF
                SET WSS-NO-PAGINA       TO TRUE
                SET WSS-FIN-S2          TO TRUE
             WHEN OTHER
                MOVE CA-MCDT010         TO ABC-OBJECT-ERROR
                MOVE CA-FETCH           TO ABC-REFERENCE1
                MOVE SQLCODE            TO ABC-SQLCODE
                PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      *
      ******************************************************************
      *               24004-CERRAR-MCDC0010                            *
      ******************************************************************
       24004-CERRAR-MCDC0010.
      * DECLARE DECLARE @BAZ027-I
      *    EXEC SQL
      *       CLOSE MCDC0010
      *    END-EXEC
      * DECLARE DECLARE @BAZ027-F
           MOVE SQLCODE       TO SQL-VALUES
      *
           IF NOT SQL-88-OK
              MOVE 'MDE0027'  TO CAA-COD-ERROR
              PERFORM 30000-FIN
           END-IF.
      *
      ******************************************************************
      *              25000-CONSULTA-CUENTA                             *
      *REALIZA FETCH A LA TABLA BGDT071 PARA CONSULTAR LAS OPERACIONES;*
      *DEL DIA EN CURSO; DE UNA CUENTA.                                *
      ******************************************************************
       25000-CONSULTA-CUENTA.
      *
           MOVE VA-FECHA-CALC       TO T071-DAT-OPERATION
           MOVE E009-NUMCUEN(5:10);   TO T071-ACC
           MOVE E009-NUMCUEN(1:4);    TO T071-CEN-REG
           MOVE CA-0127             TO T071-ENT
      * DECLARE DECLARE @BAZ.I
           PERFORM OBTEN-CUENTA-20POS
      * DECLARE DECLARE @BAZ.F
      * DECLARE DECLARE @BAZ027-I
           MOVE AUX-CTA-INPUT                      TO VA-CUENTA-AUX
      * DECLARE DECLARE @BAZ027-F
      * DECLARE DECLARE @BAZ021-INI
           IF SW-BAZ
               PERFORM 25100-VALIDA-SPROD-WALLET
           END-IF
      * DECLARE DECLARE @BAZ021-FIN
      * DECLARE DECLARE @BAZ037-I
      *    PERFORM 25001-ABRE-BGDC0071
           MOVE CA-1                       TO I-REG
      * DECLARE DECLARE @BAZ037-F
           PERFORM 25002-LEER-BGDC0071 UNTIL WSS-FIN-S1
                                       OR VN-CONTREG >= CN-99
      * DECLARE DECLARE @BAZ037-I
      *    PERFORM 25003-CERR-BGDC0071
      * DECLARE DECLARE @BAZ037-F

           EVALUATE TRUE
           WHEN VN-CONTREG = 0
              MOVE 'MCE0004'        TO CAA-COD-ERROR
           WHEN VN-CONTREG = CN-99
           WHEN WSS-NO-PAGINA
              MOVE 'MCA0003'        TO CAA-COD-AVISO1
           WHEN WSS-SI-PAGINA
              MOVE 'MCA0022'        TO CAA-COD-AVISO1
           END-EVALUATE.

      * DECLARE DECLARE @BAZ021-INI
      ******************************************************************
      *25100-VALIDA-SPROD-WALLET                                       *
      ******************************************************************
       25100-VALIDA-SPROD-WALLET.
      *
           INITIALIZE DCLBGDT140
           MOVE '0406'                         TO T140-COD-TABLE
           MOVE 'E'                            TO T140-LANGUAGE
           MOVE CAA-ENT-ACC                    TO T140-ENTITY
           MOVE V041-COD-PROD                  TO T140-KEY-TABLE(1:2); 
           MOVE V041-COD-SPROD                 TO T140-KEY-TABLE(3:4); 
      * DECLARE DECLARE @BAZ037-I
           IF TB-T140-DES-TABLE(1);              EQUAL SPACES OR
              TB-T140-DES-TABLE(1);              EQUAL LOW-VALUES
      *
              CONTINUE
           ELSE
              MOVE TB-T140-DES-TABLE(1);         TO T140-DES-TABLE
              MOVE 'MBE0011'                   TO CAA-COD-ERROR
              MOVE 'CONSULTA NO PUEDE'         TO CAA-ERR-VARIA1
              MOVE 'SER WALLET'                TO CAA-ERR-VARIA2
              PERFORM 30000-FIN
           END-IF.

      *    PERFORM QUERY-BGDT140
      *
      *    MOVE SQLCODE                        TO SQL-VALUES
      *    EVALUATE TRUE
      *      WHEN SQL-88-OK
      *        MOVE 'MBE0011'                 TO CAA-COD-ERROR
      *        MOVE 'CONSULTA NO PUEDE'       TO CAA-ERR-VARIA1
      *        MOVE 'SER WALLET'              TO CAA-ERR-VARIA2
      *        PERFORM 30000-FIN
      *      WHEN OTHER
      *        CONTINUE
      *    END-EVALUATE
      *.
      * DECLARE DECLARE @BAZ037-F
      *
      ******************************************************************
      *QUERY-BGDT140.                                                  *
      ******************************************************************
       QUERY-BGDT140.
      *
           EXEC SQL
                SELECT  T140_DES_TABLE
                  INTO :T140-DES-TABLE
                  FROM  BGDT140 with(nolock); 
                 WHERE  T140_KEY_TABLE = :T140-KEY-TABLE
                   AND  T140_COD_TABLE = :T140-COD-TABLE
                   AND  T140_LANGUAGE  = :T140-LANGUAGE
                   AND  T140_ENTITY    = :T140-ENTITY
           END-EXEC
           .
      *
      * DECLARE DECLARE @BAZ021-FIN
      ******************************************************************
      *               25002-LEER-BGDC0071                              *
      * FETCH A LA TABLA BGDT071 PARA CONSULTAR LAS OPERACIONES DEL DIA*
      ******************************************************************
       25002-LEER-BGDC0071.
      * DECLARE DECLARE @BAZ037-I
           IF I-REG                      <= CT-15-9
             IF TB-T071-NUM-OPERATION2(I-REG);  EQUAL TO SPACES OR
                TB-T071-NUM-OPERATION2(I-REG);  EQUAL TO LOW-VALUES
      *
      *         PERFORM 25100-MOVTOS-BGDT710
                SET WSS-FIN-S1                TO TRUE
             ELSE
                MOVE TB-VA-FETCH-LLAVE(I-REG);      TO VA-FETCH-LLAVE
                MOVE TB-T071-NUM-OPERATION(I-REG);  TO T071-NUM-OPERATION
                MOVE TB-T071-DAT-OPERATION(I-REG);  TO VA-FECHA-OPERACION
                MOVE TB-T071-DAT-VALUE(I-REG);      TO VA-DAT-VALUE
      * DECLARE DECLARE @BAZ063-INI
      *         MOVE TB-T071-TIM-OPERATION(I-REG);  TO T071-TIM-OPERATION
                MOVE TB-T071-TIM-OPERATION(I-REG);  TO
                                                  VA-T071-TIM-OPERATION
      * DECLARE DECLARE @BAZ063-FIN
                MOVE ZERO                         TO VA-CUANTOS-MENOS
                INSPECT TB-T071-AMOUNT2(I-REG);  TALLYING VA-CUANTOS-MENOS
                         FOR ALL '-'
                IF VA-CUANTOS-MENOS > 0
                   INSPECT TB-T071-AMOUNT2(I-REG);  REPLACING ALL '-' BY
                                                             ''
                   MOVE TB-T071-AMOUNT(I-REG);         TO T071-AMT
                   COMPUTE T071-AMT= T071-AMT * -1
                ELSE
                   MOVE TB-T071-AMOUNT(I-REG);         TO T071-AMT
                END-IF


                MOVE TB-T071-CODE(I-REG);           TO AUX-VA-COD-MOV
                MOVE TB-T071-OBSERVATIONS(I-REG);   TO T071-OBSERVATIONS
                MOVE TB-T071-COD-PRODUCT(I-REG);    TO T071-COD-PRODUCT
                MOVE TB-T071-COD-SPROD(I-REG);      TO T071-COD-SPROD
                MOVE TB-T071-FLG-FREE1(I-REG);      TO T071-FLG-FREE1
                MOVE TB-T071-USERUPD(I-REG);        TO T071-USERUPD
                MOVE TB-T071-NTNMUPD(I-REG);        TO T071-NETNAMEUPD
                MOVE TB-T071-INTREF(I-REG);         TO T071-INTREF
      * DECLARE DECLARE @BAZ063-INI
                IF AUX-VA-COD-MOV          EQUAL CA-114
                   PERFORM ACCESO-MP9C0009
                ELSE
                   INITIALIZE VA-CARGOS-REC
                              VA-MPWC0009
                END-IF
      * DECLARE DECLARE @BAZ063-FIN
                ADD WSCN-1   TO VN-LEIDOS
                IF VN-LEIDOS <= CA-LIMITE
                  INITIALIZE WSV-AUXSAL
                  MOVE VA-FETCH-LLAVE     TO VA-AUX-LLAVE
                  MOVE T071-NUM-OPERATION TO WSV-AUX-NUMOPE AUX-SEQ-T04
      * DECLARE DECLARE @BAZ038 - I
                                             VN-NUM-OPERATION
      * DECLARE DECLARE @BAZ038 - F
                  MOVE VA-FECHA-OPERACION TO WSV-AUX-FECHA
      * DECLARE DECLARE @BAZ063-INI
      *           MOVE T071-TIM-OPERATION TO WSV-AUX-HORA
                  MOVE VA-T071-TIM-OPERATION TO WSV-AUX-HORA
      * DECLARE DECLARE @BAZ063-FIN
                  MOVE T071-AMT           TO WSV-AUX-IMPT AUX-AMT-COMP3
                  MOVE T071-OBSERVATIONS  TO WSV-AUX-DESC
                                             AUX-DESC
      * DECLARE DECLARE @BAZ070-I
                  IF  SW-BAN710
                      MOVE CA-C7          TO VA-AUX-TCONS
                  ELSE
                      MOVE CA-CU          TO VA-AUX-TCONS
                  END-IF
      * DECLARE DECLARE @BAZ070-F
                  MOVE T071-INTREF        TO AUX-INTREF71
                  MOVE T071-USERUPD       TO AUX-USERUPD
                  MOVE T071-FLG-FREE1     TO AUX-CANAL071
                  MOVE T071-NETNAMEUPD    TO AUX-TERMINAL
                                             AUX-NETNAMEUPD
      *
                  MOVE T071-COD-PRODUCT   TO AUX-PRODSPRD(1:2); 
                  MOVE T071-COD-SPROD     TO AUX-PRODSPRD(3:4); 
                  MOVE AUX-VA-COD-MOV     TO VA-COD-MOV

      *
                  IF VN-LEIDOS < CA-LIMITE
      *
                    PERFORM 29980-MOVER-SALIDA
                    PERFORM 29990-MOVER-SALIDA-2
                    PERFORM 29991-MOVER-SALIDA-3
                    PERFORM 29992-MOVER-SALIDA-4
                    IF SW-WALLET
                       PERFORM 29993-MOVER-SALIDA-5
                    END-IF
                  END-IF
                ELSE
                  SET WSS-FIN-S1          TO TRUE
                  SET WSS-SI-PAGINA       TO TRUE
                  MOVE CA-S               TO VA-IND-PAGINA
      *
                  PERFORM 29980-MOVER-SALIDA
                  PERFORM 29990-MOVER-SALIDA-2
                  PERFORM 29991-MOVER-SALIDA-3
                  PERFORM 29992-MOVER-SALIDA-4
                  IF SW-WALLET
                     PERFORM 29993-MOVER-SALIDA-5
                  END-IF
      *
                END-IF

             END-IF
             ADD +1                           TO I-REG
           ELSE
             SET WSS-FIN-S1                   TO TRUE
             SET WSS-SI-PAGINA                TO TRUE
             MOVE CA-S                        TO VA-IND-PAGINA
      *BAZ044-I
             SUBTRACT 1                       FROM I-REG
      *BAZ044-F
             PERFORM 29980-MOVER-SALIDA
             PERFORM 29990-MOVER-SALIDA-2
             PERFORM 29991-MOVER-SALIDA-3
             PERFORM 29992-MOVER-SALIDA-4
             IF SW-WALLET
                PERFORM 29993-MOVER-SALIDA-5
             END-IF
             SET WSS-FIN-S1                   TO TRUE
           END-IF.
      *
      *    EXEC SQL
      *       FETCH BGDC0071
      *        INTO :VA-FETCH-LLAVE    ;
      *             :T071-NUM-OPERATION;
      *             :VA-FECHA-OPERACION;
      *             :VA-DAT-VALUE;
      *             :T071-TIM-OPERATION;
      *             :T071-AMT          ;
      *             :AUX-VA-COD-MOV    ;
      *             :T071-INTREF       ;
      *             :T071-OBSERVATIONS ;
      *             :T071-COD-PRODUCT  ;
      *             :T071-COD-SPROD    ;
      *             :T071-FLG-FREE1    ;
      * DECLARE DECLARE @BAZ016-->INI
      *             :T071-USERUPD      ;
      * DECLARE DECLARE @BAZ016<--FIN
      *             :T071-NETNAMEUPD
      *    END-EXEC
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *      WHEN SQL-88-OK
      *         ADD WSCN-1   TO VN-LEIDOS
      *         IF VN-LEIDOS <= CA-LIMITE
      *           INITIALIZE WSV-AUXSAL
      *           MOVE VA-FETCH-LLAVE     TO VA-AUX-LLAVE
      *           MOVE T071-NUM-OPERATION TO WSV-AUX-NUMOPE AUX-SEQ-T04
      *           MOVE VA-FECHA-OPERACION TO WSV-AUX-FECHA
      *           MOVE T071-TIM-OPERATION TO WSV-AUX-HORA
      *           MOVE T071-AMT           TO WSV-AUX-IMPT AUX-AMT-COMP3
      *           MOVE T071-OBSERVATIONS  TO WSV-AUX-DESC
      * DECLARE DECLARE @BAZ015-->INI
      *                                      AUX-DESC
      * DECLARE DECLARE @BAZ015<--FIN
      *           MOVE CA-CU              TO VA-AUX-TCONS
      *           MOVE T071-INTREF        TO AUX-INTREF71
      * DECLARE DECLARE @BAZ016-->INI
      *           MOVE T071-USERUPD       TO AUX-USERUPD
      * DECLARE DECLARE @BAZ016<--FIN
      *           MOVE T071-FLG-FREE1     TO AUX-CANAL071
      *           MOVE T071-NETNAMEUPD    TO AUX-TERMINAL
      * DECLARE DECLARE @BAZ023.I
      *                                      AUX-NETNAMEUPD
      * DECLARE DECLARE @BAZ023.F
      *
      * DECLARE DECLARE @BAZ005.I
      *           MOVE T071-COD-PRODUCT   TO AUX-PRODSPRD(1:2); 
      *           MOVE T071-COD-SPROD     TO AUX-PRODSPRD(3:4); 
      * DECLARE DECLARE @BAZ005.F /  DECLARE DECLARE @BAZ005C
      *           MOVE AUX-VA-COD-MOV     TO VA-COD-MOV
      *
      *
      *           IF VN-LEIDOS < CA-LIMITE
      *
      *             PERFORM 29980-MOVER-SALIDA
      *             PERFORM 29990-MOVER-SALIDA-2
      *LCR-INI2
      *             PERFORM 29991-MOVER-SALIDA-3
      *LCR-FIN2
      * DECLARE DECLARE @BAZ023.I
      *             PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
      *             IF SW-WALLET
      *                PERFORM 29993-MOVER-SALIDA-5
      *             END-IF
      *           END-IF
      * DECLARE DECLARE @BAZ027-F
      *         ELSE
      *           SET WSS-FIN-S1          TO TRUE
      *           SET WSS-SI-PAGINA       TO TRUE
      *           MOVE CA-S               TO VA-IND-PAGINA
      *
      *           PERFORM 29980-MOVER-SALIDA
      *           PERFORM 29990-MOVER-SALIDA-2
      *LCR-INI2
      *           PERFORM 29991-MOVER-SALIDA-3
      *LCR-FIN2
      * DECLARE DECLARE @BAZ023.I
      *           PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
      *           IF SW-WALLET
      *              PERFORM 29993-MOVER-SALIDA-5
      *           END-IF
      *
      *         END-IF
      *      WHEN SQL-88-NOT-FOUND
      *         PERFORM 25100-MOVTOS-BGDT710
      *         SET WSS-FIN-S1            TO TRUE
      *      WHEN OTHER
      *         MOVE CA-BGDT071           TO ABC-OBJECT-ERROR
      *         MOVE CA-FETCH             TO ABC-REFERENCE1
      *         MOVE SQLCODE              TO ABC-SQLCODE
      *         PERFORM 999999-DB2-ABEND
      *    END-EVALUATE.
      * DECLARE DECLARE @BAZ037-F
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ057-I
      ******************************************************************
      *                    25100-MOVTOS-BGDT710                        *
      *REALIZA FETCH A LA TABLA BGDT710 PARA CONSULTAR EL HISTORICO DE *
      *LAS OPERACIONES DE UNA CUENTA.                                  *
      ******************************************************************
      *25100-MOVTOS-BGDT710.
      *
      *    MOVE VA-FECHA-CALC             TO V071-DAT-OPERATION
      *    MOVE E009-NUMCUEN(5:10);         TO V071-ACC
      *    MOVE E009-NUMCUEN(1:4);          TO V071-CEN-REG
      *    MOVE CA-0127                   TO V071-ENT
      *
      *    PERFORM 25101-ABRE-BGDC0710
      *    PERFORM 25102-LEER-BGDC0710 UNTIL WSS-FIN-S2
      *                                OR VN-CONTREG >= CN-99
      *    PERFORM 25103-CERR-BGDC0710.
      *
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ057-F
      ******************************************************************
      *               25101-ABRE-BGDC0710                              *
      ******************************************************************
      *25101-ABRE-BGDC0710.
      *
      *    EXEC SQL
      *       OPEN BGDC0710
      *    END-EXEC
      *
      *    MOVE SQLCODE       TO SQL-VALUES
      *
      *    IF NOT SQL-88-OK
      *       MOVE 'MDE0026'  TO CAA-COD-ERROR
      *       PERFORM 30000-FIN
      *    END-IF.
      *
      ******************************************************************
      *               25102-LEER-BGDC0710                              *
      *REALIZA FETCH A LA TABLA BGDT710 PARA CONSULTAR EL HISTORICO DE *
      *LAS OPERACIONES DE UNA CUENTA.                                  *
      ******************************************************************
      *25102-LEER-BGDC0710.
      *
      *    EXEC SQL
      *       FETCH BGDC0710
      *        INTO :VA-FETCH-LLAVE    ;
      *             :V071-NUM-OPERATION;
      *             :VA-FECHA-OPERACION;
      *             :VA-DAT-VALUE      ;
      *             :V071-TIM-OPERATION;
      *             :V071-AMT          ;
      * DECLARE DECLARE @BAZ001-->INI
      *             :VA-COD-MOV        ;
      * DECLARE DECLARE @BAZ001<--FIN /  DECLARE DECLARE @BAZ005C
      *             :AUX-VA-COD-MOV    ;
      *             :V071-INTREF       ;
      *             :V071-OBSERVATIONS ;
      *             :V071-COD-PRODUCT  ;
      *             :V071-COD-SPROD    ;
      *             :V071-FLG-FREE1    ;
      * DECLARE DECLARE @BAZ016-->INI
      *             :V071-USERUPD      ;
      * DECLARE DECLARE @BAZ016<--FIN
      *             :V071-NETNAMEUPD
      *    END-EXEC
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *      WHEN SQL-88-OK
      *         ADD WSCN-1   TO VN-LEIDOS
      *         IF VN-LEIDOS <= CA-LIMITE
      *           INITIALIZE WSV-AUXSAL
      *           MOVE VA-FETCH-LLAVE     TO VA-AUX-LLAVE
      *           MOVE V071-NUM-OPERATION TO WSV-AUX-NUMOPE AUX-SEQ-T04
      *           MOVE VA-FECHA-OPERACION TO WSV-AUX-FECHA
      *           MOVE V071-TIM-OPERATION TO WSV-AUX-HORA
      *           MOVE V071-AMT           TO WSV-AUX-IMPT AUX-AMT-COMP3
      *           MOVE V071-OBSERVATIONS  TO WSV-AUX-DESC
      * DECLARE DECLARE @BAZ015-->INI
      *                                      AUX-DESC
      * DECLARE DECLARE @BAZ015<--FIN
      *           MOVE CA-CU              TO VA-AUX-TCONS
      *           MOVE V071-INTREF        TO AUX-INTREF71
      * DECLARE DECLARE @BAZ016-->INI
      *           MOVE V071-USERUPD       TO AUX-USERUPD
      * DECLARE DECLARE @BAZ016<--FIN
      *           MOVE V071-FLG-FREE1     TO AUX-CANAL071
      *           MOVE V071-NETNAMEUPD    TO AUX-TERMINAL
      * DECLARE DECLARE @BAZ023.I
      *                                      AUX-NETNAMEUPD
      * DECLARE DECLARE @BAZ023.F
      *
      * DECLARE DECLARE @BAZ005.I
      *           MOVE V071-COD-PRODUCT   TO AUX-PRODSPRD(1:2); 
      *           MOVE V071-COD-SPROD     TO AUX-PRODSPRD(3:4); 
      * DECLARE DECLARE @BAZ005.F /  DECLARE DECLARE @BAZ005C
      *           MOVE AUX-VA-COD-MOV     TO VA-COD-MOV
      *
      *           IF VN-LEIDOS < CA-LIMITE
      *
      *             PERFORM 29980-MOVER-SALIDA
      *             PERFORM 29990-MOVER-SALIDA-2
      *LCR-INI2
      *             PERFORM 29991-MOVER-SALIDA-3
      *LCR-FIN2
      * DECLARE DECLARE @BAZ023.I
      *             PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
      *             IF SW-WALLET
      *                PERFORM 29993-MOVER-SALIDA-5
      *             END-IF
      *           END-IF
      *
      *         ELSE
      *           SET WSS-FIN-S2          TO TRUE
      *           SET WSS-SI-PAGINA       TO TRUE
      *           MOVE CA-S               TO VA-IND-PAGINA
      *
      *           PERFORM 29980-MOVER-SALIDA
      *           PERFORM 29990-MOVER-SALIDA-2
      *LCR-INI2
      *           PERFORM 29991-MOVER-SALIDA-3
      *LCR-FIN2
      * DECLARE DECLARE @BAZ023.I
      *             PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
      *             IF SW-WALLET
      *                PERFORM 29993-MOVER-SALIDA-5
      *             END-IF
      *
      *         END-IF
      *      WHEN SQL-88-NOT-FOUND
      *         MOVE CA-MAXLLAV           TO VA-ULT-LLAVE
      *         PERFORM 26000-RETENCION-CUENTA
      * DECLARE DECLARE @BAZ027-I
      *         IF VN-LEIDOS = CA-LIMITE
      *
      *            PERFORM 29980-MOVER-SALIDA
      *            PERFORM 29990-MOVER-SALIDA-2
      *            PERFORM 29991-MOVER-SALIDA-3
      *            PERFORM 29992-MOVER-SALIDA-4
      *            IF SW-WALLET
      *               PERFORM 29993-MOVER-SALIDA-5
      *            END-IF
      *         END-IF
      *         SET WSS-NO-PAGINA       TO TRUE
      * DECLARE DECLARE @BAZ027-F
      *         SET WSS-FIN-S2            TO TRUE
      *      WHEN OTHER
      *         MOVE CA-BGDT710           TO ABC-OBJECT-ERROR
      *         MOVE CA-FETCH             TO ABC-REFERENCE1
      *         MOVE SQLCODE              TO ABC-SQLCODE
      *         PERFORM 999999-DB2-ABEND
      *    END-EVALUATE
      *    .
      *
      ******************************************************************
      *               25103-CERR-BGDC0710                              *
      ******************************************************************
      *25103-CERR-BGDC0710.
      *
      *    EXEC SQL
      *       CLOSE BGDC0710
      *    END-EXEC
      *
      *    MOVE SQLCODE       TO SQL-VALUES
      *
      *    IF NOT SQL-88-OK
      *       MOVE 'MDE0027'  TO CAA-COD-ERROR
      *       PERFORM 30000-FIN
      *    END-IF.
      *
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ057-F
      * DECLARE DECLARE @BAZ063-INI
      ******************************************************************
      *ACCESO-MP9C0009.
      * IDENTIFICA MOVIMIENTO CON CARGO RECURRENTE
      ******************************************************************
       ACCESO-MP9C0009.
      *
           INITIALIZE VA-CARGOS-REC
                      VA-MPWC0009
           MOVE T071-ENT                 TO E109-CUENTA(1:4); 
           MOVE T071-CEN-REG             TO E109-CUENTA(5:4); 
           MOVE T071-ACC                 TO E109-CUENTA(9:10); 

           MOVE T071-NUM-OPERATION       TO E109-NUM-OPE
      *
           CALL CA-MP9C0009 USING VA-MPWC0009
      *
           EVALUATE E109-COD-RETURN
           WHEN 00
           WHEN 10
             IF E109-RECURRENTE='CARGO RECURRENTE'
               MOVE E109-NUM-OPE-DEB       TO VA-S109-NUM-OPE-DEB
               MOVE E109-TIPO-OPE          TO VA-S109-TIPO-OPE
               MOVE E109-NUM-AUTO          TO VA-S109-NUM-AUTO
               MOVE E109-COD-FCC           TO VA-S109-COD-FCC
               MOVE E109-NUM-REFER         TO VA-S109-NUM-REFER
               MOVE E109-COD-ACT           TO VA-S109-COD-ACT
               MOVE E109-NUM-NEGOC         TO VA-S109-NUM-NEGOC
               MOVE E109-OPE-ORI           TO VA-S109-OPE-ORI
               MOVE E109-NUM-DECLA         TO VA-S109-NUM-DECLA
               MOVE E109-NUM-TRANS         TO VA-S109-NUM-TRANS
               MOVE E109-NUM-CARD          TO VA-S109-NUM-CARD
               MOVE E109-RECURRENTE        TO VA-S109-RECURRENTE
             ELSE
               MOVE SPACES                 TO VA-S109-NUM-OPE-DEB
                                              VA-S109-TIPO-OPE
                                              VA-S109-NUM-AUTO
                                              VA-S109-COD-FCC
                                              VA-S109-NUM-REFER
                                              VA-S109-COD-ACT
                                              VA-S109-NUM-NEGOC
                                              VA-S109-OPE-ORI
                                              VA-S109-NUM-DECLA
                                              VA-S109-NUM-TRANS
                                              VA-S109-NUM-CARD
                                              VA-S109-RECURRENTE
            END-IF
           WHEN OTHER
               MOVE SPACES                 TO VA-S109-NUM-OPE-DEB
                                              VA-S109-TIPO-OPE
                                              VA-S109-NUM-AUTO
                                              VA-S109-COD-FCC
                                              VA-S109-NUM-REFER
                                              VA-S109-COD-ACT
                                              VA-S109-NUM-NEGOC
                                              VA-S109-OPE-ORI
                                              VA-S109-NUM-DECLA
                                              VA-S109-NUM-TRANS
                                              VA-S109-NUM-CARD
                                              VA-S109-RECURRENTE
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ063-FIN

      ******************************************************************
      *              26000-RETENCION-CUENTA                            *
      ******************************************************************
       26000-RETENCION-CUENTA.
      *
           MOVE VA-FECHA-CALC          TO T089-DAT-REG
           MOVE CA-0127                TO T089-ENT
           MOVE E009-NUMCUEN(1:4);       TO T089-CEN-REG
           MOVE E009-NUMCUEN(5:10);      TO T089-ACC
      *
      * DECLARE DECLARE @BAZ021-INI
           PERFORM OBTEN-CUENTA-20POS
      * DECLARE DECLARE @BAZ027-I
           MOVE AUX-CTA-INPUT                      TO VA-CUENTA-AUX
      * DECLARE DECLARE @BAZ027-F
           IF SW-BAZ
               PERFORM 25100-VALIDA-SPROD-WALLET
           END-IF
      * DECLARE DECLARE @BAZ021-FIN
      *    PERFORM 26001-ABRIR-BGDC0089
           MOVE CA-1                   TO I-REG
           PERFORM 26002-LEER-BGDC0089 UNTIL WSS-FIN-S3
                                       OR VN-CONTREG = CN-99
      *    PERFORM 26003-CERRAR-BGDC0089
      *
           EVALUATE TRUE
             WHEN VN-CONTREG = 0
                MOVE 'MCE0004'         TO CAA-COD-ERROR
             WHEN VN-CONTREG = CN-99
             WHEN WSS-NO-PAGINA
                MOVE 'MCA0003'         TO CAA-COD-AVISO1
             WHEN WSS-SI-PAGINA
                MOVE 'MCA0022'         TO CAA-COD-AVISO1
           END-EVALUATE.
      *
      * DECLARE DECLARE @BAZ043-INI
      ******************************************************************
      *               26001-ABRIR-BGDC0089                             *
      ******************************************************************
      *26001-ABRIR-BGDC0089.
      *
      *    EXEC SQL
      *       OPEN BGDC0089
      *    END-EXEC
      *
      *    MOVE SQLCODE       TO SQL-VALUES
      *
      *    IF NOT SQL-88-OK
      *       MOVE 'MDE0026'  TO CAA-COD-ERROR
      *       PERFORM 30000-FIN
      *    END-IF.
      *
      * DECLARE DECLARE @BAZ043-FIN
      ******************************************************************
      *               26002-LEER-BGDC0089                              *
      * REALIZA FETCH A LA TABLA BGDT089 PARA CONSULTAR LAS RETENCIONES*
      * DE UNA CUENTA.                                                 *
      ******************************************************************
       26002-LEER-BGDC0089.
      * DECLARE DECLARE @BAZ037-I
           IF I-REG                      <= CT-15-9
               IF TB-T089-NUM-WHD2(I-REG);  EQUAL TO SPACES OR
                  TB-T089-NUM-WHD2(I-REG);  EQUAL TO LOW-VALUES

                   IF VN-LEIDOS              EQUAL CA-LIMITE
                       MOVE T089-COD          TO VA-COD-MOV
                       PERFORM 29980-MOVER-SALIDA
                       PERFORM 29990-MOVER-SALIDA-2
                       PERFORM 29991-MOVER-SALIDA-3
                       PERFORM 29992-MOVER-SALIDA-4
                       IF SW-WALLET
                           PERFORM 29993-MOVER-SALIDA-5
                       END-IF
                   END-IF

                   SET WSS-NO-PAGINA         TO TRUE
                   SET WSS-FIN-S3            TO TRUE

               ELSE
                   MOVE TB-RET-VA-FETCH-LLAVE(I-REG); 
                                               TO VA-FETCH-LLAVE
                   MOVE TB-T089-NUM-WHD(I-REG);  TO T089-NUM-WHD
                   MOVE TB-T089-DAT-REG(I-REG);  TO VA-FECHA-OPERACION
                   MOVE TB-T089-TIM-REG(I-REG);  TO T089-TIM-REG
                   MOVE ZERO                   TO VA-CUANTOS-MENOS
              INSPECT TB-T089-AMT-ORIGIN2(I-REG); 
                      TALLYING VA-CUANTOS-MENOS FOR ALL '-'
              IF VA-CUANTOS-MENOS > 0
                 INSPECT TB-T089-AMT-ORIGIN(I-REG);  REPLACING ALL '-' BY
                                                           ''
                 MOVE TB-T089-AMT-ORIGIN(I-REG);  TO T089-AMT-ORIGIN
                 COMPUTE T089-AMT-ORIGIN= T089-AMT-ORIGIN * -1
              ELSE
                 MOVE TB-T089-AMT-ORIGIN(I-REG);  TO T089-AMT-ORIGIN
              END-IF

              MOVE ZERO                         TO VA-CUANTOS-MENOS
              INSPECT TB-T089-AMT-CURRENT2(I-REG); 
                      TALLYING VA-CUANTOS-MENOS FOR ALL '-'
              IF VA-CUANTOS-MENOS > 0
                 INSPECT TB-T089-AMT-CURRENT2(I-REG); 
                                              REPLACING ALL '-' BY ''
                 MOVE TB-T089-AMT-CURRENT(I-REG);  TO T089-AMT-CURRENT
                 COMPUTE T089-AMT-CURRENT= T089-AMT-CURRENT * -1
              ELSE
      *
      *          MOVE TB-T089-AMT-ORIGIN(I-REG);  TO T089-AMT-CURRENT
                 MOVE TB-T089-AMT-CURRENT(I-REG);  TO T089-AMT-CURRENT
      *
              END-IF

                   MOVE TB-T089-CODE(I-REG);   TO T089-COD
                   MOVE TB-T089-OBSERVATIONS(I-REG); 
                                             TO T089-OBSERVATIONS

                   ADD WSCN-1   TO VN-LEIDOS
                   IF VN-LEIDOS <= CA-LIMITE
                     INITIALIZE WSV-AUXSAL
                     MOVE VA-FETCH-LLAVE     TO VA-AUX-LLAVE
                     MOVE T089-NUM-WHD       TO WSV-AUX-NUMOPE
                     MOVE VA-FECHA-OPERACION TO WSV-AUX-FECHA
                     MOVE T089-TIM-REG(1:2);   TO WSV-AUX-HORA(1:2); 
                     MOVE T089-TIM-REG(4:2);   TO WSV-AUX-HORA(3:2); 
      * DECLARE DECLARE @BAZ071-INI
                     MOVE T089-TIM-REG(7:2);   TO WSV-AUX-HORA(5:2); 
      * DECLARE DECLARE @BAZ071-FIN
                     IF T089-COD EQUAL CA-COD-Y84
                       MOVE T089-AMT-CURRENT TO WSV-AUX-IMPT
                                                AUX-AMT-COMP3
                     ELSE
                       MOVE T089-AMT-ORIGIN  TO WSV-AUX-IMPT
                                                AUX-AMT-COMP3
                     END-IF

      * DECLARE DECLARE @BAZ047-INI
                     IF T089-COD EQUAL CA-COD-Z28
                       MOVE T089-AMT-CURRENT TO WSV-AUX-IMPT
                                                AUX-AMT-COMP3
                     END-IF
      * DECLARE DECLARE @BAZ047-FIN

                     STRING T089-OBSERVATIONS DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                                             INTO WSV-AUX-DESC
                     MOVE CA-RC              TO VA-AUX-TCONS
                     PERFORM EDICION-DATOS-CODRET
                     IF VN-LEIDOS < CA-LIMITE
                       MOVE T089-COD         TO VA-COD-MOV
                       PERFORM 29980-MOVER-SALIDA
                       PERFORM 29990-MOVER-SALIDA-2
                       PERFORM 29991-MOVER-SALIDA-3
                       PERFORM 29992-MOVER-SALIDA-4
                       IF SW-WALLET
                          PERFORM 29993-MOVER-SALIDA-5
                       END-IF
                     END-IF
                   ELSE
                     SET WSS-FIN-S3          TO TRUE
                     SET WSS-SI-PAGINA       TO TRUE
                     MOVE CA-S               TO VA-IND-PAGINA
                     MOVE T089-COD           TO VA-COD-MOV
                     PERFORM 29980-MOVER-SALIDA
                     PERFORM 29990-MOVER-SALIDA-2
                     PERFORM 29991-MOVER-SALIDA-3
                     PERFORM 29992-MOVER-SALIDA-4
                     IF SW-WALLET
                          PERFORM 29993-MOVER-SALIDA-5
                     END-IF
                   END-IF
                   ADD +1                    TO I-REG
               END-IF
           ELSE
              SET WSS-FIN-S3          TO TRUE
              SET WSS-SI-PAGINA       TO TRUE
              MOVE CA-S               TO VA-IND-PAGINA
              MOVE T089-COD           TO VA-COD-MOV
              PERFORM 29980-MOVER-SALIDA
              PERFORM 29990-MOVER-SALIDA-2
              PERFORM 29991-MOVER-SALIDA-3
              PERFORM 29992-MOVER-SALIDA-4
              IF SW-WALLET
                   PERFORM 29993-MOVER-SALIDA-5
              END-IF
           END-IF.

      *    EXEC SQL
      *       FETCH BGDC0089
      *        INTO :VA-FETCH-LLAVE    ;
      *             :T089-NUM-WHD      ;
      *             :VA-FECHA-OPERACION;
      *             :T089-TIM-REG      ;
      *             :T089-AMT-ORIGIN   ;
      *             :T089-AMT-CURRENT  ;
      *             :T089-COD          ;
      *             :T089-OBSERVATIONS
      *    END-EXEC
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *      WHEN SQL-88-OK
      *         ADD WSCN-1   TO VN-LEIDOS
      *         IF VN-LEIDOS <= CA-LIMITE
      *           INITIALIZE WSV-AUXSAL
      *           MOVE VA-FETCH-LLAVE     TO VA-AUX-LLAVE
      *           MOVE T089-NUM-WHD       TO WSV-AUX-NUMOPE
      *           MOVE VA-FECHA-OPERACION TO WSV-AUX-FECHA
      *MCV001-INI
      *           MOVE T089-TIM-REG       TO WSV-AUX-HORA
      *           MOVE T089-TIM-REG(1:2);   TO WSV-AUX-HORA(1:2); 
      *           MOVE T089-TIM-REG(4:2);   TO WSV-AUX-HORA(3:2); 
      *MCV001-FIN
      * DECLARE DECLARE @BAZ031-I
      *           IF T089-COD EQUAL CA-COD-Y84
      *             MOVE T089-AMT-CURRENT TO WSV-AUX-IMPT
      * DECLARE DECLARE @BAZ033-I
      *                                      AUX-AMT-COMP3
      * DECLARE DECLARE @BAZ033-F
      *           ELSE
      *             MOVE T089-AMT-ORIGIN  TO WSV-AUX-IMPT
      * DECLARE DECLARE @BAZ033-I
      *                                      AUX-AMT-COMP3
      * DECLARE DECLARE @BAZ033-F
      *           END-IF
      *           MOVE T089-AMT-ORIGIN    TO WSV-AUX-IMPT
      * DECLARE DECLARE @BAZ031-F
      *           MOVE T089-OBSERVATIONS  TO WSV-AUX-DESC
      *           STRING T089-OBSERVATIONS DELIMITED BY '  '
      *                                ' ' DELIMITED BY SIZE
      *                                   INTO WSV-AUX-DESC
      *           MOVE CA-RC              TO VA-AUX-TCONS
      * DECLARE DECLARE @BAZ005.I
      *           PERFORM EDICION-DATOS-CODRET
      * DECLARE DECLARE @BAZ005.F
      *           IF VN-LEIDOS < CA-LIMITE
      * DECLARE DECLARE @BAZ017-->INI
      *             MOVE T089-COD         TO VA-COD-MOV
      * DECLARE DECLARE @BAZ017<--FIN
      *             PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
      *             PERFORM 29990-MOVER-SALIDA-2
      *             PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ023.I
      *             PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
      *             IF SW-WALLET
      *                PERFORM 29993-MOVER-SALIDA-5
      *             END-IF
      * DECLARE DECLARE @BAZ027-F
      *           END-IF
      *         ELSE
      *           SET WSS-FIN-S3          TO TRUE
      *           SET WSS-SI-PAGINA       TO TRUE
      *           MOVE CA-S               TO VA-IND-PAGINA
      * DECLARE DECLARE @BAZ017-->INI
      *             MOVE T089-COD         TO VA-COD-MOV
      * DECLARE DECLARE @BAZ017<--FIN
      *           PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
      *           PERFORM 29990-MOVER-SALIDA-2
      *           PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ023.I
      *             PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
      *             IF SW-WALLET
      *                PERFORM 29993-MOVER-SALIDA-5
      *             END-IF
      * DECLARE DECLARE @BAZ027-F
      *         END-IF
      *      WHEN SQL-88-NOT-FOUND
      *         IF VN-LEIDOS = CA-LIMITE
      * DECLARE DECLARE @BAZ017-->INI
      *             MOVE T089-COD         TO VA-COD-MOV
      * DECLARE DECLARE @BAZ017<--FIN
      *           PERFORM 29980-MOVER-SALIDA
      * DECLARE DECLARE @BAZ017-->INI
      *           PERFORM 29990-MOVER-SALIDA-2
      *           PERFORM 29991-MOVER-SALIDA-3
      * DECLARE DECLARE @BAZ017<--FIN
      * DECLARE DECLARE @BAZ023.I
      *             PERFORM 29992-MOVER-SALIDA-4
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
      *             IF SW-WALLET
      *                PERFORM 29993-MOVER-SALIDA-5
      *             END-IF
      * DECLARE DECLARE @BAZ027-F
      *
      *         END-IF
      *         SET WSS-NO-PAGINA         TO TRUE
      *         SET WSS-FIN-S3            TO TRUE
      *      WHEN OTHER
      *         MOVE CA-BGDT089           TO ABC-OBJECT-ERROR
      *         MOVE CA-FETCH             TO ABC-REFERENCE1
      *         MOVE SQLCODE              TO ABC-SQLCODE
      *         PERFORM 999999-DB2-ABEND
      *    END-EVALUATE
      *    .
      * DECLARE DECLARE @BAZ037-F
      * DECLARE DECLARE @BAZ043-INI
      ******************************************************************
      *               26003-CERRAR-BGDC0089                            *
      ******************************************************************
      *26003-CERRAR-BGDC0089.
      *
      *    EXEC SQL
      *       CLOSE BGDC0089
      *    END-EXEC
      *
      *    MOVE SQLCODE         TO SQL-VALUES
      *
      *    IF NOT SQL-88-OK
      *       MOVE 'MDE0027'    TO CAA-COD-ERROR
      *       PERFORM 30000-FIN
      *    END-IF.
      *
      * DECLARE DECLARE @BAZ043-FIN
      ******************************************************************
      *28888-TIPO-OPER
      ******************************************************************
       28888-TIPO-OPER.

           EVALUATE T043-COD-OPERATION
              WHEN '02'
                   MOVE 'RE'              TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '61'
              WHEN '60'
                   MOVE 'S'              TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '62'
                   MOVE 'T'              TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '63'
                   MOVE 'RH'             TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '64'
                   MOVE 'RQR'            TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '65'
                   MOVE 'TA'             TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '66'
                   MOVE 'CH'             TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '67'
                   MOVE 'DEX'            TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '68'
                   MOVE 'EC'             TO WSV-AUX-CLASIF
                   COMPUTE WSV-AUX-IMPT = (T043-AMT-OPERATION * -1); 
              WHEN '69'
                   MOVE 'TDC'            TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '71'
              WHEN '70'
                   MOVE 'DS'             TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '72'
                   MOVE 'DT'             TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '73'
                   MOVE 'RH'             TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '74'
                   MOVE 'RVQ'            TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '75'
                   MOVE 'RTA'            TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '76'
                   MOVE 'RC'             TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '77'
                   MOVE 'RCD'            TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '78'
                   MOVE 'NC'             TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
              WHEN '79'
                   MOVE 'RTC'             TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
           WHEN OTHER
                   MOVE 'NC'              TO WSV-AUX-CLASIF
                   MOVE  T043-AMT-OPERATION  TO  WSV-AUX-IMPT
           END-EVALUATE
           .
      *
      ******************************************************************
      *              29980-MOVER-SALIDA
      ******************************************************************
       29980-MOVER-SALIDA.
      *
           INITIALIZE MBNS0009
      *
           ADD WSCN-1                  TO VN-CONTREG
      * DECLARE DECLARE @BAZ022-INI
           IF VA-COD-MOV = '212' AND
              (WSV-AUX-DESC(1:22);  ='PORTABILIDAD DE NOMINA' OR
               WSV-AUX-DESC(1:22);  ='Portabilidad De Nomina'); 
              SET SW-PORTANOM-Y TO TRUE
              PERFORM CONSULTA-REFERENCIA-INTERB
           END-IF
      * DECLARE DECLARE @BAZ022-FIN
      * DECLARE DECLARE @BAZ048-INI
           IF VA-COD-MOV = '217'
              PERFORM CONSULTA-REFERENCIA-INTERB
           END-IF
      * DECLARE DECLARE @BAZ048-FIN
      *
      * DECLARE DECLARE @BAZ056-I
           IF VA-COD-MOV = '212' AND
              T071-NETNAMEUPD(5:4);  = CA-FS64
              PERFORM CONSULTA-REFERENCIA-INTERB
           END-IF
      * DECLARE DECLARE @BAZ056-F
      *
           MOVE WSV-AUX-FECHA          TO S009-FECHM01
           MOVE WSV-AUX-HORA           TO S009-HORAM01
           MOVE WSV-AUX-DESC           TO S009-DESCM01
           MOVE WSV-AUX-IMPT           TO S009-IMPTM01
           MOVE WSV-AUX-NUMOPE         TO S009-NOPEM01
           MOVE WSV-AUX-CLASIF         TO S009-TIPOPER
      *MCV003-I
      *    MOVE VN-CONTREG             TO S009-CONTREG
           MOVE VN-CONTREG             TO VN-AUX-TXT-R
           MOVE VN-AUX-TXT             TO S009-CONTREG
      *MCV003-F
           MOVE VA-AUX-PAGINA          TO S009-DATPAG
           MOVE VA-IND-PAGINA          TO S009-INDPAGI
      * DECLARE DECLARE @BAZ001-->INI
           IF VA-COD-MOV = '907'
              MOVE  '1'                TO S009-INDNOM
           ELSE
              MOVE  '0'                TO S009-INDNOM
           END-IF
      * DECLARE DECLARE @BAZ001<--FIN
      * DECLARE DECLARE @BAZ071-INI
           IF ((VA-COD-MOV = 'Z92' OR 'AB2');  AND
                T071-NETNAMEUPD(5:4);  = CA-MBWI); 
              PERFORM 2110-CONSULTA-OPERACION
           END-IF
      * DECLARE DECLARE @BAZ071-FIN
      * DECLARE DECLARE @BAZ051-I
           MOVE ZEROES                 TO VN-AUX1
           MOVE ZEROES                 TO VN-AUX2
           MOVE ZEROES                 TO VN-AUX3
      *
           IF WSV-AUX-DESC(1:8);  = 'PAGO TDC' AND
              (VA-COD-MOV = 'R81' OR VA-COD-MOV = '826'); 
      *
              IF WSV-AUX-DESC(9:1);  = ':'
                 MOVE 11   TO VN-AUX1
              ELSE
                 MOVE 10   TO VN-AUX1
              END-IF
      *
              IF WSV-AUX-DESC(VN-AUX1:1);  IS NUMERIC
                 INSPECT WSV-AUX-DESC(VN-AUX1:31); 
                 TALLYING VN-AUX2 FOR CHARACTERS BEFORE ' '
                 MOVE WSV-AUX-DESC(VN-AUX1:VN-AUX2); 
                                       TO VN-AUX3
                 MOVE SPACES           TO S009-DESCM01
                 MOVE WSV-AUX-DESC(1:VN-AUX1 - 1); 
                                       TO S009-DESCM01(1:VN-AUX1 - 1); 
                 MOVE '****'           TO S009-DESCM01(VN-AUX1:4); 
                 MOVE VN-AUX3          TO S009-DESCM01(VN-AUX1 + 4:4); 
              END-IF
           END-IF
      * DECLARE DECLARE @BAZ051-F
      * DECLARE DECLARE @BAZ043-INI

           IF WSV-AUX-DESC(01:05);  = 'ASP_2' AND
              (SW-169 OR VA-COD-MOV = '169'); 
      *       MOVE 'Apertura Socio Plus'         TO S009-DESCM01
              MOVE WSV-AUX-DESC(01:14);            TO S009-DESCM01
           END-IF

           IF WSV-AUX-DESC(01:06);  = 'RASP_2' AND
              (SW-160  OR VA-COD-MOV = '160'); 
      *       MOVE 'Reverso Apertura Socio Plus' TO S009-DESCM01
              MOVE WSV-AUX-DESC(01:14);            TO S009-DESCM01
           END-IF

      * DECLARE DECLARE @BAZ043-FIN


      * DECLARE DECLARE @BAZ045-INI
           IF WSV-AUX-DESC(01:05);  = 'CFP_2' AND
              (SW-169 OR VA-COD-MOV = '169'); 
      *       MOVE 'Apertura Socio Plus'         TO S009-DESCM01
              MOVE WSV-AUX-DESC(01:14);            TO S009-DESCM01
           END-IF

           IF WSV-AUX-DESC(01:06);  = 'VFP_2' AND
              (SW-160  OR VA-COD-MOV = '160'); 
      *       MOVE 'Reverso Apertura Socio Plus' TO S009-DESCM01
              MOVE WSV-AUX-DESC(01:14);            TO S009-DESCM01
           END-IF
      * DECLARE DECLARE @BAZ045-FIN
           INSPECT S009-DATPAG
                           REPLACING ALL SPACE BY '.'
      *
      * DECLARE DECLARE @BAZ063-INI
             MOVE VA-S109-NUM-OPE-DEB       TO S109-NUM-OPE-DEB
             MOVE VA-S109-TIPO-OPE          TO S109-TIPO-OPE
             MOVE VA-S109-NUM-AUTO          TO S109-NUM-AUTO
             MOVE VA-S109-COD-FCC           TO S109-COD-FCC
             MOVE VA-S109-NUM-REFER         TO S109-NUM-REFER
             MOVE VA-S109-COD-ACT           TO S109-COD-ACT
             MOVE VA-S109-NUM-NEGOC         TO S109-NUM-NEGOC
             MOVE VA-S109-OPE-ORI           TO S109-OPE-ORI
             MOVE VA-S109-NUM-DECLA         TO S109-NUM-DECLA
             MOVE VA-S109-NUM-TRANS         TO S109-NUM-TRANS
             MOVE VA-S109-NUM-CARD          TO S109-NUM-CARD
             MOVE VA-S109-RECURRENTE        TO S109-RECURRENTE
      * DECLARE DECLARE @BAZ063-FIN
           PERFORM 88888-WRITE
      *    INITIALIZE WSV-AUXSAL
           .
      *
      ******************************************************************
      *              29990-MOVER-SALIDA-2
      ******************************************************************
       29990-MOVER-SALIDA-2.
      *
           INITIALIZE MBNS2009
      * DECLARE DECLARE @BAZ066.I
           SET SW-B925-NO               TO TRUE
      * DECLARE DECLARE @BAZ066.F
      * -- Primera L�nea
           PERFORM 29991-CONSULTA-BLDT002
      *BAZ019-I
           IF VA-COD-MOV = CA-V06
             MOVE WSV-AUX-DESC         TO S209-CONCEPT
           ELSE
      * DECLARE DECLARE @BAZ050.I
              IF AUX-INTREF71(12:4);  = CA-MBL7 AND
                 VA-COD-MOV = CA-Z36
                 MOVE CA-COMP-GIFTCARD          TO S209-CONCEPT
      * DECLARE DECLARE @BAZ050.F
              ELSE
      * DECLARE DECLARE @BAZ066.I
                 IF AUX-INTREF71(12:4);  = CA-B925 AND
                    VA-COD-MOV = CA-160
                    MOVE CA-DEPGO-BAZ-USA       TO S209-CONCEPT
                    SET SW-B925-SI              TO TRUE
                 ELSE
      * DECLARE DECLARE @BAZ066.F
                    MOVE T100-BIGALP            TO S209-CONCEPT
                 END-IF
              END-IF
           END-IF
      *BAZ019-F
           PERFORM CONSULTA-BGDT606
           MOVE T606-PATH              TO S209-FOTO

      * DECLARE DECLARE @BAZ021-INI
           SET SW-OPE-WALLET-NK        TO TRUE
           IF SW-WALLET
               PERFORM CONSULTA-DESC-OPER-WALLET
           END-IF
      * -- Segunda L�nea
           IF SW-OPE-WALLET-NK OR SW-BAZ
               PERFORM CONSULTA-DESC-OPER
           END-IF
      * DECLARE DECLARE @BAZ021-FIN

           MOVE VA-DESC-OPE            TO S209-DESCOPE

      * DECLARE DECLARE @BAZ054-INI
      *     IF (VA-COD-MOV = '160' OR VA-COD-MOV = '169');  AND
      *         AUX-INTREF71(12:4);  = 'MBS4'
      *         MOVE TB-T071-OBSERVATIONS(I-REG); (1:30);  TO S209-CONCEPT
      *     END-IF
      * DECLARE DECLARE @BAZ054-FIN
      * DECLARE DECLARE @BAZ062-I
           IF (VA-COD-MOV = 'G36' OR VA-COD-MOV = 'G37');  AND
               AUX-INTREF71(12:4);  = 'MBS4'

                 INITIALIZE S209-CONCEPT
                            S209-DESCOPE
                            VA-MBS41
                            VA-MBS42
                            VA-MBS43
                            VA-MBS45
                            VA-DESC-OPE


             MOVE TB-T071-OBSERVATIONS(I-REG); (1:30); 
                                           TO S209-CONCEPT

             PERFORM ARMA-DESC-CODMBS4

             MOVE 'Para'                  TO VA-MBS41
             MOVE AUX-NOM-CTE             TO VA-MBS42
             MOVE '****'                  TO VA-MBS43
             MOVE BGNC477-ACC-CRED(17:4);   TO VA-MBS45

             STRING VA-MBS41  DELIMITED BY '  '
                          ' ' DELIMITED BY SIZE
                VA-MBS42      DELIMITED BY '  '
                          ' ' DELIMITED BY SIZE
                VA-MBS43      DELIMITED BY '  '
                          ' ' DELIMITED BY SIZE
                VA-MBS45      DELIMITED BY '  '
                              INTO VA-DESC-OPE

             MOVE VA-DESC-OPE  TO  S209-DESCOPE
           END-IF
      * DECLARE DECLARE @BAZ062-F

      * DECLARE DECLARE @BAZ021-INI
           IF SW-WALLET AND SW-OPE-WALLET-NK
               INITIALIZE S209-DESCOPE
               MOVE S209-CONCEPT       TO S209-DESCOPE
               INITIALIZE S209-CONCEPT
               MOVE VA-DESC-OPE        TO S209-CONCEPT
           END-IF
      * DECLARE DECLARE @BAZ021-FIN
           MOVE VA-COD-MOV             TO S209-IDOPER
      * DECLARE DECLARE @BAZ038 - I
           IF SW-BAZ AND SW-114 AND SW-88-PARAM-V-OK
               PERFORM VAL-SI-MOVTO-TJ-DIG
           END-IF
      * DECLARE DECLARE @BAZ038 - F

      * DECLARE DECLARE @BAZ043-INI

           IF WSV-AUX-DESC(01:05);  = 'ASP_2' AND
              (SW-169 OR VA-COD-MOV = '169'); 
              MOVE 'Apertura Socio Plus'         TO S209-DESCOPE(06:30); 
      *       MOVE WSV-AUX-DESC(01:17);            TO S209-DESCOPE(06:30); 
           END-IF

           IF WSV-AUX-DESC(01:06);  = 'RASP_2' AND
              (SW-160 OR VA-COD-MOV = '160'); 
              MOVE 'Reverso Apertura Socio Plus' TO S209-DESCOPE(04:30); 
      *       MOVE WSV-AUX-DESC(01:17);            TO S209-DESCOPE(04:30); 
           END-IF

      * DECLARE DECLARE @BAZ043-FIN


      * DECLARE DECLARE @BAZ045-INI
           IF WSV-AUX-DESC(01:05);  = 'CFP_2' AND
              (SW-169 OR VA-COD-MOV = '169'); 
              MOVE 'Compra FIAR Plus'         TO S209-DESCOPE
      *       MOVE WSV-AUX-DESC(01:14);            TO S209-DESCOPE
           END-IF

           IF WSV-AUX-DESC(01:06);  = 'VFP_2' AND
              (SW-160  OR VA-COD-MOV = '160'); 
              MOVE 'Venta FIAR Plus'          TO S209-DESCOPE
      *       MOVE WSV-AUX-DESC(01:14);            TO S209-DESCOPE
           END-IF
      * DECLARE DECLARE @BAZ045-FIN
      * DECLARE DECLARE @BAZ056-I
           PERFORM CONSULTA-DESCOPER-SAPP
      * DECLARE DECLARE @BAZ056-F
      * DECLARE DECLARE @BAZ057-I
           IF WSV-AUX-DESC(01:15);  = 'Envio a celular'
              MOVE '160'           TO S209-IDOPER
           END-IF
      *
           IF TB-T043-NUM-OPE-2(I-REG);    NOT EQUAL SPACES AND
              TB-T803-ENT-ACC(I-REG);      NOT EQUAL SPACES
      *
              MOVE SPACES                    TO S209-CONCEPT
              MOVE 'CARGO RECURRENTE'        TO S209-CONCEPT
           END-IF
      * DECLARE DECLARE @BAZ057-F
      *    PERFORM 88888-WRITE
           PERFORM 77777-WRITE
           INITIALIZE WSV-AUXSAL
           .
      *LCR-INI2
      ******************************************************************
      *              29991-MOVER-SALIDA-3
      ******************************************************************
       29991-MOVER-SALIDA-3.
      *
           INITIALIZE MBNS3009
      *
           MOVE T606-FLG-FREE1          TO  S309-STATUS
           MOVE T606-CHAR-FREE1         TO  S309-FOLIO
      * DECLARE DECLARE @BAZ015-->INI
           IF AUX-INTREF71(12:4);  = 'MB48'
              MOVE AUX-DESC(6:15);        TO  S309-BENEFIC
           ELSE
      * DECLARE DECLARE @BAZ008.I
              MOVE VA-BENEFIC           TO  S309-BENEFIC
           END-IF
      * DECLARE DECLARE @BAZ008.F
      * DECLARE DECLARE @BAZ060-I
           IF ((VA-COD-MOV = 'Z92' OR 'AB2');  AND
                T071-NETNAMEUPD(5:4);  = CA-MBWI); 
      * DECLARE DECLARE @BAZ071-INI
      *         PERFORM 2110-CONSULTA-OPERACION
                MOVE WSV-AUX-FOLOPE     TO S309-FOLIO
      * DECLARE DECLARE @BAZ071-FIN
           END-IF
      * DECLARE DECLARE @BAZ060-F
      * DECLARE DECLARE @BAZ015<--FIN
           PERFORM 66666-WRITE
           .
      *
      *LCR-FIN2
      * DECLARE DECLARE @BAZ023.I
      ******************************************************************
      *              29992-MOVER-SALIDA-4
      ******************************************************************
       29992-MOVER-SALIDA-4.
      *
           INITIALIZE MBNS4009
      *
      *BAZ053-INI
           IF TB-T043-NUM-OPE-2(I-REG);    NOT EQUAL SPACES AND
              TB-T803-ENT-ACC(I-REG);      NOT EQUAL SPACES
      *
              MOVE CT-CR                     TO S409-IDLIBRE
           ELSE
              MOVE SPACES                    TO S409-IDLIBRE
           END-IF
      *BAZ053-FIN
      *
           IF AUX-USERUPD = 'FE2C9002'
              IF AUX-NETNAMEUPD = 'IKOSF902'
                IF AUX-VA-COD-MOV = '213'
                  MOVE 'CS'          TO S409-IDCODI
                END-IF
              END-IF
           ELSE
      * DECLARE DECLARE @BAZ034-I
      *       IF AUX-INTREF71(12:4);  = 'MBD1'
              IF AUX-INTREF71(12:4);  = 'MBD1' OR
                 AUX-INTREF71(12:4);  = 'MBWB'
      * DECLARE DECLARE @BAZ034-F
                IF AUX-VA-COD-MOV = '160'
                  MOVE 'CT'          TO S409-IDCODI
                  MOVE AUX-NOM-CTE   TO S409-NOMORD
      * DECLARE DECLARE @BAZ050.I
                  MOVE BGNC477-ACC-DEB
                                     TO S409-TXTLIBR
      * DECLARE DECLARE @BAZ050.F
      * DECLARE DECLARE @BAZ072.I
                ELSE
                  IF AUX-VA-COD-MOV = '169'
                     MOVE 'PT'       TO S409-IDCODI
                     MOVE AUX2-NOMBRECTE
                                     TO S409-NOMORD
                     MOVE BGNC477-ACC-CRED
                                     TO S409-TXTLIBR
                  END-IF
      * DECLARE DECLARE @BAZ072.F
                END-IF
              END-IF
           END-IF
           PERFORM 66667-WRITE-SALIDA4
           .
      *
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ060-I
      ******************************************************************
       2110-CONSULTA-OPERACION.
      ******************************************************************
      *
           INITIALIZE                       DCLMBGT255
      * DECLARE DECLARE @BAZ071-INI
                                            WSV-AUX-FOLOPE
                                            WSV-AUX-COMPTA
      * DECLARE DECLARE @BAZ071-FIN
      *
           MOVE E009-NUMCUEN                TO T255-CTA-CAR
           MOVE T071-NUM-OPERATION          TO T255-MOV-CAR
      *
           EXEC SQL
                SELECT T255_RES_OBS
      * DECLARE DECLARE @BAZ071-INI
      *           INTO :T255-RES-OBS
                      ;T255_REF_CAR
                      ;T255_TIP_COM
                  INTO :T255-RES-OBS
                      ;:T255-REF-CAR
                      ;:T255-TIP-COM
      * DECLARE DECLARE @BAZ071-FIN
                 FROM MBDT255 with(nolock); 
                WHERE T255_CTA_CAR = :T255-CTA-CAR
                  AND T255_MOV_CAR = :T255-MOV-CAR
           END-EXEC
      *
           MOVE SQLCODE                     TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
      * DECLARE DECLARE @BAZ071-INI
      *           MOVE T255-RES-OBS(12:06);   TO S309-FOLIO
                MOVE T255-RES-OBS(12:06);     TO WSV-AUX-FOLOPE
                MOVE CA-TA                  TO WSV-AUX-TA
                MOVE T255-REF-CAR(1:10);      TO WSV-AUX-CTACAR
                MOVE T255-TIP-COM(1:15);      TO WSV-AUX-TIPCOM
                MOVE WSV-AUX-COMPTA         TO S009-DESCM01
      *      WHEN SQL-88-NOT-FOUND
      *           CONTINUE
             WHEN SQL-88-NOT-FOUND
                PERFORM 2111-CONSULTA-OPERACION
      * DECLARE DECLARE @BAZ071-FIN
             WHEN OTHER
      * DECLARE DECLARE @BAZ073 - se comenta por incidencia productiva
                 CONTINUE
      *          MOVE 'MCE0216'             TO CAA-COD-ERROR
      *          MOVE 'MBDT255/'            TO CAA-ERR-VARIA1(1:8); 
      *          MOVE T255-CTA-CAR(5:10);     TO CAA-ERR-VARIA1(9:10); 
      *          MOVE SQLCODE               TO CAA-ERR-VARIA2
      *          PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ060-F
      * DECLARE DECLARE @BAZ071-INI
      ******************************************************************
       2111-CONSULTA-OPERACION.
      ******************************************************************
      *
           INITIALIZE                       DCLMBGT258
      *
           MOVE E009-NUMCUEN                TO T258-CTA-CAR
           MOVE T071-NUM-OPERATION          TO T258-MOV-CAR
           MOVE VA-FECHA-OPERACION          TO T258-FEC-REG
      *
           EXEC SQL
                SELECT T258_RES_OBS
                      ;T258_REF_CAR
                      ;T258_TIP_COM
                  INTO :T258-RES-OBS
                      ;:T258-REF-CAR
                      ;:T258-TIP-COM
                 FROM MBDT258 with(nolock); 
                WHERE T258_CTA_CAR = :T258-CTA-CAR
                  AND T258_MOV_CAR = :T258-MOV-CAR
                  AND T258_FEC_REG = :T258-FEC-REG
           END-EXEC
      *
           MOVE SQLCODE                     TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
                MOVE T258-RES-OBS(12:06);     TO WSV-AUX-FOLOPE
                MOVE CA-TA                  TO WSV-AUX-TA
                MOVE T258-REF-CAR(1:10);      TO WSV-AUX-CTACAR
                MOVE T258-TIP-COM(1:15);      TO WSV-AUX-TIPCOM
                MOVE WSV-AUX-COMPTA         TO S009-DESCM01
             WHEN SQL-88-NOT-FOUND
                  CONTINUE
             WHEN OTHER
                 CONTINUE
      *          MOVE 'MCE0216'             TO CAA-COD-ERROR
      *          MOVE 'MBDT258/'            TO CAA-ERR-VARIA1(1:8); 
      *          MOVE T258-CTA-CAR(5:10);     TO CAA-ERR-VARIA1(9:10); 
      *          MOVE SQLCODE               TO CAA-ERR-VARIA2
      *          PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ071-FIN
      ******************************************************************
      *29991-CONSULTA-BLDT002.                                         *
      * SE CONSULTA LA TABLA;BLDT002(CAT�LOGO DE CODIGO DE OPERACIONES); *
      *PARA OBTENER LA DESCRIPCION DEL C�DIGO DE OPERACI�N.            *
      ******************************************************************
       29991-CONSULTA-BLDT002.
      *
           INITIALIZE DCLBGGT100
      *
           MOVE VA-COD-MOV             TO T100-COD
      * DECLARE DECLARE @BAZ037-I
           IF TB-T100-BIGALP(I-REG);         EQUAL SPACES OR
              TB-T100-BIGALP(I-REG);         EQUAL LOW-VALUES
      *
              MOVE SPACES                  TO T100-BIGALP
           ELSE
              MOVE TB-T100-BIGALP(I-REG);    TO T100-BIGALP
           END-IF.
      *    EXEC SQL
      *       SELECT
      *              T100_BIGALP
      *        INTO
      *             :T100-BIGALP
      *        FROM BLDT002 with (nolock); 
      *        WHERE T100_CODE     = :T100-COD
      *          AND T100_LANGUAGE = :CA-E
      *    END-EXEC.
      *
      *    MOVE SQLCODE                TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *      WHEN SQL-88-OK
      *      WHEN SQL-88-NOT-FOUND
      *          CONTINUE
      *          MOVE 'MDE0027'        TO CAA-COD-ERROR
      *          PERFORM 30000-FIN
      *      WHEN OTHER
      *          INITIALIZE QGECABC
      *          MOVE 'MB2C0009'       TO ABC-DES-PROG
      *          MOVE 'SELECT'         TO ABC-REFERENCE1
      *          MOVE 'BLDT002'        TO ABC-OBJECT-ERROR
      *          MOVE SQLERRM          TO ABC-SQLERRM
      *          MOVE SQLCODE          TO ABC-SQLCODE
      *          PERFORM 999999-DB2-ABEND
      *    END-EVALUATE.
      * DECLARE DECLARE @BAZ037-F
      * DECLARE DECLARE @BAZ0021-INI
      * DECLARE DECLARE @BAZ039 - I
      *----------------------------------------------------------------*
      *VAL-SI-MOVTO-TJ-DIG
      *  SE VALIDA SI EL MOVIMENTO FUE REALIZADO CON LA TARJETA DIGITAL
      *----------------------------------------------------------------*
       VAL-SI-MOVTO-TJ-DIG.
      *
           IF WSS-RET-CTA
               PERFORM RECUPERA-TJ-RETENCION
           ELSE
               PERFORM RECUPERA-TJ-COMPRA
           END-IF
      *
           IF SW-88-MOV-FOU-S
               PERFORM VALIDA-SI-TJ-DIG
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *  RECUPERA TARJETA DE LA COMPRA PARA VALIDAR SI ES DIGITAL
      *----------------------------------------------------------------*
       RECUPERA-TJ-RETENCION.
      *
        MOVE E009-NUMCUEN(5:2);   TO T028-TYP-ACC
        MOVE E009-NUMCUEN(1:4);   TO T028-BRN-ACC
        MOVE E009-NUMCUEN(7:8);   TO T028-ACC
        MOVE AUX-CTA-ENT        TO T028-ENT-ACC


        EXEC SQL
         SELECT TOP 1 T028_ENT_CON
               ;T028_BRN_CON
               ;T028_TYP_CON
               ;T028_NUM_CON
           INTO :T028-ENT-CON
               ;:T028-BRN-CON
               ;:T028-TYP-CON
               ;:T028-NUM-CON
          FROM MCDT028 with(nolock); 
         WHERE T028_ENT_ACC = :T028-ENT-ACC
           AND T028_BRN_ACC = :T028-BRN-ACC
           AND T028_TYP_ACC = :T028-TYP-ACC
           AND T028_ACC     = :T028-ACC
           AND T028_DAT_CONANN IS NULL
           AND T028_DAT_CONCAN IS NULL
           ORDER BY T028_DAT_CONREG DESC
        END-EXEC
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           IF SQL-88-OK
              PERFORM RECUPERA-TDD-DIG-RETENCION
           END-IF
           .

      *----------------------------------------------------------------*
      *  RECUPERA TARJETA DE LA COMPRA PARA VALIDAR SI ES DIGITAL
      *----------------------------------------------------------------*
       RECUPERA-TDD-DIG-RETENCION.
      *
        EXEC SQL
         SELECT T010_NUM_BIN_CRD
               ;T010_NUM_CARD
          INTO :T010-NUM-BIN-CRD
              ;:T010-NUM-CARD
          FROM MCDT010 with(nolock); 
         WHERE T010_NUM_CONTRACT = :T028-NUM-CON
           AND T010_BRN_CONTRACT = :T028-BRN-CON
           AND T010_TYP_CONTRACT = :T028-TYP-CON
           AND T010_ENT_CONTRACT = :T028-ENT-CON
           AND T010_NUM_CRE_ETY  > 0
           AND T010_NUM_WHD      = :T089-NUM-WHD
        END-EXEC
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           IF SQL-88-OK
              SET SW-88-MOV-FOU-S          TO TRUE
              MOVE T010-NUM-BIN-CRD        TO AUX-NUM-BIN-CRD
              MOVE T010-NUM-CARD           TO AUX-NUM-CARD
              PERFORM ENCUENTRA-TYP-CRD
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *  ENCUENTRA TIPO DE TARJETA
      *----------------------------------------------------------------*
        ENCUENTRA-TYP-CRD.
      *
        EXEC SQL
         SELECT T114_TYP_CRD
          INTO :T114-TYP-CRD
          FROM MCDT114 with(nolock); 
         WHERE T114_NUM_CRD    = :AUX-NUM-CARD
           AND T114_NUM_BINCRD = :AUX-NUM-BIN-CRD
        END-EXEC
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           IF SQL-88-OK
              MOVE T114-TYP-CRD            TO AUX-TYP-CRD
           ELSE
              MOVE SPACE                   TO AUX-TYP-CRD
           END-IF
       .
      * DECLARE DECLARE @BAZ039 - F
      * DECLARE DECLARE @BAZ038 - I
      *----------------------------------------------------------------*
      *RECUPERA-TJ-COMPRA
      *  RECUPERA TARJETA DE LA COMPRA PARA VALIDAR SI ES DIGITAL
      *----------------------------------------------------------------*
       RECUPERA-TJ-COMPRA.
      *
           INITIALIZE                         WSV-CONT
                                              WSV-CONT1
                                              DCLMCDT043
      * DECLARE DECLARE @BAZ042 - I
                                              VA-SPACE
                                              WSV-CONT2
                                              VA-STRING
      * DECLARE DECLARE @BAZ042 - F
      *
           SET SW-88-MOV-FOU-N                TO TRUE
      *
           MOVE AUX-CTA-ENT                   TO T043-ENT-ACC
           MOVE AUX-CTA-CEN                   TO T043-BRN-ACC
           MOVE AUX-CTA-NUM(1:2);               TO T043-TYP-ACC
           MOVE AUX-CTA-NUM(3:8);               TO T043-ACC
           MOVE WSV-AUX-FECHA                 TO T043-DAT-ACCT
      * DECLARE DECLARE @BAZ042 - I
           MOVE VN-NUM-OPERATION              TO T043-NUM-OPE-2
      *
           INSPECT T043-NUM-OPE-2             TALLYING VA-SPACE
                   FOR   CHARACTERS   BEFORE
             '1' OR '2' OR '3' OR '4' OR '5' OR '6' OR '7' OR '8' OR '9'

           INSPECT T043-NUM-OPE-2             TALLYING VA-STRING
                   FOR CHARACTERS  BEFORE INITIAL SPACE

           COMPUTE WSV-CONT2  = VA-STRING -      VA-SPACE
           COMPUTE WSV-CONT2  = VA-STRING -      VA-SPACE
           ADD     1                          TO VA-SPACE
           MOVE T043-NUM-OPE-2(VA-SPACE:WSV-CONT2); 
                                              TO T043-NUM-OPE-2
      * DECLARE DECLARE @BAZ042 - F
      *
           MOVE AUX-AMT-COMP3                 TO AUX-AMT-COMP043
           MOVE AUX-AMT-COMP043               TO T043-AMT-OPERATION
      *
           EXEC SQL
               SELECT  T043_NUM_BIN_CRD;
                       T043_NUM_CARD   ;
                       T043_TYP_CARD
                 INTO :T043-NUM-BIN-CRD;
                      :T043-NUM-CARD   ;
                      :T043-TYP-CARD
                 FROM MCDT043 with(nolock); 
               WHERE T043_ENT_ACC        = :T043-ENT-ACC
                 AND T043_BRN_ACC        = :T043-BRN-ACC
                 AND T043_TYP_ACC        = :T043-TYP-ACC
                 AND T043_ACC            = :T043-ACC
                 AND T043_DAT_ACCT       <> :CA-FECHINV
                 AND T043_NUM_OPE_2      = :T043-NUM-OPE-2
           END-EXEC
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           IF SQL-88-OK
              SET SW-88-MOV-FOU-S          TO TRUE
              MOVE T043-TYP-CARD           TO AUX-TYP-CRD
              MOVE T043-NUM-BIN-CRD        TO AUX-NUM-BIN-CRD
              MOVE T043-NUM-CARD           TO AUX-NUM-CARD
           END-IF
           .
      *
      *-----------------------------------------------------------------
      *VALIDA-SI-TJ-DIG.
      *-----------------------------------------------------------------
       VALIDA-SI-TJ-DIG.
      *
           MOVE AUX-NUM-BIN-CRD                TO VN-TJ-ACT-BIN
           MOVE AUX-NUM-CARD                   TO VN-TJ-ACT-CRD
      *
           IF VN-TARJETA-ACTUAL NOT EQUAL VN-TARJETA-ANTERIOR
      *
               SET SW-88-FIN-BIN-N              TO TRUE
               MOVE VN-VAL-INI                  TO VN-IND
                                                   VN-CON
      *
               PERFORM UNTIL VN-IND >= VN-FIN-CADENA OR SW-88-FIN-BIN-S
                  IF VN-DES-TABLE(VN-CON:06);  = AUX-NUM-BIN-CRD
                     ADD  6                     TO VN-CON
      *
                     IF VN-DES-TABLE(VN-CON:02);  = AUX-TYP-CRD
                        ADD  3                  TO VN-CON
                        MOVE VN-MARCA-AUX       TO S209-CONCEPT
                        SET SW-88-FIN-BIN-S     TO TRUE
                     END-IF
                  END-IF
      *
                  ADD  9                        TO VN-IND
                  MOVE VN-IND                   TO VN-CON
               END-PERFORM
      *
               MOVE AUX-NUM-BIN-CRD             TO VN-TJ-ANT-BIN
               MOVE AUX-NUM-CARD                TO VN-TJ-ANT-CRD
           ELSE
               IF SW-88-FIN-BIN-S
                  MOVE VN-MARCA-AUX   TO S209-CONCEPT
               END-IF
           END-IF
           .
      *
      * DECLARE DECLARE @BAZ038 - F
      ******************************************************************
      *CONSULTA-DESC-OPER-WALLET.                                      *
      * SE OBTIENE LA DESCRIPCI�N DEL C�DIGO DE OPERACI�N PREVIAMENTE  *
      * EVALUADO.                                                      *
      ******************************************************************
       CONSULTA-DESC-OPER-WALLET.
      *
           INITIALIZE VA-DESC-OPE
                      VA-BENEFIC
                      AUX-DESC-W
      *
           SET SW-OPE-WALLET-OK                TO TRUE
           MOVE VA-COD-MOV                      TO WSS-COD-OPERACION

      *
           IF (AUX-INTREF71(12:4);  = 'MBW4' OR 'MBW5'); 
           AND SW-WALLET
               MOVE T606-DESCRIPTION   TO S209-CONCEPT
           END-IF

           EVALUATE TRUE
             WHEN SW-T60
               IF WSV-AUX-DESC(15:4);  EQUAL '7760'
      * DECLARE DECLARE @BAZ024-INI
      *            MOVE 'a Peiimii'            TO AUX-DESC-W
                   MOVE 'a Tomiin'             TO AUX-DESC-W
      * DECLARE DECLARE @BAZ024-FIN
               ELSE
                   MOVE 'a Banco Azteca'       TO AUX-DESC-W
               END-IF
      * DECLARE DECLARE @BAZ034-I
               MOVE SPACES                     TO S209-CONCEPT
      * DECLARE DECLARE @BAZ034-F
               STRING       'Pago con QR|' DELIMITED BY SIZE
                                AUX-DESC-W DELIMITED BY '  '
                                       ' ' DELIMITED BY SIZE
                                    '****' DELIMITED BY SIZE
                        WSV-AUX-DESC(27:4);  DELIMITED BY '  '
                                           INTO VA-DESC-OPE
             WHEN SW-T59
               IF WSV-AUX-DESC(15:4);  EQUAL '7760'
      * DECLARE DECLARE @BAZ024-INI
      *            MOVE 'de Peiimii'           TO AUX-DESC-W
                   MOVE 'de Tomiin'            TO AUX-DESC-W
      * DECLARE DECLARE @BAZ024-FIN
               ELSE
                   MOVE 'de Banco Azteca'      TO AUX-DESC-W
               END-IF
      * DECLARE DECLARE @BAZ034-I
               MOVE SPACES                     TO S209-CONCEPT
      * DECLARE DECLARE @BAZ034-F
               STRING      'Cobro con QR|' DELIMITED BY SIZE
                                AUX-DESC-W DELIMITED BY '  '
                                       ' ' DELIMITED BY SIZE
                                    '****' DELIMITED BY SIZE
                        WSV-AUX-DESC(27:4);  DELIMITED BY '  '
                                           INTO VA-DESC-OPE
             WHEN SW-212
               PERFORM ARMA-DESC-TRANSFE2
               INITIALIZE VA-DESC-OPE
      * DECLARE DECLARE @BAZ027-I
      *        STRING 'Envio de dinero a|' DELIMITED BY SIZE
               STRING 'Env�o a banco|' DELIMITED BY SIZE
      * DECLARE DECLARE @BAZ027-F
                            AUX-DES-TRANF1 DELIMITED BY '  '
                                       ' ' DELIMITED BY SIZE
                            AUX-DES-TRANF3 DELIMITED BY '  '
                                           INTO VA-DESC-OPE
      * DECLARE DECLARE @BAZ041-I
      *        MOVE AUX-DES-TRANS2         TO S209-CONCEPT
               MOVE T606-DESCRIPTION       TO S209-CONCEPT
      * DECLARE DECLARE @BAZ041-F
             WHEN SW-213
               PERFORM ARMA-DESC-TRANSFER
               INITIALIZE VA-DESC-OPE
      * DECLARE DECLARE @BAZ027-I
      *        STRING 'Recepcion de dinero de|' DELIMITED BY SIZE
               MOVE T607-DESCRIPTION(09:22);        TO S209-FOTO
               STRING 'Recibo bancario|' DELIMITED BY SIZE
      * DECLARE DECLARE @BAZ027-F
      * DECLARE DECLARE @BAZ024-INI
      *                     AUX-DES-TRANF1 DELIMITED BY '  '
                            AUX-DES-TRANS1 DELIMITED BY '  '
                                           INTO VA-DESC-OPE

               MOVE AUX-DES-TRANS2         TO S209-CONCEPT
      * DECLARE DECLARE @BAZ024-FIN
             WHEN SW-169
               EVALUATE AUX-INTREF71(12:4); 
                 WHEN 'MBW4'
      * DECLARE DECLARE @BAZ027-I
      *            STRING 'Envio de dinero a Celular|' DELIMITED BY SIZE
                   STRING 'Env�o de dinero|' DELIMITED BY SIZE
      * DECLARE DECLARE @BAZ027-F
                                      'a ****' DELIMITED BY SIZE
                            WSV-AUX-DESC(15:4);  DELIMITED BY '  '
                                               INTO VA-DESC-OPE
                 WHEN 'MBW5'
      * DECLARE DECLARE @BAZ027-I
      *            STRING 'Envio de dinero a Cuenta|' DELIMITED BY SIZE
                   STRING 'Env�o a banco|' DELIMITED BY SIZE
      * DECLARE DECLARE @BAZ027-F
                                      'a ****' DELIMITED BY SIZE
                            WSV-AUX-DESC(20:4);  DELIMITED BY '  '
                                               INTO VA-DESC-OPE
                 WHEN SPACES
                  IF AUX-USERUPD EQUAL 'MB4C0100
      * DECLARE DECLARE @BAZ027-I
      *            STRING 'Envio de dinero a Celular|' DELIMITED BY SIZE
                   STRING 'Env�o de dinero|' DELIMITED BY SIZE
      * DECLARE DECLARE @BAZ027-F
                                      'a ****' DELIMITED BY SIZE
                            WSV-AUX-DESC(15:4);  DELIMITED BY '  '
                                               INTO VA-DESC-OPE
                   MOVE T606-DESCRIPTION       TO S209-CONCEPT

                  ELSE
                   SET SW-OPE-WALLET-NK        TO TRUE
                  END-IF
      * DECLARE DECLARE @BAZ041-I
                 WHEN CA-B520
                      PERFORM ARMA-DESC-COD169
                      MOVE S209-CONCEPT        TO VA-DESC-OPE
                      MOVE SPACES              TO S209-CONCEPT
                                                  S209-DESCOPE
      * DECLARE DECLARE @BAZ041-F
                 WHEN OTHER
                   SET SW-OPE-WALLET-NK        TO TRUE
               END-EVALUATE
             WHEN SW-160
               IF AUX-INTREF71(12:4);  = 'MBW4' OR
                   AUX-USERUPD EQUAL 'MB4C0100'
      * DECLARE DECLARE @BAZ027-I
      *            STRING 'Deposito de celular|' DELIMITED BY SIZE
      *
                   STRING 'Recibo de dinero|' DELIMITED BY SIZE
      * DECLARE DECLARE @BAZ027-F
                                       'de ****' DELIMITED BY SIZE
                              WSV-AUX-DESC(16:4);  DELIMITED BY '  '
                                                 INTO VA-DESC-OPE
                   MOVE T606-DESCRIPTION         TO S209-CONCEPT
      * DECLARE DECLARE @BAZ027-I
      *        ELSE
      *            SET SW-OPE-WALLET-NK          TO TRUE
      *        END-IF
               ELSE
                  IF AUX-INTREF71(12:4);  = 'MB03'
                     STRING 'Recibo de banco|' DELIMITED BY SIZE
                                      'de ****' DELIMITED BY SIZE
                             WSV-AUX-DESC(16:4);  DELIMITED BY '  '
                                                INTO VA-DESC-OPE
                     MOVE T606-DESCRIPTION      TO S209-CONCEPT
                  ELSE
      * DECLARE DECLARE @BAZ041-I
                     IF AUX-INTREF71(12:4);  = CA-B520
                        PERFORM ARMA-DESC-COD160
                        MOVE S209-CONCEPT       TO VA-DESC-OPE
                        MOVE SPACES             TO S209-CONCEPT
                                                   S209-DESCOPE
                     ELSE
      * DECLARE DECLARE @BAZ041-F
                        SET SW-OPE-WALLET-NK    TO TRUE
                     END-IF
                  END-IF
               END-IF
      * DECLARE DECLARE @BAZ027-F
             WHEN SW-SPC
               IF WSS-RET-CTA
                 IF T089-OBSERVATIONS(1:18);  = CA-ENV-ATM-TOMIIN
                    MOVE CA-RET-ATM            TO VA-DESC-OPE
                 ELSE
                   STRING 'Retenci�n Env�o a celular|' DELIMITED BY SIZE
                                        'a ****' DELIMITED BY SIZE
                              WSV-AUX-DESC(23:4);  DELIMITED BY '  '
                                                 INTO VA-DESC-OPE
                 END-IF
                 MOVE T606-DESCRIPTION         TO S209-CONCEPT
               ELSE
                   SET SW-OPE-WALLET-NK        TO TRUE
               END-IF
      * DECLARE DECLARE @BAZ027-I
             WHEN SW-PAGO-SERV
      *
                  PERFORM ARMA-DESC-PAGSERV
                  MOVE VA-DESC-OPE              TO S209-DESCOPE
                  INITIALIZE VA-DESC-OPE
                  MOVE S209-CONCEPT(1:16);        TO VA-DESC-OPE
                  INITIALIZE S209-CONCEPT
                  MOVE S209-DESCOPE             TO S209-CONCEPT
      *
             WHEN SW-656
      *
                  MOVE 'Apertura de cuenta'     TO VA-DESC-OPE
      *
             WHEN SW-T05
      *
                  PERFORM ARMA-DESC-TA
                  MOVE VA-DESC-OPE              TO S209-CONCEPT
                  MOVE 'Recarga de tiempo aire' TO VA-DESC-OPE
      *
             WHEN SW-DEV-SPEI
                  PERFORM ARMA-DESC-DEVSPEI
                  IF AUX-INTREF71(12:4);  = CA-0600
                     MOVE VA-DESC-OPE                 TO S209-CONCEPT
                     MOVE 'Devoluci�n de env�o bancario'
                                                      TO VA-DESC-OPE
                     MOVE V0040-DES-RECVENT           TO S209-FOTO
                  ELSE
                     IF AUX-INTREF71(12:4);  = CA-0700
                        MOVE 'Cancelaci�n SPEI BCO desconectado'
                                                      TO S209-CONCEPT
                        MOVE 'Cancelaci�n de env�o bancario'
                                                      TO VA-DESC-OPE
                     END-IF
                  END-IF
             WHEN SW-U50

                  IF AUX-INTREF71(12:4);  = CA-MBW9
      * DECLARE DECLARE @BAZ040-I
      *              MOVE 'Regalo Tomiin'      TO VA-DESC-OPE
                     MOVE SPACES               TO S209-CONCEPT
                     MOVE WSV-AUX-DESC(01:30);   TO VA-DESC-OPE
      * DECLARE DECLARE @BAZ040-F
                  ELSE
      *BAZ044-I
                     IF AUX-INTREF71(12:4);  = CA-BATC
                        MOVE SPACES               TO S209-CONCEPT
                        MOVE CA-BON-DOS-P         TO VA-DESC-OPE
                     ELSE
      *BAZ044-F
                         SET SW-OPE-WALLET-NK    TO TRUE
      *BAZ044-I
                     END-IF
      *BAZ044-F
                  END-IF
      * DECLARE DECLARE @BAZ033-I
             WHEN SW-W50
                  PERFORM CONSULTA-CORRESPONSAL
                  MOVE CA-DEP-EFECTIVO           TO VA-DESC-OPE
                  MOVE T148-BRN-DES              TO VA-DESC-OPE(25:15); 
             WHEN SW-Q54
                  MOVE CA-DEP-TELECOMM           TO VA-DESC-OPE
             WHEN SW-000
                  MOVE CA-DEP-BANCO              TO VA-DESC-OPE
             WHEN SW-T27
                  MOVE CA-RET-VEN-TOMIIN         TO VA-DESC-OPE
                  MOVE CA-TOMIIN                 TO S209-FOTO
             WHEN SW-C75
                  MOVE CA-RET-ATM-TOMIIN         TO VA-DESC-OPE
                  MOVE CA-TOMIIN                 TO S209-FOTO
      * DECLARE DECLARE @BAZ033-F
      * DECLARE DECLARE @BAZ027-F
      * DECLARE DECLARE @BAZ044-I
             WHEN SW-Z25
                  PERFORM ARMA-DESC-Z25
                  INSPECT S209-CONCEPT REPLACING ALL '&' BY ''
                  MOVE VA-DESC-OPE(14:60);         TO VA-DESC-AUX
                  MOVE 'Deposito con QR'         TO VA-DESC-OPE
      * DECLARE DECLARE @BAZ049-I
      *      WHEN SW-Z26
      *           PERFORM ARMA-DESC-Z26
      *           INSPECT S209-CONCEPT REPLACING ALL '&' BY ''
      *           MOVE VA-DESC-OPE(10:60);         TO VA-DESC-AUX
      *           MOVE 'Pago con QR'             TO VA-DESC-OPE
             WHEN SW-Z26
                  IF AUX-INTREF71(12:4);  = CA-MBWE
                     MOVE CA-COMISION-ENTRADA    TO VA-DESC-OPE
                     MOVE CA-COMISION-COMPRA     TO S209-CONCEPT
                     MOVE T606-DESCRIPTION       TO S209-CONCEPT(24:26); 
                  ELSE
                     IF AUX-INTREF71(12:4);  = CA-BATC
                        MOVE CA-COMI-REUT-ENTRA  TO VA-DESC-OPE
                        MOVE CA-COMI-REUT-ENTMET TO S209-CONCEPT
                     ELSE
                        IF T606-DESCRIPTION(1:15);  = CA-ADEUDO-COMI
                           MOVE CA-COMI-REUT-ENTRA  TO VA-DESC-OPE
                           MOVE CA-COMI-ADEU-ENTMET TO S209-CONCEPT
                        ELSE
                           PERFORM ARMA-DESC-Z26
                           INSPECT S209-CONCEPT REPLACING ALL '&' BY ''
                           MOVE VA-DESC-OPE(10:60); 
                                                 TO VA-DESC-AUX
                           MOVE 'Pago con QR'    TO VA-DESC-OPE
                        END-IF
                     END-IF
                  END-IF
             WHEN SW-Z51
                  IF AUX-INTREF71(12:4);  = CA-MBWE
                     MOVE CA-COMPRA-ENTRADA      TO VA-DESC-OPE
                     MOVE T606-DESCRIPTION       TO S209-CONCEPT
                  ELSE
                     IF AUX-INTREF71(12:4);  = CA-BATC
                        MOVE CA-ENTRADA-REUTILIZ TO VA-DESC-OPE
                        MOVE CA-ENTRADA-REU-MET  TO S209-CONCEPT
                     ELSE
                        IF T606-DESCRIPTION(1:14);  = CA-ADEUDO-ENTR
                           MOVE CA-ENTRADA-REUTILIZ
                                                 TO VA-DESC-OPE
                           MOVE CA-ADEU-ENTMET   TO S209-CONCEPT
                        END-IF
                     END-IF
                  END-IF
      * DECLARE DECLARE @BAZ049-F
      * DECLARE DECLARE @BAZ044-F
             WHEN OTHER
               SET SW-OPE-WALLET-NK            TO TRUE
           END-EVALUATE
      *
           .
      * DECLARE DECLARE @BAZ033-I
      *----------------------------------------------------------------*
      *CONSULTA-CORRESPONSAL                                           *
      *   RECUPERA EL CORRESPONSAL DEL DEPOSITO.                       *
      *----------------------------------------------------------------*
       CONSULTA-CORRESPONSAL.
      *
           INITIALIZE DCLBGGT148
      *
           MOVE WSV-AUX-DESC(01:30);        TO T148-REFERENCIA
           MOVE WSV-AUX-FECHA             TO T148-DAT-OPER
      *
           EXEC SQL
              SELECT T148_BRN_DES
               INTO :T148-BRN-DES
              FROM BGDT148 with (nolock); 
              WHERE T148_REFERENCIA = :T148-REFERENCIA
                AND T148_DAT_OPER   = :T148-DAT-OPER
           END-EXEC
      *
           MOVE SQLCODE                        TO SQL-VALUES
      *
           EVALUATE TRUE
              WHEN SQL-88-OK
              WHEN SQL-88-NOT-FOUND
                   CONTINUE
              WHEN OTHER
                 INITIALIZE QGECABC
                 MOVE 'MB2C0009'           TO ABC-DES-PROG
                 MOVE 'SELECT'             TO ABC-REFERENCE1
                 MOVE 'BGDT148'            TO ABC-OBJECT-ERROR
                 MOVE SQLERRM              TO ABC-SQLERRM
                 MOVE SQLCODE              TO ABC-SQLCODE
                 PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ033-F
      * DECLARE DECLARE @BAZ0021-FIN
      ******************************************************************
      *CONSULTA-DESC-OPER.                                             *
      * SE OBTIENE LA DESCRIPCI�N DEL C�DIGO DE OPERACI�N PREVIAMENTE  *
      * EVALUADO.                                                      *
      ******************************************************************
       CONSULTA-DESC-OPER.
      *
           INITIALIZE VA-DESC-OPE
                      VA-BENEFIC
      *
           MOVE VA-COD-MOV                      TO WSS-COD-OPERACION
      *
           EVALUATE TRUE
              WHEN SW-907
      *            CONTINUE
                   PERFORM ARMA-DESC-COD907
              WHEN SW-129
      * DECLARE DECLARE @BAZ030-I
      *            MOVE WSV-AUX-DESC(1:10);       TO V0011-NUM-REF-OPE
                   MOVE WSV-AUX-DESC(1:10);       TO N100-NUM-REF-OPE
      * DECLARE DECLARE @BAZ030-F
      * DECLARE DECLARE @BAZ032-I
      *            PERFORM CONSULTA-GADT001
      *            MOVE V0011-AMT-TOT-FCC       TO VN-AMT-TOT
                   MOVE ZEROES                  TO VN-AMT-TOT
      * DECLARE DECLARE @BAZ032-F
                   MOVE VN-AMT-TOT              TO VA-DESC-OPE(1:16); 
                   MOVE 'USD'                   TO VA-DESC-OPE(18:3); 
              WHEN SW-S39
                   MOVE 'TARJETA '              TO VA-DESC-OPE(1:8); 
              WHEN SW-CARGO-AUT
                   MOVE WSV-AUX-DESC            TO VA-DESC-OPE
              WHEN SW-H73
                   MOVE 'EMPLEADO'              TO VA-DESC-OPE(1:8); 
              WHEN SW-534
                   MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
                   MOVE WSV-AUX-DESC(24:7);       TO VA-DESC-OPE(8:7); 
              WHEN SW-170
                   MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
                   PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
                   MOVE VA-NUM-CHEQUE           TO VA-DESC-OPE(8:10); 
                   PERFORM CONSULTA-BADT001
                   MOVE V0010-COD-RETURN        TO T010-KEY-TABLE
                   PERFORM CONSULTA-TCDT010
                   MOVE T010-DTA-TBLKEY         TO VA-DESC-OPE(19:32); 
                   SET SW-FIN-NO                TO TRUE
                   INITIALIZE VA-NUM-CHEQUE
              WHEN SW-036
                   PERFORM ARMA-DESC-COD036
              WHEN SW-S06
                   MOVE WSV-AUX-DESC            TO VA-DESC-OPE
              WHEN SW-731
              WHEN SW-442
                   MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
                   MOVE 024                     TO VN-CONT
                   PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
                   MOVE VA-NUM-CHEQUE           TO VA-DESC-OPE(8:10); 
                   SET SW-FIN-NO                TO TRUE
                   INITIALIZE VA-NUM-CHEQUE
              WHEN SW-078
                   CONTINUE
              WHEN SW-COM-PAGO-SER
                   CONTINUE
              WHEN SW-155
                   MOVE 'No. DE REFERENCIA'     TO VA-DESC-OPE
              WHEN SW-S10
                   MOVE 'TELECOM'               TO VA-DESC-OPE
              WHEN SW-013
                   CONTINUE
              WHEN SW-550
                   MOVE CA-BAN-EMP-AZT          TO VA-DESC-OPE
              WHEN SW-465
                   MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
                   MOVE 024                     TO VN-CONT
                   PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
                   MOVE VA-NUM-CHEQUE           TO VA-DESC-OPE(8:10); 
                   SET SW-FIN-NO                TO TRUE
                   INITIALIZE VA-NUM-CHEQUE
              WHEN SW-216
                   CONTINUE
              WHEN SW-114
      *            CONTINUE
                   MOVE WSV-AUX-DESC(1:30);       TO VA-DESC-OPE
              WHEN SW-130
                   MOVE 'TIPO DE CAMBIO '       TO VA-DESC-OPE(1:15); 
              WHEN SW-A94
                   MOVE 'VALOR DE LA UNIDAD '   TO VA-DESC-OPE(1:19); 
              WHEN SW-DEPOSITO
      *            CONTINUE
                   MOVE WSV-AUX-DESC(1:30);       TO VA-DESC-OPE
              WHEN SW-533
                   MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
                   PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
                   MOVE VA-NUM-CHEQUE           TO VA-DESC-OPE(8:10); 
                   SET SW-FIN-NO                TO TRUE
                   INITIALIZE VA-NUM-CHEQUE
              WHEN SW-A36
      *            MOVE 'NOMBRE INVERSION'      TO VA-DESC-OPE
                   PERFORM ARMA-DESC-CODA36
              WHEN SW-350
              WHEN SW-363
              WHEN SW-A95
                   MOVE 'VALOR DE LA UNIDAD'    TO VA-DESC-OPE
              WHEN SW-DEP-PREST
                   PERFORM ARMA-DESCR-DEPREST
              WHEN SW-DEP-GRUP
                   MOVE 'NUMERO DE PEDIDO'      TO VA-DESC-OPE
              WHEN SW-160
      * DECLARE DECLARE @BAZ065-I
              WHEN SW-AI3
      * DECLARE DECLARE @BAZ065-F
      *            MOVE 'TITULAR'               TO VA-DESC-OPE
                   PERFORM ARMA-DESC-COD160
              WHEN SW-215
      *            CONTINUE
                   PERFORM ARMA-DESC-TRANSFER
              WHEN SW-Q54
                   CONTINUE
              WHEN SW-J15
      *            MOVE 'ATM '                  TO VA-DESC-OPE(1:4); 
                   PERFORM ARMA-DESCR-CODJ15
              WHEN SW-000
                   MOVE 'SUC. '                 TO VA-DESC-OPE(1:5); 
              WHEN SW-229
                   CONTINUE
              WHEN SW-M52
                   CONTINUE
              WHEN SW-R20
                   CONTINUE
              WHEN SW-874
                   PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
              WHEN SW-283
                   CONTINUE
              WHEN SW-DEV-SPEI
      *            CONTINUE
                   PERFORM ARMA-DESC-DEVSPEI
              WHEN SW-869
                   CONTINUE
              WHEN SW-S08
                   CONTINUE
              WHEN SW-169
      * DECLARE DECLARE @BAZ065-I
              WHEN SW-AI2
      * DECLARE DECLARE @BAZ065-F
      *            CONTINUE
                   PERFORM ARMA-DESC-COD169

              WHEN SW-152
      *            CONTINUE
                   PERFORM ARMA-DESC-TRANSFE2
              WHEN SW-A85
      *            MOVE WSV-AUX-DESC(9:7);        TO VA-NUM-CHEQUE
                   PERFORM ARMA-DESC-CODA85
              WHEN SW-M78
      *            CONTINUE
                   PERFORM ARMA-DESC-CODM78
      * DECLARE DECLARE @BAZ026-INI SE MODIFICA DESCRIPCION A MOSTRAR PARA CODIGO L67
      * DECLARE DECLARE @BAZ035-INI SE MODIFICA LA DESCRIPCION DEL L67
              WHEN SW-L67
      *            CONTINUE
      *            MOVE 'VENCIMIENTO INVERSION AZTECA MAS'
                   MOVE 'VENCIMIENTO MERCADO DE DINERO'
                                                TO S209-CONCEPT
      * DECLARE DECLARE @BAZ026-FIN
      * DECLARE DECLARE @BAZ035-INI SE MODIFICA LA DESCRIPCION DEL L67
              WHEN SW-IVA-COMISION
                   MOVE 'TASA '                 TO VA-DESC-OPE(1:5); 
              WHEN SW-548
      *            MOVE WSV-AUX-DESC            TO VA-DESC-OPE
                   PERFORM ARMA-DESC-COD548
              WHEN SW-PAGO-CHEQUE
      *            MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
      *            PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
      *            MOVE VA-NUM-CHEQUE           TO VA-DESC-OPE(8:10); 
                   PERFORM ARMA-DESC-PAGCHEQ
              WHEN SW-PAGO-INT
      *            CONTINUE
                   PERFORM ARMA-DESC-PAGINT
              WHEN SW-PAGO-INT2
      *            CONTINUE
                   PERFORM ARMA-DESC-LIQINT
              WHEN SW-880
      *            CONTINUE
                   PERFORM ARMA-DESC-INTISR
              WHEN SW-PAGO-PREST
                   PERFORM ARMA-DESC-PAGPREST
              WHEN SW-PREST-NOM
                   PERFORM ARMA-DESC-PRESTNOM
                   MOVE 'No. DE PEDIDO'         TO VA-DESC-OPE
              WHEN SW-PAGO-PP
              WHEN SW-R96
              WHEN SW-R97
                   MOVE 'No. DE PEDIDO'         TO VA-DESC-OPE
              WHEN SW-PAGO-SERV
      *            CONTINUE
                   PERFORM ARMA-DESC-PAGSERV
              WHEN SW-PAGO-TC
      *            CONTINUE
                   PERFORM ARMA-DESC-PAGOTDC
              WHEN SW-S34
                   CONTINUE
      *            PERFORM ARMA-DESC-CODS34
              WHEN SW-D25
                   CONTINUE
              WHEN SW-RENOV-INV
      *            CONTINUE
                   PERFORM ARMA-DESC-RENINVPZO
              WHEN SW-417
                   PERFORM ARMA-DESC-COD417
              WHEN SW-RET-EFECT
      *            CONTINUE
                   PERFORM ARMA-DESC-RETEFCTVO
              WHEN SW-L70
      * DECLARE DECLARE @BAZ035-INI SE MODIFICA LA DESCRIPCION DEL L70
      * DECLARE DECLARE @BAZ028-INI
      *            MOVE 'No. OPERACION '        TO VA-DESC-OPE(1:14); 
      *            MOVE WSV-AUX-NUMOPE          TO VA-DESC-OPE(15:9); 
      *            MOVE 'INVERSION AZTECA MAS'  TO S209-CONCEPT
                   MOVE 'INVERSION MERCADO DE DINERO'
                                                TO S209-CONCEPT
      * DECLARE DECLARE @BAZ028-FIN
      * DECLARE DECLARE @BAZ035-FIN SE MODIFICA LA DESCRIPCION DEL L70
              WHEN SW-111
      *            MOVE 'ATM'                   TO VA-DESC-OPE
                   PERFORM ARMA-DESC-COD111
              WHEN SW-106
      *            CONTINUE
                   PERFORM ARMA-DESC-COD106
              WHEN SW-A28
                   CONTINUE
              WHEN SW-212
      *            CONTINUE
                   PERFORM ARMA-DESC-TRANSFE2
              WHEN SW-213
      *            CONTINUE
                   PERFORM ARMA-DESC-TRANSFER
      * DECLARE DECLARE @BAZ074-I
                    IF T607-FLG-FREE1     EQUALS 'C'
                       MOVE CA-DEPALCGP         TO S209-CONCEPT
                    END-IF   
      * DECLARE DECLARE @BAZ074-F             
              WHEN SW-549
                   CONTINUE
              WHEN SW-A35
                   CONTINUE
              WHEN SW-S56
                   PERFORM ARMA-DESC-MTCNS56
              WHEN SW-S58
                   PERFORM ARMA-DESC-COMMTCNS58
      * DECLARE DECLARE @BAZ061-I
              WHEN SW-T96
                   PERFORM ARMA-DESC-MTCNT96
      * DECLARE DECLARE @BAZ061-F
              WHEN SW-T05
                   PERFORM ARMA-DESC-TA
              WHEN SW-T60
                   PERFORM ARMA-DESC-CODT60
              WHEN SW-T59
                   PERFORM ARMA-DESC-CODT59
              WHEN SW-787
                   PERFORM ARMA-DESC-COD787
              WHEN SW-786
                   PERFORM ARMA-DESC-COD786
              WHEN SW-T64
                   PERFORM ARMA-DESC-CODT64
              WHEN SW-T63
                   PERFORM ARMA-DESC-CODT63
              WHEN SW-T31
                   PERFORM ARMA-DESC-CODT31
              WHEN SW-S59
                   PERFORM ARMA-DESC-MTCNS59
              WHEN SW-S60
                   PERFORM ARMA-DESC-MTCNS60
              WHEN SW-S55
                   PERFORM ARMA-DESC-MTCNS55
              WHEN SW-S57
                   PERFORM ARMA-DESC-COMMTCNS57
              WHEN SW-683
                   PERFORM ARMA-DESC-COD683
      *LCR-INI
              WHEN SW-S02
                   MOVE 'Dep�sito cr�dito nomina' TO VA-DESC-OPE
      *LCR-FIN
              WHEN SW-R80
                   PERFORM ARMA-DESC-CODR80
              WHEN SW-656
                   PERFORM ARMA-DESC-COD656
              WHEN SW-U05
                   PERFORM ARMA-DESC-CODU05
              WHEN SW-T13
              WHEN SW-T15
                   PERFORM ARMA-DESC-OPERQR3
              WHEN SW-T39
              WHEN SW-U08
                   PERFORM ARMA-DESC-PAGOCRE
              WHEN SW-P48
              WHEN SW-P50
                   PERFORM ARMA-DESC-CREPREST
              WHEN SW-U79
              WHEN SW-U80
                   PERFORM ARMA-DESC-U79U80
              WHEN SW-U50
                   PERFORM ARMA-DESC-U50
              WHEN SW-U36
                   PERFORM ARMA-DESC-U36
      * DECLARE DECLARE @BAZ018-->INI
              WHEN SW-T53
                   INITIALIZE VA-DESC-OPE
                              S209-CONCEPT
                   MOVE WSV-AUX-DESC(1:30);       TO VA-DESC-OPE
                                                   S209-CONCEPT
      * DECLARE DECLARE @BAZ018<--FIN
      *BAZ019-I
              WHEN SW-V06
                   MOVE WSV-AUX-DESC            TO VA-DESC-OPE
      *BAZ019-F
      * DECLARE DECLARE @BAZ026-INI
      * DECLARE DECLARE @BAZ035-INI
              WHEN SW-Z00
      *            MOVE 'INVERSION AZTECA MAS'  TO S209-CONCEPT
                   MOVE 'INVERSION MERCADO DE DINERO'
                                                TO S209-CONCEPT
      *BAZ036-I
              WHEN SW-Z25
                   PERFORM ARMA-DESC-Z25
              WHEN SW-Z26
                   PERFORM ARMA-DESC-Z26
      *BAZ036-F
      * DECLARE DECLARE @BAZ026-FIN
      * DECLARE DECLARE @BAZ035-FIN
      * DECLARE DECLARE @BAZ073-I
              WHEN SW-G89
                   MOVE 'DEPOSITO PRESTA PRENDA' TO S209-CONCEPT
      * DECLARE DECLARE @BAZ073-F
              WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * DECLARE DECLARE @BAZ056-I
      ******************************************************************
       CONSULTA-DESCOPER-SAPP.
      * SE OBTIENE LA DESCRIPCI�N DEL C�DIGO DE OPERACI�N PREVIAMENTE  *
      * EVALUADO EN SUPER APP      .                                   *
      ******************************************************************
      *
           MOVE VA-COD-MOV                      TO WSS-COD-OPERACION
      *
           EVALUATE TRUE
               WHEN SW-Z83
               WHEN SW-Z84
                    MOVE CA-SUCRBAZ             TO S209-CONCEPT
               WHEN SW-Z85
               WHEN SW-Z86
                    MOVE CA-PACR                TO S209-CONCEPT
               WHEN SW-Z87
               WHEN SW-Z88
                    MOVE CA-ENCTABAZ            TO S209-CONCEPT
      * DECLARE DECLARE @BAZ074-I
                    IF AUX-INTREF71(1:1);   EQUALS 'C'     AND
                       AUX-INTREF71(12:4);  EQUALS 'MBWL'
                       MOVE CA-DEPALCGP         TO S209-CONCEPT
                    END-IF   
      * DECLARE DECLARE @BAZ074-F              
               WHEN SW-Z89
               WHEN SW-Z90
                    MOVE CA-PASER               TO S209-CONCEPT
               WHEN SW-Z91
               WHEN SW-Z92
                    MOVE CA-VETIAI              TO S209-CONCEPT
               WHEN SW-Z93
               WHEN SW-Z94
                    MOVE CA-ENCELBAZ            TO S209-CONCEPT
      * DECLARE DECLARE @BAZ059-I
               WHEN SW-Z96
               WHEN SW-Z98
                    MOVE CA-PAQRBAZ             TO S209-CONCEPT
               WHEN SW-Z95
               WHEN SW-Z97
                    MOVE CA-COQRBAZ             TO S209-CONCEPT
      * DECLARE DECLARE @BAZ059-F
               WHEN SW-Z99
               WHEN SW-AA0
                    MOVE CA-TRCTACHBAZ          TO S209-CONCEPT
               WHEN SW-AA1
               WHEN SW-AA2
                    MOVE CA-COPEBAZ             TO S209-CONCEPT
               WHEN SW-AA3
               WHEN SW-AA4
                    MOVE CA-TRDOBAZ             TO S209-CONCEPT
               WHEN SW-AA5
               WHEN SW-AA6
                    MOVE CA-COMINTBAZ           TO S209-CONCEPT
      * DECLARE DECLARE @BAZ069-INI
      *        WHEN SW-AA7
      *        WHEN SW-AA8
      *             MOVE CA-RECSALBAZ           TO S209-CONCEPT
               WHEN SW-AA7
                    MOVE CA-RECARBAZ            TO S209-CONCEPT
               WHEN SW-AA8
                    MOVE CA-ENVPAREC            TO S209-CONCEPT
      *        WHEN SW-AA9
      *        WHEN SW-AB0
      *             MOVE CA-RETIBAZ             TO S209-CONCEPT
               WHEN SW-AA9
                    MOVE CA-REEMENTEF           TO S209-CONCEPT
               WHEN SW-AB0
                    MOVE CA-RECEPEFEC           TO S209-CONCEPT
      * DECLARE DECLARE @BAZ069-FIN
               WHEN SW-AB1
               WHEN SW-AB2
                    MOVE CA-COMPTIEAIR          TO S209-CONCEPT
               WHEN SW-AB3
               WHEN SW-AB4
                    MOVE CA-COMTARPRE           TO S209-CONCEPT
               WHEN SW-AB5
               WHEN SW-AB6
                    MOVE CA-VENTARPRE           TO S209-CONCEPT
               WHEN SW-AB7
               WHEN SW-AB8
                    MOVE CA-PAGCREDOT           TO S209-CONCEPT
               WHEN SW-AB9
               WHEN SW-AC0
                    MOVE CA-PAGSERVOT           TO S209-CONCEPT
               WHEN SW-AD2
               WHEN SW-AD3
                    MOVE CA-SURCREDIT           TO S209-CONCEPT
               WHEN SW-AD4
               WHEN SW-AD5
                    MOVE CA-COMPAGSER           TO S209-CONCEPT
               WHEN SW-AD6
               WHEN SW-AD7
                    MOVE CA-COMTIEAIR           TO S209-CONCEPT
               WHEN SW-AE0
                    MOVE CA-DEPTDCSUP           TO S209-CONCEPT
               WHEN SW-AE1
                    MOVE CA-RETTDCSUP           TO S209-CONCEPT
               WHEN SW-AE2
               WHEN SW-AE3
                    MOVE CA-PAGCREOTR           TO S209-CONCEPT
               WHEN SW-AF0
               WHEN SW-AF1
      * DECLARE DECLARE @BAZ059-I
               WHEN SW-AG0
      * DECLARE DECLARE @BAZ059-F
                    MOVE CA-RECTARBAZ           TO S209-CONCEPT
      * DECLARE DECLARE @BAZ074-I
                    IF AUX-INTREF71(1:1);   EQUALS 'C'
                       MOVE CA-DEPALCGP         TO S209-CONCEPT
                    END-IF   
               WHEN SW-AD1
                    PERFORM ARMA-DESC-RET-AD1
                    IF VA-USER-089  EQUALS 'BUSMPSAP'
                       MOVE CA-DEPALCGP         TO S209-CONCEPT
                    END-IF
      * DECLARE DECLARE @BAZ074-F 
      * DECLARE DECLARE @BAZ065-I
               WHEN SW-AI2
               WHEN SW-AI3
      *
                    INITIALIZE TCGT010
                               S209-CONCEPT
                               VN-CON2
      *
                    IF CAA-CHANN EQUAL '54'
      * DECLARE DECLARE @BAZ071-INI
                       IF T071-NETNAMEUPD(5:4);  = 'B884'
                          MOVE '6816'           TO T010-COD-TABLE
                       ELSE
                          MOVE '6817'           TO T010-COD-TABLE
                       END-IF
      * DECLARE DECLARE @BAZ071-FIN
                    ELSE
                       MOVE '6615'              TO T010-COD-TABLE
                    END-IF
      *
                    MOVE CAA-ENTIDAD            TO T010-ENTITY
                    MOVE T071-INTREF(1:2);        TO T010-KEY-TABLE(1:2); 
                    MOVE SPACES                 TO T010-KEY-TABLE(3:2); 
      *
                    MOVE CA-COMREDEXT           TO  S209-DESCOPE

                    PERFORM RECUPERA-COMERCIO
      *
                    IF SW-AI3
                       MOVE 'Pago a '           TO S209-CONCEPT
                       MOVE VA-TCDT010-6631(1:VN-CON2); 
                                              TO S209-CONCEPT(8:VN-CON2); 
                    ELSE
                       MOVE 'Pago de '          TO S209-CONCEPT
                       MOVE VA-TCDT010-6631(1:VN-CON2); 
                                              TO S209-CONCEPT(9:VN-CON2); 
                    END-IF

               WHEN SW-AQ1
               WHEN SW-AQ2
                    MOVE CA-COMMISENT           TO S209-CONCEPT
               WHEN SW-AQ3
               WHEN SW-AQ4
                    MOVE CA-SERMISENT           TO S209-CONCEPT
               WHEN SW-AQ5
               WHEN SW-AQ5
                    MOVE CA-PROMISENT           TO S209-CONCEPT
      * DECLARE DECLARE @BAZ065-F
      * DECLARE DECLARE @BAZ069-INI
               WHEN SW-AV5
               WHEN SW-AV6
                    MOVE CA-TRASPSERV           TO S209-CONCEPT
      * DECLARE DECLARE @BAZ069-FIN

               WHEN  OTHER
                    CONTINUE
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ056-F
      * DECLARE DECLARE @BAZ065-I
      ******************************************************************
       RECUPERA-COMERCIO.
      ******************************************************************
      *
           EXEC SQL
              SELECT DTA_TBLKEY
                INTO :T010-DTA-TBLKEY
                 FROM TCDT010 with (nolock); 
                 WHERE COD_TABLE       = :T010-COD-TABLE
                 AND   ENTITY          = :T010-ENTITY
                 AND   KEY_TABLE       = :T010-KEY-TABLE
           END-EXEC
      *
           MOVE SQLCODE                      TO SQL-VALUES
      *
           EVALUATE TRUE
              WHEN SQL-88-OK
                   INSPECT T010-DTA-TBLKEY TALLYING VN-CON2
                        FOR CHARACTERS BEFORE '  '
      *
                   MOVE T010-DTA-TBLKEY(1:VN-CON2); 
                                             TO VA-TCDT010-6631
              WHEN SQL-88-NOT-FOUND
                   CONTINUE
              WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE 'MB2C0009'           TO ABC-DES-PROG
                   MOVE 'SELECT'             TO ABC-REFERENCE1
                   MOVE 'TCDT010'            TO ABC-OBJECT-ERROR
                   MOVE SQLERRM              TO ABC-SQLERRM
                   MOVE SQLCODE              TO ABC-SQLCODE
                   PERFORM 999999-DB2-ABEND
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ065-F

      *BAZ036-I
      ******************************************************************
      *ARMA-DESC-Z25.                                             *
      ******************************************************************
       ARMA-DESC-Z25.
      *
           IF WSV-AUX-DESC(1:04);  = ('DAPP' OR 'Dapp'); 
              INITIALIZE S209-CONCEPT
              MOVE WSV-AUX-DESC   TO S209-CONCEPT
              PERFORM OBTEN-DET600-DAPP
           END-IF
           .
      *
      ******************************************************************
      *ARMA-DESC-Z26.                                              *
      ******************************************************************
       ARMA-DESC-Z26.
      *
           IF WSV-AUX-DESC(1:04);  = ('DAPP' OR 'Dapp'); 
              INITIALIZE S209-CONCEPT
              MOVE WSV-AUX-DESC   TO S209-CONCEPT
              PERFORM OBTEN-DET600-DAPP
           END-IF.
      *
      *BAZ036-F
      ******************************************************************
      *MUEVE-CHEQUE.                                                   *
      ******************************************************************
       MUEVE-CHEQUE.
      *
           IF WSV-AUX-DESC(VN-CONT:1);  IS NUMERIC
              ADD 1                   TO VN-CONQ
              MOVE WSV-AUX-DESC(VN-CONT:1); 
                                      TO VA-NUM-CHEQUE(VN-CONQ:1); 
           ELSE
              SET SW-FIN-OK           TO TRUE
              MOVE ZEROES             TO VN-CONT VN-CONQ
           END-IF
           ADD 1 TO VN-CONT.
      *
      ******************************************************************
      *CONSULTA-TCDT010.                                               *
      * SE CONSULTA LA TABLA; TCDT010 (CATALOGO DE DESCRIPCIONES); ; PARA*
      *TRAEAR EL MOTIVO DE LA DEVOLUCI�N DEL CHEQUE.                   *
      ******************************************************************
       CONSULTA-TCDT010.
      *
           EXEC SQL
              SELECT
                     DTA_TBLKEY
               INTO
                    :T010-DTA-TBLKEY
               FROM TCDT010 with (nolock); 
               WHERE COD_TABLE = :CA-0140
               AND   LNG_DATA  = :CA-E
               AND   KEY_TABLE = :T010-KEY-TABLE
               AND   ENTITY    = :CA-0127
           END-EXEC.
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
                 CONTINUE
             WHEN SQL-88-NOT-FOUND
      *          MOVE 'MDE0027'            TO CAA-COD-ERROR
      *          PERFORM 30000-FIN
                 CONTINUE
             WHEN OTHER
                 INITIALIZE QGECABC
                 MOVE 'MB2C0009'           TO ABC-DES-PROG
                 MOVE 'SELECT'             TO ABC-REFERENCE1
                 MOVE 'TCDT040'            TO ABC-OBJECT-ERROR
                 MOVE SQLERRM              TO ABC-SQLERRM
                 MOVE SQLCODE              TO ABC-SQLCODE
                 PERFORM 999999-DB2-ABEND
           END-EVALUATE.
      *
      ******************************************************************
      *CONSULTA-TCDT040.                                               *
      * SE CONSULTA LA TABLA; TCDT040 (CAT�LOGO DE BANCOS); ; PARA TRAER *
      *LA DESCRIPCI�N DE LA ENTIDAD EMISORA DEL CHEQUE.                *
      ******************************************************************
       CONSULTA-TCDT040.
      *
           EXEC SQL
              SELECT
                     DES_ENTITY
               INTO
                    :V040-DES-ENTITY
               FROM TCDT040 with (nolock); 
               WHERE COD_ENTITY = :V040-COD-ENTITY
           END-EXEC.
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
                 CONTINUE
             WHEN SQL-88-NOT-FOUND
      *          MOVE 'MDE0027'            TO CAA-COD-ERROR
      *          PERFORM 30000-FIN
                 MOVE SPACES               TO V040-DES-ENTITY
             WHEN OTHER
                 MOVE SPACES               TO V040-DES-ENTITY
                 INITIALIZE QGECABC
                 MOVE 'MB2C0009'           TO ABC-DES-PROG
                 MOVE 'SELECT'             TO ABC-REFERENCE1
                 MOVE 'TCDT040'            TO ABC-OBJECT-ERROR
                 MOVE SQLERRM              TO ABC-SQLERRM
                 MOVE SQLCODE              TO ABC-SQLCODE
      *          PERFORM 999999-DB2-ABEND
           END-EVALUATE.
      *
      ******************************************************************
      *CONSULTA-BADT001.                                               *
      * REALIZA CONSULTA A LA TABLA;BADT001 (CHEQUES SBC);  PARA OBETENER*
      *LA ENTIDAD EMISORA DEL CHEQUE.                                  *
      ******************************************************************
       CONSULTA-BADT001.
      *
           INITIALIZE DCLBADV0010
           EXEC SQL
              SELECT T001_ENT_DRWRCHK;
                     T001_CHN;
                     T001_FLG_RETURN;
                     T001_COD_RETURN
               INTO :V0010-ENT-DRWRCHK;
                    :V0010-CHN;
                    :V0010-FLG-RETURN;
                    :V0010-COD-RETURN
               FROM BADT001 with (nolock); 
               WHERE T001_CHN       = :VA-NUM-CHEQUE-9
                 AND T001_DAT_VALUE = :VA-DAT-VALUE
                 AND T001_AMT_CHECK = :AUX-AMT-COMP3
           END-EXEC.
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
             WHEN SQL-88-OK
             WHEN SQL-88-NOT-FOUND
                 CONTINUE
      *          MOVE 'MDE0027'            TO CAA-COD-ERROR
      *          PERFORM 30000-FIN
             WHEN OTHER
                 MOVE 'MB2C0009'           TO ABC-DES-PROG
                 MOVE 'SELECT'             TO ABC-REFERENCE1
                 MOVE 'BADT001'            TO ABC-OBJECT-ERROR
                 MOVE SQLERRM              TO ABC-SQLERRM
                 MOVE SQLCODE              TO ABC-SQLCODE
      *          PERFORM 999999-DB2-ABEND
           END-EVALUATE.
      * DECLARE DECLARE @BAZ032-I
      ******************************************************************
      *CONSULTA-GADT001.                                               *
      * REALIZA CONSULTA A LA TABLA;GADT001; PARA OBTENER LA CANTIDAD  *
      * DE DOLARES ADQUIRIDA POR EL CLIENTE.                           *
      ******************************************************************
       CONSULTA-GADT001.
      *
      * DECLARE DECLARE @BAZ030-I
      *    EXEC SQL
      *       SELECT T001_NUM_REF_OPE;
      *              T001_AMT_TOT_FCC
      *        INTO :V0011-NUM-REF-OPE;
      *             :V0011-AMT-TOT-FCC
      *        FROM GADT001 with (nolock); 
      *        WHERE T001_NUM_REF_OPE = :V0011-NUM-REF-OPE
      *    END-EXEC.
      *
      *    EXEC SQL
      *       SELECT T001_AMT_TOT_FCC
      *        INTO :V0011-AMT-TOT-FCC
      *        FROM GADT001 with (nolock); 
      *        WHERE T001_NUM_REF_OPE = :N100-NUM-REF-OPE
      *    END-EXEC.
      * DECLARE DECLARE @BAZ030-F
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *      WHEN SQL-88-OK
      *          CONTINUE
      *      WHEN SQL-88-NOT-FOUND
      *          MOVE 'MDE0027'            TO CAA-COD-ERROR
      *          PERFORM 30000-FIN
      *      WHEN OTHER
      *          INITIALIZE QGECABC
      *          MOVE 'MB2C0009'           TO ABC-DES-PROG
      *          MOVE 'SELECT'             TO ABC-REFERENCE1
      *          MOVE 'GADT001'            TO ABC-OBJECT-ERROR
      *          MOVE SQLERRM              TO ABC-SQLERRM
      *          MOVE SQLCODE              TO ABC-SQLCODE
      *          PERFORM 999999-DB2-ABEND
      *    END-EVALUATE.
      * DECLARE DECLARE @BAZ032-F
      ******************************************************************
      *CONSULTA-BGDT606.                                               *
      ******************************************************************
       CONSULTA-BGDT606.
      *
           INITIALIZE DCLBGDT606
      *
           MOVE E009-NUMCUEN(5:10);          TO T606-ACC
      *    MOVE T071-NUM-OPERATION         TO T606-NUM-OPERATION
           MOVE WSV-AUX-NUMOPE             TO T606-NUM-OPERATION
           MOVE WSV-AUX-FECHA              TO T606-DAT-OPERATION
      * DECLARE DECLARE @BAZ037-I
           IF TB-T606-ACC(I-REG);                 EQUAL SPACES OR
              TB-T606-ACC(I-REG);                 EQUAL LOW-VALUES
      *
               MOVE SPACES                      TO T606-ACC
                                                   T606-DESCRIPTION
                                                   T606-PATH
                                                   T606-DAT-OPERATION
                                                   T606-FLG-FREE1
                                                   T606-CHAR-FREE1
      *
               MOVE ZEROS                       TO T606-AMOUNT
                                                   T606-NUM-OPERATION
                                                   T606-GPS-LAT
                                                   T606-GPS-LONG
           ELSE
              MOVE TB-T606-ACC(I-REG);            TO T606-ACC


              MOVE ZERO                         TO VA-CUANTOS-MENOS
              INSPECT TB-T606-AMOUNT2(I-REG);  TALLYING VA-CUANTOS-MENOS
                       FOR ALL '-'
              IF VA-CUANTOS-MENOS > 0
                 INSPECT TB-T606-AMOUNT2(I-REG);  REPLACING ALL '-' BY
                                                           ''
                 MOVE TB-T606-AMOUNT(I-REG);         TO T606-AMOUNT
                 COMPUTE T606-AMOUNT= T606-AMOUNT * -1
              ELSE
                 MOVE TB-T606-AMOUNT(I-REG);         TO T606-AMOUNT
              END-IF


              MOVE TB-T606-NUM-OPERATION(I-REG);  TO T606-NUM-OPERATION
              MOVE TB-T606-DESCRIPTION(I-REG);    TO T606-DESCRIPTION
              MOVE TB-T606-PATH(I-REG);           TO T606-PATH
              MOVE TB-T606-GPS-LAT(I-REG);        TO T606-GPS-LAT
              MOVE TB-T606-GPS-LONG(I-REG);       TO T606-GPS-LONG
              MOVE TB-T606-DAT-OPERATION(I-REG);  TO T606-DAT-OPERATION
              MOVE TB-T606-FLG-FREE1(I-REG);      TO T606-FLG-FREE1
              MOVE TB-T606-CHAR-FREE1(I-REG);     TO T606-CHAR-FREE1
           END-IF.
      *
      *    EXEC SQL
      *       SELECT
      *                T606_ACC
      *              ; T606_AMOUNT
      *              ; T606_NUM_OPERATION
      *              ; T606_DESCRIPTION
      *              ; T606_PATH
      *              ; T606_GPS_LAT
      *              ; T606_GPS_LONG
      *              ; T606_DAT_OPERATION
      *              ; T606_FLG_FREE1
      *LCR-INI2
      *              ; T606_CHAR_FREE1
      *LCR-FIN2
      *
      *        INTO
      *               :T606-ACC
      *              ;:T606-AMOUNT
      *              ;:T606-NUM-OPERATION
      *              ;:T606-DESCRIPTION
      *              ;:T606-PATH
      *              ;:T606-GPS-LAT
      *              ;:T606-GPS-LONG
      *              ;:T606-DAT-OPERATION
      *              ;:T606-FLG-FREE1
      *LCR-INI2
      *              ;:T606-CHAR-FREE1
      *LCR-FIN2
      *
      *        FROM BGDT606 with (nolock); 
      *       WHERE T606_ACC           = :T606-ACC
      *         AND T606_NUM_OPERATION = :T606-NUM-OPERATION
      *         AND T606_DAT_OPERATION = :T606-DAT-OPERATION
      *    END-EXEC.
      *
      *    MOVE SQLCODE                    TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *       WHEN SQL-88-OK
      *LCR-INI2
      *          PERFORM 29991-MOVER-SALIDA-3
      *LCR-FIN2
      *       WHEN SQL-88-NOT-FOUND
      *          CONTINUE
      *          PERFORM INSERT-BGDT606
      *
      *          PERFORM 30000-FIN
      *       WHEN OTHER
      *          MOVE 'MB2C0009'           TO ABC-DES-PROG
      *          MOVE 'SELECT'             TO ABC-REFERENCE1
      *          MOVE 'BGDT606'            TO ABC-OBJECT-ERROR
      *          MOVE SQLERRM              TO ABC-SQLERRM
      *          MOVE SQLCODE              TO ABC-SQLCODE
      *          PERFORM 999999-DB2-ABEND
      *    END-EVALUATE
      *    .
      * DECLARE DECLARE @BAZ037-F
      * DECLARE DECLARE @BAZ.I***********************************************************
      *.PN ARMA-DESC-COD907.                                           *
      * OBTIENE EMISORA DE BANCA EMPRESARIAL                           *
      ******************************************************************
       ARMA-DESC-COD907.
      * DECLARE DECLARE @BAZ0007I.I
           IF WSV-AUX-DESC(1:20);  = 'DEPOSITO DE TERCEROS'
              INITIALIZE S209-CONCEPT
              MOVE WSV-AUX-DESC(1:30);  TO S209-CONCEPT
           END-IF
      * DECLARE DECLARE @BAZ0007I.F
           INITIALIZE DCLDMDT003.
      *
           MOVE E009-NUMCUEN(5:10);    TO T003-ACC-EMP
           MOVE E009-NUMCUEN(1:4);     TO T003-BRN-EMP
           MOVE CAA-ENT-ACC          TO T003-ENT-EMP T003-ENT
           EXEC SQL
             SELECT TOP 1
                    T003_NUM_E
              INTO :T003-NUM-E
             FROM DMDT003 with(nolock); 
               WHERE T003_BRN_EMP = :T003-BRN-EMP
                 AND T003_ACC_EMP = :T003-ACC-EMP
                 AND T003_ENT_EMP = :T003-ENT-EMP
                 AND T003_ENT     = :T003-ENT
             ORDER BY T003_DAT_UPDATE DESC
           END-EXEC.
           MOVE SQLCODE         TO SQL-VALUES
      *
           EVALUATE TRUE
           WHEN SQL-88-OK
                MOVE T003-NUM-E TO VA-DESC-OPE
           WHEN OTHER
                MOVE SPACES     TO VA-DESC-OPE
           END-EVALUATE
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODA36.                                           *
      * OBTIENE NOMBRE DE INVERSION                                    *
      ******************************************************************
       ARMA-DESC-CODA36.
      *
            INITIALIZE DCLBGGT235
            INITIALIZE BGVC041
            INITIALIZE WPWC0010   AUX-DESINT-APERT
      *
            MOVE WSV-AUX-DESC(21:10);       TO T235-ACC
            MOVE CAA-ENT-ACC              TO T235-ENT-ASSO
            MOVE E009-NUMCUEN(1:4);         TO T235-CEN-ASSO
            MOVE E009-NUMCUEN(5:10);        TO T235-ACC-ASSO
      *
            PERFORM QUERY-BGDT235
      *
            MOVE SQLCODE                  TO SQL-VALUES
            EVALUATE TRUE
            WHEN SQL-88-OK
                 MOVE T235-CEN-REG        TO AUX-CENTRO
            WHEN OTHER
                 MOVE E009-NUMCUEN(1:4);    TO AUX-CENTRO
            END-EVALUATE
      *
            MOVE CAA-ENT-ACC              TO V041-ENT
            MOVE AUX-CENTRO               TO V041-CEN-REG
            MOVE T235-ACC                 TO V041-ACC
      *
            PERFORM QUERY-BGDT041
      *
            MOVE SQLCODE TO SQL-VALUES
            IF SQL-88-OK
               MOVE CAA-ENT-ACC           TO T003-COD-ENTITY
               MOVE V041-COD-PROD         TO T003-COD-PRODUCT
               MOVE V041-COD-SPROD        TO T003-CODE
               MOVE '2'                   TO T003-TYP-CODE
               MOVE 'E'                   TO T003-COD-LANGUAGE
               PERFORM QUERY-WPDT003
               MOVE SQLCODE TO SQL-VALUES
               EVALUATE TRUE
               WHEN SQL-88-OK
      *             MOVE T003-SDE-CODE    TO VA-DESC-OPE(1:15); 
      *             MOVE V041-CEN-REG     TO VA-DESC-OPE(17:04); 
      *             MOVE V041-ACC         TO VA-DESC-OPE(21:10); 
                    MOVE T003-DES-CODE    TO AUX-DESINT-APERT1
                    MOVE V041-CEN-REG     TO AUX-DESINT-APERT2(1:4); 
                    MOVE V041-ACC         TO AUX-DESINT-APERT2(5:10); 
                    STRING AUX-DESINT-APERT1  DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                            AUX-DESINT-APERT2 DELIMITED BY '  '
                                              INTO VA-DESC-OPE
               WHEN OTHER
                   MOVE 'NOMBRE INVERSION ' TO VA-DESC-OPE(1:17); 
                   MOVE V041-CEN-REG        TO VA-DESC-OPE(18:4); 
                   MOVE V041-ACC            TO VA-DESC-OPE(22:10); 
               END-EVALUATE
            ELSE
               MOVE 'INVERSION No.'        TO VA-DESC-OPE(1:13); 
               MOVE T235-ACC               TO VA-DESC-OPE(14:10); 
            END-IF
            .
      *
      ******************************************************************
      *.PN ARMA-DESC-COD160.                                           *
      * OBTIENE PERSONA QUIEN DEPOSITA                                 *
      ******************************************************************
       ARMA-DESC-COD160.
      * --Celular
           IF WSV-AUX-DESC(1:08);  = ('Envio de' OR 'ENVIO DE'); 
              INITIALIZE S209-CONCEPT
      * DECLARE DECLARE @BAZ011.I
      *       MOVE WSV-AUX-DESC(10:10);    TO AUX-CEL
              MOVE WSV-AUX-DESC(10:15);    TO AUX-CEL
      * DECLARE DECLARE @BAZ011.F
              PERFORM SELECT-MCDT403-CEL
      * DECLARE DECLARE @BAZ016-->INI
              IF AUX-USERUPD EQUAL 'BCARDIGI'
                 MOVE 'Env�o Realizado Wallet   ' TO S209-CONCEPT
                 MOVE  AUX-CEL                    TO VA-DESC-OPE
              ELSE
                 MOVE 'Envio de dinero a celular' TO S209-CONCEPT
                 MOVE 'De '                 TO VA-DESC-OPE(1:3); 
                 MOVE AUX-NOMBRECTE         TO VA-DESC-OPE(4:40); 
              END-IF
      * DECLARE DECLARE @BAZ016<--FIN
           ELSE
      * DECLARE DECLARE @BAZ007C.I
      * --Aportaci�n Afore
            IF (AUX-INTREF71(12:4);  = 'MB41'); 
               MOVE 'Aportaci�n voluntaria Afore' TO VA-DESC-OPE
            ELSE
      * DECLARE DECLARE @BAZ007C.F
      * --Socio Plus
             IF (AUX-INTREF71(12:4);  = 'MB45' OR
                 WSV-AUX-DESC(1:16);  = 'Venta Socio Plus'); 
                 IF AUX-INTREF71(12:4);  = 'B520'
                   MOVE WSV-AUX-DESC     TO VA-DESC-OPE
                 ELSE
      * DECLARE DECLARE @BAZ007H.I
      *            MOVE 'Retiro de cuenta Socio Plus' TO VA-DESC-OPE
                   INITIALIZE S209-CONCEPT
                   MOVE 'Dep�sito por Retiro de Cuenta Socio Plus'
      *                                               TO VA-DESC-OPE
                                                      TO S209-CONCEPT
      * DECLARE DECLARE @BAZ007H.F
                 END-IF
             ELSE
      * --Traspasos
      * DECLARE DECLARE @BAZ016-->INI
                IF (AUX-INTREF71(12:4);  = 'MB03');  AND
                  AUX-USERUPD = 'BCARDIGI'
                  INITIALIZE S209-CONCEPT
                  MOVE 'Env�o Realizado Wallet   ' TO S209-CONCEPT
                  MOVE AUX-DESC(10:15);              TO VA-DESC-OPE
                ELSE
      *
                  INITIALIZE BGNC477
      *
                  MOVE WSV-AUX-NUMOPE         TO BGNC477-NUM-OP
                  MOVE AUX-CTA-INPUT          TO BGNC477-ACC
      *
                  EXEC CICS
                     LINK PROGRAM (CA-BG7C4770); 
                     COMMAREA(CA-BGNC477); 
                     NOHANDLE
                  END-EXEC
      *
                  IF EIBRESP EQUAL DFHRESP(NORMAL); 
                     IF BGNC477-COD-ERR EQUAL TO SPACES
                        PERFORM COD160-ABONO
                     ELSE
                        MOVE SPACES           TO VA-DESC-OPE
                     END-IF
                  ELSE
                     MOVE SPACES              TO VA-DESC-OPE
                  END-IF
                END-IF
               END-IF
              END-IF
           END-IF
      * DECLARE DECLARE @BAZ016<--FIN

      * DECLARE DECLARE @BAZ045-INI
           IF (AUX-INTREF71(12:4);  = 'MBPB' OR
               WSV-AUX-DESC(1:05);  = 'VFP_2'); 
                 INITIALIZE S209-CONCEPT
                 MOVE 'Dep�sito por Retiro de Cuenta FIAR Plus'
                                                    TO S209-CONCEPT
           END-IF
      * DECLARE DECLARE @BAZ045-FIN
           .
      *
      * DECLARE DECLARE @BAZ062-I
      ******************************************************************
      * ARMA-DESC-CODMBS4
      ******************************************************************
       ARMA-DESC-CODMBS4.

            INITIALIZE BGNC477

            MOVE WSV-AUX-NUMOPE         TO BGNC477-NUM-OP
            MOVE AUX-CTA-INPUT          TO BGNC477-ACC

            EXEC CICS
               LINK PROGRAM (CA-BG7C4770); 
               COMMAREA(CA-BGNC477); 
               NOHANDLE
            END-EXEC

            IF EIBRESP EQUAL DFHRESP(NORMAL); 
               IF BGNC477-COD-ERR EQUAL TO SPACES
                  PERFORM CODMBS4-CARGO
               ELSE
                  MOVE SPACES           TO VA-DESC-OPE
               END-IF
            ELSE
               MOVE SPACES              TO VA-DESC-OPE
            END-IF .
      * DECLARE DECLARE @BAZ062-F
      ******************************************************************
      *.PN ARMA-DESC-COD169.                                           *
      * OBTIENE PERSONA BENEFICIARIO.                                  *
      ******************************************************************
       ARMA-DESC-COD169.
      * -- Celular
           IF WSV-AUX-DESC(1:07);  = ('Envio a' OR 'ENVIO A'); 
              INITIALIZE S209-CONCEPT AUX-DES169-TRASCEL1
      * DECLARE DECLARE @BAZ011.I
      *       MOVE WSV-AUX-DESC(09:10);    TO AUX-CEL
              MOVE WSV-AUX-DESC(09:15);    TO AUX-CEL
      * DECLARE DECLARE @BAZ011.F
              PERFORM SELECT-MCDT403-CEL
      * DECLARE DECLARE @BAZ016-->INI
              IF AUX-USERUPD EQUAL 'BCARDIGI'
                 MOVE 'Env�o Realizado Wallet   ' TO S209-CONCEPT
                 MOVE  AUX-CEL                    TO VA-DESC-OPE
              ELSE
                 MOVE 'Envio de dinero a celular' TO S209-CONCEPT
                 MOVE 'Para'                      TO AUX-DES169-TRASCEL1
                 MOVE AUX-NOMBRECTE               TO AUX-DES169-TRASCEL2
                 MOVE ';'                         TO AUX-DES169-TRASCEL3
                 MOVE AUX-CEL                     TO AUX-DES169-TRASCEL4
                 STRING AUX-DES169-TRASCEL1  DELIMITED BY '  '
                              ' ' DELIMITED BY SIZE
                    AUX-DES169-TRASCEL2 DELIMITED BY '  '
      *                       ' ' DELIMITED BY SIZE
                    AUX-DES169-TRASCEL3  DELIMITED BY '  '
                              ' ' DELIMITED BY SIZE
                    AUX-DES169-TRASCEL4 DELIMITED BY '  '
                                  INTO VA-DESC-OPE
              END-IF
      *
      * DECLARE DECLARE @BAZ016<--FIN
           ELSE
      * DECLARE DECLARE @BAZ007C.I
      * --Aportaci�n Afore
            IF (AUX-INTREF71(12:4);  = 'MB41'); 
               MOVE 'Aportaci�n voluntaria Afore' TO VA-DESC-OPE
            ELSE
      * DECLARE DECLARE @BAZ007C.F
      * --Socio Plus
             IF (AUX-INTREF71(12:4);  = 'MB45' OR
                 WSV-AUX-DESC(1:17);  = 'Compra Socio Plus'); 
                 IF AUX-INTREF71(12:4);  = 'B520'
                   MOVE WSV-AUX-DESC     TO VA-DESC-OPE
                 ELSE
      * DECLARE DECLARE @BAZ007H.I
      *            MOVE 'Dep�sito a cuenta Socio Plus' TO VA-DESC-OPE
                   INITIALIZE S209-CONCEPT
                   MOVE 'Cargo por dep�sito a Cuenta Socio Plus'
      *                                                TO VA-DESC-OPE
                                                       TO S209-CONCEPT
      * DECLARE DECLARE @BAZ007H.F
                 END-IF
             ELSE
      * --Traspasos
      * DECLARE DECLARE @BAZ016-->INI
                IF (AUX-INTREF71(12:4);  = 'MB03');  AND
                  AUX-USERUPD = 'BCARDIGI'
                  INITIALIZE S209-CONCEPT
                  MOVE 'Env�o Realizado Wallet   ' TO S209-CONCEPT
                  MOVE AUX-DESC(10:15);              TO VA-DESC-OPE
                ELSE
      *
                  INITIALIZE BGNC477
      *
                  MOVE WSV-AUX-NUMOPE         TO BGNC477-NUM-OP
                  MOVE AUX-CTA-INPUT          TO BGNC477-ACC
      *
                  EXEC CICS
                     LINK PROGRAM (CA-BG7C4770); 
                     COMMAREA(CA-BGNC477); 
                     NOHANDLE
                  END-EXEC
      *
                  IF EIBRESP EQUAL DFHRESP(NORMAL); 
                     IF BGNC477-COD-ERR EQUAL TO SPACES
                        PERFORM COD169-CARGO
                     ELSE
                        MOVE SPACES           TO VA-DESC-OPE
                     END-IF
                  ELSE
                     MOVE SPACES              TO VA-DESC-OPE
                  END-IF
                END-IF
               END-IF
              END-IF
           END-IF

      * DECLARE DECLARE @BAZ045-INI
           IF (AUX-INTREF71(12:4);  = 'MBPB' OR
               WSV-AUX-DESC(1:05);  = 'CFP_2'); 
                 INITIALIZE S209-CONCEPT
                 MOVE 'Cargo por dep�sito a Cuenta FIAR Plus'
                                                    TO S209-CONCEPT
           END-IF
      * DECLARE DECLARE @BAZ045-FIN
           .
      *
      *
      ******************************************************************
      *.PN COD160-ABONO.                                               *
      ******************************************************************
       COD160-ABONO.
      *
           INITIALIZE AUX1-CTABEN
                      AUX2-NUMCTA  AUX-NUMCUS8-TRSP
      * DECLARE DECLARE @BAZ041-I
                      AUX-NOM-CTE
                      AUX2-NOMBRECTE
      * DECLARE DECLARE @BAZ041-F
           MOVE BGNC477-ACC-DEB         TO AUX1-CTABEN
           MOVE AUX1-CTABEN(5:4);         TO AUX2-NUMCEN
           MOVE AUX1-CTABEN(11:10);       TO AUX2-NUMACC
      *    -- Obtiene Nombre del Beneficiario
           PERFORM OBTEN-DATOS-CLIENTE
      * DECLARE DECLARE @BAZ023.I
              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SCDNAME DELIMITED BY '   '
                                    INTO AUX-NOM-CTE
              WHEN OTHER
                   MOVE SPACES TO AUX-NOM-CTE
              END-EVALUATE
      * DECLARE DECLARE @BAZ023.F
      *
           MOVE AUX-NUMCUS8             TO AUX-NUMCUS8-DEB
           MOVE AUX-NOMBRECTE           TO AUX-NOMCUS8-DEB
           INITIALIZE AUX1-CTABEN
                      AUX2-NUMCTA
           MOVE BGNC477-ACC-CRED        TO AUX1-CTABEN
           MOVE AUX1-CTABEN(5:4);         TO AUX2-NUMCEN
           MOVE AUX1-CTABEN(11:10);       TO AUX2-NUMACC
      *    -- Obtiene Nombre del Depositante Ordenante
           PERFORM OBTEN-DATOS-CLIENTE
      * DECLARE DECLARE @BAZ041-I
              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SCDNAME DELIMITED BY '   '
                                    INTO AUX2-NOMBRECTE
              WHEN OTHER
                   MOVE SPACES          TO AUX2-NOMBRECTE
              END-EVALUATE
      * DECLARE DECLARE @BAZ041-F
           MOVE AUX-NUMCUS8             TO AUX-NUMCUS8-CRE
           MOVE AUX-NOMBRECTE           TO AUX-NOMCUS8-CRE
      *
           IF AUX-NUMCUS8-DEB = SPACES AND AUX-NUMCUS8-CRE = SPACES
              MOVE 'TITULAR '           TO VA-DESC-OPE(1:8); 
              MOVE AUX-NOMBRECTE        TO VA-DESC-OPE(9:40); 
           ELSE
              IF AUX-NUMCUS8-DEB EQUAL AUX-NUMCUS8-CRE
      *         -- Propias
      * DECLARE DECLARE @BAZ066.I
                IF SW-B925-SI
                   CONTINUE
                ELSE
      * DECLARE DECLARE @BAZ066.F
      *
                   INITIALIZE S209-CONCEPT
      * DECLARE DECLARE @BAZ005D.I
      *         MOVE 'Traspaso a cuenta ' TO S209-CONCEPT
                   MOVE 'Traspaso a cuentas propias ' TO S209-CONCEPT
      * DECLARE DECLARE @BAZ005D.F
                END-IF
      *
                MOVE 'De '                TO VA-DESC-OPE(1:3); 
                MOVE '****'               TO VA-DESC-OPE(4:4); 
                MOVE BGNC477-ACC-DEB(17:4);    TO VA-DESC-OPE(8:4); 
      **        MOVE BGNC477-ACC-CRED(17:4);   TO VA-DESC-OPE(8:4); 
      *         MOVE BGNC477-ACC-CRED(11:10);  TO VA-DESC-OPE(4:10); 
      *         MOVE 'De'                 TO AUX-DES160-TRASTER1
      *         MOVE AUX-NOMCUS8-CRE      TO AUX-DES160-TRASTER2
      *         MOVE '****'               TO AUX-DES160-TRASTER3(1:4); 
      *         MOVE BGNC477-ACC-CRED(17:4);  TO AUX-DES160-TRASTER3(5:4); 
              ELSE
      *         -- Terceros
      * DECLARE DECLARE @BAZ066.I
                IF SW-B925-SI
                   CONTINUE
                ELSE
      * DECLARE DECLARE @BAZ066.F
      *
                   INITIALIZE S209-CONCEPT
                   MOVE 'Traspaso de cuenta ' TO S209-CONCEPT
                END-IF
                MOVE 'De'                 TO AUX-DES160-TRASTER1
      *         MOVE AUX-NOMCUS8-CRE      TO AUX-DES160-TRASTER2
                MOVE AUX-NOMCUS8-DEB      TO AUX-DES160-TRASTER2
                MOVE '****'               TO AUX-DES160-TRASTER3(1:4); 
      *         MOVE BGNC477-ACC-CRED(17:4);  TO AUX-DES160-TRASTER3(5:4); 
                MOVE BGNC477-ACC-DEB(17:4);   TO AUX-DES160-TRASTER3(5:4); 
                STRING AUX-DES160-TRASTER1 DELIMITED BY '  '
                                       ' ' DELIMITED BY SIZE
                       AUX-DES160-TRASTER2 DELIMITED BY '  '
                                       ' ' DELIMITED BY SIZE
                       AUX-DES160-TRASTER3 DELIMITED BY '  '
                                           INTO VA-DESC-OPE
              END-IF
      * DECLARE DECLARE @BAZ008B.I -- se complementa con el concepto de referencia
              MOVE T606-DESCRIPTION(14:50);  TO VA-BENEFIC
      * DECLARE DECLARE @BAZ008B.F
           END-IF
      * DECLARE DECLARE @BAZ066.I
           SET SW-B925-NO                 TO TRUE
      * DECLARE DECLARE @BAZ066.F
      *
           .
      *
      * DECLARE DECLARE @BAZ062-I
      ************************************************************
      * CODMBS4-CARGO.
      ************************************************************
       CODMBS4-CARGO.
      *
           INITIALIZE AUX1-CTABEN
                      AUX2-NUMCTA  AUX-NUMCUS8-TRSP
                      AUX-NOM-CTE
                      AUX2-NOMBRECTE

           MOVE BGNC477-ACC-CRED        TO AUX1-CTABEN
           MOVE AUX1-CTABEN(5:4);         TO AUX2-NUMCEN
           MOVE AUX1-CTABEN(11:10);       TO AUX2-NUMACC
      *    -- Obtiene Nombre del Beneficiario
           PERFORM OBTEN-DATOS-CLIENTE

              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SCDNAME DELIMITED BY '   '
                                    INTO AUX-NOM-CTE
              WHEN OTHER
                   MOVE SPACES TO AUX-NOM-CTE
              END-EVALUATE .
      * DECLARE DECLARE @BAZ062-F
      ******************************************************************
      *.PN COD169-CARGO.                                               *
      ******************************************************************
       COD169-CARGO.
      *
           INITIALIZE AUX1-CTABEN
                      AUX2-NUMCTA  AUX-NUMCUS8-TRSP
      * DECLARE DECLARE @BAZ041-I
                      AUX-NOM-CTE
                      AUX2-NOMBRECTE
      * DECLARE DECLARE @BAZ041-F
           MOVE BGNC477-ACC-CRED        TO AUX1-CTABEN
           MOVE AUX1-CTABEN(5:4);         TO AUX2-NUMCEN
           MOVE AUX1-CTABEN(11:10);       TO AUX2-NUMACC
      *    -- Obtiene Nombre del Beneficiario
           PERFORM OBTEN-DATOS-CLIENTE
      * DECLARE DECLARE @BAZ041-I
              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SCDNAME DELIMITED BY '   '
                                    INTO AUX-NOM-CTE
              WHEN OTHER
                   MOVE SPACES TO AUX-NOM-CTE
              END-EVALUATE
      * DECLARE DECLARE @BAZ041-F

           MOVE AUX-NUMCUS8             TO AUX-NUMCUS8-CRE
           MOVE AUX-NOMBRECTE           TO AUX-NOMCUS8-CRE
           INITIALIZE AUX1-CTABEN
                      AUX2-NUMCTA
           MOVE BGNC477-ACC-DEB         TO AUX1-CTABEN
           MOVE AUX1-CTABEN(5:4);         TO AUX2-NUMCEN
           MOVE AUX1-CTABEN(11:10);       TO AUX2-NUMACC
      *    -- Obtiene Nombre del Depositante Ordenante
           PERFORM OBTEN-DATOS-CLIENTE
      * DECLARE DECLARE @BAZ041-I
              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SCDNAME DELIMITED BY '   '
                                    INTO AUX2-NOMBRECTE
              WHEN OTHER
                   MOVE SPACES TO AUX2-NOMBRECTE
              END-EVALUATE
      * DECLARE DECLARE @BAZ041-F

           MOVE AUX-NUMCUS8             TO AUX-NUMCUS8-DEB
           MOVE AUX-NOMBRECTE           TO AUX-NOMCUS8-DEB
      *
           IF AUX-NUMCUS8-CRE = SPACES AND AUX-NUMCUS8-DEB = SPACES
              MOVE 'BENEFICIARIO '      TO VA-DESC-OPE(1:13); 
              MOVE AUX-NOMBRECTE        TO VA-DESC-OPE(14:40); 
           ELSE
              IF AUX-NUMCUS8-CRE EQUAL AUX-NUMCUS8-DEB
      *         -- Propias
                INITIALIZE S209-CONCEPT
      * DECLARE DECLARE @BAZ005D.I
      *         MOVE 'Traspaso a cuenta ' TO S209-CONCEPT
                MOVE 'Traspaso a cuentas propias ' TO S209-CONCEPT
      * DECLARE DECLARE @BAZ005D.F
                MOVE 'Para '              TO VA-DESC-OPE(1:5); 
                MOVE '****'               TO VA-DESC-OPE(6:4); 
                MOVE BGNC477-ACC-CRED(17:4);  TO VA-DESC-OPE(10:4); 
      *         MOVE BGNC477-ACC-DEB(17:4);   TO VA-DESC-OPE(10:4); 
      *         MOVE BGNC477-ACC-DEB(11:10);  TO VA-DESC-OPE(6:10); 
      *         MOVE 'Para'               TO AUX-DES169-TRASTER1
      *         MOVE AUX-NOMCUS8-DEB      TO AUX-DES169-TRASTER2
      *         MOVE '****'               TO AUX-DES169-TRASTER3(1:4); 
      *         MOVE BGNC477-ACC-DEB(17:4);  TO AUX-DES169-TRASTER3(5:4); 
              ELSE
      *         -- Terceros
                INITIALIZE S209-CONCEPT
                MOVE 'Traspaso de cuenta ' TO S209-CONCEPT
                MOVE 'Para'               TO AUX-DES169-TRASTER1
      *         MOVE AUX-NOMCUS8-DEB      TO AUX-DES169-TRASTER2
                MOVE AUX-NOMCUS8-CRE      TO AUX-DES169-TRASTER2
                MOVE '****'               TO AUX-DES169-TRASTER3(1:4); 
      *         MOVE BGNC477-ACC-DEB(17:4);  TO AUX-DES169-TRASTER3(5:4); 
                MOVE BGNC477-ACC-CRED(17:4);  TO AUX-DES169-TRASTER3(5:4); 
                STRING AUX-DES169-TRASTER1 DELIMITED BY '  '
                                       ' ' DELIMITED BY SIZE
                       AUX-DES169-TRASTER2 DELIMITED BY '  '
                                       ' ' DELIMITED BY SIZE
                       AUX-DES169-TRASTER3 DELIMITED BY '  '
                                           INTO VA-DESC-OPE
              END-IF
      * DECLARE DECLARE @BAZ008B.I -- se complementa con el concepto de referencia
              MOVE T606-DESCRIPTION(14:50);  TO VA-BENEFIC
      * DECLARE DECLARE @BAZ008B.F
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODM78.                                           *
      * OBTIENE NOMBRE DE PRODUCTO                                     *
      ******************************************************************
       ARMA-DESC-CODM78.
      *
            INITIALIZE DCLBGGT235
            INITIALIZE BGVC041
            INITIALIZE WPWC0010  AUX-DESINT-APERT
      *
           IF WSV-AUX-DESC(1:06);  = 'APERT '
            MOVE WSV-AUX-DESC(20:10);       TO T235-ACC
            MOVE CAA-ENT-ACC              TO T235-ENT-ASSO
            MOVE E009-NUMCUEN(1:4);         TO T235-CEN-ASSO
            MOVE E009-NUMCUEN(5:10);        TO T235-ACC-ASSO
      *
            PERFORM QUERY-BGDT235
      *
            MOVE SQLCODE                  TO SQL-VALUES
            EVALUATE TRUE
            WHEN SQL-88-OK
                 MOVE T235-CEN-REG        TO AUX-CENTRO
            WHEN OTHER
                 MOVE E009-NUMCUEN(1:4);    TO AUX-CENTRO
            END-EVALUATE
      *
            MOVE CAA-ENT-ACC              TO V041-ENT
            MOVE AUX-CENTRO               TO V041-CEN-REG
            MOVE T235-ACC                 TO V041-ACC
      *
            PERFORM QUERY-BGDT041
      *
            MOVE SQLCODE TO SQL-VALUES
            IF SQL-88-OK
               MOVE CAA-ENT-ACC           TO T003-COD-ENTITY
               MOVE V041-COD-PROD         TO T003-COD-PRODUCT
               MOVE V041-COD-SPROD        TO T003-CODE
               MOVE '2'                   TO T003-TYP-CODE
               MOVE 'E'                   TO T003-COD-LANGUAGE
               PERFORM QUERY-WPDT003
               MOVE SQLCODE TO SQL-VALUES
               EVALUATE TRUE
               WHEN SQL-88-OK
      *             MOVE T003-SDE-CODE    TO VA-DESC-OPE(1:15); 
      *             MOVE V041-CEN-REG     TO VA-DESC-OPE(17:04); 
      *             MOVE V041-ACC         TO VA-DESC-OPE(21:10); 
                    MOVE T003-DES-CODE    TO AUX-DESINT-APERT1
                    MOVE V041-CEN-REG     TO AUX-DESINT-APERT2(1:4); 
                    MOVE V041-ACC         TO AUX-DESINT-APERT2(5:10); 
                    STRING AUX-DESINT-APERT1  DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                            AUX-DESINT-APERT2 DELIMITED BY '  '
                                              INTO VA-DESC-OPE
               WHEN OTHER
                   MOVE 'NOMBRE INVERSION ' TO VA-DESC-OPE(1:17); 
                   MOVE V041-CEN-REG        TO VA-DESC-OPE(18:4); 
                   MOVE V041-ACC            TO VA-DESC-OPE(22:10); 
               END-EVALUATE
            ELSE
               MOVE 'INVERSION No.'        TO VA-DESC-OPE(1:13); 
               MOVE T235-ACC               TO VA-DESC-OPE(14:10); 
            END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-PAGINT.                                           *
      * OBTIENE NOMBRE DE PRODUCTO DE PAGO DE INTERES                  *
      ******************************************************************
       ARMA-DESC-PAGINT.
      *
            INITIALIZE DCLBGGT235
            INITIALIZE BGVC041
            INITIALIZE WPWC0010     AUX-DESINT-APERT
      *
           IF VA-COD-MOV = '182'
            MOVE WSV-AUX-DESC(20:10);       TO T235-ACC
           ELSE
            MOVE WSV-AUX-DESC(21:10);       TO T235-ACC
           END-IF
            MOVE CAA-ENT-ACC              TO T235-ENT-ASSO
            MOVE E009-NUMCUEN(1:4);         TO T235-CEN-ASSO
            MOVE E009-NUMCUEN(5:10);        TO T235-ACC-ASSO
      *
            PERFORM QUERY-BGDT235
      *
            MOVE SQLCODE                  TO SQL-VALUES
            EVALUATE TRUE
            WHEN SQL-88-OK
                 MOVE T235-CEN-REG        TO AUX-CENTRO
            WHEN OTHER
                 MOVE E009-NUMCUEN(1:4);    TO AUX-CENTRO
            END-EVALUATE
      *
            MOVE CAA-ENT-ACC              TO V041-ENT
            MOVE AUX-CENTRO               TO V041-CEN-REG
            MOVE T235-ACC                 TO V041-ACC
      *
            PERFORM QUERY-BGDT041
      *
            MOVE SQLCODE TO SQL-VALUES
            IF SQL-88-OK
               MOVE CAA-ENT-ACC           TO T003-COD-ENTITY
               MOVE V041-COD-PROD         TO T003-COD-PRODUCT
               MOVE V041-COD-SPROD        TO T003-CODE
               MOVE '2'                   TO T003-TYP-CODE
               MOVE 'E'                   TO T003-COD-LANGUAGE
               PERFORM QUERY-WPDT003
               MOVE SQLCODE TO SQL-VALUES
               EVALUATE TRUE
               WHEN SQL-88-OK
                    MOVE T003-DES-CODE    TO AUX-DESINT-APERT1
                    MOVE V041-CEN-REG     TO AUX-DESINT-APERT2(1:4); 
                    MOVE V041-ACC         TO AUX-DESINT-APERT2(5:10); 
                    STRING AUX-DESINT-APERT1  DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                            AUX-DESINT-APERT2 DELIMITED BY '  '
                                              INTO VA-DESC-OPE
               WHEN OTHER
                   MOVE 'NOMBRE INVERSION ' TO VA-DESC-OPE(1:17); 
                   MOVE V041-CEN-REG        TO VA-DESC-OPE(18:4); 
                   MOVE V041-ACC            TO VA-DESC-OPE(22:10); 
               END-EVALUATE
            ELSE
               MOVE 'INVERSION No.'        TO VA-DESC-OPE(1:13); 
               MOVE T235-ACC               TO VA-DESC-OPE(14:10); 
            END-IF
            .
      *
      ******************************************************************
      *.PN ARMA-DESC-PAGSERV.                                          *
      * VERIFICA SERVICIO PAGADO [Servicio que pagaste] + [referencia] *
      ******************************************************************
       ARMA-DESC-PAGSERV.
      *
      *    INITIALIZE PR-PARAMETROS.
      *
      *    MOVE WSV-AUX-FECHA      TO PR-FECH-OPER
      *    MOVE WSV-AUX-IMPT       TO PR-MONTO
      *    MOVE WSV-AUX-DESC       TO PR-REFERENCIA
      *    MOVE E009-NUMCUEN(1:4);   TO PR-CUENTA(1:4); 
      *    MOVE E009-NUMCUEN(5:10);  TO PR-CUENTA(5:10); 
      *    MOVE WSV-AUX-NUMOPE     TO VA-REF
      *    MOVE VA-REF             TO PR-NUM-OPER
      *
      *    CALL CA-BS9C6101 USING  PR-PARAMETROS
      *
      *    IF PR-ERROR EQUAL ZEROS
      *       MOVE PR-NOM-EMI      TO VA-DESC-OPE
      *    ELSE
      *       MOVE SPACES          TO VA-DESC-OPE
      *    END-IF
           IF VA-COD-MOV = ('G07' OR 'G16');  AND
              AUX-INTREF71(12:4);  = 'BS03'
              INITIALIZE S209-CONCEPT
              MOVE 'Pago de Servicio ' TO S209-CONCEPT(1:17); 
              IF T606-DESCRIPTION = SPACES
                 CONTINUE
              ELSE
                 MOVE T606-DESCRIPTION(1:30);  TO S209-CONCEPT(18:30); 
      * DECLARE DECLARE @BAZ007F.I
                 IF S209-CONCEPT(1:20);  = 'Pago de Servicio SAT'
                    MOVE 'Pago de Impuestos SAT' TO S209-CONCEPT
                 END-IF
      * DECLARE DECLARE @BAZ007F.F
              END-IF
              MOVE 'Ref.: '           TO VA-DESC-OPE(1:6); 
              MOVE WSV-AUX-DESC(1:30);  TO VA-DESC-OPE(8:30); 
           ELSE
             INITIALIZE AUX-DESPAG-SERV
             MOVE T606-DESCRIPTION(1:30);   TO AUX-DESPAG-SERV1
             MOVE T606-DESCRIPTION(31:40);  TO AUX-DESPAG-SERV2
             STRING AUX-DESPAG-SERV1  DELIMITED BY '  '
                                  ' ' DELIMITED BY SIZE
                     AUX-DESPAG-SERV2 DELIMITED BY '  '
                                      INTO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-PAGOTDC.                                          *
      ******************************************************************
       ARMA-DESC-PAGOTDC.
      *
           INITIALIZE S209-CONCEPT  DCLMBDT010  AUX-DESTDC-PAGO
      *
           MOVE 'Pago a tarjeta de cr�dito' TO S209-CONCEPT
      *
           IF VA-COD-MOV = 'R81'
              IF WSV-AUX-DESC(1:9);  = 'PAGO TDC '
      *          MOVE WSV-AUX-DESC(10:16);     TO VA-DESC-OPE
                 MOVE E009-BDMID             TO T010-BDMID
                 MOVE WSV-AUX-DESC(10:16);     TO T010-CTA-DESTINO
                 PERFORM QUERY-MBDT010
                 MOVE SQLCODE                TO SQL-VALUES
                 IF SQL-88-OK AND T010-ALIAS <> SPACES
                    MOVE T010-ALIAS          TO AUX-DESTDC-PAGO1
                    MOVE '****'              TO AUX-DESTDC-PAGO2(1:4); 
                    MOVE WSV-AUX-DESC(22:4);   TO AUX-DESTDC-PAGO2(5:4); 
                 ELSE
                    MOVE WSV-AUX-DESC(10:6);   TO AUX-BIN
                    PERFORM VALIDA-BINBANCO
      *             MOVE WSV-AUX-DESC(10:16);  TO VA-DESC-OPE(1:16); 
      *             MOVE AUX-DESBANCO        TO VA-DESC-OPE(17:20); 
                 END-IF
                     STRING AUX-DESTDC-PAGO1 DELIMITED BY '  '
                                         ' ' DELIMITED BY SIZE
                            AUX-DESTDC-PAGO2 DELIMITED BY '  '
                                             INTO VA-DESC-OPE
              ELSE
                IF WSV-AUX-DESC(1:3);  = 'AB '
      *            MOVE WSV-AUX-DESC(4:16);   TO VA-DESC-OPE
                   MOVE E009-BDMID          TO T010-BDMID
                   MOVE WSV-AUX-DESC(4:16);   TO T010-CTA-DESTINO
                   PERFORM QUERY-MBDT010
                   MOVE SQLCODE             TO SQL-VALUES
                   IF SQL-88-OK AND T010-ALIAS <> SPACES
                     MOVE T010-ALIAS         TO AUX-DESTDC-PAGO1
                     MOVE '****'             TO AUX-DESTDC-PAGO2(1:4); 
                     MOVE WSV-AUX-DESC(16:4);  TO AUX-DESTDC-PAGO2(5:4); 
                   ELSE
                     MOVE WSV-AUX-DESC(4:6);   TO AUX-BIN
                     PERFORM VALIDA-BINBANCO
      *              MOVE WSV-AUX-DESC(4:16);  TO VA-DESC-OPE(1:16); 
      *              MOVE AUX-DESBANCO       TO VA-DESC-OPE(17:20); 
                   END-IF
                     STRING AUX-DESTDC-PAGO1 DELIMITED BY '  '
                                         ' ' DELIMITED BY SIZE
                            AUX-DESTDC-PAGO2 DELIMITED BY '  '
                                             INTO VA-DESC-OPE
                ELSE
                   MOVE WSV-AUX-DESC        TO VA-DESC-OPE
                END-IF
              END-IF
           ELSE
              INITIALIZE DCLMCDT028
              IF WSV-AUX-DESC(1:10);  = 'PAGO TDC: '
      *          MOVE WSV-AUX-DESC(11:14);    TO VA-DESC-OPE
                 PERFORM OBTEN-PRODUCTO-TARJ
              ELSE
      *          MOVE WSV-AUX-DESC(23:8);     TO VA-DESC-OPE
      *          MOVE WSV-AUX-DESC(21:10);    TO VA-DESC-OPE
                 IF WSV-AUX-DESC(23:8);  IS NUMERIC
                    MOVE 'Banco Azteca '    TO VA-DESC-OPE(1:13); 
                    MOVE CAA-ENT-ACC        TO T028-ENT-CON
                    MOVE WSV-AUX-DESC(23:8);  TO T028-NUM-CON
      *             MOVE CAA-ENT-ACC        TO T028-ENT-ACC
      *             MOVE E009-NUMCUEN(1:4);   TO T028-BRN-ACC
      *             MOVE E009-NUMCUEN(5:2);   TO T028-TYP-ACC
      *             MOVE E009-NUMCUEN(7:8);   TO T028-ACC
                    PERFORM QUERY-MCDT028
                    MOVE SQLCODE TO SQL-VALUES
                    IF SQL-88-OK
                       MOVE T028-ENT-CON    TO T114-ENT-CON
                       MOVE T028-BRN-CON    TO T114-BRN-CON
                       MOVE T028-TYP-CON    TO T114-TYP-CON
                       MOVE T028-NUM-CON    TO T114-NUM-CON
                       MOVE 'T'             TO T114-KEY-PRTC
                       MOVE '01'            TO T114-PRTCORD
                       PERFORM QUERY-MCDT114-BIN
                       MOVE SQLCODE         TO SQL-VALUES
                       IF SQL-88-OK
                          MOVE '****'            TO VA-DESC-OPE(14:4); 
                          MOVE T114-NUM-CRD(7:4);  TO VA-DESC-OPE(18:4); 
                       ELSE
                          MOVE WSV-AUX-DESC(21:10);  TO VA-DESC-OPE(14:10); 
                       END-IF
                    ELSE
                       MOVE 'Banco Azteca '     TO VA-DESC-OPE(1:13); 
                       MOVE WSV-AUX-DESC(21:10);  TO VA-DESC-OPE(14:10); 
                    END-IF
                 ELSE
      *             MOVE 'Banco Azteca '    TO VA-DESC-OPE(1:13); 
      *             MOVE WSV-AUX-DESC(1:30);  TO VA-DESC-OPE(14:40); 
                    MOVE WSV-AUX-DESC(1:30);  TO VA-DESC-OPE
                 END-IF
              END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODS34.                                           *
      ******************************************************************
      *ARMA-DESC-CODS34.
      *
      *
      ******************************************************************
      *.PN ARMA-DESC-RENINVPZO.                                        *
      * 923 - [Nombre del producto] + [Numero de inversion] + [Plazo]  *
      * 664 - "Renovaci�n a " + [Periodo de renovaci�n]                *
      * 178 - "Renovaci�n a " + [Periodo de renovaci�n]                *
      ******************************************************************
       ARMA-DESC-RENINVPZO.
      *
           INITIALIZE DCLBGGT235
           INITIALIZE BGVC041
           INITIALIZE WPWC0010   AUX-DESINT-RENEJE
                                 AUX-DESINT-RENPZO
      *
           IF VA-COD-MOV = '923'
            MOVE WSV-AUX-DESC(21:10);       TO T235-ACC
            MOVE CAA-ENT-ACC              TO T235-ENT-ASSO
            MOVE E009-NUMCUEN(1:4);         TO T235-CEN-ASSO
            MOVE E009-NUMCUEN(5:10);        TO T235-ACC-ASSO
      *
            PERFORM QUERY-BGDT235
      *
            MOVE SQLCODE                  TO SQL-VALUES
            EVALUATE TRUE
            WHEN SQL-88-OK
                 MOVE T235-CEN-REG        TO AUX-CENTRO
            WHEN OTHER
                 MOVE E009-NUMCUEN(1:4);    TO AUX-CENTRO
            END-EVALUATE
      *
            MOVE CAA-ENT-ACC              TO V041-ENT
            MOVE AUX-CENTRO               TO V041-CEN-REG
            MOVE T235-ACC                 TO V041-ACC
      *
            PERFORM QUERY-BGDT041
      *
            MOVE SQLCODE TO SQL-VALUES
            IF SQL-88-OK
               MOVE CAA-ENT-ACC           TO T003-COD-ENTITY
               MOVE V041-COD-PROD         TO T003-COD-PRODUCT
               MOVE V041-COD-SPROD        TO T003-CODE
               MOVE '2'                   TO T003-TYP-CODE
               MOVE 'E'                   TO T003-COD-LANGUAGE
               PERFORM QUERY-WPDT003
               MOVE SQLCODE TO SQL-VALUES
               EVALUATE TRUE
               WHEN SQL-88-OK
                    MOVE T003-SDE-CODE    TO AUX-DESINT-RENEJE1
                    MOVE V041-CEN-REG     TO AUX-DESINT-RENEJE2(1:4); 
                    MOVE V041-ACC         TO AUX-DESINT-RENEJE2(5:10); 
                    PERFORM BUSCA-PLAZO
                    MOVE 'd�as'           TO AUX-DESINT-RENEJE4
                    STRING AUX-DESINT-RENEJE1  DELIMITED BY '  '
                                           ' ' DELIMITED BY SIZE
                            AUX-DESINT-RENEJE2 DELIMITED BY '  '
                                           ' ' DELIMITED BY SIZE
                            AUX-DESINT-RENEJE3 DELIMITED BY SIZE
      *                                    ' ' DELIMITED BY SIZE
                            AUX-DESINT-RENEJE4 DELIMITED BY '  '
                                               INTO VA-DESC-OPE
               WHEN OTHER
                   MOVE 'NOMBRE INVERSION ' TO VA-DESC-OPE(1:17); 
                   MOVE V041-CEN-REG        TO VA-DESC-OPE(18:4); 
                   MOVE V041-ACC            TO VA-DESC-OPE(22:10); 
               END-EVALUATE
            ELSE
               MOVE 'INVERSION No.'        TO VA-DESC-OPE(1:13); 
               MOVE T235-ACC               TO VA-DESC-OPE(14:10); 
            END-IF
           END-IF
      *
           IF VA-COD-MOV = ('664' OR '178'); 
              MOVE 'Renovaci�n a'        TO AUX-DESINT-RENPZO1
              MOVE AUX-INTREF71(5:3);      TO AUX-DIAS
              MOVE 1 TO AUX-X
              MOVE 1 TO AUX-Y
              PERFORM UNTIL SW-FINEDIT-Y
               IF AUX-X < 4
                 IF AUX-DIAS(AUX-X:1);  NOT EQUAL TO SPACE
                    MOVE AUX-DIAS(AUX-X:1); 
                         TO AUX-DESINT-RENPZO2(AUX-Y:1); 
                    ADD 1 TO AUX-X
                    ADD 1 TO AUX-Y
                 ELSE
                    ADD 1 TO AUX-X
                 END-IF
               ELSE
                 SET SW-FINEDIT-Y      TO TRUE
               END-IF
              END-PERFORM
              SET SW-FINEDIT-N TO TRUE
              MOVE 'd�as'                TO AUX-DESINT-RENPZO3
                    STRING AUX-DESINT-RENPZO1 DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                           AUX-DESINT-RENPZO2 DELIMITED BY '  '
                                          ' ' DELIMITED BY SIZE
                           AUX-DESINT-RENPZO3 DELIMITED BY '  '
                                             INTO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-TRANSFER.                                         *
      * lin1:  Transferencia Bancaria                                  *
      * lin2:  banco emisor(20);  + concepto(30);                          *
      ******************************************************************
       ARMA-DESC-TRANSFER.
      *
           INITIALIZE AUX-DES-TRANS
                      FENC1000
           INITIALIZE S209-CONCEPT
           MOVE 'Transferencia Bancaria' TO S209-CONCEPT
      * DECLARE DECLARE @BAZ009.I
      *    MOVE CAA-ENT-ACC             TO N1000-ENT
      *    MOVE AUX-CTA-CEN             TO N1000-CEN-REG
      *    MOVE AUX-CTA-NUM             TO N1000-ACC
      *    MOVE VA-COD-MOV              TO N1000-CODE
      *    MOVE AUX-AMT-COMP3           TO N1000-AMOUNT
      *    MOVE AUX-INTREF71            TO N1000-INTREF
      *    MOVE WSV-AUX-FECHA           TO N1000-DAT-OPERATION
      *    MOVE VA-DAT-VALUE            TO N1000-DAT-VALUEI
      *
      *    IF VA-COD-MOV = ('215' OR '213'); 
      *       MOVE WSV-AUX-NUMOPE       TO N1000-INTREF
      *       MOVE AUX-SEQ-T04          TO N1000-INTREF
      *    END-IF
      *
      *    EXEC CICS
      *       LINK PROGRAM ('FE7C1000'); 
      *       COMMAREA(VA-FENC1000); 
      *       NOHANDLE
      *    END-EXEC
      *
      *    IF EIBRESP EQUAL DFHRESP(NORMAL); 
      *       IF N1000-COD-RETURN = '00'
      *          MOVE N1000-BCO-ORIGIN      TO AUX-DES-TRANS1
      *          MOVE N1000-CONCEPTOP       TO AUX-DES-TRANS2
      *       ELSE
      *         IF ((N1000-BCO-ORIGIN EQUAL SPACES OR LOW-VALUES);  AND
      *             (N1000-CONCEPTOP EQUAL SPACES OR LOW-VALUES); ); 
      *             PERFORM OBTEN-DETALLE-BT600
      *         ELSE
      *            MOVE N1000-BCO-ORIGIN      TO AUX-DES-TRANS1
      *            MOVE N1000-CONCEPTOP       TO AUX-DES-TRANS2
      *         END-IF
      *       END-IF
      *    ELSE
              MOVE SPACES TO VA-DESC-OPE
              PERFORM OBTEN-DETALLE-BT600
      *    END-IF
      * DECLARE DECLARE @BAZ009.F
              STRING AUX-DES-TRANS1  DELIMITED BY '  '
                                 ' ' DELIMITED BY SIZE
                     AUX-DES-TRANS2  DELIMITED BY '  '
                                     INTO VA-DESC-OPE
            .
      *BAZ036-I
      ******************************************************************
      *.PN OBTEN-DET600-DAPP.                                          *
      ******************************************************************
       OBTEN-DET600-DAPP.
      *
           INITIALIZE DCLBGGT600
           MOVE CAA-ENT-ACC            TO T600-ENT
           MOVE AUX-CTA-CEN            TO T600-CEN-REG
           MOVE AUX-CTA-NUM            TO T600-ACC
           MOVE AUX-SEQ-T04            TO T600-NUM-OPERATION
           MOVE VA-COD-MOV             TO T600-CODE
      *    MOVE WSV-AUX-FECHA          TO T600-DAT-OPERATION

           EXEC SQL
              SELECT  T600_DESCRIPTION
                INTO :T600-DESCRIPTION
                FROM BGDT600 with (nolock); 
               WHERE T600_ACC           = :T600-ACC
                 AND T600_CEN_REG       = :T600-CEN-REG
                 AND T600_NUM_OPERATION = :T600-NUM-OPERATION
                 AND T600_CODE          = :T600-CODE
                 AND T600_ENT           = :T600-ENT
      *          AND T600_DAT_ACCT      = :T600-DAT-OPERATION
           END-EXEC
      *
           MOVE SQLCODE                   TO SQL-VALUES
           IF SQL-88-OK
      *       MOVE T600-DESCRIPTION(01:30);  TO AUX-DES-TRANS1
      *       MOVE WSV-AUX-DESC(1:30);       TO AUX-DES-TRANS2
              MOVE T600-DESCRIPTION        TO VA-DESC-OPE
           ELSE
              MOVE SPACES TO AUX-DES-TRANS
           END-IF
           .
      *BAZ036-F
      ******************************************************************
      *.PN OBTEN-DETALLE-BT600.                                        *
      ******************************************************************
       OBTEN-DETALLE-BT600.
      *
           INITIALIZE DCLBGGT600
           MOVE CAA-ENT-ACC            TO T600-ENT
           MOVE AUX-CTA-CEN            TO T600-CEN-REG
           MOVE AUX-CTA-NUM            TO T600-ACC
           MOVE AUX-SEQ-T04            TO T600-NUM-OPERATION
           MOVE VA-COD-MOV             TO T600-CODE
      *    MOVE WSV-AUX-FECHA          TO T600-DAT-OPERATION

           EXEC SQL
              SELECT  T600_DESCRIPTION
                INTO :T600-DESCRIPTION
                FROM BGDT600 with (nolock); 
               WHERE T600_ACC           = :T600-ACC
                 AND T600_CEN_REG       = :T600-CEN-REG
                 AND T600_NUM_OPERATION = :T600-NUM-OPERATION
                 AND T600_CODE          = :T600-CODE
                 AND T600_ENT           = :T600-ENT
      *          AND T600_DAT_ACCT      = :T600-DAT-OPERATION
           END-EXEC
      *
           MOVE SQLCODE                   TO SQL-VALUES
           IF SQL-88-OK
              MOVE T600-DESCRIPTION(43:20);  TO AUX-DES-TRANS1
              MOVE WSV-AUX-DESC(1:30);       TO AUX-DES-TRANS2
           ELSE
              MOVE SPACES TO AUX-DES-TRANS
      * DECLARE DECLARE @BAZ009.I
              PERFORM OBTEN-DETALLE-BT607
      * DECLARE DECLARE @BAZ009.F
           END-IF
           .
      *
      * DECLARE DECLARE @BAZ009.I********************************************************
      *.PN OBTEN-DETALLE-BT607.                                        *
      ******************************************************************
       OBTEN-DETALLE-BT607.
      *
           INITIALIZE DCLBGGT607
           MOVE CAA-ENT-ACC            TO T607-ENT
           MOVE AUX-CTA-CEN            TO T607-CEN-REG
           MOVE AUX-CTA-NUM            TO T607-ACC
           MOVE AUX-SEQ-T04            TO T607-NUM-OPERATION
           MOVE VA-COD-MOV             TO T607-CODE
      *    MOVE WSV-AUX-FECHA          TO T607-DAT-OPERATION

           EXEC SQL
              SELECT  T607_DESCRIPTION;
                      T607_DAT_ACCT
      * DECLARE DECLARE @BAZ074-I      
                      ;T607_FLG_FREE1
      * DECLARE DECLARE @BAZ074-F                            
                INTO :T607-DESCRIPTION;
                     :T607-DAT-ACCT
      * DECLARE DECLARE @BAZ074-I      
                     ;:T607-FLG-FREE1
      * DECLARE DECLARE @BAZ074-F               
                FROM BGDT607 with (nolock); 
               WHERE T607_ACC           = :T607-ACC
                 AND T607_CEN_REG       = :T607-CEN-REG
                 AND T607_NUM_OPERATION = :T607-NUM-OPERATION
                 AND T607_ENT           = :T607-ENT
                 AND T607_CODE          = :T607-CODE
      *          AND T607_DAT_ACCT      = :T607-DAT-OPERATION
           END-EXEC
      *
           MOVE SQLCODE                   TO SQL-VALUES
           IF SQL-88-OK
              MOVE T607-DESCRIPTION(09:20);  TO AUX-DES-TRANS1
              MOVE T607-DESCRIPTION(191:30);  TO AUX-DES-TRANS2
           ELSE
              MOVE SPACES TO AUX-DES-TRANS
           END-IF
           .
      * DECLARE DECLARE @BAZ009.F
      ******************************************************************
      *.PN ARMA-DESC-TRANSFE2.                                         *
      * lin1:  Transferencia Bancaria                                  *
      * lin2:  banco (20);  + concepto(23); + ***cuenta(07);                 *
      * lin3:  beneficiario (50);                                        *
      ******************************************************************
       ARMA-DESC-TRANSFE2.
      *
           INITIALIZE FEVC0040
                      AUX-FEDT004
      *
           MOVE CAA-ENT-ACC          TO V0040-ENT-ORIGIN OF FEVC0040
           MOVE AUX-CTA-CEN          TO V0040-BRN-ORIGIN OF FEVC0040
           MOVE AUX-CTA-NUM          TO V0040-ACC-ORIGIN OF FEVC0040
           MOVE AUX-INTREF71         TO AUX-FEDT004
           MOVE AUX-FESEQ            TO V0040-NUM-SEQOPE OF FEVC0040
      *
           EXEC SQL
              SELECT TOP 1
                     T004_DES_ASSIGNER ;
                     T004_DES_RECVENT  ;
                     T004_ACC_NUMDEST  ;
                     T004_DES_BEN      ;
                     T004_ITEM1        ;
                     T004_ADR          ;
                     T004_INFTOENT3
              INTO  :FEVC0040.V0040-DES-ASSIGNER ;
                    :FEVC0040.V0040-DES-RECVENT ;
                    :FEVC0040.V0040-ACC-NUMDEST ;
                    :FEVC0040.V0040-DES-BEN     ;
                    :FEVC0040.V0040-ITEM1       ;
                    :FEVC0040.V0040-ADR         ;
                    :FEVC0040.V0040-INFTOENT3
                FROM FEDT004 with(nolock); 
                WHERE T004_ENT_ORIGIN = :FEVC0040.V0040-ENT-ORIGIN
                  AND T004_BRN_ORIGIN = :FEVC0040.V0040-BRN-ORIGIN
                  AND T004_ACC_ORIGIN = :FEVC0040.V0040-ACC-ORIGIN
                  AND T004_NUM_SEQOPE = :FEVC0040.V0040-NUM-SEQOPE
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
           WHEN SQL-88-OK
                INITIALIZE S209-CONCEPT
                           AUX-DES-TRANF
                MOVE 'Transferencia Bancaria' TO S209-CONCEPT
                MOVE V0040-DES-RECVENT OF FEVC0040 TO AUX-DES-TRANF1
                MOVE V0040-ITEM1       OF FEVC0040 TO AUX-DES-TRANF2
                MOVE '***'             TO AUX-DES-TRANF3(1:3); 
      * DECLARE DECLARE @BAZ005C.I
      *         MOVE V0040-ACC-NUMDEST OF FEVC0040(7:4);  TO
      *                                              AUX-DES-TRANF3(4:4); 
                MOVE V0040-INFTOENT3 OF FEVC0040(19:4);  TO
                                                     AUX-DES-TRANF3(4:4); 
      * DECLARE DECLARE @BAZ005C.F
                STRING AUX-DES-TRANF1  DELIMITED BY '  '
                                   ' ' DELIMITED BY SIZE
                        AUX-DES-TRANF2 DELIMITED BY '  '
                                   ' ' DELIMITED BY SIZE
                        AUX-DES-TRANF3 DELIMITED BY '  '
                                       INTO VA-DESC-OPE
      * DECLARE DECLARE @BAZ008.I -- complemento beneficiario
                MOVE V0040-DES-BEN   OF FEVC0040       TO VA-BENEFIC
      * DECLARE DECLARE @BAZ008.F
           WHEN OTHER
                MOVE SPACES TO VA-DESC-OPE
                               VA-BENEFIC
           END-EVALUATE
           .
      *

      * DECLARE DECLARE @BAZ009.F
      ******************************************************************
      *.PN CONSULTA-REFERENCIA-INTERB                                  *
      ******************************************************************
       CONSULTA-REFERENCIA-INTERB.
      ****
           INITIALIZE FEVC0040
                      AUX-FEDT004
      *
           MOVE CAA-ENT-ACC          TO V0040-ENT-ORIGIN OF FEVC0040
           MOVE AUX-CTA-CEN          TO V0040-BRN-ORIGIN OF FEVC0040
           MOVE AUX-CTA-NUM          TO V0040-ACC-ORIGIN OF FEVC0040
           MOVE AUX-INTREF71         TO AUX-FEDT004
           MOVE AUX-FESEQ            TO V0040-NUM-SEQOPE OF FEVC0040
      *
           EXEC SQL
              SELECT TOP 1 T004_ADR
              INTO   :FEVC0040.V0040-ADR
                FROM FEDT004 with(nolock); 
                WHERE T004_ENT_ORIGIN = :FEVC0040.V0040-ENT-ORIGIN
                  AND T004_BRN_ORIGIN = :FEVC0040.V0040-BRN-ORIGIN
                  AND T004_ACC_ORIGIN = :FEVC0040.V0040-ACC-ORIGIN
                  AND T004_NUM_SEQOPE = :FEVC0040.V0040-NUM-SEQOPE
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
           WHEN SQL-88-OK
                MOVE V0040-ADR OF FEVC0040 TO AUX-NUM-REFERENCIA
                MOVE AUX-NUM-REFERENCIA    TO WSV-AUX-DESC
           WHEN OTHER
                CONTINUE
           END-EVALUATE.
      *
      ******************************************************************
      *.PN ARMA-DESCR-CODJ15..                                         *
      ******************************************************************
       ARMA-DESCR-CODJ15.
      *
           INITIALIZE S209-CONCEPT  TCGT010
      *
           MOVE 'Dep�sito en ATM'  TO S209-CONCEPT
           PERFORM QUERY-TCDT010-CAJERO
           MOVE SQLCODE            TO SQL-VALUES
           IF SQL-88-OK
              MOVE 'No.'           TO VA-DESC-OPE(1:3); 
              MOVE T010-DTA-TBLKEY(1:6);  TO VA-DESC-OPE(4:6); 
           ELSE
              MOVE SPACES          TO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-RETEFCTVO.                                        *
      ******************************************************************
       ARMA-DESC-RETEFCTVO.
      *
           IF VA-COD-MOV = 'J14'
              INITIALIZE S209-CONCEPT
              MOVE 'Retiro en cajero sin tarjeta' TO S209-CONCEPT
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-MTCNS56.                                 MB20-MB90*
      ******************************************************************
       ARMA-DESC-MTCNS56.
      *
           INITIALIZE S209-CONCEPT
      *    IF WSV-AUX-DESC(1:4);  = 'MTCN'
      *       MOVE 'Env�o Dinero Express MTCN' TO S209-CONCEPT(1:25); 
      *       MOVE WSV-AUX-DESC(5:11);           TO S209-CONCEPT(27:11); 
      *       MOVE 'Para'                      TO VA-DESC-OPE(1:4); 
      *       MOVE T606-DESCRIPTION(1:40);       TO VA-DESC-OPE(6:40); 
           IF (WSV-AUX-DESC(1:4);  = 'MTCN' OR
      * DECLARE DECLARE @BAZ008C.I
               WSV-AUX-DESC(1:6);  = 'ENVIO ' OR
      * DECLARE DECLARE @BAZ008C.F
               WSV-AUX-DESC(1:9);  = 'ENVIO DEX' OR
               WSV-AUX-DESC(1:3);  = 'DEX'); 
              INITIALIZE S209-CONCEPT
              MOVE 'Env�o de Dinero '     TO S209-CONCEPT(1:16); 
              MOVE T606-DESCRIPTION(1:40);  TO S209-CONCEPT(17:34); 
              MOVE 'MTCN '                TO VA-DESC-OPE(1:5); 
              MOVE WSV-AUX-DESC(1:30);      TO VA-DESC-OPE(6:45); 
      * DECLARE DECLARE @BAZ008C.I -- Complementar MTCN p/Homologar detalle como Trx.MB48
              IF WSV-AUX-DESC(1:6);  = 'ENVIO '
                 MOVE WSV-AUX-DESC(7:15);   TO VA-BENEFIC
              END-IF
              IF WSV-AUX-DESC(1:13);  = 'ENVIO DEX-INT'
                 MOVE WSV-AUX-DESC(14:15);  TO VA-BENEFIC
              END-IF
      * DECLARE DECLARE @BAZ008C.F
           ELSE
              IF WSV-AUX-DESC(1:3);  = 'IVA'
                 INITIALIZE S209-CONCEPT
                 MOVE 'IVA de comisi�n'  TO S209-CONCEPT(1:15); 
                 MOVE 'Tasa'             TO VA-DESC-OPE(1:4); 
                 MOVE '16%'              TO VA-DESC-OPE(6:3); 
              END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-COMMTCNS58.                              MB20-MB90*
      ******************************************************************
       ARMA-DESC-COMMTCNS58.
      *
           INITIALIZE S209-CONCEPT
           IF WSV-AUX-DESC(1:8);  = ('COMISION' OR 'Comisi�n' OR
                                   'Comision'); 
              MOVE 'Comisi�n env�o Dinero Express' TO S209-CONCEPT
           END-IF
           .
      *
      * DECLARE DECLARE @BAZ061-I
      ******************************************************************
      *.PN ARMA-DESC-MTCNT96.                                 MBB9-MB90*
      ******************************************************************
       ARMA-DESC-MTCNT96.
      *
           INITIALIZE S209-CONCEPT
           IF (WSV-AUX-DESC(1:4);  = 'MTCN' OR
               WSV-AUX-DESC(1:6);  = 'ENVIO ' OR
               WSV-AUX-DESC(1:9);  = 'ENVIO DEX' OR
               WSV-AUX-DESC(1:3);  = 'DEX'); 
              INITIALIZE S209-CONCEPT
              MOVE 'Env�o de Dinero '     TO S209-CONCEPT(1:16); 
              MOVE T606-DESCRIPTION(1:40);  TO S209-CONCEPT(17:34); 
              MOVE 'MTCN '                TO VA-DESC-OPE(1:5); 
              MOVE WSV-AUX-DESC(1:30);      TO VA-DESC-OPE(6:45); 
              IF WSV-AUX-DESC(1:6);  = 'ENVIO '
                 MOVE WSV-AUX-DESC(7:15);   TO VA-BENEFIC
              END-IF
              IF WSV-AUX-DESC(1:13);  = 'ENVIO DEX-INT'
                 MOVE WSV-AUX-DESC(14:15);  TO VA-BENEFIC
              END-IF
           ELSE
              IF WSV-AUX-DESC(1:3);  = 'IVA'
                 INITIALIZE S209-CONCEPT
                 MOVE 'IVA de comisi�n'  TO S209-CONCEPT(1:15); 
                 MOVE 'Tasa'             TO VA-DESC-OPE(1:4); 
                 MOVE '16%'              TO VA-DESC-OPE(6:3); 
              END-IF
           END-IF
      *
           IF WSV-AUX-DESC(1:8);  = ('COMISION' OR 'Comisi�n' OR
                                   'Comision'); 
              INITIALIZE S209-CONCEPT
              MOVE 'Comisi�n env�o Western Union' TO S209-CONCEPT
           END-IF
           .
      * DECLARE DECLARE @BAZ061-F
      ******************************************************************
      *.PN ARMA-DESC-TA.                                               *
      ******************************************************************
       ARMA-DESC-TA.
      *
           IF WSV-AUX-DESC(1:11);  = 'TIEMPO AIRE'
              INITIALIZE S209-CONCEPT  AUX-DEST05-TRASCEL
              MOVE 'Compra tiempo aire'   TO S209-CONCEPT
              MOVE 'Para celular'         TO AUX-DEST05-TRASCEL1
      * DECLARE DECLARE @BAZ011.I
      *       MOVE T606-DESCRIPTION(1:10);  TO AUX-DEST05-TRASCEL2
              MOVE T606-DESCRIPTION(1:15);  TO AUX-DEST05-TRASCEL2
      * DECLARE DECLARE @BAZ011.F
              MOVE WSV-AUX-DESC(14:12);     TO AUX-DEST05-TRASCEL3
              STRING AUX-DEST05-TRASCEL1  DELIMITED BY '  '
                           ' ' DELIMITED BY SIZE
                 AUX-DEST05-TRASCEL2 DELIMITED BY '  '
                           ' ' DELIMITED BY SIZE
                 AUX-DEST05-TRASCEL3 DELIMITED BY '  '
                               INTO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODT60.                                           *
      ******************************************************************
       ARMA-DESC-CODT60.
      *
           INITIALIZE S209-CONCEPT
      *               BGVC111
      *    INITIALIZE DCLPEDV0080
           INITIALIZE AUX1-CTABEN
                      AUX2-NUMCTA
                      AUX-NOMBRECTE AUX-NUMCUS8
      *
      *    MOVE 'Pago Facil'      TO S209-CONCEPT
      **   MOVE AUX-CTA-INPUT     TO V111-ACC-DEBIT
      **   MOVE WSV-AUX-NUMOPE    TO V111-MOV-DEBIT
      *    MOVE AUX-CTA-INPUT     TO V111-ACC-CREDIT
      *    MOVE WSV-AUX-NUMOPE    TO V111-MOV-CREDIT
      *    MOVE AUX-AMT-COMP3     TO V111-AMT
      *
      **   PERFORM QUERY-BGDT111-DEB
      *    PERFORM QUERY-BGDT111-CRE
      *
      *    MOVE SQLCODE           TO SQL-VALUES
      *    IF SQL-88-OK
      **      MOVE V111-ACC-CREDIT(1:4);  TO COD-ENTITY OF DCLPEDV0080
      **      MOVE V111-ACC-CREDIT(5:4);  TO BRN-OPEN OF DCLPEDV0080
      **      MOVE V111-ACC-CREDIT(11:2);  TO COD-PRODSERV OF DCLPEDV0080
      **      MOVE V111-ACC-CREDIT(13:10);  TO NUM-ACCOUNT OF DCLPEDV0080
      *       MOVE V111-ACC-DEBIT(1:4);  TO COD-ENTITY   OF DCLPEDV0080
      *       MOVE V111-ACC-DEBIT(5:4);  TO BRN-OPEN     OF DCLPEDV0080
      *       MOVE V111-ACC-DEBIT(11:2);  TO COD-PRODSERV OF DCLPEDV0080
      *       MOVE V111-ACC-DEBIT(13:10);  TO NUM-ACCOUNT  OF DCLPEDV0080
      *       MOVE 'T'            TO KEY-PARTIC   OF DCLPEDV0080
      *       MOVE '01'           TO PARTSEQ      OF DCLPEDV0080
      *
      *       PERFORM QUERY-PEDT008
      *
      *       MOVE SQLCODE           TO SQL-VALUES
      *       IF SQL-88-OK
      *          MOVE AUX-NUMCUS8    TO NUM-CUS    OF DCLPEDV0011
      *          MOVE CAA-ENT-ACC    TO COD-ENTITY OF DCLPEDV0011
      *          PERFORM QUERY-PEDT001
      *          MOVE SQLCODE        TO SQL-VALUES
      *          EVALUATE TRUE
      *          WHEN SQL-88-OK
      *               STRING AUX-NAME  DELIMITED BY '   '
      *                            ' ' DELIMITED BY SIZE
      *                    AUX-SURNAME DELIMITED BY '   '
      *                                INTO AUX-NOMBRECTE
      *          WHEN OTHER
      *               MOVE SPACES    TO AUX-NOMBRECTE
      *          END-EVALUATE
      *       ELSE
      *          MOVE SPACES         TO AUX-NOMBRECTE
      *       END-IF
      *    ELSE
      *       MOVE SPACES            TO AUX-NOMBRECTE
      *    END-IF
      *
      *    MOVE AUX-NOMBRECTE        TO VA-DESC-OPE
      * DECLARE DECLARE @BAZ005.I
           IF WSV-AUX-FECHA > FECHCOD-INVERTIDO
      * T60(-); 
      * DECLARE DECLARE @BAZ007D.I
      *      MOVE 'PagaFacil'         TO S209-CONCEPT
      * --- MB03: solicitud efectivo / paga facil
            IF WSV-AUX-DESC(1:2);  = 'CO'
      *       MOVE 'Recibir efectivo' TO S209-CONCEPT
              MOVE 'Recibiste Efectivo' TO S209-CONCEPT
            ELSE
      * DECLARE DECLARE @BAZ007L
      *       MOVE 'PagaFacil'        TO S209-CONCEPT
      *       MOVE 'PagaM�vil'        TO S209-CONCEPT
              MOVE 'Pago con la App'  TO S209-CONCEPT
            END-IF
      * DECLARE DECLARE @BAZ007D.F
             IF T606-DESCRIPTION(1:30);  = SPACES
               INITIALIZE BGNC477
               MOVE WSV-AUX-NUMOPE    TO BGNC477-NUM-OP
               MOVE AUX-CTA-INPUT     TO BGNC477-ACC
      *
               EXEC CICS
                  LINK PROGRAM (CA-BG7C4770); 
                  COMMAREA(CA-BGNC477); 
                  NOHANDLE
               END-EXEC
      *
               IF EIBRESP EQUAL DFHRESP(NORMAL); 
                  IF BGNC477-COD-ERR EQUAL TO SPACES
                     MOVE BGNC477-ACC-CRED   TO AUX1-CTABEN
                     MOVE AUX1-CTABEN(5:4);    TO AUX2-NUMCEN
                     MOVE AUX1-CTABEN(11:10);  TO AUX2-NUMACC
      *              -- Obtiene Nombre a quien env�o
                     PERFORM OBTEN-DATOS-CLIENTE
                     MOVE 'para '       TO VA-DESC-OPE(1:5); 
                     MOVE AUX-NOMBRECTE TO VA-DESC-OPE(6:40); 
      * DECLARE DECLARE @BAZ007D.I
                     IF WSV-AUX-DESC(1:2);  = 'CO'
                        MOVE 'De '         TO VA-DESC-OPE(1:3); 
                        MOVE AUX-NOMBRECTE TO VA-DESC-OPE(4:40); 
                     END-IF
      * DECLARE DECLARE @BAZ007D.F
                  ELSE
                     MOVE SPACES        TO VA-DESC-OPE
                  END-IF
               ELSE
                  MOVE SPACES           TO VA-DESC-OPE
               END-IF
             ELSE
              MOVE 'para '                TO VA-DESC-OPE(1:5); 
              MOVE T606-DESCRIPTION(1:30);  TO VA-DESC-OPE(6:40); 
      * DECLARE DECLARE @BAZ007D.I
              IF WSV-AUX-DESC(1:2);  = 'CO'
                 MOVE 'De '                  TO VA-DESC-OPE(1:3); 
                 MOVE T606-DESCRIPTION(1:30);  TO VA-DESC-OPE(4:40); 
              END-IF
      * DECLARE DECLARE @BAZ007D.F
             END-IF
           ELSE
      * T60(+); 
      * DECLARE DECLARE @BAZ007L
      *     MOVE 'CobraFacil'      TO S209-CONCEPT
      *      MOVE 'CobraM�vil'      TO S209-CONCEPT
            MOVE 'Cobro con la App'      TO S209-CONCEPT
            INITIALIZE BGNC477
            MOVE WSV-AUX-NUMOPE    TO BGNC477-NUM-OP
            MOVE AUX-CTA-INPUT     TO BGNC477-ACC
      *
            EXEC CICS
               LINK PROGRAM (CA-BG7C4770); 
               COMMAREA(CA-BGNC477); 
               NOHANDLE
            END-EXEC
      *
            IF EIBRESP EQUAL DFHRESP(NORMAL); 
               IF BGNC477-COD-ERR EQUAL TO SPACES
      *           MOVE BGNC477-ACC-CRED   TO AUX1-CTABEN
                  MOVE BGNC477-ACC-DEB    TO AUX1-CTABEN
                  MOVE AUX1-CTABEN(5:4);    TO AUX2-NUMCEN
                  MOVE AUX1-CTABEN(11:10);  TO AUX2-NUMACC
      *           -- Obtiene Nombre de quien recibo
                  PERFORM OBTEN-DATOS-CLIENTE
      *           MOVE AUX-NOMBRECTE TO VA-DESC-OPE
                  MOVE 'De '         TO VA-DESC-OPE(1:3); 
                  MOVE AUX-NOMBRECTE TO VA-DESC-OPE(4:40); 
               ELSE
                  MOVE SPACES        TO VA-DESC-OPE
               END-IF
            ELSE
               MOVE SPACES           TO VA-DESC-OPE
            END-IF
           END-IF
           .
      * DECLARE DECLARE @BAZ005.F
      ******************************************************************
      *.PN ARMA-DESC-CODT59.                                           *
      ******************************************************************
       ARMA-DESC-CODT59.
      *
           INITIALIZE S209-CONCEPT
      *               BGVC111
      *    INITIALIZE DCLPEDV0080
           INITIALIZE AUX1-CTABEN
                      AUX2-NUMCTA
                      AUX-NOMBRECTE AUX-NUMCUS8
      *
      *    MOVE 'Cobro Facil'     TO S209-CONCEPT
      **   MOVE AUX-CTA-INPUT     TO V111-ACC-CREDIT
      **   MOVE WSV-AUX-NUMOPE    TO V111-MOV-CREDIT
      *    MOVE AUX-CTA-INPUT     TO V111-ACC-DEBIT
      *    MOVE WSV-AUX-NUMOPE    TO V111-MOV-DEBIT
      *    MOVE AUX-AMT-COMP3     TO V111-AMT
      *
      **   PERFORM QUERY-BGDT111-DEB
      *    PERFORM QUERY-BGDT111-CRE
      *
      *    MOVE SQLCODE           TO SQL-VALUES
      *    IF SQL-88-OK
      **      MOVE V111-ACC-DEBIT(1:4);  TO COD-ENTITY   OF DCLPEDV0080
      **      MOVE V111-ACC-DEBIT(5:4);  TO BRN-OPEN     OF DCLPEDV0080
      **      MOVE V111-ACC-DEBIT(11:2);  TO COD-PRODSERV OF DCLPEDV0080
      **      MOVE V111-ACC-DEBIT(13:10);  TO NUM-ACCOUNT  OF DCLPEDV0080
      *       MOVE V111-ACC-CREDIT(1:4);  TO COD-ENTITY OF DCLPEDV0080
      *       MOVE V111-ACC-CREDIT(5:4);  TO BRN-OPEN OF DCLPEDV0080
      *       MOVE V111-ACC-CREDIT(11:2);  TO COD-PRODSERV OF DCLPEDV0080
      *       MOVE V111-ACC-CREDIT(13:10);  TO NUM-ACCOUNT OF DCLPEDV0080
      *       MOVE 'T'            TO KEY-PARTIC   OF DCLPEDV0080
      *       MOVE '01'           TO PARTSEQ      OF DCLPEDV0080
      *
      *       PERFORM QUERY-PEDT008
      *
      *       MOVE SQLCODE        TO SQL-VALUES
      *       IF SQL-88-OK
      *          MOVE AUX-NUMCUS8    TO NUM-CUS    OF DCLPEDV0011
      *          MOVE CAA-ENT-ACC    TO COD-ENTITY OF DCLPEDV0011
      *          PERFORM QUERY-PEDT001
      *          MOVE SQLCODE        TO SQL-VALUES
      *          EVALUATE TRUE
      *          WHEN SQL-88-OK
      *               STRING AUX-NAME  DELIMITED BY '   '
      *                            ' ' DELIMITED BY SIZE
      *                    AUX-SURNAME DELIMITED BY '   '
      *                                INTO AUX-NOMBRECTE
      *          WHEN OTHER
      *               MOVE SPACES TO AUX-NOMBRECTE
      *          END-EVALUATE
      *       ELSE
      *          MOVE SPACES      TO AUX-NOMBRECTE
      *       END-IF
      *    ELSE
      *       MOVE SPACES         TO AUX-NOMBRECTE
      *    END-IF
      *
      *    MOVE AUX-NOMBRECTE     TO VA-DESC-OPE
      * DECLARE DECLARE @BAZ005.I
           IF WSV-AUX-FECHA > FECHCOD-INVERTIDO
      * T59(+); 
      * DECLARE DECLARE @BAZ007D.I
      *        MOVE 'CobraFacil'   TO S209-CONCEPT
      * --- MB03: solicitud efectivo / cobra facil
            IF WSV-AUX-DESC(1:2);  = 'CO'
      *       MOVE 'entregar efectivo' TO S209-CONCEPT
              MOVE 'Entregaste Efectivo' TO S209-CONCEPT
            ELSE
      * DECLARE DECLARE @BAZ007L
      *       MOVE 'CobraFacil'        TO S209-CONCEPT
      *        MOVE 'CobraM�vil'        TO S209-CONCEPT
              MOVE 'Cobro con la App'      TO S209-CONCEPT
            END-IF
      * DECLARE DECLARE @BAZ007D.F
               INITIALIZE BGNC477
               MOVE WSV-AUX-NUMOPE    TO BGNC477-NUM-OP
               MOVE AUX-CTA-INPUT     TO BGNC477-ACC
      *
               EXEC CICS
                  LINK PROGRAM (CA-BG7C4770); 
                  COMMAREA(CA-BGNC477); 
                  NOHANDLE
               END-EXEC
      *
               IF EIBRESP EQUAL DFHRESP(NORMAL); 
                  IF BGNC477-COD-ERR EQUAL TO SPACES
                     MOVE BGNC477-ACC-DEB    TO AUX1-CTABEN
                     MOVE AUX1-CTABEN(5:4);    TO AUX2-NUMCEN
                     MOVE AUX1-CTABEN(11:10);  TO AUX2-NUMACC
      *              -- Obtiene Nombre de quien recibo
                     PERFORM OBTEN-DATOS-CLIENTE
                     MOVE 'De '          TO VA-DESC-OPE(1:3); 
                     MOVE AUX-NOMBRECTE  TO VA-DESC-OPE(4:40); 
      * DECLARE DECLARE @BAZ007D.I
                     IF WSV-AUX-DESC(1:2);  = 'CO'
                        MOVE 'A '          TO VA-DESC-OPE(1:2); 
                        MOVE AUX-NOMBRECTE TO VA-DESC-OPE(3:40); 
                     END-IF
      * DECLARE DECLARE @BAZ007D.F
                  ELSE
                     MOVE SPACES           TO VA-DESC-OPE
                  END-IF
               ELSE
                  MOVE SPACES              TO VA-DESC-OPE
               END-IF
           ELSE
      * T59(-); 
      * DECLARE DECLARE @BAZ007L
      *     MOVE 'PagaFacil'       TO S209-CONCEPT
      *      MOVE 'PagaM�vil'       TO S209-CONCEPT
            MOVE 'Pago con la App'  TO S209-CONCEPT
            IF T606-DESCRIPTION(1:30);  = SPACES
              INITIALIZE BGNC477
              MOVE WSV-AUX-NUMOPE    TO BGNC477-NUM-OP
              MOVE AUX-CTA-INPUT     TO BGNC477-ACC
      *
              EXEC CICS
                 LINK PROGRAM (CA-BG7C4770); 
                 COMMAREA(CA-BGNC477); 
                 NOHANDLE
              END-EXEC
      *
              IF EIBRESP EQUAL DFHRESP(NORMAL); 
                 IF BGNC477-COD-ERR EQUAL TO SPACES
      *             MOVE BGNC477-ACC-DEB    TO AUX1-CTABEN
                    MOVE BGNC477-ACC-CRED   TO AUX1-CTABEN
                    MOVE AUX1-CTABEN(5:4);    TO AUX2-NUMCEN
                    MOVE AUX1-CTABEN(11:10);  TO AUX2-NUMACC
      *             -- Obtiene Nombre a quien env�o
                    PERFORM OBTEN-DATOS-CLIENTE
      *             MOVE AUX-NOMBRECTE  TO VA-DESC-OPE
                    MOVE 'para '        TO VA-DESC-OPE(1:5); 
                    MOVE AUX-NOMBRECTE  TO VA-DESC-OPE(6:40); 
                 ELSE
                    MOVE SPACES           TO VA-DESC-OPE
                 END-IF
              ELSE
                 MOVE SPACES              TO VA-DESC-OPE
              END-IF
            ELSE
      *       MOVE T606-DESCRIPTION(1:30);  TO VA-DESC-OPE
              MOVE 'para '                TO VA-DESC-OPE(1:5); 
              MOVE T606-DESCRIPTION(1:30);  TO VA-DESC-OPE(6:40); 
            END-IF
           END-IF
           .
      * DECLARE DECLARE @BAZ005.F
      ******************************************************************
      *.PN ARMA-DESC-COD787.                                cargo -MXP *
      ******************************************************************
       ARMA-DESC-COD787.
      *
           INITIALIZE AUX-DESTIP-CAMBIO
                      TCGV0811   S209-CONCEPT
                      AUX-VALOR  AUX-UNIDA
      *
           IF T606-DESCRIPTION(32:10);  = 'Pago facil'
      * DECLARE DECLARE @BAZ007L
      *       MOVE 'PagaFacil'          TO S209-CONCEPT
      *        MOVE 'PagaM�vil'          TO S209-CONCEPT
              MOVE 'Pago con la App'  TO S209-CONCEPT
           ELSE
              MOVE 'Compra de d�lares'    TO S209-CONCEPT
           END-IF
      *    MOVE 'Compra de d�lares'    TO S209-CONCEPT
      *    IF AUX-INTREF71(1:7);  IS NOT NUMERIC
           IF AUX-INTREF71(1:5);  IS NOT NUMERIC
             PERFORM TIPO-CAMBIO
      *      MOVE AUX-TIP-CAMCPRA        TO AUX-MONTO-TIP
             MOVE AUX-TIP-CAMVTA         TO AUX-MONTO-TIP
             MOVE 1                      TO AUX-PASO
             PERFORM EDITA-VALOR
           ELSE
             MOVE AUX-INTREF71(2:2);     TO AUX-VALOR(1:2); 
             MOVE '.'                  TO AUX-VALOR(3:1); 
             MOVE AUX-INTREF71(4:2);     TO AUX-VALOR(4:2); 
             MOVE AUX-INTREF71(1:5);     TO AUX-TIPCAM-INTERF
      *      MOVE AUX-INTREF71(1:7);     TO AUX-TIPCAM-INTERF
      *      MOVE AUX-TIPCAM-INTERF-R  TO AUX-TIP-CAMCPRA
             MOVE AUX-TIPCAM-INTERF-R  TO AUX-TIP-CAMVTA
           END-IF
           MOVE 'TC.'                  TO AUX-DESTIP-CAMBIO0
           MOVE AUX-VALOR(1:5);          TO AUX-DESTIP-CAMBIO1
           MOVE ';'                    TO AUX-DESTIP-CAMBIOC
      *    COMPUTE AUX-UNIDADES = AUX-AMT-COMP3 / AUX-TIP-CAMCPRA
           COMPUTE AUX-UNIDADES = AUX-AMT-COMP3 / AUX-TIP-CAMVTA
           MOVE AUX-UNIDADES           TO AUX-MONTO-UNI
           MOVE 2                      TO AUX-PASO
           PERFORM EDITA-VALOR
           MOVE AUX-UNIDA              TO AUX-DESTIP-CAMBIO2
      *    MOVE 'unidades'             TO AUX-DESTIP-CAMBIO3
           MOVE 'USD'                  TO AUX-DESTIP-CAMBIO3
                STRING AUX-DESTIP-CAMBIO0  DELIMITED BY '   '
                                       ' ' DELIMITED BY SIZE
                       AUX-DESTIP-CAMBIO1  DELIMITED BY '   '
      *                                ' ' DELIMITED BY SIZE
                        AUX-DESTIP-CAMBIOC DELIMITED BY '   '
                                       ' ' DELIMITED BY SIZE
                        AUX-DESTIP-CAMBIO2 DELIMITED BY '   '
                                       ' ' DELIMITED BY SIZE
                        AUX-DESTIP-CAMBIO3 DELIMITED BY '   '
                                           INTO VA-DESC-OPE
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-COD786.                                abono +MXP *
      ******************************************************************
       ARMA-DESC-COD786.
      *
           INITIALIZE AUX-DESTIP-CAMBIO
                      TCGV0811  S209-CONCEPT
                      AUX-VALOR  AUX-UNIDA
      *
           IF T606-DESCRIPTION(32:10);  = 'Pago facil'
      * DECLARE DECLARE @BAZ007L
      *       MOVE 'CobraFacil'         TO S209-CONCEPT
      *        MOVE 'CobraM�vil'         TO S209-CONCEPT
              MOVE 'Cobro con la App'      TO S209-CONCEPT
           ELSE
      *       MOVE 'Compra de d�lares'    TO S209-CONCEPT
              MOVE 'Venta de d�lares'     TO S209-CONCEPT
           END-IF
      *    IF AUX-INTREF71(1:7);  IS NOT NUMERIC
           IF AUX-INTREF71(1:5);  IS NOT NUMERIC
             PERFORM TIPO-CAMBIO
      *      MOVE AUX-TIP-CAMVTA       TO AUX-MONTO-TIP
             MOVE AUX-TIP-CAMCPRA      TO AUX-MONTO-TIP
             MOVE 1                    TO AUX-PASO
             PERFORM EDITA-VALOR
           ELSE
             MOVE AUX-INTREF71(2:2);     TO AUX-VALOR(1:2); 
             MOVE '.'                  TO AUX-VALOR(3:1); 
             MOVE AUX-INTREF71(4:2);     TO AUX-VALOR(4:2); 
             MOVE AUX-INTREF71(1:5);     TO AUX-TIPCAM-INTERF
      *      MOVE AUX-INTREF71(1:7);     TO AUX-TIPCAM-INTERF
      *      MOVE AUX-TIPCAM-INTERF-R  TO AUX-TIP-CAMVTA
             MOVE AUX-TIPCAM-INTERF-R  TO AUX-TIP-CAMCPRA
           END-IF
           MOVE 'TC.'                  TO AUX-DESTIP-CAMBIO0
           MOVE AUX-VALOR(1:5);          TO AUX-DESTIP-CAMBIO1
           MOVE ';'                    TO AUX-DESTIP-CAMBIOC
      *    COMPUTE AUX-UNIDADES = AUX-AMT-COMP3 / AUX-TIP-CAMVTA
           COMPUTE AUX-UNIDADES = AUX-AMT-COMP3 / AUX-TIP-CAMCPRA
           MOVE AUX-UNIDADES           TO AUX-MONTO-UNI
           MOVE 2                      TO AUX-PASO
           PERFORM EDITA-VALOR
           MOVE AUX-UNIDA              TO AUX-DESTIP-CAMBIO2
      *    MOVE 'unidades'             TO AUX-DESTIP-CAMBIO3
           MOVE 'USD'                  TO AUX-DESTIP-CAMBIO3
                STRING AUX-DESTIP-CAMBIO0  DELIMITED BY '   '
                                       ' ' DELIMITED BY SIZE
                       AUX-DESTIP-CAMBIO1  DELIMITED BY '   '
      *                                ' ' DELIMITED BY SIZE
                        AUX-DESTIP-CAMBIOC DELIMITED BY '   '
                                       ' ' DELIMITED BY SIZE
                        AUX-DESTIP-CAMBIO2 DELIMITED BY '   '
                                       ' ' DELIMITED BY SIZE
                        AUX-DESTIP-CAMBIO3 DELIMITED BY '   '
                                           INTO VA-DESC-OPE
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODT64.                                cargo -USD *
      ******************************************************************
       ARMA-DESC-CODT64.
      *
           INITIALIZE TCGV0811  S209-CONCEPT
                      AUX-VALOR
      *
           IF T606-DESCRIPTION(32:10);  = 'Pago facil'
      * DECLARE DECLARE @BAZ007L
      *       MOVE 'PagaFacil'          TO S209-CONCEPT
      *       MOVE 'PagaM�vil'          TO S209-CONCEPT
              MOVE 'Pago con la App'  TO S209-CONCEPT
           ELSE
      *       MOVE 'Compra de d�lares'  TO S209-CONCEPT
              MOVE 'Venta de d�lares'   TO S209-CONCEPT
           END-IF
      *    IF AUX-INTREF71(1:7);  IS NOT NUMERIC
           IF AUX-INTREF71(1:5);  IS NOT NUMERIC
             PERFORM TIPO-CAMBIO
             MOVE AUX-TIP-CAMCPRA      TO AUX-MONTO-TIP
             MOVE 1                    TO AUX-PASO
             PERFORM EDITA-VALOR
           ELSE
             MOVE AUX-INTREF71(2:2);     TO AUX-VALOR(1:2); 
             MOVE '.'                  TO AUX-VALOR(3:1); 
             MOVE AUX-INTREF71(4:2);     TO AUX-VALOR(4:2); 
           END-IF
           MOVE 'Tipo De Cambio '      TO VA-DESC-OPE(1:15); 
           MOVE AUX-VALOR              TO VA-DESC-OPE(16:16); 

           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODT63.                                abono +USD *
      ******************************************************************
       ARMA-DESC-CODT63.
      *
           INITIALIZE TCGV0811  S209-CONCEPT
                      AUX-VALOR
      *
           IF T606-DESCRIPTION(32:10);  = 'Pago facil'
      * DECLARE DECLARE @BAZ007L
      *       MOVE 'CobraFacil'         TO S209-CONCEPT
      *        MOVE 'CobraM�vil'         TO S209-CONCEPT
              MOVE 'Cobro con la App'    TO S209-CONCEPT
           ELSE
            MOVE 'Compra de d�lares'    TO S209-CONCEPT
           END-IF
      *    MOVE 'Compra de d�lares'    TO S209-CONCEPT
      *    IF AUX-INTREF71(1:7);  IS NOT NUMERIC
           IF AUX-INTREF71(1:5);  IS NOT NUMERIC
             PERFORM TIPO-CAMBIO
             MOVE AUX-TIP-CAMVTA       TO AUX-MONTO-TIP
             MOVE 1                    TO AUX-PASO
             PERFORM EDITA-VALOR
           ELSE
             MOVE AUX-INTREF71(2:2);     TO AUX-VALOR(1:2); 
             MOVE '.'                  TO AUX-VALOR(3:1); 
             MOVE AUX-INTREF71(4:2);     TO AUX-VALOR(4:2); 
           END-IF
           MOVE 'Tipo De Cambio '      TO VA-DESC-OPE(1:15); 
           MOVE AUX-VALOR              TO VA-DESC-OPE(16:16); 
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODT31.                                           *
      ******************************************************************
       ARMA-DESC-CODT31.
      *
           INITIALIZE S209-CONCEPT  TCGT010
      *MCV002-I
      * DECLARE DECLARE @BAZ005G.I
      *    MOVE 'RETIRO CON CELULAR'  TO S209-CONCEPT
           MOVE 'RETIRO CON CELULAR EN CAJERO' TO S209-CONCEPT
      * DECLARE DECLARE @BAZ005G.I
      * DECLARE DECLARE @BAZ068-INI
           IF WSV-AUX-DESC(1:11);  = 'Envio a ATM'
           OR WSV-AUX-DESC(1:24);    = 'Retiro para otra persona'
              MOVE 'Retiro para otra persona'  TO S209-CONCEPT
           END-IF
      * DECLARE DECLARE @BAZ068-FIN
      *MCV002-F
           IF (WSV-AUX-DESC(1:11);  = 'Envio a ATM' OR
               WSV-AUX-DESC(1:13);  = 'Retiro en ATM'
      * DECLARE DECLARE @BAZ068-INI
             OR WSV-AUX-DESC(1:24); = 'Retiro para otra persona'); 

      * DECLARE DECLARE @BAZ068-FIN
      *
      *        MOVE 'Retiro en ATM'    TO S209-CONCEPT
               PERFORM QUERY-TCDT010-CAJERO
               MOVE SQLCODE            TO SQL-VALUES
               IF SQL-88-OK
      * DECLARE DECLARE @BAZ005G.I
      *           MOVE 'No.'           TO VA-DESC-OPE(1:3); 
      *           MOVE T010-DTA-TBLKEY(1:6);  TO VA-DESC-OPE(4:6); 
                  MOVE 'Banco Azteca '      TO VA-DESC-OPE(1:13); 
                  MOVE 'No.'                TO VA-DESC-OPE(14:3); 
                  MOVE T010-DTA-TBLKEY(1:6);  TO VA-DESC-OPE(17:6); 
      * DECLARE DECLARE @BAZ005G.F
               ELSE
                  MOVE SPACES          TO VA-DESC-OPE
               END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-COD111.                                           *
      ******************************************************************
       ARMA-DESC-COD111.
      *
           INITIALIZE DCLMCDT043  AUX-CAJERO
                      S209-CONCEPT
      *MCV002-I
           MOVE 'RETIRO EN CAJERO'     TO S209-CONCEPT
      *MCV002-F
           IF AUX-INTREF71(2:5);  = SPACES
      *       MOVE 'Retiro en ATM'     TO S209-CONCEPT
              MOVE AUX-CTA-ENT         TO T043-ENT-ACC
              MOVE AUX-CTA-CEN         TO T043-BRN-ACC
              MOVE AUX-CTA-NUM(1:2);     TO T043-TYP-ACC
              MOVE AUX-CTA-NUM(3:8);     TO T043-ACC
              MOVE WSV-AUX-FECHA       TO T043-DAT-ACCT
              MOVE WSV-AUX-FECHA       TO AUX-TIMESTP043(1:10); 
              MOVE '-'                 TO AUX-TIMESTP043(11:1); 
              MOVE WSV-AUX-HORA(1:2);    TO AUX-TIMESTP043(12:2); 
              MOVE '.'                 TO AUX-TIMESTP043(14:1); 
              MOVE WSV-AUX-HORA(3:2);    TO AUX-TIMESTP043(15:2); 
              MOVE AUX-AMT-COMP3       TO AUX-AMT-COMP043
              MOVE AUX-AMT-COMP043     TO T043-AMT-OPERATION
              MOVE WSV-AUX-HORA        TO AUX-TIME043
              MOVE AUX-TIME043         TO T043-TIM-OPERATION
      *
              EXEC SQL
               SELECT T043_TXT_DIG_30
                INTO :AUX-CAJERO
                FROM MCDT043 with(nolock); 
               WHERE T043_ACC      = :T043-ACC
                 AND T043_BRN_ACC  = :T043-BRN-ACC
                 AND T043_TYP_ACC  = :T043-TYP-ACC
                 AND T043_ENT_ACC  = :T043-ENT-ACC
                 AND T043_DAT_ACCT = :T043-DAT-ACCT
                 AND T043_AMT_OPERATION  = :T043-AMT-OPERATION
      *          AND SUBSTRING(T043_STP;1;16); =:AUX-TIMESTP043
      *          AND T043_TIM_OPERATION = :T043-TIM-OPERATION
              END-EXEC
      *
              MOVE SQLCODE                TO SQL-VALUES
              IF SQL-88-OK
                 MOVE AUX-CAJERO(4:6);      TO VA-DESC-OPE
              ELSE
                 MOVE SPACES              TO VA-DESC-OPE
              END-IF
           ELSE
              MOVE AUX-INTREF71(1:6);       TO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-MTCNS59.                                      MB48*
      ******************************************************************
       ARMA-DESC-MTCNS59.
      *
           INITIALIZE S209-CONCEPT
      *MCV002-I
           MOVE AUX-CTA-INPUT          TO V111-ACC-CREDIT
           MOVE T071-NUM-OPERATION     TO V111-MOV-CREDIT
           MOVE T071-AMT               TO V111-AMT
           PERFORM QUERY-BGDT111-CRE
           EVALUATE TRUE
           WHEN SQL-88-OK
               MOVE 'Cobro de '          TO S209-CONCEPT(1:9); 
               MOVE V111-REF-DEBIT(1:40);  TO S209-CONCEPT(10:40); 
               MOVE WSV-AUX-DESC(1:30);    TO VA-DESC-OPE
           WHEN SQL-88-NOT-FOUND
               IF WSV-AUX-DESC(1:5);  = 'MTCN:'
                  MOVE 'Cobro de '            TO S209-CONCEPT(1:9); 
                  MOVE T606-DESCRIPTION(1:40);  TO S209-CONCEPT(10:40); 
                  MOVE WSV-AUX-DESC(1:30);  TO VA-DESC-OPE
               ELSE
                  MOVE WSV-AUX-DESC(1:30);  TO VA-DESC-OPE
               END-IF
           WHEN OTHER
              MOVE SPACES                 TO S209-CONCEPT
                                             VA-DESC-OPE
           END-EVALUATE
      *MCV002-F
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-MTCNS60.                                      MB48*
      ******************************************************************
       ARMA-DESC-MTCNS60.
      *
           INITIALIZE S209-CONCEPT
           IF WSV-AUX-DESC(1:5);  = 'MTCN:'
              MOVE 'Env�o de '            TO S209-CONCEPT(1:9); 
              MOVE T606-DESCRIPTION(1:40);  TO S209-CONCEPT(10:40); 
              MOVE WSV-AUX-DESC(1:30);  TO VA-DESC-OPE
           ELSE
              MOVE WSV-AUX-DESC(1:30);  TO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-MTCNS55.                                 MB20-MB90*
      ******************************************************************
       ARMA-DESC-MTCNS55.
      *
           IF (WSV-AUX-DESC(1:5);  = 'MTCN:' OR
      * DECLARE DECLARE @BAZ008C.I
               WSV-AUX-DESC(1:5);  = 'MTCN ' OR
               WSV-AUX-DESC(1:5);  = 'DEXI ' OR
      * DECLARE DECLARE @BAZ008C.F
               WSV-AUX-DESC(1:7);  = 'DEX INT'); 
              INITIALIZE S209-CONCEPT
              MOVE 'Env�o de Dinero '          TO S209-CONCEPT(1:16); 
              MOVE WSV-AUX-DESC(1:30);           TO VA-DESC-OPE
      * DECLARE DECLARE @BAZ008C.I -- Complementar MTCN p/Homologar detalle como Trx.MB48
              IF (WSV-AUX-DESC(1:5);  = 'MTCN ' OR
                  WSV-AUX-DESC(1:5);  = 'DEXI '); 
                 MOVE WSV-AUX-DESC(6:15);  TO VA-BENEFIC
              END-IF
      * DECLARE DECLARE @BAZ008C.F
           ELSE
              IF WSV-AUX-DESC(1:3);  = 'IVA'
                 MOVE 'IVA de comisi�n'  TO S209-CONCEPT(1:15); 
                 MOVE 'Tasa'             TO VA-DESC-OPE(1:4); 
                 MOVE '16%'              TO VA-DESC-OPE(6:3); 
              END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-COMMTCNS57.                              MB20-MB90*
      ******************************************************************
       ARMA-DESC-COMMTCNS57.
      *
           IF (WSV-AUX-DESC(1:8);  = ('COMISION' OR 'Comisi�n' OR
                                   'Comision');  OR
               WSV-AUX-DESC(1:5);  = 'MTCN:'); 
              INITIALIZE S209-CONCEPT
              MOVE 'Comisi�n env�o Dinero Express' TO S209-CONCEPT
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-COD683.                                           *
      ******************************************************************
       ARMA-DESC-COD683.
      *
           INITIALIZE S209-CONCEPT
      * DECLARE DECLARE @BAZ0007G.I
      *    MOVE 'Impuesto a cuenta ahorro plazo' TO S209-CONCEPT
           MOVE 'ISR a cargo por pago de Intereses' TO S209-CONCEPT
      * DECLARE DECLARE @BAZ0007G.F
           MOVE WSV-AUX-DESC(21:10);               TO VA-DESC-OPE
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-PAGPREST.                                         *
      ******************************************************************
       ARMA-DESC-PAGPREST.
      *LCR-INI
           IF VA-COD-MOV = 'D23'
      *      IF WSV-AUX-DESC = ('Nomina big' OR 'nomina big'); 
      *        MOVE 'Nomina'             TO VA-DESC-OPE
      *      ELSE
      *        MOVE 'Pago credito'       TO VA-DESC-OPE
      *      END-IF
             INITIALIZE S209-CONCEPT
             MOVE 'Pago cr�dito '        TO S209-CONCEPT
      *      MOVE 'N�mina'               TO VA-DESC-OPE
           ELSE
      *LCR-FIN
             MOVE 'No. DE PEDIDO'        TO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-PRESTNOM.                                         *
      ******************************************************************
       ARMA-DESC-PRESTNOM.
      *MCV002-I
           IF VA-COD-MOV EQUAL TO 'S20'
              INITIALIZE S209-CONCEPT
              MOVE 'DEPOSITO CREDITO NOMINA'   TO S209-CONCEPT
           END-IF
           .
      *MCV002-F
      ******************************************************************
      *.PN ARMA-DESC-CODR80.                                           *
      ******************************************************************
       ARMA-DESC-CODR80.
      *
           INITIALIZE AUX-DESCOD-R80
                      S209-CONCEPT
      *
      *    IF WSV-AUX-DESC(1:4);  = 'ALTA'
      *      MOVE 'Devoluci�n por cargo de verificaci�n' TO S209-CONCEPT
           IF (WSV-AUX-DESC(1:4);  = 'ALTA' OR
               WSV-AUX-DESC(1:8);  = 'DEPOSITO' OR
               WSV-AUX-DESC(1:14);  = 'ABONO POR ALTA'); 
      *        MOVE 'Dep�sito con tarjeta de cr�dito/d�bito'
               MOVE 'Dep�sito con tarjeta                  '
                                                       TO S209-CONCEPT
           END-IF
      *
           IF T606-DESCRIPTION = SPACES OR LOW-VALUES
              MOVE SPACES TO VA-DESC-OPE
           ELSE
             MOVE T606-DESCRIPTION(1:6);     TO AUX-BIN
             PERFORM QUERY-MCDT097
             MOVE SQLCODE  TO SQL-VALUES
             IF SQL-88-OK
                MOVE AUX-DESBANCO          TO AUX-DESCOD-R801
                IF AUX-DESBANCO(1:8); = 'AMEXBANK'
                   MOVE 'American Express' TO AUX-DESCOD-R801
                END-IF
             ELSE
                IF AUX-BIN(2:1);  = '3'
                 MOVE 'American Express'   TO AUX-DESCOD-R801
                ELSE
                 MOVE SPACES               TO AUX-DESCOD-R801
                END-IF
             END-IF
      *
             MOVE '****'                   TO AUX-DESCOD-R802(1:4); 
             MOVE T606-DESCRIPTION(13:4);    TO AUX-DESCOD-R802(5:4); 
                  STRING AUX-DESCOD-R801 DELIMITED BY '  '
                                      ' ' DELIMITED BY SIZE
                         AUX-DESCOD-R802 DELIMITED BY '  '
                                          INTO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-COD036.                                           *
      ******************************************************************
       ARMA-DESC-COD036.
      *
           INITIALIZE S209-CONCEPT
                      DCLBGGT089
                      DCLBADV0010
                      AUX-DESDEP-CHEQ
      *
      *    MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
      *    IF WSV-AUX-DESC(22:9);  = SPACES OR LOW-VALUES
      *        MOVE WSV-AUX-DESC (22:9);  TO VA-DESC-OPE(8:9); 
      *                                         VA-NUM-CHEQUE
      *    END-IF
      *    MOVE VA-NUM-CHEQUE TO VA-NUM-CHEQUE-9
      *    PERFORM CONSULTA-BADT001
      *    MOVE V0010-ENT-DRWRCHK       TO V040-COD-ENTITY
      *    PERFORM CONSULTA-TCDT040
      *    MOVE V040-DES-ENTITY         TO VA-DESC-OPE(18:33); 
      *
           MOVE 'Dep�sito cheque'       TO S209-CONCEPT
           IF WSV-AUX-DESC(22:9);  IS NUMERIC
              MOVE WSV-AUX-DESC(22:9);    TO VA-NUM-CHEQUE
              MOVE E009-NUMCUEN(1:4);     TO T089-CEN-REG
              MOVE E009-NUMCUEN(5:10);    TO T089-ACC
              MOVE CAA-ENT-ACC          TO T089-ENT
              MOVE AUX-AMT-COMP3        TO T089-AMT-ORIGIN
              MOVE WSV-AUX-FECHA        TO T089-DAT-REG
              MOVE WSV-AUX-HORA(1:2);     TO T089-TIM-REG(1:2); 
              MOVE '.'                  TO T089-TIM-REG(3:1); 
              MOVE WSV-AUX-HORA(3:2);     TO T089-TIM-REG(4:2); 
              MOVE '.'                  TO T089-TIM-REG(6:1); 
              MOVE '00'                 TO T089-TIM-REG(7:2); 
              MOVE WSV-AUX-DESC(1:30);    TO T089-OBSERVATIONS
              PERFORM QUERY-BGDT089
              MOVE SQLCODE              TO SQL-VALUES
              IF SQL-88-OK
                 MOVE E009-NUMCUEN(1:4);  TO V0010-BRN-ICMACC
                 MOVE E009-NUMCUEN(5:2);  TO V0010-TYP-ICMACC
                 MOVE E009-NUMCUEN(7:8);  TO V0010-NUM-ICMACC
                 MOVE CAA-ENT-ACC       TO V0010-ENT-ICMACC
                 MOVE T089-NUM-WHD      TO V0010-NUM-WHD
                 PERFORM QUERY-BADT001
                 IF V040-DES-ENTITY = SPACES
                    MOVE VA-NUM-CHEQUE  TO VA-DESC-OPE
                 ELSE
                    MOVE V040-DES-ENTITY TO AUX-DESDEP-CHEQ1
                    MOVE VA-NUM-CHEQUE   TO AUX-DESDEP-CHEQ2
                    STRING AUX-DESDEP-CHEQ1 DELIMITED BY '  '
                                        ' ' DELIMITED BY SIZE
                           AUX-DESDEP-CHEQ2 DELIMITED BY '  '
                                            INTO VA-DESC-OPE
                 END-IF
              ELSE
                MOVE VA-NUM-CHEQUE      TO VA-DESC-OPE
              END-IF
           ELSE
              MOVE SPACES  TO VA-DESC-OPE
           END-IF
           INITIALIZE VA-NUM-CHEQUE
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODA85.                                           *
      ******************************************************************
       ARMA-DESC-CODA85.
      *
           INITIALIZE S209-CONCEPT
                      AUX-DESDEP-CHEQ
      * DECLARE DECLARE @BAZ007B.I
      *    MOVE 'Dep�sito cheque'       TO S209-CONCEPT
      *    IF WSV-AUX-DESC(9:7);  IS NUMERIC
      *       MOVE WSV-AUX-DESC(9:7);     TO VA-NUM-CHEQUE
      *       PERFORM CONSULTA-BADT001
      *       IF V0010-ENT-DRWRCHK = SPACES
      *          MOVE VA-NUM-CHEQUE     TO VA-DESC-OPE
      *       ELSE
      *        MOVE V0010-ENT-DRWRCHK    TO V040-COD-ENTITY
      *        PERFORM CONSULTA-TCDT040
      *        MOVE V040-DES-ENTITY      TO AUX-DESDEP-CHEQ1
      *        MOVE VA-NUM-CHEQUE        TO AUX-DESDEP-CHEQ2
      *         STRING AUX-DESDEP-CHEQ1 DELIMITED BY '  '
      *                             ' ' DELIMITED BY SIZE
      *                AUX-DESDEP-CHEQ2 DELIMITED BY '  '
      *                                 INTO VA-DESC-OPE
      *       END-IF
      *    ELSE
      *       MOVE SPACES  TO VA-DESC-OPE
      *    END-IF
      *    INITIALIZE VA-NUM-CHEQUE
      *--
           MOVE 'Fondeo para pago de cheque' TO S209-CONCEPT
           IF WSV-AUX-DESC(9:7);  IS NUMERIC
              MOVE 'No. De Cheque '    TO AUX-DESDEP-CHEQ1
              MOVE WSV-AUX-DESC(9:7);    TO AUX-DESDEP-CHEQ2
               STRING AUX-DESDEP-CHEQ1 DELIMITED BY '  '
                                   ' ' DELIMITED BY SIZE
                      AUX-DESDEP-CHEQ2 DELIMITED BY '  '
                                       INTO VA-DESC-OPE
           ELSE
              MOVE SPACES  TO VA-DESC-OPE
           END-IF
      * DECLARE DECLARE @BAZ007B.F
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-PAGCHEQ.                                          *
      ******************************************************************
       ARMA-DESC-PAGCHEQ.
      *
           IF VA-COD-MOV = '875'
      * DECLARE DECLARE @BAZ007B.I
      *       MOVE 'CHEQUE'                TO VA-DESC-OPE(1:6); 
      *       PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
      *       MOVE VA-NUM-CHEQUE           TO VA-DESC-OPE(8:10); 
              INITIALIZE S209-CONCEPT
              MOVE 'Pago de cheque No. '   TO S209-CONCEPT(1:19); 
              MOVE 024                     TO VN-CONT
              PERFORM MUEVE-CHEQUE UNTIL SW-FIN-OK
              MOVE VA-NUM-CHEQUE           TO S209-CONCEPT(20:09); 
              SET SW-FIN-NO                TO TRUE
              INITIALIZE VA-NUM-CHEQUE
      * DECLARE DECLARE @BAZ007B.F
           ELSE
              INITIALIZE S209-CONCEPT
              MOVE 'Dep�sito cheque'       TO S209-CONCEPT
              MOVE 'BANCO AZTECA'          TO AUX-DESDEP-CHEQ1
              MOVE WSV-AUX-DESC(24:7);       TO AUX-DESDEP-CHEQ2
                STRING AUX-DESDEP-CHEQ1 DELIMITED BY '  '
                                    ' ' DELIMITED BY SIZE
                       AUX-DESDEP-CHEQ2 DELIMITED BY '  '
                                        INTO VA-DESC-OPE
           END-IF
           .
      *
      * DECLARE DECLARE @BAZ005.I********************************************************
      *.PN ARMA-DESC-COD656.                                           *
      ******************************************************************
       ARMA-DESC-COD656.
      *--Guardadito Go
           IF AUX-PRODSPRD = '990001'
              MOVE 'Alta de tarjeta prepagada' TO S209-CONCEPT
              MOVE SPACES                      TO VA-DESC-OPE
           END-IF
           .
      * DECLARE DECLARE @BAZ005.F
      * DECLARE DECLARE @BAZ074-I
      ******************************************************************
      * ARMA-DESC-RET-AD1.                                             *
      ******************************************************************
       ARMA-DESC-RET-AD1.
      *
           INITIALIZE S209-CONCEPT
                      DCLBGGT089

              MOVE E009-NUMCUEN(1:4);     TO T089-CEN-REG
              MOVE E009-NUMCUEN(5:10);    TO T089-ACC
              MOVE CAA-ENT-ACC          TO T089-ENT
              MOVE WSV-AUX-NUMOPE       TO T089-NUM-WHD

              PERFORM QUERY-BGDT089-USER

              MOVE SQLCODE              TO SQL-VALUES
              IF SQL-88-OK
                 MOVE T089-USERREG      TO VA-USER-089
              ELSE
                 MOVE SPACES            TO VA-USER-089
              END-IF
           .
      * DECLARE DECLARE @BAZ074-F
      ******************************************************************
      *.PN VALIDA-BINBANCO.    AUX-DESTDC-PAGO1 = NOMBRE BANCO         *
      *                        AUX-DESTDC-PAGO2 = **** + ultimos 4 tarj*
      ******************************************************************
       VALIDA-BINBANCO.
      *
           PERFORM QUERY-MCDT097
      *
           MOVE SQLCODE  TO SQL-VALUES
           IF SQL-88-OK
              IF WSV-AUX-DESC(1:3);  = 'AB '
                 MOVE '****'             TO AUX-DESTDC-PAGO2(1:4); 
                 MOVE WSV-AUX-DESC(16:4);  TO AUX-DESTDC-PAGO2(5:4); 
                 IF AUX-DESBANCO(1:8); = 'AMEXBANK'
                   MOVE 'American Express'  TO AUX-DESBANCO
      *            MOVE WSV-AUX-DESC(15:4);   TO AUX-DESTDC-PAGO2(5:4); 
      * DECLARE DECLARE @BAZ046-I
                 ELSE
                    PERFORM QUERY-MCDT279
      * DECLARE DECLARE @BAZ046-I
                 END-IF
              ELSE
                 MOVE '****'             TO AUX-DESTDC-PAGO2(1:4); 
                 MOVE WSV-AUX-DESC(22:4);  TO AUX-DESTDC-PAGO2(5:4); 
                 IF AUX-DESBANCO(1:8); = 'AMEXBANK'
                   MOVE 'American Express'  TO AUX-DESBANCO
      *            MOVE WSV-AUX-DESC(21:4);   TO AUX-DESTDC-PAGO2(5:4); 
      * DECLARE DECLARE @BAZ046-I
                 ELSE
                    PERFORM QUERY-MCDT279
      * DECLARE DECLARE @BAZ046-I
                 END-IF
              END-IF
              MOVE AUX-DESBANCO          TO AUX-DESTDC-PAGO1
           ELSE
              IF WSV-AUX-DESC(1:3);  = 'AB '
                 MOVE '****'             TO AUX-DESTDC-PAGO2(1:4); 
                 MOVE WSV-AUX-DESC(16:4);  TO AUX-DESTDC-PAGO2(5:4); 
                 IF AUX-BIN(2:1);  = '3'
                  MOVE 'American Express'  TO AUX-DESBANCO
      *           MOVE WSV-AUX-DESC(15:4);   TO AUX-DESTDC-PAGO2(5:4); 
      * DECLARE DECLARE @BAZ046-I
                 ELSE
                    PERFORM QUERY-MCDT279
      * DECLARE DECLARE @BAZ046-I
                 END-IF
              ELSE
                 MOVE '****'             TO AUX-DESTDC-PAGO2(1:4); 
                 MOVE WSV-AUX-DESC(22:4);  TO AUX-DESTDC-PAGO2(5:4); 
                 IF AUX-BIN(2:1);  = '3'
                  MOVE 'American Express'  TO AUX-DESBANCO
      *           MOVE WSV-AUX-DESC(21:4);   TO AUX-DESTDC-PAGO2(5:4); 
      * DECLARE DECLARE @BAZ046-I
                 ELSE
                    PERFORM QUERY-MCDT279
      * DECLARE DECLARE @BAZ046-I
                 END-IF
              END-IF
              MOVE AUX-DESBANCO          TO AUX-DESTDC-PAGO1
           END-IF
           .
      *
      ******************************************************************
      *.PN SELECT-MCDT403-CEL.                                         *
      ******************************************************************
       SELECT-MCDT403-CEL.
      *
           INITIALIZE AUX-CTECEL
                      AUX-NOMBRECTE
           MOVE AUX-CEL           TO T403-TEL-CEL
      *
           EXEC SQL
              SELECT TOP 1
                     T403_NUM_CLTE
               INTO :AUX-CTECEL
               FROM MCDT403 with (nolock); 
              WHERE T403_TEL_CEL =  :T403-TEL-CEL
           END-EXEC
      *
           MOVE SQLCODE          TO SQL-VALUES
           IF SQL-88-OK
              MOVE AUX-CTECEL    TO NUM-CUS    OF DCLPEDV0011
              MOVE CAA-ENT-ACC   TO COD-ENTITY OF DCLPEDV0011
              PERFORM QUERY-PEDT001
              MOVE SQLCODE       TO SQL-VALUES
              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                    INTO AUX-NOMBRECTE
              WHEN OTHER
                   MOVE SPACES TO AUX-NOMBRECTE
              END-EVALUATE
           END-IF
           .
      *
      ******************************************************************
      *.PN OBTEN-PRODUCTO-TARJ.                                        *
      ******************************************************************
       OBTEN-PRODUCTO-TARJ.
      *
           INITIALIZE DCLMCDT114
                      DCLMCDT011.
      *
           MOVE CAA-ENT-ACC             TO T114-ENT-CON
           MOVE WSV-AUX-DESC(11:04);      TO T114-BRN-CON
           MOVE WSV-AUX-DESC(15:02);      TO T114-TYP-CON
           MOVE WSV-AUX-DESC(17:08);      TO T114-NUM-CON
           MOVE 'T'                     TO T114-KEY-PRTC
           MOVE '01'                    TO T114-PRTCORD
      *    MOVE '6'                     TO T114-FLG-CRDST
           PERFORM QUERY-MCDT114-BIN
           MOVE SQLCODE                 TO SQL-VALUES
           EVALUATE TRUE
           WHEN SQL-88-OK
      * DECLARE DECLARE @BAZ021-INI
      *         MOVE CAA-ENT-ACC        TO T011-ENT
      *         MOVE T114-NUM-BINCRD    TO T011-NUM-BINCRD
      *         MOVE T114-TYP-CRD       TO T011-TYP-CRD
      * DECLARE DECLARE @ DECLARE DECLARE @BAZ021-FIN
      * DECLARE DECLARE @BAZ007J.I
      *         PERFORM QUERY-MCDT011-BIN
                MOVE '****'             TO VA-DESC-OPE(14:4); 
                MOVE T114-NUM-CRD(7:4);   TO VA-DESC-OPE(18:4); 
           WHEN SQL-88-NOT-FOUND
      *         MOVE WSV-AUX-DESC(11:14);  TO VA-DESC-OPE
                MOVE WSV-AUX-DESC(17:08);  TO VA-DESC-OPE(14:8); 
      * DECLARE DECLARE @BAZ007J.F
           WHEN OTHER
                MOVE SPACES              TO VA-DESC-OPE
           END-EVALUATE
      * DECLARE DECLARE @BAZ007J.I
           MOVE 'Banco Azteca '          TO VA-DESC-OPE(1:13); 
           .
      *
      ******************************************************************
      *.PN OBTEN-DATOS-CLIENTE.                                        *
      ******************************************************************
       OBTEN-DATOS-CLIENTE.
      *
           INITIALIZE DCLPEDV0080
                      AUX-NOMBRECTE   AUX-NUMCUS8
      *
           MOVE CAA-ENT-ACC     TO COD-ENTITY   OF DCLPEDV0080
           MOVE AUX2-NUMCEN     TO BRN-OPEN     OF DCLPEDV0080
           MOVE AUX2-NUMPRD     TO COD-PRODSERV OF DCLPEDV0080
           MOVE AUX2-NUMCAC     TO NUM-ACCOUNT  OF DCLPEDV0080
           MOVE 'T'             TO KEY-PARTIC   OF DCLPEDV0080
           MOVE '01'            TO PARTSEQ      OF DCLPEDV0080
      *
           PERFORM QUERY-PEDT008
      *
           MOVE SQLCODE         TO SQL-VALUES
           IF SQL-88-OK
              MOVE AUX-NUMCUS8  TO NUM-CUS    OF DCLPEDV0011
              MOVE CAA-ENT-ACC  TO COD-ENTITY OF DCLPEDV0011
              PERFORM QUERY-PEDT001
              MOVE SQLCODE      TO SQL-VALUES
              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                    INTO AUX-NOMBRECTE
              WHEN OTHER
                   MOVE SPACES TO AUX-NOMBRECTE
              END-EVALUATE
           ELSE
              MOVE SPACES TO AUX-NOMBRECTE
              MOVE SPACES TO AUX-NUMCUS8
           END-IF
           .
      *
      * DECLARE DECLARE @BAZ029-INI
      ******************************************************************
      *.PN CLIENTE TUTOR GUARDADITO KIDS.                              *
      ******************************************************************
       OBTEN-CTE-GKIDS.
      *
           INITIALIZE DCLPEDV0080
                      AUX-NOMBRECTE   AUX-NUMCUS8
      *
           SET   WSS-CTA-NO       TO TRUE
           MOVE CAA-ENT-ACC       TO COD-ENTITY   OF DCLPEDV0080
           MOVE E009-NUMCUEN(1:4);  TO BRN-OPEN     OF DCLPEDV0080
           MOVE E009-NUMCUEN(5:2);  TO COD-PRODSERV OF DCLPEDV0080
           MOVE E009-NUMCUEN(7:8);  TO NUM-ACCOUNT  OF DCLPEDV0080
           MOVE 'E'               TO KEY-PARTIC   OF DCLPEDV0080
           MOVE '01'              TO PARTSEQ      OF DCLPEDV0080
      *
      * DECLARE DECLARE @BAZ055-INI
           MOVE WSE-NUMCUS        TO NUM-CUS      OF DCLPEDV0080
      *    PERFORM QUERY-PEDT008
           PERFORM QUERY-PEDT008-GK
      * DECLARE DECLARE @BAZ055-FIN
      *
           MOVE SQLCODE         TO SQL-VALUES
           IF SQL-88-OK
              MOVE AUX-NUMCUS8  TO NUM-CUS    OF DCLPEDV0011
              MOVE CAA-ENT-ACC  TO COD-ENTITY OF DCLPEDV0011
              PERFORM QUERY-PEDT001
              MOVE SQLCODE      TO SQL-VALUES
              EVALUATE TRUE
              WHEN SQL-88-OK
                   STRING AUX-NAME  DELIMITED BY '   '
                                ' ' DELIMITED BY SIZE
                        AUX-SURNAME DELIMITED BY '   '
                                    INTO AUX-NOMBRECTE
                   SET  WSS-CTA-SI   TO TRUE
              WHEN OTHER
                   MOVE SPACES TO AUX-NOMBRECTE
              END-EVALUATE
           ELSE
              MOVE SPACES TO AUX-NOMBRECTE
              MOVE SPACES TO AUX-NUMCUS8
           END-IF
           .

      * DECLARE DECLARE @BAZ029-FIN
      ******************************************************************
      *.PN OBTEN-CUENTA-20POS.                                         *
      ******************************************************************
       OBTEN-CUENTA-20POS.
      *
           INITIALIZE BGVC041
           MOVE CAA-ENT-ACC            TO V041-ENT
           MOVE E009-NUMCUEN(1:4);       TO V041-CEN-REG
           MOVE E009-NUMCUEN(5:10);      TO V041-ACC
      * DECLARE DECLARE @BAZ037-I
           IF TB-T041-COD-PRODUCT(1);       EQUAL SPACES OR
              TB-T041-COD-PRODUCT(1);       EQUAL LOW-VALUES
      *
              MOVE V041-ENT               TO AUX-CTA-ENT
              MOVE V041-CEN-REG           TO AUX-CTA-CEN
              MOVE ZEROES                 TO AUX-CTA-DIG1
              MOVE ZEROES                 TO AUX-CTA-DIG2
              MOVE V041-ACC               TO AUX-CTA-NUM
           ELSE
              MOVE TB-T041-CAC-DIG1(1);     TO V041-CAC-DIG1
              MOVE TB-T041-CAC-DIG2(1);     TO V041-CAC-DIG2
              MOVE TB-T041-COD-PRODUCT(1);  TO V041-COD-PROD
              MOVE TB-T041-COD-SPROD(1);    TO V041-COD-SPROD
              MOVE TB-T041-CEN-ACCT(1);     TO V041-CEN-ACCT
              MOVE TB-T041-FCC(1);          TO V041-FCC
      *
              MOVE V041-ENT               TO AUX-CTA-ENT
              MOVE V041-CEN-REG           TO AUX-CTA-CEN
              MOVE V041-CAC-DIG1          TO AUX-CTA-DIG1
              MOVE V041-CAC-DIG2          TO AUX-CTA-DIG2
              MOVE V041-ACC               TO AUX-CTA-NUM
           END-IF.
      *    PERFORM QUERY-BGDT041
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    IF SQL-88-OK
      *       MOVE V041-ENT            TO AUX-CTA-ENT
      *       MOVE V041-CEN-REG        TO AUX-CTA-CEN
      *       MOVE V041-CAC-DIG1       TO AUX-CTA-DIG1
      *       MOVE V041-CAC-DIG2       TO AUX-CTA-DIG2
      *       MOVE V041-ACC            TO AUX-CTA-NUM
      *    ELSE
      *       MOVE V041-ENT            TO AUX-CTA-ENT
      *       MOVE V041-CEN-REG        TO AUX-CTA-CEN
      *       MOVE ZEROES              TO AUX-CTA-DIG1
      *       MOVE ZEROES              TO AUX-CTA-DIG2
      *       MOVE V041-ACC            TO AUX-CTA-NUM
      *    END-IF
      *    .
      * DECLARE DECLARE @BAZ037-F
      *
      ******************************************************************
      *.PN QUERY-BGDT235.                                              *
      ******************************************************************
       QUERY-BGDT235.
      *
           EXEC SQL
              SELECT T235_CEN_REG
               INTO :T235-CEN-REG
                FROM BGDT235 with(nolock); 
               WHERE T235_ACC_ASSO = :T235-ACC-ASSO
                 AND T235_ACC      = :T235-ACC
                 AND T235_CEN_ASSO = :T235-CEN-ASSO
                 AND T235_ENT_ASSO = :T235-ENT-ASSO
           END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-BGDT041.                                              *
      ******************************************************************
       QUERY-BGDT041.
      *
           EXEC SQL
              SELECT  T041_CAC_DIG1   ;
                      T041_CAC_DIG2   ;
                      T041_COD_PRODUCT;
                      T041_COD_SPROD  ;
                      T041_CEN_ACCT   ;
                      T041_FCC
                 INTO :V041-CAC-DIG1  ;
                      :V041-CAC-DIG2  ;
                      :V041-COD-PROD  ;
                      :V041-COD-SPROD ;
                      :V041-CEN-ACCT  ;
                      :V041-FCC
                 FROM BGDT041 with(nolock); 
                WHERE T041_CEN_REG = :V041-CEN-REG
                  AND T041_ACC     = :V041-ACC
                  AND T041_ENT     = :V041-ENT
           END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-PEDT008.                                              *
      ******************************************************************
       QUERY-PEDT008.
      *
           EXEC SQL
              SELECT  NUM_CUS
                INTO :AUX-NUMCUS8
                FROM PEDT008 with(nolock); 
               WHERE NUM_ACCOUNT  = :DCLPEDV0080.NUM-ACCOUNT
                 AND BRN_OPEN     = :DCLPEDV0080.BRN-OPEN
                 AND COD_PRODSERV = :DCLPEDV0080.COD-PRODSERV
                 AND COD_ENTITY   = :DCLPEDV0080.COD-ENTITY
                 AND KEY_PARTIC   = :DCLPEDV0080.KEY-PARTIC
                 AND PARTSEQ      = :DCLPEDV0080.PARTSEQ
           END-EXEC.
      * DECLARE DECLARE @BAZ055-INI
      ******************************************************************
      *.PN QUERY-PEDT008-GK                                            *
      ******************************************************************
       QUERY-PEDT008-GK.
      *
           EXEC SQL
              SELECT  NUM_CUS
                INTO :AUX-NUMCUS8
                FROM PEDT008 with(nolock); 
               WHERE NUM_CUS      = :DCLPEDV0080.NUM-CUS
                 AND NUM_ACCOUNT  = :DCLPEDV0080.NUM-ACCOUNT
                 AND BRN_OPEN     = :DCLPEDV0080.BRN-OPEN
                 AND COD_PRODSERV = :DCLPEDV0080.COD-PRODSERV
                 AND COD_ENTITY   = :DCLPEDV0080.COD-ENTITY
                 AND KEY_PARTIC   = :DCLPEDV0080.KEY-PARTIC
                 AND PARTSEQ      = :DCLPEDV0080.PARTSEQ
           END-EXEC.
      *
      * DECLARE DECLARE @BAZ055-FIN
      *
      ******************************************************************
      *.PN QUERY-PEDT001.                                              *
      ******************************************************************
       QUERY-PEDT001.
      *
           INITIALIZE AUX-NAME
                      AUX-SCDNAME
                      AUX-SURNAME
           EXEC SQL
              SELECT SURNAME    ;
                     SCDSURNAME ;
                     NAME
                INTO :AUX-SURNAME;
                     :AUX-SCDNAME;
                     :AUX-NAME
               FROM PEDT001 with(nolock); 
              WHERE NUM_CUS    = :DCLPEDV0011.NUM-CUS
                AND COD_ENTITY = :DCLPEDV0011.COD-ENTITY
           END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-WPDT003.                                              *
      ******************************************************************
       QUERY-WPDT003.
      *
            EXEC SQL
              SELECT T003_DES_CODE;
                     T003_SDE_CODE
                INTO :T003-DES-CODE;
                     :T003-SDE-CODE
                FROM WPDT003 with(nolock); 
                WHERE T003_COD_ENTITY   = :T003-COD-ENTITY
                  AND T003_COD_PRODUCT  = :T003-COD-PRODUCT
                  AND T003_CODE         = :T003-CODE
                  AND T003_TYP_CODE     = :T003-TYP-CODE
                  AND T003_COD_LANGUAGE = :T003-COD-LANGUAGE
            END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-TCDT010-CAJERO.                                       *
      ******************************************************************
       QUERY-TCDT010-CAJERO.
      *
            EXEC SQL
              SELECT DTA_TBLKEY
                INTO :T010-DTA-TBLKEY
                FROM TCDT010 with (nolock); 
                WHERE COD_TABLE = :COD-0497
                  AND LNG_DATA  = :CA-E
                  AND KEY_TABLE = :AUX-TERMINAL
                  AND ENTITY    = :CA-0127
            END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-TCDT010-LIB.                                          *
      ******************************************************************
       QUERY-TCDT010-LIB.
      *
            EXEC SQL
              SELECT DTA_TBLKEY
                INTO :T010-DTA-TBLKEY
                FROM TCDT010 with (nolock); 
                WHERE COD_TABLE = :VA-COD-TC
                  AND LNG_DATA  = :CA-E
                  AND KEY_TABLE = :VA-KEY-TC
                  AND ENTITY    = :CA-0127
            END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-MBDT010.                                              *
      ******************************************************************
       QUERY-MBDT010.
      *
            EXEC SQL
              SELECT TOP 1
                     T010_ALIAS
               INTO :T010-ALIAS
               FROM MBDT010 with (nolock); 
              WHERE T010_BDMID       = :T010-BDMID
                AND T010_CTA_DESTINO = :T010-CTA-DESTINO
                AND T010_STATUS      = 'A'
             ORDER BY T010_STP_LASTMOD ASC
            END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-MCDT097.                                              *
      ******************************************************************
       QUERY-MCDT097.
      *
           INITIALIZE DCLMCDT097     AUX-DESBANCO
           MOVE CAA-ENT-ACC          TO T097-ENTITY
           MOVE AUX-BIN              TO T097-NUM-BIN
           MOVE CA-S                 TO T097-FLG-ACTIVE
      *
           EXEC SQL
              SELECT T097_DES_BANK
                INTO :AUX-DESBANCO
                FROM MCDT097 with (nolock); 
               WHERE T097_NUM_BIN    = :T097-NUM-BIN
                 AND T097_FLG_ACTIVE = :T097-FLG-ACTIVE
                 AND T097_ENTITY     = :T097-ENTITY
           END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-BGDT089.                                              *
      ******************************************************************
       QUERY-BGDT089.
      *
            EXEC SQL
             SELECT  T089_NUM_WHD
               INTO  :T089-NUM-WHD
               FROM BGDT089 with (nolock); 
              WHERE T089_CEN_REG    = :T089-CEN-REG
                AND T089_ACC        = :T089-ACC
                AND T089_ENT        = :T089-ENT
                AND T089_AMT_ORIGIN = :T089-AMT-ORIGIN
                AND T089_DAT_REG    = :T089-DAT-REG
                AND T089_TIM_REG    = :T089-TIM-REG
            END-EXEC.
      *
      * DECLARE DECLARE @BAZ074-I
      ******************************************************************
      * QUERY-BGDT089-USER                                             *
      ******************************************************************
       QUERY-BGDT089-USER.
      *
            EXEC SQL
             SELECT T089_USERREG
               INTO :T089-USERREG
               FROM BGDT089 with (nolock); 
              WHERE T089_CEN_REG    = :T089-CEN-REG
                AND T089_ACC        = :T089-ACC
                AND T089_ENT        = :T089-ENT
                AND T089_NUM_WHD    = :T089-NUM-WHD
            END-EXEC.
      *
      * DECLARE DECLARE @BAZ074-F
      ******************************************************************
      *.PN QUERY-BADT001.                                              *
      ******************************************************************
       QUERY-BADT001.
      *
           EXEC SQL
              SELECT T001_ENT_DRWRCHK;
                     T001_FLG_RETURN;
                     T001_COD_RETURN
               INTO :V0010-ENT-DRWRCHK;
                    :V0010-FLG-RETURN;
                    :V0010-COD-RETURN
               FROM BADT001 with (nolock); 
               WHERE T001_ENT_ICMACC = :V0010-ENT-ICMACC
                 AND T001_BRN_ICMACC = :V0010-BRN-ICMACC
                 AND T001_TYP_ICMACC = :V0010-TYP-ICMACC
                 AND T001_NUM_ICMACC = :V0010-NUM-ICMACC
                 AND T001_NUM_WHD    = :V0010-NUM-WHD
           END-EXEC
      *
           MOVE SQLCODE              TO SQL-VALUES
           IF SQL-88-OK
              MOVE V0010-ENT-DRWRCHK TO V040-COD-ENTITY
              PERFORM CONSULTA-TCDT040
           ELSE
              MOVE ZEROES TO V0010-ENT-DRWRCHK
                             V040-COD-ENTITY
           END-IF
           .
      *
      ******************************************************************
      *.PN QUERY-BGDT111-DEB.                                          *
      ******************************************************************
      *QUERY-BGDT111-DEB.
      *
      *     EXEC SQL
      *      SELECT  T111_REF_CREDIT
      *             ;T111_ACC_CREDIT
      *             ;T111_MOV_CREDIT
      *        INTO  :V111-REF-CREDIT
      *             ;:V111-ACC-CREDIT
      *             ;:V111-MOV-CREDIT
      *        FROM BGDT111 with (nolock); 
      *       WHERE T111_ACC_DEBIT  = :V111-ACC-DEBIT
      *         AND T111_MOV_DEBIT  = :V111-MOV-DEBIT
      *         AND T111_AMT        = :V111-AMT
      *     END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-BGDT111-CRE.                                          *
      ******************************************************************
       QUERY-BGDT111-CRE.
      *
            EXEC SQL
             SELECT  T111_REF_DEBIT
                    ;T111_ACC_DEBIT
                    ;T111_MOV_DEBIT
               INTO  :V111-REF-DEBIT
                    ;:V111-ACC-DEBIT
                    ;:V111-MOV-DEBIT
               FROM BGDT111 with (nolock); 
              WHERE T111_ACC_CREDIT = :V111-ACC-CREDIT
                AND T111_MOV_CREDIT = :V111-MOV-CREDIT
                AND T111_AMT        = :V111-AMT
            END-EXEC.
      *
      ******************************************************************
      *.PN QUERY-MCDT114-BIN.                                          *
      ******************************************************************
       QUERY-MCDT114-BIN.
      *
           EXEC SQL
              SELECT TOP 1
                     T114_NUM_BINCRD   ;
      * DECLARE DECLARE @BAZ021-INI
                     T114_NUM_CRD
      *              T114_NUM_CRD      ;
      *              T114_TYP_CRD      ;
      *              T114_ENT_CON
      * DECLARE DECLARE @BAZ021-FIN
               INTO :T114-NUM-BINCRD   ;
      * DECLARE DECLARE @BAZ021-INI
                    :T114-NUM-CRD
      *             :T114-NUM-CRD      ;
      *             :T114-TYP-CRD      ;
      *             :T114-ENT-CON
      * DECLARE DECLARE @BAZ021-FIN
               FROM MCDT114 with(nolock); 
              WHERE T114_ENT_CON    = :T114-ENT-CON
                AND T114_BRN_CON    = :T114-BRN-CON
                AND T114_TYP_CON    = :T114-TYP-CON
                AND T114_NUM_CON    = :T114-NUM-CON
                AND T114_KEY_PRTC   = :T114-KEY-PRTC
                AND T114_PRTCORD    = :T114-PRTCORD
                AND T114_DAT_CONANN IS NULL
                AND T114_COD_CRDWHD = ' '
      * DECLARE DECLARE @BAZ021-INI
                AND T114_FLG_CRDST  >= ' '
      * DECLARE DECLARE @BAZ021-FIN
      *         AND T114_FLG_CRDST  = :T114-FLG-CRDST
                ORDER BY T114_DAT_CRDST DESC
           END-EXEC.
      * DECLARE DECLARE @BAZ021-INI
      ******************************************************************
      *.PN QUERY-MCDT011-BIN.                                          *
      ******************************************************************
      *QUERY-MCDT011-BIN.
      *
      *    EXEC SQL
      *       SELECT T011_DES_CRDTYP
      *        INTO :T011-DES-CRDTYP
      *         FROM MCDT011 with (nolock); 
      *        WHERE T011_ENT        =:T011-ENT
      *          AND T011_NUM_BINCRD =:T011-NUM-BINCRD
      *          AND T011_TYP_CRD    =:T011-TYP-CRD
      *    END-EXEC
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *    WHEN SQL-88-OK
      *         MOVE T011-DES-CRDTYP         TO VA-DESC-OPE
      *    WHEN SQL-88-NOT-FOUND
      *         MOVE WSV-AUX-DESC(11:14);      TO VA-DESC-OPE
      *    WHEN OTHER
      *         MOVE SPACES                  TO VA-DESC-OPE
      *    END-EVALUATE
      *    .
      * DECLARE DECLARE @BAZ021-FIN
      ******************************************************************
      *.PN QUERY-MCDT028.                                              *
      ******************************************************************
       QUERY-MCDT028.
      *
           EXEC SQL
            SELECT TOP 1
                   T028_BRN_CON
                  ;T028_TYP_CON
              INTO :T028-BRN-CON
                  ;:T028-TYP-CON
             FROM MCDT028 with(nolock); 
            WHERE T028_NUM_CON = :T028-NUM-CON
              AND T028_ENT_CON = :T028-ENT-CON
              AND T028_TYP_CON NOT IN ('50';'80';'70'); 
      *     WHERE T028_ENT_ACC = :T028-ENT-ACC
      *       AND T028_BRN_ACC = :T028-BRN-ACC
      *       AND T028_TYP_ACC = :T028-TYP-ACC
      *       AND T028_ACC     = :T028-ACC
      *       AND T028_NUM_CON = :T028-NUM-CON
      *       AND T028_ENT_CON = :T028-ENT-CON
           END-EXEC
           .
      *
      ******************************************************************
      *.PN BUSCA-PLAZO.                                                *
      ******************************************************************
       BUSCA-PLAZO.
      *
           IF V041-COD-PROD = '16'
              EVALUATE V041-COD-SPROD
                  WHEN '0011'
                  WHEN '0036'
                      MOVE 030   TO AUX-DIAS
                  WHEN '0012'
                  WHEN '0037'
                      MOVE 060   TO AUX-DIAS
                  WHEN '0013'
                  WHEN '0038'
                      MOVE 090   TO AUX-DIAS
                  WHEN '0014'
                  WHEN '0039'
                      MOVE 180   TO AUX-DIAS
                  WHEN '0015'
                  WHEN '0040'
                      MOVE 270   TO AUX-DIAS
                  WHEN '0016'
                  WHEN '0041'
                      MOVE 360   TO AUX-DIAS
                  WHEN '0031'
                      MOVE 360   TO AUX-DIAS
                  WHEN '0035'
                      MOVE 360   TO AUX-DIAS
                  WHEN OTHER
                      MOVE 360   TO AUX-DIAS
              END-EVALUATE
           END-IF
      *
           IF V041-COD-PROD = '07'
              EVALUATE V041-COD-SPROD
                  WHEN '0010'
                      MOVE 007   TO AUX-DIAS
                  WHEN '0020'
                      MOVE 014   TO AUX-DIAS
                  WHEN '0030'
                      MOVE 028   TO AUX-DIAS
                  WHEN '0040'
                      MOVE 091   TO AUX-DIAS
                  WHEN '0050'
                      MOVE 182   TO AUX-DIAS
                  WHEN '0060'
                      MOVE 364   TO AUX-DIAS
                  WHEN OTHER
                      MOVE 364   TO AUX-DIAS
              END-EVALUATE
           END-IF
      *
           MOVE 1 TO AUX-X
           MOVE 1 TO AUX-Y
      *
           PERFORM UNTIL SW-FINEDIT-Y
            IF AUX-X < 4
              IF AUX-DIAS(AUX-X:1);  NOT EQUAL TO SPACE
                 MOVE AUX-DIAS(AUX-X:1);  TO AUX-DESINT-RENEJE3(AUX-Y:1); 
                 ADD 1 TO AUX-X
                 ADD 1 TO AUX-Y
              ELSE
                 ADD 1 TO AUX-X
              END-IF
            ELSE
              SET SW-FINEDIT-Y      TO TRUE
            END-IF
           END-PERFORM
           SET SW-FINEDIT-N TO TRUE
           .
      *
      ******************************************************************
      *.PN TIPO-CAMBIO.                                                *
      ******************************************************************
       TIPO-CAMBIO.
      *
           INITIALIZE TCEC0810.
           MOVE 'USD'               TO TCEC0810-COD-SWIFTFCCS.
      *--WSV-AUX-FECHA = YYYY-MM-DD  TCEC0810-DAT-EXCHANGE = DD-MM-YYYY
           MOVE WSV-AUX-FECHA(9:2);   TO TCEC0810-DAT-EXCHANGE(1:2); 
           MOVE WSV-AUX-FECHA(6:2);   TO TCEC0810-DAT-EXCHANGE(4:2); 
           MOVE WSV-AUX-FECHA(1:4);   TO TCEC0810-DAT-EXCHANGE(7:4); 
           MOVE CAA-ENT-ACC         TO TCEC0810-COD-ENTITY.
           MOVE 'B'                 TO TCEC0810-FLG-FCCB3.
           MOVE QBEC999             TO VA-QBEC999-TCEC0810
      *
           EXEC CICS
               LINK PROGRAM(CA-TC7C0810); 
               COMMAREA (VA-TCEC0810); 
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP EQUAL DFHRESP(NORMAL); 
              IF TCEC0810-COD-RETURN = '00'
                 MOVE TCEC0810-BIDRATE    TO AUX-TIP-CAMCPRA
                 MOVE TCEC0810-OFFERRATE  TO AUX-TIP-CAMVTA
              ELSE
                 MOVE CAA-ENT-ACC   TO V081-COD-ENTITY
                 MOVE WSV-AUX-FECHA TO V081-DAT-EXCHANGE
                 MOVE 'USD'         TO V081-COD-FCC
                 MOVE 'B'           TO V081-FLG-FCCB
                 EXEC SQL
                   SELECT OFFERRATE;
                          BIDRATE;
                          FIXRATE;
                          DAT_EXCHANGE;
                          FLG_QUOTATION
                     INTO :V081-OFFERRATE;
                          :V081-BIDRATE;
                          :V081-FIXRATE;
                          :V081-DAT-EXCHANGE;
                          :V081-FLG-QUOTATION
                     FROM TCDT081 with (nolock); 
                     WHERE COD_ENTITY   = :V081-COD-ENTITY
                       AND DAT_EXCHANGE = :V081-DAT-EXCHANGE
                       AND COD_FCC      = :V081-COD-FCC
                       AND FLG_FCCB3    = :V081-FLG-FCCB
                 END-EXEC
      *
                 MOVE SQLCODE TO SQL-VALUES
                 IF SQL-88-OK
                    MOVE V081-BIDRATE   TO AUX-TIP-CAMCPRA
                    MOVE V081-OFFERRATE TO AUX-TIP-CAMVTA
                 ELSE
                    MOVE ZEROES TO AUX-TIP-CAMCPRA
                                   AUX-TIP-CAMVTA
                 END-IF
              END-IF
           ELSE
              MOVE ZEROES TO AUX-TIP-CAMCPRA
                             AUX-TIP-CAMVTA
           END-IF
      *
           IF (AUX-TIP-CAMVTA <> ZEROES AND AUX-TIP-CAMCPRA <> ZEROES); 
              INITIALIZE TCEC0820-INPUT
              MOVE CAA-ENT-ACC      TO TCEC0820-COD-ENTITY
              MOVE E009-NUMCUEN(1:4);  TO TCEC0820-COD-BRANCH
              MOVE 'USD'            TO TCEC0820-COD-FCC
              MOVE AUX-TIP-CAMCPRA  TO TCEC0820-TIP-BIDRATE
              MOVE AUX-TIP-CAMVTA   TO TCEC0820-TIP-OFFERRATE
              MOVE QBEC999          TO VA-QBEC999-TCEC0820
      *
              EXEC CICS
                  LINK PROGRAM(CA-TC7C0820); 
                      COMMAREA (VA-TCEC0820); 
                      NOHANDLE
              END-EXEC
      *
              IF EIBRESP EQUAL DFHRESP(NORMAL); 
                 EVALUATE TCEC0820-COD-RETURN
                 WHEN 00
                 WHEN 40
                     IF TCEC0820-OFFERRATE NOT EQUAL ZEROS AND
                        TCEC0820-BIDRATE NOT EQUAL ZEROS
                        MOVE TCEC0820-BIDRATE   TO AUX-TIP-CAMCPRA
                        MOVE TCEC0820-OFFERRATE TO AUX-TIP-CAMVTA
                     END-IF
                 END-EVALUATE
              END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN EDITA-VALOR.                                                *
      ******************************************************************
       EDITA-VALOR.
      *
           IF AUX-PASO = 1
             MOVE 1 TO AUX-X
             MOVE 1 TO AUX-Y
      *
             PERFORM UNTIL SW-FINEDIT-Y
              IF AUX-X < 17
                IF AUX-MONTO-TIP(AUX-X:1);  NOT EQUAL TO SPACE
                   MOVE AUX-MONTO-TIP(AUX-X:1);  TO AUX-VALOR(AUX-Y:1); 
                   ADD 1 TO AUX-X
                   ADD 1 TO AUX-Y
                ELSE
                   ADD 1 TO AUX-X
                END-IF
              ELSE
                SET SW-FINEDIT-Y      TO TRUE
              END-IF
             END-PERFORM
             SET SW-FINEDIT-N TO TRUE
           END-IF
           IF AUX-PASO = 2
             MOVE 1 TO AUX-X
             MOVE 1 TO AUX-Y
      *
             PERFORM UNTIL SW-FINEDIT-Y
      *       IF AUX-X < 19
              IF AUX-X < 17
                IF AUX-MONTO-UNI(AUX-X:1);  NOT EQUAL TO SPACE
                   MOVE AUX-MONTO-UNI(AUX-X:1);  TO AUX-UNIDA(AUX-Y:1); 
                   ADD 1 TO AUX-X
                   ADD 1 TO AUX-Y
                ELSE
                   ADD 1 TO AUX-X
                END-IF
              ELSE
                SET SW-FINEDIT-Y      TO TRUE
              END-IF
             END-PERFORM
             SET SW-FINEDIT-N TO TRUE
           END-IF
           .
      *
      * DECLARE DECLARE @BAZ005.I********************************************************
      *.PN EDICION-DATOS-CODRET.                                       *
      ******************************************************************
       EDICION-DATOS-CODRET.
      *
           IF T089-COD  = 'E20'
              MOVE 'Recibo de Nomina por liberar' TO WSV-AUX-DESC
           END-IF
      * DECLARE DECLARE @BAZ005F.I
           IF WSV-AUX-DESC(1:11);  = 'Envio a ATM'
      * DECLARE DECLARE @BAZ068-INI
      *       MOVE 'Envio para retirar sin Tarjeta' TO WSV-AUX-DESC
              MOVE 'Retiro para otra persona'   TO WSV-AUX-DESC
      * DECLARE DECLARE @BAZ068-FIN
           END-IF
      * DECLARE DECLARE @BAZ005F.F
           .
      *
      * DECLARE DECLARE @BAZ021-INI
      ******************************************************************
      *.PN VALIDA-BDMID-MBDT036.                                      *
      ******************************************************************
       VALIDA-BDMID-MBDT036.
      *
      * DECLARE DECLARE @BAZ027-I
      *    MOVE CAA-ENT-ACC         TO T036-ENT
      *    MOVE E009-NUMCUEN(1:4);    TO T036-CEN-REG
      *    MOVE E009-NUMCUEN(5:10);   TO T036-ACC
           IF WSS-SEGCON-NO
              MOVE CAA-ENT-ACC                    TO T036-ENT
              MOVE E009-NUMCUEN(1:4);               TO T036-CEN-REG
              MOVE E009-NUMCUEN(5:10);              TO T036-ACC
           END-IF
      * DECLARE DECLARE @BAZ027-F

           EXEC SQL
            SELECT
                  T036_ENT
                 ;T036_CEN_REG
                 ;T036_ACC
                 ;T036_BDMID
                 ;T036_FLG_STAT
      * DECLARE DECLARE @BAZ027-I
                 ;T036_NAME_CUS
                 ;T036_ALIAS
      * DECLARE DECLARE @BAZ027-F
             INTO
                  :T036-ENT
                 ;:T036-CEN-REG
                 ;:T036-ACC
                 ;:T036-BDMID
                 ;:T036-FLG-STAT
      * DECLARE DECLARE @BAZ027-I
                 ;:T036-NAME-CUS
                 ;:T036-ALIAS
      * DECLARE DECLARE @BAZ027-F


            FROM  MBDT036 with(nolock); 
            WHERE T036_ENT         = :T036-ENT
              AND T036_CEN_REG     = :T036-CEN-REG
              AND T036_ACC         = :T036-ACC
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
           EVALUATE TRUE
           WHEN SQL-88-OK
      * DECLARE DECLARE @BAZ027-I
               IF WSS-SEGCON-NO
      * DECLARE DECLARE @BAZ027-F
                  IF T036-FLG-STAT NOT EQUAL SPACES
                     MOVE E009-NUMCUEN        TO CAA-ERR-VARIA1
                     MOVE 'MCE0101'           TO CAA-SW-ERRCOD
                     PERFORM 30000-FIN
                  ELSE
                     IF T036-BDMID NOT EQUAL E009-BDMID
                         MOVE E009-NUMCUEN    TO CAA-ERR-VARIA1
                         MOVE 'MCE0007'       TO CAA-SW-ERRCOD
                         PERFORM 30000-FIN
                     END-IF
                  END-IF
      * DECLARE DECLARE @BAZ027-I
                  MOVE T036-NAME-CUS          TO VA-NAME-CUS-AUX
                  MOVE T036-ALIAS             TO VA-ALIAS-AUX
               END-IF
      *
      * DECLARE DECLARE @BAZ027-F
           WHEN SQL-88-NOT-FOUND
               MOVE E009-NUMCUEN           TO CAA-ERR-VARIA1
               MOVE 'MCE0007'              TO CAA-SW-ERRCOD
               PERFORM 30000-FIN
           WHEN OTHER
               MOVE 'MBE1115'              TO CAA-SW-ERRCOD
               MOVE 'MBDT036'              TO CAA-ERR-VARIA1
               MOVE SQLCODE                TO CAA-ERR-VARIA2
               MOVE 'VALIDA-BDMID-MBDT036' TO CAA-WARN1VARIA1
               PERFORM 30000-FIN
           END-EVALUATE
            .
      *
      * DECLARE DECLARE @BAZ021-FIN
      ******************************************************************
      *.PN VALIDA-RELACION-BDMID.                                      *
      ******************************************************************
       VALIDA-RELACION-BDMID.
      *
           INITIALIZE WSE-ENTRADA
                      DCLPEDV0080
                      AUX-NUMCUS8
           MOVE E009-BDMID            TO  T403-BDMID
      * DECLARE DECLARE @BAZ037-I
           IF TB-T403-TEL-CEL(1);       EQUAL SPACES OR
              TB-T403-TEL-CEL(1);       EQUAL LOW-VALUES
      *
               MOVE 'MCE0007'         TO CAA-SW-ERRCOD
               MOVE 'BDMID MCDT403'   TO CAA-ERR-VARIA1
               MOVE SQLCODE           TO CAA-ERR-VARIA2
               PERFORM 30000-FIN
           ELSE
               MOVE TB-T403-NUM-BIN(1);  TO T403-NUM-BIN
               MOVE TB-T403-NUM-CRD(1);  TO T403-NUM-CRD
               MOVE TB-T403-NUM-CLTE(1); TO T403-NUM-CLTE
               MOVE TB-T403-NUM-CTA(1);  TO T403-NUM-CTA
               MOVE TB-T403-TEL-CEL(1);  TO T403-TEL-CEL
      *
               MOVE T403-NUM-CLTE      TO WSVA-NUMCTE
               PERFORM VALIDA-RELACION-DATOS
           END-IF.
      *
      *    EXEC SQL
      *      SELECT T403_NUM_BIN    ;
      *             T403_NUM_CRD    ;
      *             T403_NUM_CLTE   ;
      *             T403_NUM_CTA    ;
      *             T403_TEL_CEL
      *       INTO :T403-NUM-BIN    ;
      *            :T403-NUM-CRD    ;
      *            :T403-NUM-CLTE   ;
      *            :T403-NUM-CTA    ;
      *            :T403-TEL-CEL
      *       FROM MCDT403 with (nolock); 
      *      WHERE T403_BDMID    = :T403-BDMID
      *    END-EXEC
      *
      *    MOVE SQLCODE TO SQL-VALUES
      *
      *    EVALUATE TRUE
      *    WHEN SQL-88-OK
      *         MOVE T403-NUM-CLTE     TO WSVA-NUMCTE
      *         PERFORM VALIDA-RELACION-DATOS
      *    WHEN SQL-88-NOT-FOUND
      *         MOVE 'MCE0007'         TO CAA-SW-ERRCOD
      *         MOVE 'BDMID MCDT403'   TO CAA-ERR-VARIA1
      *         MOVE SQLCODE           TO CAA-ERR-VARIA2
      *         PERFORM 30000-FIN
      *    WHEN OTHER
      *         MOVE 'MPE0966'         TO CAA-SW-ERRCOD
      *         MOVE 'BDMIDSL/MCDT403' TO CAA-ERR-VARIA1
      *         MOVE SQLCODE           TO CAA-ERR-VARIA2
      *         PERFORM 30000-FIN
      *    END-EVALUATE
      *    .
      * DECLARE DECLARE @BAZ037-F
      *
      ******************************************************************
      *.PN VALIDA-RELACION-DATOS.                                      *
      ******************************************************************
       VALIDA-RELACION-DATOS.
      *
           MOVE T403-NUM-BIN        TO WSE-BIN
           MOVE T403-NUM-CRD        TO WSE-CRD
           MOVE T403-NUM-CLTE       TO WSE-NUMCUS
           MOVE T403-NUM-CTA        TO WSE-CTA-20
           IF WSS-TARJETA OR WSS-RET-TRJ
              IF E009-NUMTARJ(1:16);  = WSE-TARJETA(1:16); 
                 CONTINUE
              ELSE
                 PERFORM 24001-CONSULTA-MCDT114
                 MOVE T114-ENT-CON  TO COD-ENTITY   OF DCLPEDV0080
                 MOVE T114-BRN-CON  TO BRN-OPEN     OF DCLPEDV0080
                 MOVE T114-TYP-CON  TO COD-PRODSERV OF DCLPEDV0080
                 MOVE T114-NUM-CON  TO NUM-ACCOUNT  OF DCLPEDV0080
                 MOVE 'T'           TO KEY-PARTIC   OF DCLPEDV0080
                 MOVE '01'          TO PARTSEQ      OF DCLPEDV0080
                 PERFORM QUERY-PEDT008
                 MOVE SQLCODE       TO SQL-VALUES
                 IF SQL-88-OK
                    IF WSE-NUMCUS = AUX-NUMCUS8
                       CONTINUE
                    ELSE
                       MOVE 'TARJ NO CORRESPONDE ' TO CAA-ERR-VARIA1
                       MOVE 'CON REGISTRO BDMID. ' TO CAA-ERR-VARIA2
                       MOVE 'MCE5230'              TO CAA-COD-ERROR
                       PERFORM 30000-FIN
                    END-IF
                 ELSE
                    MOVE 'MPE0966'         TO CAA-SW-ERRCOD
                    MOVE 'BDMIDTR/PEDT008' TO CAA-ERR-VARIA1
                    MOVE SQLCODE           TO CAA-ERR-VARIA2
                    PERFORM 30000-FIN
                 END-IF
              END-IF
           END-IF
      * DECLARE DECLARE @BAZ070-I
           IF WSS-CUENTA OR WSS-RET-CTA  OR WSS-CUENTA-710
      * DECLARE DECLARE @BAZ070-F
              IF (E009-NUMCUEN(1:04);  = WSE-CENACC AND
                  E009-NUMCUEN(5:10);  = WSE-CTA); 
                  CONTINUE
              ELSE
                 MOVE CAA-ENT-ACC       TO COD-ENTITY   OF DCLPEDV0080
                 MOVE E009-NUMCUEN(1:4);  TO BRN-OPEN     OF DCLPEDV0080
                 MOVE E009-NUMCUEN(5:2);  TO COD-PRODSERV OF DCLPEDV0080
                 MOVE E009-NUMCUEN(7:8);  TO NUM-ACCOUNT  OF DCLPEDV0080
                 MOVE 'T'               TO KEY-PARTIC   OF DCLPEDV0080
                 MOVE '01'              TO PARTSEQ      OF DCLPEDV0080
      * DECLARE DECLARE @BAZ037-I
                IF TB-NUM-CUS(1);             EQUAL SPACES OR
                   TB-NUM-CUS(1);             EQUAL LOW-VALUES
      *
                   PERFORM  OBTEN-CTE-GKIDS
                   IF WSS-CTA-SI
                       CONTINUE
                   ELSE
                       MOVE 'MPE0966'         TO CAA-SW-ERRCOD
                       MOVE 'BDMIDCT/PEDT008' TO CAA-ERR-VARIA1
                       MOVE SQLCODE           TO CAA-ERR-VARIA2
                       PERFORM 30000-FIN
                   END-IF
                ELSE
                   MOVE TB-NUM-CUS(1);        TO AUX-NUMCUS8

                   IF WSE-NUMCUS = AUX-NUMCUS8
                      CONTINUE
                   ELSE
                      PERFORM  OBTEN-CTE-GKIDS
                      IF WSS-CTA-SI
                           CONTINUE
                      ELSE
      * DECLARE DECLARE @BAZ055-INI
                        IF SQL-88-NOT-FOUND
                           MOVE 'NO EXISTE CLIENTE   ' TO CAA-ERR-VARIA1
                           MOVE 'GK/PEDT008'           TO CAA-ERR-VARIA2
                           MOVE 'MCE5230'              TO CAA-COD-ERROR
                           PERFORM 30000-FIN
                        ELSE
                          IF SQL-88-SEVERAL
                           MOVE 'CLIENTE DUPLICADO   ' TO CAA-ERR-VARIA1
                           MOVE 'GK/PEDT008'           TO CAA-ERR-VARIA2
                           MOVE 'MCE5230'              TO CAA-COD-ERROR
                           PERFORM 30000-FIN
                          END-IF
                        END-IF
      * DECLARE DECLARE @BAZ055-FIN
                           MOVE 'CTA  NO CORRESPONDE ' TO CAA-ERR-VARIA1
                           MOVE 'CON REGISTRO BDMID. ' TO CAA-ERR-VARIA2
                           MOVE 'MCE5230'              TO CAA-COD-ERROR
                           PERFORM 30000-FIN
                      END-IF
                   END-IF
                END-IF

      *          PERFORM QUERY-PEDT008
      *          MOVE SQLCODE       TO SQL-VALUES
      *          IF SQL-88-OK
      *             IF WSE-NUMCUS = AUX-NUMCUS8
      *                CONTINUE
      *             ELSE
      * DECLARE DECLARE @BAZ029-INI
      *                PERFORM  OBTEN-CTE-GKIDS
      *                IF WSS-CTA-SI
      *                 CONTINUE
      *                ELSE
      * DECLARE DECLARE @BAZ029-FIN
      *                MOVE 'CTA  NO CORRESPONDE ' TO CAA-ERR-VARIA1
      *                MOVE 'CON REGISTRO BDMID. ' TO CAA-ERR-VARIA2
      *                MOVE 'MCE5230'              TO CAA-COD-ERROR
      *                PERFORM 30000-FIN
      * DECLARE DECLARE @BAZ029-FIN
      *                END-IF
      * DECLARE DECLARE @BAZ029-INI
      *             END-IF
      *          ELSE
      * DECLARE DECLARE @BAZ029-INI
      *           PERFORM  OBTEN-CTE-GKIDS
      *           IF WSS-CTA-SI
      *              CONTINUE
      *           ELSE
      * DECLARE DECLARE @BAZ029-FIN
      *
      *             MOVE 'MPE0966'         TO CAA-SW-ERRCOD
      *             MOVE 'BDMIDCT/PEDT008' TO CAA-ERR-VARIA1
      *             MOVE SQLCODE           TO CAA-ERR-VARIA2
      *             PERFORM 30000-FIN
      * DECLARE DECLARE @BAZ029-INI
      *           END-IF
      * DECLARE DECLARE @BAZ029-FIN
      *          END-IF
      * DECLARE DECLARE @BAZ037-F
              END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CODU05.                                           *
      ******************************************************************
       ARMA-DESC-CODU05.
      *
           INITIALIZE S209-CONCEPT
      *
           MOVE 'Dep�sito con tarjeta'     TO S209-CONCEPT
      *
           IF WSV-AUX-DESC(15:16);  = SPACES
              MOVE SPACES TO VA-DESC-OPE
           ELSE
             MOVE WSV-AUX-DESC(15:16);  TO VA-DESC-OPE
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESCR-DEPREST.                                         *
      ******************************************************************
       ARMA-DESCR-DEPREST.
      *
           IF VA-COD-MOV = 'G10'
              INITIALIZE S209-CONCEPT
              MOVE 'Dep�sito cr�dito personal' TO S209-CONCEPT
           ELSE
      * DECLARE DECLARE @BAZ007I.I
            IF VA-COD-MOV = 'S79'
              INITIALIZE S209-CONCEPT
              MOVE 'Dep�sito por Pr�stamo Personal' TO S209-CONCEPT
              MOVE 'Pedido:'               TO VA-DESC-OPE(1:7); 
              MOVE WSV-AUX-DESC(7:14);       TO VA-DESC-OPE(8:14); 
            ELSE
      * DECLARE DECLARE @BAZ007I.F
              MOVE 'NUMERO DE PEDIDO'      TO VA-DESC-OPE
            END-IF
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-OPERQR3.                                          *
      ******************************************************************
       ARMA-DESC-OPERQR3.
      *
           INITIALIZE S209-CONCEPT
      *
           MOVE 'Compra en Elektra con C�digo Azteca' TO S209-CONCEPT
           IF SW-T15
              MOVE T606-DESCRIPTION        TO VA-DESC-OPE
           END-IF
           .
      * DECLARE DECLARE @BAZ005.F
      * DECLARE DECLARE @BAZ007.I********************************************************
      *.PN ARMA-DESC-COD417.                                           *
      ******************************************************************
       ARMA-DESC-COD417.
      *
           INITIALIZE S209-CONCEPT
      *
           MOVE 'Deposito'                 TO S209-CONCEPT
           MOVE WSV-AUX-DESC               TO VA-DESC-OPE
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-PAGOCRE.                                          *
      ******************************************************************
       ARMA-DESC-PAGOCRE.
      *
           INITIALIZE S209-CONCEPT
      *
           MOVE 'Pago a Cr�dito'           TO S209-CONCEPT
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-CREPREST.                                         *
      ******************************************************************
       ARMA-DESC-CREPREST.
      *
           INITIALIZE S209-CONCEPT
      *
           MOVE 'Cr�dito por Pr�stamo Personal' TO S209-CONCEPT
      * DECLARE DECLARE @BAZ007I.I
           IF VA-COD-MOV = 'P48'
             INITIALIZE S209-CONCEPT
             MOVE 'Dep�sito por Pr�stamo Personal' TO S209-CONCEPT
             MOVE 'Pedido:'               TO VA-DESC-OPE(1:7); 
             MOVE WSV-AUX-DESC(7:14);       TO VA-DESC-OPE(8:14); 
           END-IF
      * DECLARE DECLARE @BAZ007I.F
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-COD106.                                           *
      ******************************************************************
       ARMA-DESC-COD106.
      *
           INITIALIZE S209-CONCEPT
      *
           MOVE 'Retiro en Cajero'         TO S209-CONCEPT
           MOVE WSV-AUX-DESC(1:22);          TO VA-DESC-OPE
           .
      * DECLARE DECLARE @BAZ007.F
      * DECLARE DECLARE @BAZ007G.I*******************************************************
      *.PN ARMA-DESC-LIQINT.                                           *
      ******************************************************************
       ARMA-DESC-LIQINT.
      *
           INITIALIZE S209-CONCEPT
      *
      *    MOVE 'Abono de Intereses del periodo' TO S209-CONCEPT
           MOVE 'Abono de intereses del periodo (Gravable); '
                                                 TO S209-CONCEPT
           .
      * DECLARE DECLARE @BAZ007G.F
      * DECLARE DECLARE @BAZ007H.I*******************************************************
      *.PN ARMA-DESC-INTISR.                                           *
      ******************************************************************
       ARMA-DESC-INTISR.
      *
           INITIALIZE S209-CONCEPT
      * DECLARE DECLARE @BAZ005D.I
      *    MOVE 'Abono de intereses del periodo (Excento); '
      *                                          TO S209-CONCEPT
      *    MOVE 'Abono de intereses del periodo (Excentos de ISR); '
      *LCR-INI3
           MOVE 'Abono de intereses del periodo (Exentos de ISR); '
      *LCR-FIN
                                                 TO S209-CONCEPT
      * DECLARE DECLARE @BAZ005D.F
           .
      * DECLARE DECLARE @BAZ007H.F
      * DECLARE DECLARE @BAZ007J.I*******************************************************
      *.PN ARMA-DESC-COD548.                                           *
      ******************************************************************
       ARMA-DESC-COD548.
      *
           INITIALIZE S209-CONCEPT
                      DCLDMDT001
                      DCLCGDT002
      *
           MOVE 'Pago a terceros'    TO S209-CONCEPT
      * DECLARE DECLARE @BAZ005D.I
      *    MOVE CAA-ENT-ACC          TO T002-ENTITY
      *    MOVE 'DM'                 TO T002-APPLICATION
      *    MOVE 'IN'                 TO T002-CHANNEL
      *    MOVE AUX-INTREF71(1:9);     TO T002-SEQUENCE
      *
      *    EXEC SQL
      *      SELECT T002_TRANSMITTER
      *       INTO :T002-TRANSMITTER
      *       FROM CGDT002 with(nolock); 
      *      WHERE T002_ENTITY       = :T002-ENTITY
      *        AND T002_APPLICATION  = :T002-APPLICATION
      *        AND T002_CHANNEL      = :T002-CHANNEL
      *        AND T002_SEQUENCE     = :T002-SEQUENCE
      *    END-EXEC.
      *
      *    MOVE SQLCODE              TO SQL-VALUES
      *    EVALUATE TRUE
      *    WHEN SQL-88-OK
      *         MOVE T002-TRANSMITTER TO T001-NUM-E
      *         MOVE CAA-ENT-ACC      TO T001-ENT
      *         EXEC SQL
      *           SELECT T001_NAME_E
      *            INTO :T001-NAME-E
      *            FROM DMDT001 with(nolock); 
      *           WHERE T001_ENT   = :T001-ENT
      *             AND T001_NUM_E = :T001-NUM-E
      *         END-EXEC
      *         MOVE SQLCODE          TO SQL-VALUES
      *         IF SQL-88-OK
      *            MOVE 'De: '        TO VA-DESC-OPE(1:4); 
      *            MOVE T001-NAME-E   TO VA-DESC-OPE(5:50); 
      *         ELSE
      *            MOVE SPACES        TO VA-DESC-OPE
      *         END-IF
      *    WHEN OTHER
      *         MOVE SPACES           TO VA-DESC-OPE
      *    END-EVALUATE
      * DECLARE DECLARE @BAZ007J.F
           MOVE AUX-INTREF71(4:5);      TO T001-NUM-E
           MOVE CAA-ENT-ACC           TO T001-ENT
           EXEC SQL
             SELECT T001_NAME_E
              INTO :T001-NAME-E
              FROM DMDT001 with(nolock); 
             WHERE T001_ENT   = :T001-ENT
               AND T001_NUM_E = :T001-NUM-E
           END-EXEC
           MOVE SQLCODE               TO SQL-VALUES
           IF SQL-88-OK
              MOVE 'De: '             TO VA-DESC-OPE(1:4); 
              MOVE T001-NAME-E        TO VA-DESC-OPE(5:50); 
           ELSE
              MOVE SPACES             TO VA-DESC-OPE
           END-IF
           .
      * DECLARE DECLARE @BAZ005D.F
      * DECLARE DECLARE @BAZ007K.I*******************************************************
      *.PN ARMA-DESC-DEVSPEI.                                          *
      * lin1:  Leyenda devoluci�n                                      *
      * lin2:  motivo                                                  *
      ******************************************************************
       ARMA-DESC-DEVSPEI.
      *
           INITIALIZE FEVC0040    AUX-DEVSPEI
           INITIALIZE S209-CONCEPT   TCGT010  VA-DATOS-TC
      *
           MOVE 'Devoluci�n de Transferencia SPEI' TO S209-CONCEPT
           MOVE CAA-ENT-ACC          TO V0040-ENT-ORIGIN OF FEVC0040
           MOVE WSV-AUX-FECHA        TO V0040-DAT-OPERATION OF FEVC0040
           MOVE WSV-AUX-DESC(8:19);    TO V0040-ADR        OF FEVC0040
      *
           EXEC SQL
              SELECT TOP 1
                     T004_DES_ASSIGNER ;
                     T004_DES_RECVENT  ;
                     T004_DES_BEN      ;
                     T004_FLG_OPEST    ;
                     T004_ITEM1        ;
      * DECLARE DECLARE @BAZ027-I
      *              T004_COD_RETURN
                     T004_COD_RETURN   ;
                     T004_INFTOENT3
      * DECLARE DECLARE @BAZ027-I
              INTO  :FEVC0040.V0040-DES-ASSIGNER ;
                    :FEVC0040.V0040-DES-RECVENT  ;
                    :FEVC0040.V0040-DES-BEN      ;
                    :FEVC0040.V0040-FLG-OPEST    ;
                    :FEVC0040.V0040-ITEM1        ;
      * DECLARE DECLARE @BAZ027-I
      *             :FEVC0040.V0040-COD-RETURN
                    :FEVC0040.V0040-COD-RETURN   ;
                    :FEVC0040.V0040-INFTOENT3
      * DECLARE DECLARE @BAZ027-F
                FROM FEDT004 with(nolock); 
                WHERE T004_ENT_ORIGIN    = :FEVC0040.V0040-ENT-ORIGIN
                 AND T004_DAT_OPERATION >= :FEVC0040.V0040-DAT-OPERATION
                 AND T004_ADR            = :FEVC0040.V0040-ADR
           END-EXEC
      *
           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
           WHEN SQL-88-OK
                IF (V0040-FLG-OPEST = '6' OR 'E' OR 'D'); 
                  MOVE V0040-COD-RETURN OF FEVC0040 TO AUX-CLAV-DEVSPEI
      * DECLARE DECLARE @BAZ010.I
      *           EXEC SQL
      *              SELECT DESCRIPCION
      *               INTO :AUX-MOTIV-DEVSPEI
      *              FROM SPEI.SPEI.SPEI_CAUSASDEVOLUCIONES with(nolock); 
      *              WHERE CLAVEDEV = :AUX-CLAV-DEVSPEI
      *           END-EXEC
                  MOVE '6442'             TO VA-COD-TC
                  MOVE AUX-CLAV-DEVSPEI   TO VA-KEY-TC
      * DECLARE DECLARE @BAZ027-I
                  IF AUX-CLAV-DEVSPEI(01:02);  < '10'
      *
                     MOVE SPACES                   TO VA-KEY-TC
                     MOVE AUX-CLAV-DEVSPEI(02:01);   TO VA-KEY-TC
      *
                  END-IF
      * DECLARE DECLARE @BAZ027-F
                  PERFORM QUERY-TCDT010-LIB
                  MOVE SQLCODE TO SQL-VALUES
                  IF SQL-88-OK
      *              MOVE AUX-MOTIV-DEVSPEI     TO VA-DESC-OPE
                     MOVE T010-DTA-TBLKEY(1:50);  TO VA-DESC-OPE
      * DECLARE DECLARE @BAZ010.F
                  ELSE
                     MOVE SPACES TO VA-DESC-OPE
                  END-IF
                ELSE
                  MOVE SPACES TO VA-DESC-OPE
                END-IF
           WHEN OTHER
                MOVE SPACES TO VA-DESC-OPE
           END-EVALUATE
           .
      * DECLARE DECLARE @BAZ007K.F
      * DECLARE DECLARE @BAZ005E.I*******************************************************
      *.PN ARMA-DESC-U79U80.                                           *
      * CONCEPT => enviar lo contenido en T071_OBSERVATIONS            *
      ******************************************************************
       ARMA-DESC-U79U80.
      *
           INITIALIZE S209-CONCEPT
      *
      *LCR-INI4
           MOVE 'Compra Con Tarjeta' TO S209-CONCEPT
           MOVE 'Compra'             TO VA-DESC-OPE(1:6); 
           MOVE WSV-AUX-DESC(1:30);    TO VA-DESC-OPE(8:30); 
      *    MOVE WSV-AUX-DESC(1:30);    TO S209-CONCEPT
      *    MOVE SPACES               TO VA-DESC-OPE
      *LCR-FIN
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-U50.                                              *
      * conceptoOperacion = CONCEPT => leyenda de T071_OBSERVATIONS    *
      * descripcion = DESCM01       => concepto de T071_OBSERVATIONS   *
      * descripcionOperacion = DESCOPE => BLDT002 � leyenda PeticionUser
      ******************************************************************
       ARMA-DESC-U50.
      *
           IF WSV-AUX-DESC(1:30);  = 'BONIFICACION 5% COMPRA ELEKTRA'
              INITIALIZE S209-CONCEPT
              MOVE WSV-AUX-DESC(1:30);                  TO S209-CONCEPT
              MOVE 'PROMOCION BUEN FIN BANCA DIGITAL' TO VA-DESC-OPE
           ELSE
              MOVE S209-CONCEPT                       TO VA-DESC-OPE
              INITIALIZE S209-CONCEPT
              MOVE WSV-AUX-DESC(1:30);                  TO S209-CONCEPT
           END-IF
           .
      *
      ******************************************************************
      *.PN ARMA-DESC-U36.               Cancela pedido por QR3.Trx.MB61*
      ******************************************************************
       ARMA-DESC-U36.
      *
           INITIALIZE S209-CONCEPT
      *
           MOVE 'Dep�sito por Cancelaci�n' TO S209-CONCEPT
           .
      * DECLARE DECLARE @BAZ005E.F
      ******************************************************************
      *                        30000-FIN                               *
      ******************************************************************
       30000-FIN.
      *
           EXEC CICS
              RETURN
           END-EXEC.
      *
      ******************************************************************
      * 88888-WRITE.                                                   *
      ******************************************************************
       88888-WRITE.
      *
           MOVE LENGTH OF MBNS0009      TO VN-TSLTH2
           MOVE SPACES                  TO VA-TSCNT2
           MOVE MBNS0009                TO VA-FMTCNT2
           MOVE 'MBNS0009'              TO VA-DES-FORMAT2
           ADD +8                       TO VN-TSLTH2
           PERFORM REMPLA-HEX-W1

           EXEC CICS
                WRITEQ TS QUEUE(VA-TS2); 
                FROM(VA-TSCNT2); 
                LENGTH(VN-TSLTH2); 
                MAIN NOHANDLE
           END-EXEC
      *
           INITIALIZE EIBRESP
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL); 
              INITIALIZE QGECABC
              MOVE CA-WRITEQQUEUE       TO ABC-REFERENCIA
              PERFORM 999999-CICS-ABEND
           END-IF
      *
           MOVE '+DC1'                  TO CAA-TB-DES1(1); 
           MOVE CAA-SW-LNG-TERM         TO CAA-TB-LNG(1); 
           MOVE 'P'                     TO CAA-TB-SCRDOCU(1); .
      *
      ******************************************************************
      *   ESCRIBIR COPY 2
      ******************************************************************
       77777-WRITE.
      *
           MOVE LENGTH OF MBNS2009      TO VN-TSLTH2
           MOVE SPACES                  TO VA-TSCNT2
           MOVE MBNS2009                TO VA-FMTCNT2
           MOVE 'MBNS2009'              TO VA-DES-FORMAT2
           ADD +8                       TO VN-TSLTH2
           PERFORM REMPLA-HEX-W1

           EXEC CICS
                WRITEQ TS QUEUE(VA-TS2); 
                FROM(VA-TSCNT2); 
                LENGTH(VN-TSLTH2); 
                MAIN NOHANDLE
           END-EXEC
      *
           INITIALIZE EIBRESP
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL); 
              INITIALIZE QGECABC
              MOVE CA-WRITEQQUEUE       TO ABC-REFERENCIA
              PERFORM 999999-CICS-ABEND
           END-IF
      *
           MOVE '+DC1'                  TO CAA-TB-DES1(1); 
           MOVE CAA-SW-LNG-TERM         TO CAA-TB-LNG(1); 
           MOVE 'P'                     TO CAA-TB-SCRDOCU(1); .
      *LCR-INI2
      ******************************************************************
      *   ESCRIBIR COPY 3
      ******************************************************************
       66666-WRITE.
      *
           MOVE LENGTH OF MBNS3009      TO VN-TSLTH2
           MOVE SPACES                  TO VA-TSCNT2
           MOVE MBNS3009                TO VA-FMTCNT2
           MOVE 'MBNS3009'              TO VA-DES-FORMAT2
           ADD +8                       TO VN-TSLTH2
           PERFORM REMPLA-HEX-W1

           EXEC CICS
                WRITEQ TS QUEUE(VA-TS2); 
                FROM(VA-TSCNT2); 
                LENGTH(VN-TSLTH2); 
                MAIN NOHANDLE
           END-EXEC
      *
           INITIALIZE EIBRESP
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL); 
              INITIALIZE QGECABC
              MOVE CA-WRITEQQUEUE       TO ABC-REFERENCIA
              PERFORM 999999-CICS-ABEND
           END-IF
      *
           MOVE '+DC1'                  TO CAA-TB-DES1(1); 
           MOVE CAA-SW-LNG-TERM         TO CAA-TB-LNG(1); 
           MOVE 'P'                     TO CAA-TB-SCRDOCU(1); 
           .
      *LCR-FIN2
      * DECLARE DECLARE @BAZ023.I
      ******************************************************************
      *   ESCRIBIR SALIDA 4                                            *
      ******************************************************************
       66667-WRITE-SALIDA4.
      *
           MOVE LENGTH OF MBNS4009      TO VN-TSLTH2
           MOVE SPACES                  TO VA-TSCNT2
           MOVE MBNS4009                TO VA-FMTCNT2
           MOVE 'MBNS4009'              TO VA-DES-FORMAT2
           ADD +8                       TO VN-TSLTH2
           PERFORM REMPLA-HEX-W1

           EXEC CICS
                WRITEQ TS QUEUE(VA-TS2); 
                FROM(VA-TSCNT2); 
                LENGTH(VN-TSLTH2); 
                MAIN NOHANDLE
           END-EXEC
      *
           INITIALIZE EIBRESP
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL); 
              INITIALIZE QGECABC
              MOVE CA-WRITEQQUEUE       TO ABC-REFERENCIA
              PERFORM 999999-CICS-ABEND
           END-IF
      *
           MOVE '+DC1'                  TO CAA-TB-DES1(1); 
           MOVE CAA-SW-LNG-TERM         TO CAA-TB-LNG(1); 
           MOVE 'P'                     TO CAA-TB-SCRDOCU(1); 
           .
      * DECLARE DECLARE @BAZ023.F
      * DECLARE DECLARE @BAZ027-I
      ******************************************************************
      * INFORMACION ESPECIFICA DE OPERACIONES WALLET                   *
      ******************************************************************
       29993-MOVER-SALIDA-5.
      *
           INITIALIZE MBNS5009
      *
           SET WSS-SEGCON-SI TO TRUE
      *
           IF AUX-AMT-COMP3 >= CA-0
              MOVE VA-CUENTA-AUX(5:4);               TO S509-CTAPARA(1:4); 
              MOVE VA-CUENTA-AUX(11:10);             TO S509-CTAPARA(5:10); 
              MOVE VA-ALIAS-AUX                    TO S509-ALIASPA
              MOVE VA-NAME-CUS-AUX                 TO VA-NOMBRE-COMPLE
              PERFORM 29995-FORMATE-NOMBRE
              MOVE VA-NOMBRE-AUX                   TO S509-NOMBPA
           ELSE
              MOVE VA-CUENTA-AUX(5:4);               TO S509-CTADE(1:4); 
              MOVE VA-CUENTA-AUX(11:10);             TO S509-CTADE(5:10); 
              MOVE VA-ALIAS-AUX                    TO S509-ALIASDE
              MOVE VA-NAME-CUS-AUX                 TO VA-NOMBRE-COMPLE
              PERFORM 29995-FORMATE-NOMBRE
              MOVE VA-NOMBRE-AUX                   TO S509-NOMBDE
           END-IF
      *
           EVALUATE TRUE
               WHEN SW-T60
      *             CARGO A CUENTA TOMIIN POR PAGO CON QR A TOMIIN
                    IF AUX-INTREF71(12:04);  = CA-MBW4
      *
                       MOVE CA-TOMIIN                TO S509-BANCOPA
                       MOVE AUX-DESC(11:04);           TO T036-ENT
                       MOVE AUX-DESC(15:04);           TO T036-CEN-REG
                                                     S509-CTAPARA(1:4); 
                       MOVE AUX-DESC(21:10);           TO T036-ACC
                                                      S509-CTAPARA(5:10); 
                       PERFORM VALIDA-BDMID-MBDT036
      *
                       MOVE T036-NAME-CUS            TO VA-NOMBRE-COMPLE
                       PERFORM 29995-FORMATE-NOMBRE
                       MOVE VA-NOMBRE-AUX            TO S509-NOMBPA
                       MOVE T036-ALIAS               TO S509-ALIASPA
                    END-IF
      *             CARGO A TOMIIN POR PAGO CON QR A BANCO AZTECA
                    IF AUX-INTREF71(12:04);  = CA-MBW5
                       PERFORM 30000-RECUPERA-BENEFICI
                       MOVE CA-BAZ                   TO S509-BANCOPA
                       MOVE AUX1-CTABEN(5:4);         TO S509-CTAPARA(1:4); 
                       MOVE AUX1-CTABEN(11:10);      TO S509-CTAPARA(5:10); 
                       MOVE AUX2-NOMBRECTE           TO S509-NOMBPA
                       MOVE SPACES                   TO S509-ALIASPA
                    END-IF
      *
               WHEN SW-T59
      *             ENVIO COBRO A TOMIIN CON QR TOMIIN
                    IF AUX-INTREF71(12:04);  = CA-MBW4
                       MOVE CA-TOMIIN                TO S509-BANCOPA
      *
                       MOVE AUX-DESC(11:04);           TO T036-ENT
                       MOVE AUX-DESC(15:04);           TO T036-CEN-REG
                                                        S509-CTADE(1:4); 
                       MOVE AUX-DESC(21:10);           TO T036-ACC
                                                        S509-CTADE(5:10); 
                       PERFORM VALIDA-BDMID-MBDT036
      *
                       MOVE T036-NAME-CUS            TO VA-NOMBRE-COMPLE
                       PERFORM 29995-FORMATE-NOMBRE
                       MOVE VA-NOMBRE-AUX            TO S509-NOMBDE
                       MOVE T036-ALIAS               TO S509-ALIASDE
                    END-IF

      *             COBRO A CTA BAZ CON QR TOMIIN
                    IF AUX-INTREF71(12:04);  = CA-MB03
                        MOVE CA-TOMIIN               TO S509-BANCOPA
                        PERFORM 30000-RECUPERA-BENEFICI
                        MOVE AUX1-CTABEN(5:4);         TO S509-CTADE(1:4); 
                        MOVE AUX1-CTABEN(11:10);       TO S509-CTADE(5:10); 
                        MOVE AUX2-NOMBRECTE          TO S509-NOMBDE
                        MOVE CA-BAZ                  TO S509-ALIASDE
                    END-IF
               WHEN SW-212
      *             ENVIO DE TOMIIN A OTROS BANCOS(SPEI); 
                    IF AUX-INTREF71(12:04);  = CA-F648
      * DECLARE DECLARE @BAZ034-I
                       PERFORM 29996-FORMATE-NOMBRE-SPEI
                       MOVE VA-NOMBRE-AUX            TO S509-NOMBPA
      * DECLARE DECLARE @BAZ034-F
                       MOVE SPACES                   TO S509-ALIASPA
                       MOVE V0040-DES-RECVENT        TO S509-BANCOPA
      *
                       IF V0040-INFTOENT3(1:2);  = '10'
                          MOVE V0040-INFTOENT3(13:10); 
                                                     TO S509-CTAPARA

                       ELSE
                          IF V0040-INFTOENT3(1:2);  = '03'
                             MOVE V0040-INFTOENT3(7:16); 
                                                     TO S509-CTAPARA

                          ELSE
                             IF V0040-INFTOENT3(1:2);  = '40'
                                MOVE V0040-INFTOENT3(5:18); 
                                                     TO S509-CTAPARA
                             END-IF
                          END-IF
                       END-IF
                    END-IF
               WHEN SW-213
      *             RECEPCION TOMIIN POR ENVIO DE OTROS BANCOS(SPEI); 
                    IF AUX-INTREF71(12:04);  = CA-0500
      *
                       MOVE CA-TOMIIN                TO S509-BANCOPA
      *
                       MOVE SPACES                   TO S509-ALIASDE
                       MOVE T607-DESCRIPTION(39:22);   TO S509-CTADE
                       MOVE T607-DESCRIPTION(72:22);   TO S509-NOMBDE
                       MOVE T607-DESCRIPTION(166:15);  TO S509-REFEOPE
                    END-IF
               WHEN SW-DEV-SPEI
      *             DECOLUVION DE TRANFERENCIA SPEI BCO NO DISPONIBLE
                    IF AUX-INTREF71(12:04);  = CA-0600 OR
                       AUX-INTREF71(12:04);  = CA-0700
      *
                       MOVE CA-TOMIIN                TO S509-BANCOPA

                       MOVE SPACES                   TO S509-ALIASDE
      *
                       IF S209-IDOPER = CA-217
                          MOVE SPACES                TO S509-NOMBDE
                                                        S509-CTADE
                       ELSE
      * DECLARE DECLARE @BAZ034-I
                          MOVE V0040-DES-BEN         TO VA-BENEFIC
                          PERFORM 29996-FORMATE-NOMBRE-SPEI
                          MOVE VA-NOMBRE-AUX         TO S509-NOMBDE
      * DECLARE DECLARE @BAZ034-F
                          IF V0040-INFTOENT3(1:2);  = CA-10
                             MOVE V0040-INFTOENT3(13:10); 
                                                  TO S509-CTADE

                          ELSE

                             IF V0040-INFTOENT3(1:2);  = CA-03
                                MOVE V0040-INFTOENT3(7:16); 
                                                  TO S509-CTADE
                             ELSE
                               IF V0040-INFTOENT3(1:2);  = CA-40
                                  MOVE V0040-INFTOENT3(5:18); 
                                                  TO S509-CTADE
                               END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-IF
               WHEN SW-160
      *             ENVIO CTA DE TOMIIN A CTA TOMMIN
                    IF AUX-INTREF71(12:04);  = CA-MBW4
                       MOVE CA-TOMIIN                TO S509-BANCOPA
      *
                       MOVE AUX-DESC(10:10);           TO T036-NUM-CEL
                       PERFORM 29994-RECUPERA-INF-RET
                       MOVE T036-CEN-REG             TO S509-CTADE(1:4); 
                       MOVE T036-ACC                 TO S509-CTADE(5:10); 
      *
                       MOVE T036-NAME-CUS            TO VA-NOMBRE-COMPLE
                       PERFORM 29995-FORMATE-NOMBRE
                       MOVE VA-NOMBRE-AUX            TO S509-NOMBDE
                       MOVE T036-ALIAS               TO S509-ALIASDE
      *
                    END-IF
      *             ENVIO CTA BAZ A TOMIIN
                    IF AUX-INTREF71(12:04);  = CA-MB03
                       MOVE CA-TOMIIN                TO S509-BANCOPA
                       PERFORM 30000-RECUPERA-BENEFICI
                       MOVE AUX2-NOMBRECTE           TO S509-NOMBDE
                       MOVE AUX1-CTABEN(5:4);          TO S509-CTADE(1:4); 
                       MOVE AUX1-CTABEN(11:10);        TO S509-CTADE(5:10); 
                       MOVE CA-BAZ                   TO S509-ALIASDE
                    END-IF
      *             RECEPCION DE LIBERACION RETENCION TOMIIN A TOMIIN
                    IF AUX-INTREF71(12:04);  =  SPACES AND
                       AUX-USERUPD = CA-MB4C0100
      *
                       MOVE CA-TOMIIN                TO S509-BANCOPA
      *
                       MOVE AUX-DESC(10:10);           TO T036-NUM-CEL
                       PERFORM 29994-RECUPERA-INF-RET
                       MOVE T036-CEN-REG             TO S509-CTADE(1:4); 
                       MOVE T036-ACC                 TO S509-CTADE(5:10); 
      *
                       MOVE T036-NAME-CUS            TO VA-NOMBRE-COMPLE
                       PERFORM 29995-FORMATE-NOMBRE
                       MOVE VA-NOMBRE-AUX            TO S509-NOMBDE

                       MOVE T036-ALIAS               TO S509-ALIASDE
                    END-IF
      * DECLARE DECLARE @BAZ041-I
      *             RECEPCION POR TRANSAFERENCIA CTAS PROPIAS
                    IF AUX-INTREF71(12:04);  = CA-B520
      *
                       IF BGNC477-ACC-DEB(5:4);  = CA-7760
                          MOVE BGNC477-ACC-DEB(1:4); 
                                                    TO T036-ENT
                          MOVE BGNC477-ACC-DEB(5:4); 
                                                    TO T036-CEN-REG
                          MOVE BGNC477-ACC-DEB(11:10); 
                                                    TO T036-ACC
      *
                          PERFORM VALIDA-BDMID-MBDT036
      *
                          MOVE T036-ALIAS           TO S509-ALIASDE

                       END-IF
      *
                       MOVE CA-TOMIIN               TO S509-BANCOPA
                       MOVE AUX-NOM-CTE             TO S509-NOMBDE
                       MOVE BGNC477-ACC-DEB(5:4);     TO S509-CTADE(1:4); 
                       MOVE BGNC477-ACC-DEB(11:10);   TO S509-CTADE(5:10); 
                    END-IF
      * DECLARE DECLARE @BAZ041-F
               WHEN SW-169
      *             CARGO A CTA TOMIIN DE ENVIO A CTA TOMIIN
                    IF AUX-INTREF71(12:04);  = CA-MBW4
      *
                       MOVE CA-TOMIIN                TO S509-BANCOPA
      *
                       MOVE AUX-DESC(09:10);           TO T036-NUM-CEL
                       PERFORM 29994-RECUPERA-INF-RET
                       MOVE T036-CEN-REG          TO S509-CTAPARA(1:4); 
                       MOVE T036-ACC              TO S509-CTAPARA(5:10); 
      *
                       MOVE T036-NAME-CUS            TO VA-NOMBRE-COMPLE
                       PERFORM 29995-FORMATE-NOMBRE
                       MOVE VA-NOMBRE-AUX            TO S509-NOMBPA
      *
                       MOVE T036-ALIAS               TO S509-ALIASPA
      *
                    END-IF
      *             CARGO A CTA TOMIIN DE ENVIO A BAZ
                    IF AUX-INTREF71(12:04);  = CA-MBW5
                       PERFORM 30000-RECUPERA-BENEFICI
                       MOVE AUX2-NOMBRECTE           TO S509-NOMBPA
                       MOVE CA-BAZ                   TO S509-BANCOPA
                       MOVE AUX1-CTABEN(5:4);         TO S509-CTAPARA(1:4); 
                       MOVE AUX1-CTABEN(11:10);      TO S509-CTAPARA(5:10); 
                       MOVE SPACES                   TO S509-ALIASPA
                    END-IF
      *             CARGO POR LIBERACION DE RETENCION TOMIIN A TOMIIN
                    IF AUX-INTREF71(12:04);  =  SPACES AND
                       AUX-USERUPD = CA-MB4C0100
      *
                       MOVE CA-TOMIIN                TO S509-BANCOPA
      *
                       MOVE AUX-DESC(09:10);           TO T036-NUM-CEL
                       PERFORM 29994-RECUPERA-INF-RET
                       MOVE T036-CEN-REG            TO S509-CTAPARA(1:4); 
                       MOVE T036-ACC              TO S509-CTAPARA(5:10); 
      *
                       MOVE T036-NAME-CUS            TO VA-NOMBRE-COMPLE
                       PERFORM 29995-FORMATE-NOMBRE
                       MOVE VA-NOMBRE-AUX            TO S509-NOMBPA
      *
                       MOVE T036-ALIAS               TO S509-ALIASPA
      *
                    END-IF
      * DECLARE DECLARE @BAZ041-I
      *             ENVIO POR TRANSAFERENCIA SUCURLAR CTAS PROPIAS
                    IF AUX-INTREF71(12:04);  = CA-B520
      *
                       IF BGNC477-ACC-CRED(5:4);  = CA-7760
                          MOVE CA-TOMIIN            TO S509-BANCOPA
                          MOVE BGNC477-ACC-CRED(1:4); 
                                                    TO T036-ENT
                          MOVE BGNC477-ACC-CRED(5:4); 
                                                    TO T036-CEN-REG
                          MOVE BGNC477-ACC-CRED(11:10); 
                                                    TO T036-ACC
      *
                          PERFORM VALIDA-BDMID-MBDT036
      *
                          MOVE T036-ALIAS           TO S509-ALIASPA
      *
                       ELSE
                          MOVE CA-BAZ               TO S509-BANCOPA
                       END-IF
      *
                       MOVE AUX-NOM-CTE             TO S509-NOMBPA
                       MOVE BGNC477-ACC-CRED(5:4);    TO S509-CTAPARA(1:4); 
                       MOVE BGNC477-ACC-CRED(11:10); 
                                                   TO S509-CTAPARA(5:10); 
                    END-IF
      * DECLARE DECLARE @BAZ041-F
      *
               WHEN SW-PAGO-SERV
      *             CARGO A TOMIIN POR PAGO DE SERVICIO
                    IF VA-COD-MOV = (CA-G07 OR CA-G16);  AND
                       AUX-INTREF71(12:04);  = CA-BS03
      *
                       MOVE T606-DESCRIPTION(1:20);    TO S509-ALIASPA
                       MOVE S009-DESCM01(01:20);       TO S509-REFEOPE
      *
                    END-IF
               WHEN SW-T05
      *             CARGO A CUENTA TOMIIN POR COMPRA DE TIEMPO AIRE
                    IF AUX-INTREF71(12:04);  = CA-MBW8
      *
                       MOVE S209-CONCEPT(14:10);       TO S509-CTAPARA
                       MOVE S209-CONCEPT(25:20);       TO S509-ALIASPA
      *
                    END-IF
               WHEN SW-U50
      *             INCENTIVO TOMIIN
                    IF AUX-INTREF71(12:04);  = CA-MBW9
                       MOVE CA-TOMIIN                TO S509-BANCOPA
                    END-IF
               WHEN SW-000
               WHEN SW-W50
      *             DEPOSITOS DE EFECTIVO POR CORRESPONSALES
                    IF AUX-INTREF71(12:04);  = CA-B601
                       MOVE CA-TOMIIN              TO S509-BANCOPA
                    END-IF
      * DECLARE DECLARE @BAZ044-I
               WHEN SW-Z25
               WHEN SW-Z26
      * DECLARE DECLARE @BAZ049-I
      *             DEPOSITOS Y PAGOS CON QR DAPP
      *             IF AUX-INTREF71(12:04);  = CA-MBW5
      *                UNSTRING VA-DESC-AUX DELIMITED BY '   '
      *                INTO S509-NOMBPA
      *             END-IF
                    IF AUX-INTREF71(12:04);  = CA-MBW5
                       UNSTRING VA-DESC-AUX DELIMITED BY '   '
                       INTO S509-NOMBPA
                    ELSE
                       IF AUX-INTREF71(12:04);  = CA-MBWE OR CA-BATC OR
                          T606-DESCRIPTION(1:15);  = CA-ADEUDO-COMI
                          MOVE CA-SERVICIO-TOM       TO S509-NOMBPA
                       END-IF
                    END-IF
               WHEN SW-Z51
                    IF AUX-INTREF71(12:04);  = CA-MBWE OR CA-BATC OR
                       T606-DESCRIPTION(1:14);  = CA-ADEUDO-ENTR
                       MOVE CA-ENTRADA-METRO         TO S509-NOMBPA
                    END-IF
      * DECLARE DECLARE @BAZ049-F
      * DECLARE DECLARE @BAZ044-F
               WHEN OTHER
      *             CODIGO NO CONTROLADO
                    CONTINUE
           END-EVALUATE
      *
           SET WSS-SEGCON-NO                         TO TRUE
      *
           PERFORM 66668-WRITE-SALIDA5
           .
      *
      ******************************************************************
      *FORMATE NOMBRE PARA ENVIO A SALIDA 5                            *
      ******************************************************************
       29995-FORMATE-NOMBRE.
      *
           INITIALIZE VA-NOMBRE-AUX
      *
      * DECLARE DECLARE @BAZ034-I
      *    STRING VA-NOMBRES DELIMITED BY SPACES
      *           ' ' DELIMITED BY SIZE
      *           VA-APELLIDO-PAT DELIMITED BY SPACES
      *           ' ' DELIMITED BY SIZE
      *           VA-APELLIDO-MAT DELIMITED BY SPACES
      *           INTO VA-NOMBRE-AUX
      *    END-STRING
           STRING VA-NOMBRES DELIMITED BY '  '
                  ' ' DELIMITED BY SIZE
                  VA-APELLIDO-PAT DELIMITED BY '  '
                  ' ' DELIMITED BY SIZE
                  VA-APELLIDO-MAT DELIMITED BY '  '
                  INTO VA-NOMBRE-AUX
           END-STRING
      * DECLARE DECLARE @BAZ034-F
           .
      * DECLARE DECLARE @BAZ034-I
      *
      ******************************************************************
      *FORMATE NOMBRE SPEI PARA ENVIO SALIDA 5                         *
      ******************************************************************
       29996-FORMATE-NOMBRE-SPEI.
      *
           INITIALIZE VA-NOMBRE-AUX
                      VN-SPC
      *
           INSPECT VA-BENEFIC REPLACING ALL CA-NA BY SPACES
      *
           MOVE ZEROES                           TO VN-SPC
           INSPECT VA-BENEFIC TALLYING VN-SPC FOR LEADING ' '
           MOVE VA-BENEFIC(VN-SPC + 1:50 - VN-SPC); 
                                                  TO VA-NOMBRE-AUX
           .
      * DECLARE DECLARE @BAZ034-F
      *
      ******************************************************************
      *.PN 29994-RECUPERA-INF-RET.                                     *
      ******************************************************************
       29994-RECUPERA-INF-RET.
      *
           EXEC SQL
            SELECT
                  T036_ENT
                 ;T036_CEN_REG
                 ;T036_ACC
                 ;T036_NAME_CUS
                 ;T036_ALIAS
             INTO
                  :T036-ENT
                 ;:T036-CEN-REG
                 ;:T036-ACC
                 ;:T036-NAME-CUS
                 ;:T036-ALIAS
            FROM  MBDT036 with(nolock); 
            WHERE T036_NUM_CEL = :T036-NUM-CEL
           END-EXEC
      *
           MOVE SQLCODE                    TO SQL-VALUES
      *
           EVALUATE TRUE
           WHEN SQL-88-OK
                CONTINUE
           WHEN OTHER
                MOVE SPACES                TO T036-ENT
                                              T036-CEN-REG
                                              T036-ACC
                                              T036-NAME-CUS
                                              T036-ALIAS
           END-EVALUATE
            .
      *
      ******************************************************************
      * RECUPERA DATOS DE CLIENTES DEL BENEFICIARIO
      ******************************************************************
       30000-RECUPERA-BENEFICI.
      *
           INITIALIZE BGNC477
                      AUX2-NOMBRECTE
      *
           MOVE S009-NOPEM01                     TO BGNC477-NUM-OP
           MOVE VA-CUENTA-AUX                    TO BGNC477-ACC
      *
           EXEC CICS
              LINK PROGRAM (CA-BG7C4770); 
              COMMAREA(CA-BGNC477); 
              NOHANDLE
           END-EXEC
      *
           IF EIBRESP EQUAL DFHRESP(NORMAL); 
              IF BGNC477-COD-ERR EQUAL TO SPACES
      *          -- Obtiene Nombre a quien env�o
                 IF BGNC477-ACC = BGNC477-ACC-CRED
                    MOVE BGNC477-ACC-DEB         TO AUX1-CTABEN
                 ELSE
                    MOVE BGNC477-ACC-CRED        TO AUX1-CTABEN
                 END-IF
      *
                 MOVE AUX1-CTABEN(5:4);            TO AUX2-NUMCEN
                 MOVE AUX1-CTABEN(11:10);          TO AUX2-NUMACC
                 PERFORM OBTEN-DATOS-CLIENTE
      * DECLARE DECLARE @BAZ034-I
                 EVALUATE TRUE
                      WHEN SQL-88-OK
                           STRING AUX-NAME  DELIMITED BY '   '
                                         ' ' DELIMITED BY SIZE
                                AUX-SURNAME DELIMITED BY '   '
                                         ' ' DELIMITED BY SIZE
                                AUX-SCDNAME DELIMITED BY '   '
                                            INTO AUX2-NOMBRECTE
                     WHEN OTHER
                           MOVE SPACES TO AUX2-NOMBRECTE
                 END-EVALUATE
      * DECLARE DECLARE @BAZ034-F
              END-IF
           END-IF
           .
      ******************************************************************
      *ACCESO MCDT279 PARA BUSCAR DESCRIPCION DEL BANCO SI NO FUE
      *ENCONTRADO EN EL ACCESO A LA MCDT097
      ******************************************************************
       QUERY-MCDT279.

           INITIALIZE DCLMCDT279

           MOVE AUX-BIN                TO T279-BIN-ID
      *
           EXEC SQL
              SELECT T279_TYP_CARD;
                     T279_BANK_NAME;
                     T279_BANK_ID
                INTO
                     :T279-TYP-CARD;
                     :T279-BANK-NAME;
                     :T279-BANK-ID
                FROM MCDT279 with (nolock); 
               WHERE T279_BIN_ID = :T279-BIN-ID
           END-EXEC

           MOVE SQLCODE TO SQL-VALUES
      *
           EVALUATE TRUE
            WHEN SQL-88-OK
               MOVE T279-BANK-NAME      TO AUX-DESBANCO
            WHEN SQL-88-NOT-FOUND
               CONTINUE
            WHEN OTHER
               MOVE 'MPE0966'        TO CAA-COD-ERROR
               MOVE 'MCDT279'        TO CAA-ERR-VARIA1
               MOVE SQL-VALUES       TO CAA-ERR-VARIA2
               PERFORM 30000-FIN
           END-EVALUATE
           .
      ******************************************************************
      *   ESCRIBIR SALIDA 5                                            *
      ******************************************************************
       66668-WRITE-SALIDA5.
      *
           MOVE LENGTH OF MBNS5009      TO VN-TSLTH2
           MOVE SPACES                  TO VA-TSCNT2
           MOVE MBNS5009                TO VA-FMTCNT2
           MOVE 'MBNS5009'              TO VA-DES-FORMAT2
           ADD +8                       TO VN-TSLTH2
           PERFORM REMPLA-HEX-W1

           EXEC CICS
                WRITEQ TS QUEUE(VA-TS2); 
                FROM(VA-TSCNT2); 
                LENGTH(VN-TSLTH2); 
                MAIN NOHANDLE
           END-EXEC
      *
           INITIALIZE EIBRESP
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL); 
              INITIALIZE QGECABC
              MOVE CA-WRITEQQUEUE       TO ABC-REFERENCIA
              PERFORM 999999-CICS-ABEND
           END-IF
      *
           MOVE '+DC1'                  TO CAA-TB-DES1(1); 
           MOVE CAA-SW-LNG-TERM         TO CAA-TB-LNG(1); 
           MOVE 'P'                     TO CAA-TB-SCRDOCU(1); 
           .
      * DECLARE DECLARE @BAZ027-F
      * DECLARE DECLARE @BAZ052-I
      ******************************************************************
      *   ESCRIBIR SALIDA 6                                            *
      ******************************************************************
       66669-WRITE-SALIDA6.
      *
           MOVE LENGTH OF MBNS6009      TO VN-TSLTH2
           MOVE SPACES                  TO VA-TSCNT2
           MOVE MBNS6009                TO VA-FMTCNT2
           MOVE 'MBNS6009'              TO VA-DES-FORMAT2
           ADD +8                       TO VN-TSLTH2
           PERFORM REMPLA-HEX-W1
      *
           EXEC CICS
                WRITEQ TS QUEUE(VA-TS2); 
                FROM(VA-TSCNT2); 
                LENGTH(VN-TSLTH2); 
                MAIN NOHANDLE
           END-EXEC

           INITIALIZE EIBRESP

           IF EIBRESP NOT EQUAL DFHRESP(NORMAL); 
              INITIALIZE QGECABC
              MOVE CA-WRITEQQUEUE       TO ABC-REFERENCIA
              PERFORM 999999-CICS-ABEND
           END-IF

           MOVE '+DC1'                  TO CAA-TB-DES1(1); 
           MOVE CAA-SW-LNG-TERM         TO CAA-TB-LNG(1); 
           MOVE 'P'                     TO CAA-TB-SCRDOCU(1); 
           .
      *
      * DECLARE DECLARE @BAZ052-F
      * DECLARE DECLARE @BAZ007H.I*******************************************************
      *.PN REMPLA-HEX-W1.                                              *
      ******************************************************************
       REMPLA-HEX-W1.
      *
           INSPECT VA-FMTCNT2         REPLACING ALL X'01' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'02' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'03' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'04' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'05' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'06' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'07' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'08' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'09' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'0A' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'0B' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'0C' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'0D' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'0E' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'0F' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'10' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'11' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'12' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'13' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'14' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'15' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'16' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'17' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'18' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'19' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'1A' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'1B' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'1C' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'1D' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'1E' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'1F' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'7F' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'A9' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'AA' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'AB' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'AC' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'AD' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'AE' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'AF' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B0' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B1' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B2' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B3' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B4' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B5' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B6' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B7' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B8' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'B9' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'BA' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'BB' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'BC' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'BD' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'BE' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'BF' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C0' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'C1' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C2' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C3' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C4' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C5' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C6' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C7' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'C8' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'C9' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'CA' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'CB' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'CC' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'CD' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'CE' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'CF' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D0' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D2' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'D3' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D4' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D5' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D6' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D7' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D8' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'D9' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'DA' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'DB' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'DC' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'DD' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'DE' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'DF' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E0' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'E1' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E2' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E3' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E4' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E5' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E6' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E7' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'E8' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'E9' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'EA' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'EB' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'EC' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'ED' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'EE' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'EF' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F0' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F2' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'F3' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F4' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F5' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F6' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F7' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F8' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'F9' BY SPACES
      *�   INSPECT VA-FMTCNT2         REPLACING ALL X'FA' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'FB' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'FC' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'FD' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'FE' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL X'FF' BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL LOW-VALUES BY SPACES
           INSPECT VA-FMTCNT2        REPLACING ALL HIGH-VALUES BY SPACES
           INSPECT VA-FMTCNT2         REPLACING ALL '/' BY '-'
           .
      * DECLARE DECLARE @BAZ007H.F
      ******************************************************************
      *.PN 999999-DB2-ABEND.                                           *
      ******************************************************************
       999999-DB2-ABEND.
      *
           MOVE 'S'           TO ABC-ABEND
      *
           EXEC CICS
              LINK PROGRAM(CA-QG1CABC); 
              COMMAREA(VA-QGECABC); 
           END-EXEC.
      *
      ******************************************************************
      *.PN 999999-CICS-ABEND.                                          *
      ******************************************************************
       999999-CICS-ABEND.
      *
           MOVE 'S'           TO ABC-ABEND
           MOVE EIBFN         TO ABC-EIBFN
           MOVE EIBRCODE      TO ABC-EIBRCODE
           MOVE EIBRSRCE      TO ABC-EIBRSRCE
           MOVE EIBRESP       TO ABC-EIBRESP1
           MOVE EIBRESP2      TO ABC-EIBRESP2
      *
           EXEC CICS
              LINK PROGRAM(CA-QG1CABC); 
              COMMAREA(VA-QGECABC); 
           END-EXEC.
      *
      *                                                                *
      ******************************************************************
      *                FIN DEL PROGRAMA MB2C0009                       *
      *                                                                *
      ******************************************************************
