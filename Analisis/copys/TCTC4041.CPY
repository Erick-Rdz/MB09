      * TCTC4041: COPY DE LA TABLA DE REGIONES CAMBIARIAS EXTRANJERO   *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *                                                                *
      *.A.MS AS0001DTC  JASL   10-09-04 ADICIONAR FACTORES DE AJUSTE   *
      *                                 ESPECIALES A LA COMPRA Y A LA  *
      *                                 VENTA DE LA REGION CAMBIARIA   *
      ******************************************************************
      *B.MF TCTC4041                                                   *
      *B.MF KEY                  KEY                                   *
      *B.MF COD-TBL              TABLE CODE                            *
      *B.MF COD-ENT              ENTITY CODE                           *
      *B.MF KEY-TBL              TABLE KEY                             *
      *B.MF CLAVE-R              CLAVE DE REGION CAMBIARIA             *
      *B.MF FILLER               FILLER                                *
      *B.MF DATA                 DATA                                  *
      *B.MF NOMBRE-R             NOMBRE DE REGION CAMBIARIA            *
      *B.MF FACTOR-C             FACTOR DE AJUSTE A LA COMPRA          *
      *B.MF FACTOR-V             FACTOR DE AJUSTE A LA VENTA           *
      *B.MF OBSERVA              OBSERVACIONES                         *
      *B.MF FILLER               FILLER                                *
      ******************************************************************
       02  TCTC4041.
         03 TCTC4041-KEY.
           05 TCTC4041-COD-TBL                   PIC X(4).
           05 TCTC4041-COD-ENT                   PIC X(4).
           05 TCTC4041-KEY-TBL.
              07 TCTC4041-CLAVE-R                PIC X(05).
              07 FILLER                          PIC X(18).
         03 TCTC4041-DATA.
           05 TCTC4041-NOMBRE-R                  PIC X(30).
           05 TCTC4041-FACTOR-C                  PIC 9(04)V9(06).
           05 TCTC4041-FACTOR-V                  PIC 9(04)V9(06).
      *.A.MS AS0001DTC  I  JASL
           05 TCTC4041-FACTOR-ESP-C              PIC 9(04)V9(06).
           05 TCTC4041-FACTOR-ESP-V              PIC 9(04)V9(06).
      *.A.MS AS0001DTC  F  JASL
           05 TCTC4041-OBSERVA                   PIC X(30).
           05 FILLER                             PIC X(150).
