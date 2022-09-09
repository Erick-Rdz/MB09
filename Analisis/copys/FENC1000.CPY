      ******************************************************************
      *         COPY DE RUTINA FE7C1000 - CONSULTA DE TRANSFERENCIAS   *
      ******************************************************************
      *                     MODIFICATIONS LOG.                         *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     -------- --------- -------- ------------------------------ *
      ******************************************************************
      *    FENC1000                   FENC1000                         *
      *    V1000-INPUT.                                                *
      *       ENT                     ENTIDAD                          *
      *       CEN-REG                 CENTRO                           *
      *       ACC                     CUENTA                           *
      *       CODE                    CODIGO DE OPERACION              *
      *       AMOUNT                  MONTO DE OPERACION               *
      *       INTREF                  REFERENCIA                       *
      *       DAT-OPERATION           FECHA DE OPERACION               *
      *       DAT-VALUEI              FECHA VALOR                      *
      *    V1000-OUTPUT.                                               *
      *       COD-RETURN              CODIGO DE RETORNO                *
      *                               00 OK                            *
      *                               10 NOT FOUND                     *
      *                               20 ERROR EN DATOS                *
      *                               99 ERROR ABEND                   *
      *       DES-ERROR               DESCRIPCION DEL ERROR            *
      *       DATA-ORIG               DATOS DEL ORIGINADOR             *
      *           DES-ASSN            NOMBRE ORIGEN                    *
      *           ENT-ORIGIN          ENTIDAD ORIGEN                   *
      *           BRN-ORIGIN          CENTRO ORIGEN                    *
      *           CDG02               DIGITO DE CONTROL                *
      *           ACC-ORIGIN          CUENTA ORIGEN                    *
      *           NUM-SEQOPE          NUMERO DE SECUENCIA DE OPERACION *
      *       DATA-DEST               DATOS DEL DESTINATARIO           *
      *           DES-BEN1            NOMBRE DEL BENEFICIARIO          *
      *           BCO-BENEF.          BANCO BENEFICIARIO               *
      *              ENT-DEST         ENTIDAD DESTINO                  *
      *              BCO-DEST         NOMBRE BANCO DESTINO             *
      *           TIPO-CTA            TIPO DE CUENTA                   *
      *           ACC-DEST            CUENTA DESTINO                   *
      *           RFC                 RFC                              *
      *       DATA-OPER               DATOS DE LA OPERACION            *
      *           CVE-RASTREO         CLAVE DE RASTREO                 *
      *           FCC                 DIVISA                           *
      *           AMT-OPERATION       MONTO DE OPERACION               *
      *           PORIVA              PORCENTAJE DE IVA                *
      *           AMT-IVA             MONTO DEL IVA                    *
      *           AMT-COMMISSION      MONTO DE LA COMISION             *
      *           TOTALOPE            TOTAL DE LA OPERACION            *
      *           CONCEPTOP           CONCEPTO DE PAGO                 *
      *           DESC-STA            DESCRIPCION DE ESTATUS           *
      *           FLG-TRFTYP          TIPO DE TRANSFERENCIA            *
      *           DAT-VALUE           FECHA VALOR                      *
      *           FLG-OPEST           BANDERA DE ESTATUS               *
      *           TYP-OPERATION       TIPO DE OPERACION                *
      *           FOLEMIS             FOLIO EMISOR                     *
      *           DESC-DEV            DESCRIPCION DE DEVOLUCION        *
      *           DAT-RETURN          FECHA DE DEVOLUCON               *
      *           COD-DEV             CODIGO DE DEVOLUCION             *
      *           TIM-AUT             HORA DE AUTENTICACION            *
      *           TIM-SHI             HORA SISTEMA INICIAL             *
      ******************************************************************
       02 FENC1000.
           03 N1000-INPUT.
               05 N1000-ENT                    PIC X(04).
               05 N1000-CEN-REG                PIC X(04).
               05 N1000-ACC                    PIC X(10).
               05 N1000-CODE                   PIC X(03).
               05 N1000-AMOUNT                 PIC 9(13)V99.
               05 N1000-INTREF                 PIC X(15).
               05 N1000-DAT-OPERATION          PIC X(10).
               05 N1000-DAT-VALUEI             PIC X(10).
               05 N1000-CAM-LIBREI             PIC X(30).
           03 N1000-OUTPUT.
               04 N1000-COD-RETURN             PIC X(02).
               04 N1000-DES-ERROR              PIC X(40).
               04 N1000-DATA-ORIG.
                   05 N1000-DES-ASSN           PIC X(36).
                   05 N1000-BCO-ORIG.
                       06 N1000-ENT-ORIGIN     PIC X(04).
      *                06 N1000-BRN-ORIGIN     PIC X(04).
                       06 FILLER               PIC X(03) VALUE ' - '.
                       06 N1000-BCO-ORIGIN     PIC X(20).
                   05 N1000-TIPO-CTAO          PIC X(02).
                   05 N1000-ACC-ORIGIN         PIC X(20).
                   05 N1000-RFC-ORD            PIC X(18).
               04 N1000-DATA-DEST.
                   05 N1000-DES-BEN1           PIC X(40).
                   05 N1000-BCO-BENEF.
                       06 N1000-ENT-DEST       PIC X(04).
      *                06 N1000-BRN-DEST       PIC X(03).
                       06 FILLER               PIC X(03) VALUE ' - '.
                       06 N1000-BCO-DEST       PIC X(20).
                   05 N1000-TIPO-CTA           PIC X(02).
                   05 N1000-ACC-DEST           PIC X(20).
                   05 N1000-RFC                PIC X(18).
               04 N1000-DATA-OPER.
                   05 N1000-NUM-SEQOPE         PIC x(21).
                   05 N1000-CVE-RASTREO        PIC X(36).
                   05 N1000-FCC                PIC X(03).
                   05 N1000-AMT-OPERATION      PIC 9(13)V99.
                   05 N1000-PORIVA             PIC 9(02).
                   05 N1000-AMT-IVA            PIC 9(13)V99.
                   05 N1000-AMT-COMMISSION     PIC 9(13)V99.
                   05 N1000-TOTALOPE           PIC 9(13)V99.
                   05 N1000-CONCEPTOP          PIC X(40).
                   05 N1000-DESC-STA           PIC X(20).
                   05 N1000-FLG-TRFTYP         PIC X(01).
                   05 N1000-DAT-VALUE          PIC X(10).
                   05 N1000-FLG-OPEST          PIC X(01).
                   05 N1000-TYP-OPERATION      PIC X(02).
                   05 N1000-FOLEMIS            PIC X(09).
                   05 N1000-DESC-DEV           PIC X(40).
                   05 N1000-DAT-RETURN         PIC X(10).
                   05 N1000-COD-DEV            PIC X(02).
                   05 N1000-TIM-AUT            PIC X(08).
                   05 N1000-TIM-SHI            PIC X(08).
                   05 N1000-CAM-LIBREO         PIC X(40).
      *
      ******************************************************************
