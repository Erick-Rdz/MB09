      * MBEC0100:  COMMAREA MB7C0100 -- ALTA GEOLOCALIZACION           *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *  CODE       AUTHOR  DATE     DESCRIPTION                       *
      *  ---------- ------- -------- --------------------------------- *
      *                                                                *
      *****************************************************************
      *01 MB7C0100-01.
      * 02 PR-PARAM-GEO.                                               *
      *   05 PR-GEO-CLIENTE              PIC X(08).                    *
      *   05 PR-GEO-CODOPER              PIC X(04).                    *
      *   05 PR-GEO-NUMOPER              PIC 9(18).                    *
      *   05 PR-GEO-LATITUD              PIC S9(03)V9(06).             *
      *   05 PR-GEO-LONGITUD             PIC S9(03)V9(06).             *
      *   05 PR-GEO-CEL                  PIC X(10).                    *
      *   05 PR-GEO-CUENTA               PIC X(16).                    *
      *   05 PR-GEO-MONTO                PIC 9(13)V99.                 *
      *   05 PR-GEO-CENTRO               PIC X(04).                    *
      *   05 PR-GEO-TERMINAL             PIC X(04).                    *
      *   05 PR-GEO-USUARIO              PIC X(10).                    *
      *   05 PR-GEO-ERROR                PIC X(02).                    *
      *   05 PR-GEO-AVISO                PIC X(100).                   *
      ******************************************************************
      * ****************************************************************
        02 PR-PARAM-GEO.
          05 PR-GEO-CLIENTE              PIC X(08).
          05 PR-GEO-CODOPER              PIC X(04).
          05 PR-GEO-NUMOPER              PIC 9(18).
          05 PR-GEO-LATITUD              PIC S9(03)V9(06).
          05 PR-GEO-LONGITUD             PIC S9(03)V9(06).
          05 PR-GEO-CEL                  PIC X(15).
          05 PR-GEO-CUENTA               PIC X(20).
          05 PR-GEO-MONTO                PIC 9(13)V99.
          05 PR-GEO-CENTRO               PIC X(04).
          05 PR-GEO-TERMINAL             PIC X(04).
          05 PR-GEO-USUARIO              PIC X(10).
          05 PR-GEO-ERROR                PIC X(02).
          05 PR-GEO-AVISO                PIC X(100).
          05 PR-GEO-TYPAPPL              PIC X(04).
          05 PR-GEO-NUMREF               PIC X(40).
          05 PR-GEO-FLG                  PIC X(01).
          05 PR-GEO-FLG2                 PIC X(01).
          05 PR-GEO-CHAR2                PIC X(20).
          05 PR-GEO-MONTO2               PIC 9(13)V99.
      *
      ******************************************************************
