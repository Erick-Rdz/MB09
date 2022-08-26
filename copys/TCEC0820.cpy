      * TCEC0820:                                                      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *     BA00001    ACRUZ   20080504 MODIFICACION LONGITUD DE MONTOS*
      ******************************************************************
      *B.MF TCEC0820                  TC2C1810 LINK AREA (ON LINE      *
      *B/MF                           MODULE INTERFACE FOREIGN         *
      *B/MF                           EXCHANGES CHANGE)                *
      *B.MF CPYLTH                    COPY LENGTH                      *
      *B.IF INPUT                     INPUT FIELDS                     *
      *B.IF COD-ENTITY-A              ENTITY CODE                      *
      *B.IF COD-ENTITY                ENTITY CODE                      *
      *B.IF COD-BRANCH-A              BRANCH CODE                      *
      *B.IF COD-BRANCH                BRANCH CODE                      *
      *B.IF COD-FCC                   CURRENCY SWIFT CODE              *
      *B.IF TIP-OFFERRATE             GLOBAL EXCHANGES OFFERRATE       *
      *B.IF TIP-BIDRATE               GLOBAL EXCHANGES BIDRATE         *
      *B.OF OUTPUT                    OUTPUT FIELDS                    *
      *B.OF COD-RETURN                RETURN CODE                      *
      *B/OF                           '00' RETURN OK                   *
      *B/OF                           '10' ENTITY CODE ERROR           *
      *B/OF                           '15' NON-INFORMED ENTITY         *
      *B/OF                           '20' BRANCH CODE ERROR           *
      *B/OF                           '25' NON-INFORMED BRANCH         *
      *B/OF                           '30' WRONG CURRENCY              *
      *B/OF                           '35' NON-INFORMED CHANGES        *
      *B/OF                           '40' ERROR ACCESO RUTINA BRANCH  *
      *B/OF                           '45' ERROR ACCESO RUTINA REGION  *
      *B/OF                           '99' DB2 ERROR                   *
      *B.OF ERR-DB2                   DB2 ERROR INFORMATION            *
      *B.OF SQLCODE                   SQL RETURN CODE                  *
      *B.OF SQLERRM                   SQLERRM DB2                      *
      *B.OF SQLERRMLON                SQL RETURN MESSAGE LENGTH        *
      *B.OF DTA-SQLERRM               SQL RETURN MESSAGE DATA          *
      *B.OF DES-TABLE                 DB2 TABLE NAME                   *
      *B.OF FILLER                    FILLER                           *
      *B.OF OFFERRATE                 OFFER RATE                       *
      *B.OF BIDRATE                   BUYER CHANGE                     *
      *B.OF FIXRATE                   FIXING CHANGE                    *
      *B.OF FAC-OFFERRATE             EXCHANGE OFFER FACTOR            *
      *B.OF FAC-BIDRATE               EXCHANGE BUYER FACTOR            *
      *B.OF FILLER                    FILLER                           *
      *
      ******************************************************************
         03 TCEC0820.
           08 TCEC0820-CPYLTH               PIC S9(4) COMP
                 VALUE +150.
           08 TCEC0820-INPUT.
             13 TCEC0820-COD-ENTITY-A.
               18 TCEC0820-COD-ENTITY       PIC 9(4).
             13 TCEC0820-COD-BRANCH-A.
               18 TCEC0820-COD-BRANCH       PIC 9(4).
             13 TCEC0820-COD-FCC            PIC X(3).
      * BA00001 - INI
      *      13 TCEC0820-TIP-OFFERRATE      PIC S9(5)V9(5) COMP-3.
      *      13 TCEC0820-TIP-BIDRATE        PIC S9(5)V9(5) COMP-3.
             13 TCEC0820-TIP-OFFERRATE      PIC S9(6)V9(6) COMP-3.
             13 TCEC0820-TIP-BIDRATE        PIC S9(6)V9(6) COMP-3.
      * BA00001 - FIN
           08 TCEC0820-OUTPUT.
             13 TCEC0820-COD-RETURN         PIC X(2).
             13 TCEC0820-ERR-DB2.
               18 TCEC0820-SQLCODE          PIC S9(9) COMP.
               18 TCEC0820-SQLERRM.
               23 TCEC0820-SQLERRMLON       PIC S9(4) COMP.
               23 TCEC0820-DTA-SQLERRM      PIC X(30).
               18 TCEC0820-DES-TABLE        PIC X(5).
               18 FILLER                    PIC X(5).
      * BA00001 - INI
      *      13 TCEC0820-OFFERRATE          PIC S9(5)V9(5) COMP-3.
      *      13 TCEC0820-BIDRATE            PIC S9(5)V9(5) COMP-3.
      *      13 TCEC0820-FIXRATE            PIC S9(5)V9(5) COMP-3.
      *      13 TCEC0820-FAC-OFFERRATE      PIC S9(5)V9(5) COMP-3.
      *      13 TCEC0820-FAC-BIDRATE        PIC S9(5)V9(5) COMP-3.
             13 TCEC0820-OFFERRATE          PIC S9(6)V9(6) COMP-3.
             13 TCEC0820-BIDRATE            PIC S9(6)V9(6) COMP-3.
             13 TCEC0820-FIXRATE            PIC S9(6)V9(6) COMP-3.
             13 TCEC0820-FAC-OFFERRATE      PIC S9(6)V9(6) COMP-3.
             13 TCEC0820-FAC-BIDRATE        PIC S9(6)V9(6) COMP-3.
      * BA00001 - FIN
           08 FILLER                        PIC X(35).
      *
