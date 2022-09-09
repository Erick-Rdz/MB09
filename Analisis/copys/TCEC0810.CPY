      * TCEC0810:                                                      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *     BA00001    ACRUZ   20080504 MODIFICACION LONGITUD DE MONTOS*
      ******************************************************************
      *B.MF TCEC0810                  TC2C1810 LINK AREA (ON LINE      *
      *B/MF                           MODULE INTERFACE FOREIGN         *
      *B/MF                           EXCHANGES CHANGE)                *
      *B.MF CPYLTH                    COPY LENGTH                      *
      *B.IF INPUT                     INPUT FIELDS                     *
      *B.IF COD-ENTITY-A              ENTITY CODE                      *
      *B.IF COD-ENTITY                ENTITY CODE                      *
      *B.IF DAT-EXCHANGE              CHANGE DATE                      *
      *B/IF                           IF IT IS NOT INFORMED, TAKE THE  *
      *B/IF                           LAST DATE INTRODUCED IN CHANGES  *
      *B/IF                           TABLE                            *
      *B.IF FLG-FCCB3                 CURRENCY/BILL FLAG  (D/B)        *
      *B.IF COD-SWIFTFCCS             CURRENCY SWIFT CODE              *
      *B.OF OUTPUT                    OUTPUT FIELDS                    *
      *B.OF COD-RETURN                RETURN CODE                      *
      *B/OF                           '00' RETURN OK                   *
      *B/OF                           '10' WRONG OR ILLOGICAL DATE     *
      *B/OF                           '15' WRONG D/B FLAG              *
      *B/OF                           '20' NON-EXISTENT CURRENCY       *
      *B/OF                           '25' NOT INFORMED CURRENCY CODE  *
      *B/OF                           '30' ANNULLED CURRENCY           *
      *B/OF                           '35' NOT LISTED CURRENCY         *
      *B/OF                           '40' CHANGES INQUIRY             *
      *B/OF                           '45' CHANGES NOT AVAILABLE       *
      *B/OF                           '50' NON-EXISTENT CHANGES        *
      *B/OF                           '55' CHANGES DATA INCONSISTENCY  *
      *B/OF                           '60' ENTITY CODE ERROR           *
      *B/OF                           '85' ROUTINE ACCESS ERROR        *
      *B/OF                           '99' DB2 ERROR                   *
      *B.OF ERR-DB2                   DB2 ERROR INFORMATION            *
      *B.OF SQLCODE                   SQL RETURN CODE                  *
      *B.OF SQLERRM                   SQLERRM DB2                      *
      *B.OF SQLERRMLON                SQL RETURN MESSAGE LENGTH        *
      *B.OF DTA-SQLERRM               SQL RETURN MESSAGE DATA          *
      *B.OF DES-TABLE                 DB2 TABLE NAME                   *
      *B.OF FILLER                    FILLER                           *
      *B.OF DAT-LSTCHANGE             CHANGE DATE (IF IS NOT INFORMED  *
      *B/OF                           IN THE ENTER AREA ONLY)          *
      *B.OF FLG-QUOTE                 QUOTE FLAG                       *
      *B.OF EXCHUNIT1                 EXCHANGE RATE UNIT               *
      *B.OF OFFERRATE                 OFFER RATE                       *
      *B.OF BIDRATE                   BUYER CHANGE                     *
      *B.OF FIXRATE                   FIXING CHANGE                    *
      *B.OF COD-BCC                   BASE CURRENCY CODE               *
      *B.OF COD-FCCIRF                BASE CURRENCY CODE (REDEFINITION)*
      *B.OF FILLER                    FILLER                           *
      *
      ******************************************************************
         03 TCEC0810.
           08 TCEC0810-CPYLTH               PIC S9(4) COMP
                 VALUE +135.
           08 TCEC0810-INPUT.
             13 TCEC0810-COD-ENTITY-A.
               18 TCEC0810-COD-ENTITY       PIC 9(4).
             13 TCEC0810-DAT-EXCHANGE       PIC X(10).
             13 TCEC0810-FLG-FCCB3          PIC X(1).
             13 TCEC0810-COD-SWIFTFCCS      PIC X(3).
           08 TCEC0810-OUTPUT.
             13 TCEC0810-COD-RETURN         PIC X(2).
             13 TCEC0810-ERR-DB2.
               18 TCEC0810-SQLCODE          PIC S9(9) COMP.
               18 TCEC0810-SQLERRM.
               23 TCEC0810-SQLERRMLON       PIC S9(4) COMP.
               23 TCEC0810-DTA-SQLERRM      PIC X(30).
               18 TCEC0810-DES-TABLE        PIC X(16).
               18 FILLER                    PIC X(5).
             13 TCEC0810-DAT-LSTCHANGE      PIC X(10).
             13 TCEC0810-FLG-QUOTE          PIC X(1).
             13 TCEC0810-EXCHUNIT1          PIC S9(3) COMP-3.
      * BA00001 - INI
      *      13 TCEC0810-OFFERRATE          PIC S9(4)V9(6) COMP-3.
      *      13 TCEC0810-BIDRATE            PIC S9(4)V9(6) COMP-3.
      *      13 TCEC0810-FIXRATE            PIC S9(4)V9(6) COMP-3.
             13 TCEC0810-OFFERRATE          PIC S9(6)V9(6) COMP-3.
             13 TCEC0810-BIDRATE            PIC S9(6)V9(6) COMP-3.
             13 TCEC0810-FIXRATE            PIC S9(6)V9(6) COMP-3.
      * BA00001 - FIN
             13 TCEC0810-COD-BCC            PIC X(3).
             13 TCEC0810-COD-FCCIRF REDEFINES TCEC0810-COD-BCC
                                            PIC X(3).
           08 FILLER                        PIC X(29).
      *
