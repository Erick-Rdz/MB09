      * TCEC9900:                                                      *
      ******************************************************************
      *                     MODIFICATIONS LOG           EIMZ           *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      ******************************************************************
      *B.MF TCEC9900                  LINK AREA INTERFACE ROUTINE      *
      *B/MF                           BATCH AND ON-LINE WITH ENTITY    *
      *B/MF                           STRUCTURAL PARAMETRES            *
      *B/MF                           ()                       *
      *B.MF RECORD                    RECORD                           *
      *B.IF INPUT                     INPUT FIELDS                     *
      *B.IF OPTION                    OPTION                           *
      *B/IF                           '1' ACCESS BY ENTITY. ENTITY     *
      *B/IF                               FIELD MUST BE INFORMED       *
      *B/IF                           '3' ACCESS BY ENTITY INDICATING  *
      *B/IF                               IF COEXISTENCE PERIOD        *
      *B.IF KEY                       ENTITY CODE                      *
      *B.IF COD-LNGKEY                LANGUAGE KEY CODE                *
      *B.OF OUTPUT                    OUTPUT FIELDS                    *
      *B.OF COD-RETURN                RETURN CODE                      *
      *B.OF ERR-DB2                   DB2 ERROR INFORMATION            *
      *B.OF SQLCODE                   SQL RETURN CODE                  *
      *B/OF                           '00' RETURN OK                   *
      *B/OF                           '10' WRONG INPUT DATA            *
      *B/OF                           '70' RECORD NOT FOUND (DB2)      *
      *B/OF                           '99' WRONG DB2 ACCESS            *
      *B.OF SQLERRM                   SQLERRM DB2                      *
      *B.OF SQLERRMLON                SQLERRM-LON DB2                  *
      *B.OF DTA-SQLERRM               SQLERRM-DATA DB2                 *
      *B.OF DES-TABLE                 DB2 TABLENAME                    *
      *B.OF ERR-CICS                  CICS ERROR INFORMATION           *
      *B.OF EIBRCODE                  W990-EIBRCODE                    *
      *B.OF EIBFN                     W990-EIBFN                       *
      *B.OF EIBRSRCE                  W990-EIBRSRCE                    *
      *B.OF DATA                      DATA                             *
      *B.OF DATA-PARAM                ENTITY PARAMETERS                *
      *B.OF COD-ENTITY                ENTITY CODE                      *
      *B.OF LNG-DATA                  DATA LANGUAGE                    *
      *B.OF COD-SHORT                 SHORT CODE                       *
      *B.OF COD-NB                    NATIONAL BANK CODE               *
      *B.OF COD-NATCC                 NATIONAL CURRENCY CODE           *
      *B.OF COD-NATCCSHORT            SHORT NATIONAL CURRENCY CODE     *
      *B.OF COD-OCCCTRY               COUNTRY ORIGIN CURRENCY CODE     *
      *B.OF COD-OFCCSHORT             SHORT ORIGIN CURRENCY CODE       *
      *B.OF COD-RCC                   REFERENCE CURRENCY CODE          *
      *B.OF COD-RFCCSHORT             SHORT REFERENCE CURRENCY CODE    *
      *B.OF COD-NRESFCC               NON RESIDENT CURRENCY CODE       *
      *B.OF COD-NATCTRY               NATIONAL COUNTRY CODE            *
      *B.OF SW-FLG-EURCTRY            COUNTRY EURO FLAG                *
      *B.FV   SW-FLG-EUR-YESCTRY      COUNTRY EURO                     *
      *B/FV                           'S'                              *
      *B.FV   SW-FLG-EUR-NONCTRY      COUNTRY NON EURO WARNING         *
      *B/FV                           'N'                              *
      *B.OF SW-FLG-EXCHANGE           APPLICABLE CHANGE                *
      *B.FV   SW-FLG-EXC-DIR          CURRENCY CHANGE FLAG DIRECT      *
      *B/FV                           CHANGE                           *
      *B/FV                           'D'                              *
      *B.FV   SW-FLG-EXC-INDIR        CURRENCY CHANGE FLAG INVERSE     *
      *B/FV                           CHANGE                           *
      *B/FV                           'I'                              *
      *B.OF LNG-OFDATA                OFFICIAL DATA LANGUAGE           *
      *B.OF FLG-OFCACC                OFFICIAL ACCOUNT FLAG            *
      *B.OF FCCCOEXC                  CURRENCIES COEXISTENCE           *
      *B.OF FILLER                    FILLER                           *
      *B.OF DATA-DESCRIP              PARAMETERS DESCRIPTION           *
      *B.OF DES-ENTITY                ENTITY DESCRIPTION               *
      *B.OF DES-NB                    NATIONAL BANK DESCRIPTION        *
      *B.OF DES-NATCC                 NATIONAL CURRENCY DESCRIPTION    *
      *B.OF SDE-FCC                   NATIONAL CURRENCY SHORT          *
      *B/OF                           DESCRIPTION                      *
      *B.OF DES-OCC                   ORIGIN CURRENCY DESCRIPTION      *
      *B.OF SDES-ORIFCC               ORIGIN CURRENCY SHORT            *
      *B/OF                           DESCRIPTION                      *
      *B.OF DES-REFFCC                REFERENCE CURRENCY DESCRIPTION   *
      *B.OF DES-NRESFCC               NON RESIDENT CURRENCY            *
      *B/OF                           DESCRIPTION                      *
      *B.OF DES-NATCTRY               NATIONAL COUNTRY DESCRIPTION     *
      *B.OF SDE-ENTITY                SHORT DESCRIPTION OF ENTITY      *
      *B.OF DATA-STAMP                MODIFICATION DATES (STAMPS)      *
      *B.OF LASTMODUSER               LAST MODIFICATION USER           *
      *B.OF LASTMODTRM                LAST MODIFICATION TERMINAL       *
      *B.OF TIMESTAMP                 LAST MODIFICATION                *
      *B.OF FILLER                    FILLER                           *
      *B.OF FILLER                    FILLER                           *
      ******************************************************************
       02 TCEC9900.
         03 TCEC9900-RECORD.
            05 TCEC9900-INPUT.
               10 TCEC9900-OPTION           PIC X(1).
               10 TCEC9900-KEY              PIC X(4).
               10 TCEC9900-COD-LNGKEY       PIC X.
      *
            05 TCEC9900-OUTPUT.
               10 TCEC9900-COD-RETURN       PIC X(2).
               10 TCEC9900-ERR-DB2.
                  15 TCEC9900-SQLCODE       PIC S9(9) COMP.
                  15 TCEC9900-SQLERRM.
                     20 TCEC9900-SQLERRMLON PIC S9(4) COMP.
                     20 TCEC9900-DTA-SQLERRM          PIC X(30).
      *
               10 TCEC9900-DES-TABLE        PIC X(16).
               10 TCEC9900-ERR-CICS.
                  15 TCEC9900-EIBRCODE      PIC X(6).
                  15 TCEC9900-EIBFN         PIC X(2).
                  15 TCEC9900-EIBRSRCE      PIC X(8).
      *
               10 TCEC9900-DATA.
                  15 TCEC9900-DATA-PARAM.
                     20 TCEC9900-COD-ENTITY PIC X(4).
                     20 TCEC9900-LNG-DATA   PIC X.
                     20 TCEC9900-COD-SHORT  PIC X(2).
                     20 TCEC9900-COD-NB     PIC X(4).
                     20 TCEC9900-COD-NATCC  PIC X(3).
                     20 TCEC9900-COD-NATCCSHORT            PIC X.
                     20 TCEC9900-COD-OCCCTRY               PIC X(3).
                     20 TCEC9900-COD-OFCCSHORT             PIC X.
                     20 TCEC9900-COD-RCC                   PIC X(3).
      *
                     20 TCEC9900-COD-RFCCSHORT             PIC X.
                     20 TCEC9900-COD-NRESFCC               PIC X(3).
                     20 TCEC9900-COD-NATCTRY               PIC  X(3).
      *
                     20 TCEC9900-SW-FLG-EURCTRY            PIC X.
                        88 TCEC9900-SW-FLG-EUR-YESCTRY     VALUE 'S'.
                        88 TCEC9900-SW-FLG-EUR-NONCTRY     VALUE 'N'.
                     20 TCEC9900-SW-FLG-EXCHANGE           PIC  X.
      *
                        88 TCEC9900-SW-FLG-EXC-DIR         VALUE 'D'.
                        88 TCEC9900-SW-FLG-EXC-INDIR       VALUE 'I'.
                     20 TCEC9900-LNG-OFDATA PIC X.
                     20 TCEC9900-FLG-OFCACC PIC X(1).
                     20 TCEC9900-FCCCOEXC   PIC X(1).
                     20 FILLER              PIC X(16).
                  15 TCEC9900-DATA-DESCRIP.
                     20 TCEC9900-DES-ENTITY PIC X(40).
                     20 TCEC9900-DES-NB     PIC X(40).
                     20 TCEC9900-DES-NATCC  PIC X(20).
                     20 TCEC9900-SDE-FCC    PIC X(3).
                     20 TCEC9900-DES-OCC    PIC X(20).
                     20 TCEC9900-SDES-ORIFCC                PIC X(3).
                     20 TCEC9900-DES-REFFCC PIC X(20).
                     20 TCEC9900-DES-NRESFCC                PIC X(20).
                     20 TCEC9900-DES-NATCTRY                PIC X(40).
                     20 TCEC9900-SDE-ENTITY PIC X(10).
                  15 TCEC9900-DATA-STAMP.
                     20 TCEC9900-LASTMODUSER                PIC X(8).
                     20 TCEC9900-LASTMODTRM PIC X(4).
                     20 TCEC9900-TIMESTAMP  PIC X(26).
                     20 FILLER              PIC X(49).
      *
            05 FILLER                       PIC X(89).
      *
