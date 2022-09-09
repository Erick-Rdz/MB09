      ******************************************************************
      *A.OR.S
      *  QBEC999: COMMAREA CON DATOS DE ENTIDAD PARA COMUNICACION CON
      *           LOS MODULOS.
      *A.OR.E
      *B.OR.S
      *  QBEC999: COMMAREA WITH ENTITY DATA FOR COMMUNICATION AMONG
      *           MODULES
      *B.OR.E
      *----------------------------------------------------------------
      *B.MF QBEC999     QBEC999
      *B.MF DATA-PARAM  DATA
      *B.MF COD-ENTITY  ENTITY CODE
      *B.MF ENTITY-DATA ENTITY DATA
      *B.MF LNG-DATA    DATA LANGUAGE. IN THIS CASE, THE SAME AS
      *B/MF             LNG-OFDATA.
      *B.MF COD-SHORT   SHROT CODE.
      *B.MF COD-NB      NATIONAL BANK CODE
      *B.MF COD-NATCC   NATIONAL CURRENCY CODE
      *B.MF COD-NATCCSHORT  SHORT NATIONAL CURRENCY CODE
      *B.MF COD-OCCCTRY COUNTRY ORIGIN CURRENCY CODE (IN OFFICIAL LAN-
      *B/MF             GUAGE LNG-OFDATA.
      *B.MF COD-OFCCSHORT   COUNTRY ORIGIN CURRENCY CODE (IN OFFICIAL
      *B/MF                 LANGUAGE LNG-OFDATA.
      *B.MF COD-RCC     REFERENCE CURRENCY CODE
      *B.MF COD-RFCCSHORT   SHORT REFERENCE CURRENCY CODE (IN OFFICIAL
      *B/MF                 LANGUAGE LNG-OFDATA.
      *B.MF COD-NRESFCC NON RESIDENT CURRENCY CODE
      *B.MF COD-NATCTRY NATIONAL COUNTRY CODE
      *B.MF FLG-EURCTRY COUNTRY EURO FLAG
      *B.FV 88-FLG-YESEURCTRY  COUNTRY EURO
      *B.FV 88-FLG-NONEURCTRY  COUNTRY NON EURO WARNING
      *B.MF FLG-EXCHANGE APPLICABLE CHANGE
      *B.FV 88-FLG-DIRCHG   CURRENCY CHANGE FLAG DIRECT CHANGE
      *B.FV 88-FLG-INDIRCHG CURRENCY CHANGE FLAG INVERSE CHANGE
      *B.MF LNG-OFDATA   OFFICIAL LANGUAGE
      *B.MF FLG-OFCACC   OFFICIAL ACCOUNT FLAG
      *B.MF FCCCOEXC     CURRENCIES COEXISTENCE
      *B.MF FREE         FILLER
      ******************************************************************

       02 QBEC999.
          05 QBEC999-DATA-PARAM.
             10 QBEC999-COD-ENTITY                   PIC X(04).
             10 QBEC999-ENTITY-DATA.
                15 QBEC999-LNG-DATA                  PIC X(01).
                15 QBEC999-COD-SHORT                 PIC X(02).
                15 QBEC999-COD-NB                    PIC X(04).
                15 QBEC999-COD-NATCC                 PIC X(03).
                15 QBEC999-COD-NATCCSHORT            PIC X(01).
                15 QBEC999-COD-OCCCTRY               PIC X(03).
                15 QBEC999-COD-OFCCSHORT             PIC X(01).
                15 QBEC999-COD-RCC                   PIC X(03).
                15 QBEC999-COD-RFCCSHORT             PIC X(01).
                15 QBEC999-COD-NRESFCC               PIC X(03).
                15 QBEC999-COD-NATCTRY               PIC X(03).
                15 QBEC999-FLG-EURCTRY               PIC X(01).
                   88 QBEC999-88-FLGYESEURCTRY       VALUE 'S'.
                   88 QBEC999-88-FLGNONEURCTRY       VALUE 'N'.
                15 QBEC999-FLG-EXCHANGE              PIC X(01).
                   88 QBEC999-88-FLGDIRCHG           VALUE 'D'.
                   88 QBEC999-88-FLGINDIRCHG         VALUE 'I'.
                15 QBEC999-LNG-OFDATA                PIC X(01).
                15 QBEC999-FLG-OFCACC                PIC X(01).
                15 QBEC999-FCCCOEXC                  PIC X(01).
                15 QBEC999-FREE                      PIC X(16).
