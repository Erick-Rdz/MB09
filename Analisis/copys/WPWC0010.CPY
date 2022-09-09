      * WPWC0010: CONECTION WITH MODULE WP8C0010 TO OBTAIN ALL         *
      *           INFORMATION ABOUT PRODUCT                            *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      ******************************************************************
      *B.IF INPUT               INPUT DATA                             *
      *B.IF   COD-ENTITY        ENTITY CODE                            *
      *B.IF   COD-PRODUCT       PRODUCT CODE                           *
      *B.IF   COD-SUBPRODUCT    BYPRODUCT CODE                         *
      *B.IF   COD-FCURRENCY     FOREING CURRENCY                       *
      *B.IF   DAT-OPERATION     TRANSACTION DATE                       *
      *B.IF   COD-LANGUAGE      LANGUAGE                               *
      *B.IF   COD-OPTION        OPTION:1- VALIDATION EXISTENCE ONLY    *
      *B/IF                            2- CHARACTERISTICS INQUIRY      *
      *B/IF                            3- REQUEST INQUIRY              *
      *B.IF   FLG-MASTER        FLAG TO RETRIEVE DATA FROM MASTER ONLY *
      *B.IF   FLG-DESCRIPTION   FLAG TO RETRIEVE DESCRIPTION ONLY      *
      *B.IF   FLG-CONTCHAN      FLAG TO RETRIEVE CONTRACTING CHANNEL   *
      *B.IF   FLG-RESTOPERCHAN  FLAG TO RETRIEVE RESTRICTED OPERATING  *
      *B/IF                     CHANNELS ONLY.                         *
      *B.IF   FLG-GEOADISAMB    FLAG TO RETRIEVE GEOGRAPHICAL AREA     *
      *B/IF                     DISTRIBUTION AMBIT CODE ONLY.          *
      *B.IF   FLG-CENDISAMB     FLAG TO RETRIEVE CENTRES               *
      *B.IF   FLG-SEGDISAMB     FLAG TO RETRIEVE SEGMENT DISTRIBUTION  *
      *B/IF                     AMBIT CODE.                            *
      *B.IF   FLG-ADMITFCC      FLAG TO RETRIEVE ONLY ADMITED FOREING  *
      *B/IF                     CURRENCIES.                            *
      *B.IF   FLG-MULBALFCC     FLAG TO RETRIEVE ONLY CURRENCIES OF    *
      *B/IF                     MULTICURRENCY BALANCES.                *
      *B.IF   FLG-ADMITSERV     FLAG TO RETRIEVE ONLY ADMITED SERVICES *
      *B.IF   FLG-NALLWOPE      FLAG TO RETRIEVE ONLY NOT ALLOWED OPER.*
      *B.IF   FLG-NCBA          FLAG TO RETRIEVE ONLY DESTINATION RANKS*
      *B.IF   FLG-COMMINDI      FLAG TO RETRIEVE ONLY COMMERCIAL FLAGS *
      *B.IF   FLG-SHARESCHEME   FLAG TO RETRIEVE ONLY SHARE SCHEME.    *
      *B.OF OUTPUT               OUTPUT DATA                           *
      *B.OF   ERROR              ERROR                                 *
      *B.OF     DES-PGMERROR     PROGRAM ERROR CODE                    *
      *B.OF     COD-SQLCODERROR  SQLCODE ERROR CODE                    *
      *B.OF     DES-TBLERROR     TABLE ERROR DESCRIPTION               *
      *B.OF     COD-ABEND        RETURN CODE                           *
      *B/OF                         00 RETURN OK                       *
      *B/OF                         05 VALIDATION ERROR                *
      *B/OF                         10 PRODUCT-BYPROD. NOT FOUND       *
      *B/OF                         20 ERROR ACCESS TO EXTERNAL ROUTINE*
      *B/OF                         30 OVERFLOW INTERNAL TABLES        *
      *B/OF                         31 ERROR CALL WP9C0020             *
      *B/OF                         32 ERROR CALL WP9C0030             *
      *B/OF                         33 ERROR CALL WP9C0040             *
      *B/OF                         34 ERROR CALL WP9C0050             *
      *B/OF                         99 ERROR DB2                       *
      *B.OF     TXT-AUXFLD       AUXILIAR FIELD                        *
      *B.OF   DATA                                                     *
      *B.OF     COD-APPBLG       BELONG TO APPLICATION                 *
      *B.OF     DAT-VALBEG       BEGIN OF VALIDATE DATE                *
      *B.OF     DAT-VALEND       END OF VALIDATE DATE                  *
      *B.OF     DES-PRODUCT      PRODUCT DESCRIPTION                   *
      *B.OF     SDE-PRODUCT      PRODUCT SHORT DESCRIPTION             *
      *B.OF     DES-SUBPRODUCT   BYPRODUCT DESCRIPTION                 *
      *B.OF     SDE-SUBPRODUCT   BYPRODUCT SHORT DESCRIPTION           *
      *B.OF     SW-FLG-KDPROD    KIND OF PRODUCT                       *
      *B.FV       SW-KDP-ASSET               ASSET                     *
      *B.FV       SW-KDP-LIABILITY           LIABILITY                 *
      *B.FV       SW-KDP-DISINTERMEDIATION   DESINTERMEDIATION         *
      *B.OF     SW-FLG-TIPPROD   TYPE OF PRODUCT FLAG                  *
      *B.FV       SW-TIP-TERM                TERM                      *
      *B.FV       SW-TIP-SIGCURACC           SIGHT CURRENT ACCOUNT     *
      *B.FV       SW-TIP-DMDSAVACC           DEMAND SAVINGS ACCOUNT    *
      *B.FV       SW-TIP-CREDIT              CREDIT                    *
      *B.FV       SW-TIP-LOAN                LOAN                      *
      *B.FV       SW-TIP-FUND                FUND                      *
      *B.FV       SW-TIP-VOSACC              VOSTRO ACCOUNT            *
      *B.FV       SW-TIP-CLEACC              CLEARING ACCOUNT          *
      *B.FV       SW-TIP-ENTACC              ENTITY ACCOUNT            *
      *B.FV       SW-TIP-BNK-GUA             BANK GUARANTEE            *
      *B.OF     SW-FLG-STPROD    PRODUCT STATUS FLAG                   *
      *B.FV       SW-STP-OFFER-ACTIVE        OFFER AND ACTIVE          *
      *B.FV       SW-STP-ACTIVE-NO-OFFER     ACTIVE NO OFFER           *
      *B.FV       SW-STP-INACTIVE-NO-OFFER   INACTIVE NO OFFER         *
      *B.FV       SW-STP-NO-AVAILABLE        NO AVAILABLE              *
      *B.OF     DAT-CHANGEOFFER  PRODUCT STATUS CHANGE DATE            *
      *B.OF     FLG-MULTIFCC     MULTICURRENCY FLAG                    *
      *B.OF     FLG-INDIVIDUAL   INDIVIDUAL FLAG                       *
      *B.OF     FLG-ENT-LEGAL    LEGAL ENTITY FLAG                     *
      *B.OF     FLG-PUBLIC       PUBLIC FLAG                           *
      *B.OF     FLG-RESIDENT     RESIDENT FLAG                         *
      *B.OF     FLG-NO-RESIDENT  NON-RESIDENT FLAG                     *
      *B.OF     FLG-SPECOND      SPECIFIC CONDITIONS FLAG              *
      *B.OF     NUM-MINTERM      MINIMUN TERM                          *
      *B.OF     NUM-MAXTERM      MAXIMUN TERM                          *
      *B.OF     SW-FLG-PURPOSE   PURPOSE FLAG                          *
      *B.FV       SW-PUR-INVESTMENT          INVESTMENT                *
      *B.FV       SW-PUR-FINANCIN            FINANCING                 *
      *B.FV       SW-PUR-MONETRAN            MONEY TRANSACTIONS.       *
      *B.FV       SW-PUR-RISKCOVER           RISK COVER                *
      *B.OF     FLG-TERMCST      TERM CUSTOMIZED FLAG                  *
      *B.OF     FLG-STDTERM      STANDARD TERM FLAG                    *
      *B.OF     FLG-QUOTA        QUOTA FLAG                            *
      *B.OF     FLG-CREDENT      CREDIT ENTITIES FLAG                  *
      *B.OF     COD-CONTCHAN     CONTRACTOR CHANEL                     *
      *B.OF     COD-RESTOPERCHAN RESTRICTYED CHANEL                    *
      *B.OF     COD-GEOADISAMB   GEOGRAPHICAL AREA DISTRIBUTION AMBIT  *
      *B/OF                      CODE                                  *
      *B.OF     COD-CENDISAMB    CENTRE DISTRIBUTION AMBIT CODE        *
      *B.OF     COD-SEGDISAMB    SEGMENT DISTRIBUTION AMBIT CODE       *
      *B.OF     COD-ADMITFCC     ADMITED FOREING CURRENCIES            *
      *B.OF     COD-MULBALFCC    CURRENCIES FOR MULTIPLES BALANCES     *
      *B.OF     COD-SERV         ADMITED SERVICES                      *
      *B.OF       COD-ADMITSERV      ADMITED SERVICES CODE             *
      *B.OF       COD-MNEMOSERV      ADMITED SERVICES MNEMOTECNIC      *
      *B.OF     COD-OPE          RESTRICTED OPERATIONS                 *
      *B.OF       COD-NALLWOPE       RESTRICTED OPERATIONS CODE        *
      *B.OF       COD-MNEMOOPE       RESTRICTED OPERATIONS MNEMOTECNIC *
      *B.OF     ADMITSERV        ADMITED SERVICES FLAG                 *
      *B.FV       SW-FLG-CREDITCARD          CREDIT CARD               *
      *B.FV       SW-FLG-TRANSFER            TRANSFER                  *
      *B.FV       SW-FLG-CHECKBOOK           CHECK BOOK                *
      *B.FV       SW-FLG-DIRECTDEBIT         DIRECT DEBIT              *
      *B.FV       SW-FLG-EXTDIRECTDEBIT      EXTERNAL DIRECT DEBIT     *
      *B.FV       SW-FLG-PAYROLL             PAYROLL                   *
      *B.FV       SW-FLG-PENSION             PENSION                   *
      *B.FV       SW-FLG-INSURANCE           INSURANCE                 *
      *B.FV       SW-FLG-PRN                 PROMISSORY NOTES          *
      *B.OF     NALLWOPE         NOT ALLOWED OPERATION FLAG            *
      *B.FV       SW-FLG-RENEWAL             RENEWAL                   *
      *B.FV       SW-FLG-AUTCAN              AUTOMATIC CANCELLATION    *
      *B.FV       SW-FLG-MANCAN              MANUAL CANCELLATION       *
      *B.FV       SW-FLG-EARLCAN             EARLY CANCELLATION        *
      *B.FV       SW-FLG-LIMITREDU           LIMIT REDUCTION           *
      *B.FV       SW-FLG-SETTCONDCHG         SETTLEMENT CONDITIONS CHAN*
      *B.FV       SW-FLG-PRODCHG             PRODUCT CHANGE            *
      *B.FV       SW-FLG-CENCHG              CENTRE CHANGE             *
      *B.FV       SW-FLG-PARTICCHG           PARTICIPATION CHANGE      *
      *B.FV       SW-FLG-FLWWDRW             FOLLOWING WITHDRAWALS     *
      *B.FV       SW-FLG-ADVREPA             EARLY REPAYMENT           *
      *B.FV       SW-FLG-CANPAR              PARTIAL CANCELLATION      *
      *B.FV       SW-FLG-SURRGATION          SUBROGATION               *
      *B.FV       SW-FLG-NEWAL               RENEWAL                   *
      *B.FV       SW-FLG-WAITGUAR            CLEARING GUARANTEES       *
      *B.OF     COD-NCBA         NCBA CODE                             *
      *B.OF     COD-COMMINDI     COMMERCIAL FLAGS CODES                *
      *B.OF     FLG-COLLECTIVE   COLLECTIVE FLAG                       *
      *B.OF     COD-SHARESCHEME  SHARE SCHEME                          *
      *B.OF     COD-SHARESEQ     SHARE SEQUENCE                        *
      *B.OF     FLG-SHARE        PARTICIPANT                           *
      *B.OF     NUM-SEQUENCE     SEQUENCE                              *
      *B.OF     SW-FLG-NEEDTYPE  NEED OF TYPE                          *
      *B.FV       SW-NEE-MANDATORI           MANDATORY                 *
      *B.FV       SW-NEE-OPTIONAL            OPTIONAL                  *
      *B.OF     NUM-MAXSHARE     MAX.NUMBER OF PARTICIPANTS            *
      *B.OF     SW-FLG-NATURESHARE  KIND OF PARTICIPANT                *
      *B.FV       SW-NAT-PHYSICAL            INDIVIDUAL                *
      *B.FV       SW-NAT-JURIDICAL           LEGAL                     *
      *B.OF     FLG-RESIDENCE    RESIDENT FLAG                         *
      *B.OF     TYP-RELATEDSHARE TYPE OF RELATED PARTICIPANT           *
      ******************************************************************
       02  WPWC0010.
         05  W001-INPUT.
           10  W001-COD-ENTITY                  PIC X(4).
           10  W001-COD-PRODUCT                 PIC X(2).
           10  W001-COD-SUBPRODUCT              PIC X(4).
           10  W001-COD-FCURRENCY               PIC X(3).
           10  W001-DAT-OPERATION               PIC X(10).
           10  W001-COD-LANGUAGE                PIC X(1).
           10  W001-COD-OPTION                  PIC X(1).
           10  W001-FLG-MASTER                  PIC X(1).
           10  W001-FLG-DESCRIPTION             PIC X(1).
           10  W001-FLG-CONTCHAN                PIC X(1).
           10  W001-FLG-RESTOPERCHAN            PIC X(1).
           10  W001-FLG-GEOADISAMB              PIC X(1).
           10  W001-FLG-CENDISAMB               PIC X(1).
           10  W001-FLG-SEGDISAMB               PIC X(1).
           10  W001-FLG-ADMITFCC                PIC X(1).
           10  W001-FLG-MULBALFCC               PIC X(1).
           10  W001-FLG-ADMITSERV               PIC X(1).
           10  W001-FLG-NALLWOPE                PIC X(1).
           10  W001-FLG-NCBA                    PIC X(1).
           10  W001-FLG-COMMINDI                PIC X(1).
           10  W001-FLG-SHARESCHEME             PIC X(1).
           10  FILLER                           PIC X(20).
         05  W001-OUTPUT.
           10  W001-ERROR.
             15  W001-DES-PGMERROR              PIC X(8).
             15  W001-COD-SQLCODERROR           PIC S9(9)  COMP.
             15  W001-DES-TBLERROR              PIC X(8).
             15  W001-COD-ABEND                 PIC X(2).
             15  W001-TXT-AUXFLD                PIC X(20).
           10  W001-DATA.
             15  W001-COD-APPBLG              PIC X(2).
             15  W001-DAT-VALBEG              PIC X(10).
             15  W001-DAT-VALEND              PIC X(10).
             15  W001-DES-PRODUCT             PIC X(50).
             15  W001-SDE-PRODUCT             PIC X(15).
             15  W001-DES-SUBPRODUCT          PIC X(50).
             15  W001-SDE-SUBPRODUCT          PIC X(15).
             15  W001-SW-FLG-KDPROD           PIC X.
               88  W001-SW-KDP-ASSET                VALUE  'A'.
               88  W001-SW-KDP-LIABILITY            VALUE  'P'.
               88  W001-SW-KDP-DISINTERMEDIATION    VALUE  'C'.
             15  W001-SW-FLG-TIPPROD          PIC X.
               88  W001-SW-TIP-TERM                 VALUE  'A'.
               88  W001-SW-TIP-SIGCURACC            VALUE  'B'.
               88  W001-SW-TIP-DMDSAVACC            VALUE  'C'.
               88  W001-SW-TIP-CREDIT               VALUE  'D'.
               88  W001-SW-TIP-LOAN                 VALUE  'E'.
               88  W001-SW-TIP-FUND                 VALUE  'F'.
               88  W001-SW-TIP-VOSACC               VALUE  'G'.
               88  W001-SW-TIP-CLEACC               VALUE  'H'.
               88  W001-SW-TIP-ENTACC               VALUE  'I'.
      *.MC.S @V111754DVG
               88  W001-SW-TIP-BNK-GUA              VALUE  'J'.
      *.MC.E @V111754DVG
             15  W001-SW-FLG-STPROD           PIC X.
               88  W001-SW-STP-OFFER-ACTIVE         VALUE  'O'.
               88  W001-SW-STP-ACTIVE-NO-OFFER      VALUE  'A'.
               88  W001-SW-STP-INACTIVE-NO-OFFER    VALUE  'I'.
               88  W001-SW-STP-NO-AVAILABLE         VALUE  'N'.
             15  W001-DAT-CHANGEOFFER         PIC X(10).
             15  W001-FLG-MULTIFCC            PIC X.
             15  W001-FLG-INDIVIDUAL          PIC X.
             15  W001-FLG-ENT-LEGAL           PIC X.
             15  W001-FLG-PUBLIC              PIC X.
             15  W001-FLG-RESIDENT            PIC X.
             15  W001-FLG-NO-RESIDENT         PIC X.
             15  W001-FLG-SPECOND             PIC X.
      *
             15  W001-NUM-MINTERM             PIC  9(4).
             15  W001-NUM-MAXTERM             PIC  9(4).
             15  W001-SW-FLG-PURPOSE          PIC X.
               88  W001-SW-PUR-INVESTMENT           VALUE '1'.
               88  W001-SW-PUR-FINANCIN             VALUE '2'.
               88  W001-SW-PUR-MONETRAN             VALUE '3'.
               88  W001-SW-PUR-RISKCOVER            VALUE '4'.
             15  W001-FLG-TERMCST             PIC X.
             15  W001-FLG-STDTERM             PIC X.
             15  W001-FLG-QUOTA               PIC X.
             15  W001-FLG-CREDENT             PIC X.
             15  W001-COD-CONTCHAN     OCCURS 10  TIMES   PIC X(2).
             15  W001-COD-RESTOPERCHAN OCCURS 10  TIMES   PIC X(2).
             15  W001-COD-GEOADISAMB   OCCURS 100 TIMES   PIC X(4).
             15  W001-COD-CENDISAMB    OCCURS 30  TIMES   PIC X(4).
             15  W001-COD-SEGDISAMB    OCCURS 20  TIMES   PIC X(2).
             15  W001-COD-ADMITFCC     OCCURS 10  TIMES   PIC X(3).
             15  W001-COD-MULBALFCC    OCCURS 10  TIMES   PIC X(3).
      *
             15  W001-COD-SERV         OCCURS 20 TIMES.
               20  W001-COD-ADMITSERV         PIC X(6).
               20  W001-COD-MNEMOSERV         PIC X(10).
             15  W001-COD-OPE          OCCURS 20 TIMES.
               20  W001-COD-NALLWOPE          PIC X(6).
               20  W001-COD-MNEMOOPE          PIC X(10).
             15  W001-ADMITSERV.
               20  W001-SW-FLG-CREDITCARD     PIC X(1).
                   88  W001-SW-CRE-YES              VALUE '1'.
               20  W001-SW-FLG-TRANSFER       PIC X(1).
                   88  W001-SW-TRA-YES              VALUE '1'.
               20  W001-SW-FLG-CHECKBOOK      PIC X(1).
                   88  W001-SW-CHE-YES              VALUE '1'.
               20  W001-SW-FLG-DIRECTDEBIT    PIC X(1).
                   88  W001-SW-DIR-YES              VALUE '1'.
               20  W001-SW-FLG-EXTDIRECTDEBIT PIC X(1).
                   88  W001-SW-EXT-YES              VALUE '1'.
               20  W001-SW-FLG-PAYROLL        PIC X(1).
                   88  W001-SW-PAY-YES              VALUE '1'.
               20  W001-SW-FLG-PENSION        PIC X(1).
                   88  W001-SW-PEN-YES              VALUE '1'.
               20  W001-SW-FLG-INSURANCE      PIC X(1).
                   88  W001-SW-INS-YES              VALUE '1'.
               20  W001-SW-FLG-PRN            PIC X(1).
                   88  W001-SW-PRN-YES              VALUE '1'.
               20  FILLER                     PIC X(20).
             15  W001-NALLWOPE.
               20  W001-SW-FLG-RENEWAL        PIC X(1).
                   88  W001-SW-REN-YES              VALUE '1'.
               20  W001-SW-FLG-AUTCAN         PIC X(1).
                   88  W001-SW-AUT-YES              VALUE '1'.
               20  W001-SW-FLG-MANCAN         PIC X(1).
                   88  W001-SW-MAN-YES              VALUE '1'.
               20  W001-SW-FLG-EARLCAN        PIC X(1).
                   88  W001-SW-EAR-YES              VALUE '1'.
               20  W001-SW-FLG-LIMITREDU      PIC X(1).
                   88  W001-SW-LIM-YES              VALUE '1'.
               20  W001-SW-FLG-SETTCONDCHG    PIC X(1).
                   88  W001-SW-SET-YES              VALUE '1'.
               20  W001-SW-FLG-PRODCHG        PIC X(1).
                   88  W001-SW-PRO-YES              VALUE '1'.
               20  W001-SW-FLG-CENCHG         PIC X(1).
                   88  W001-SW-CEN-YES              VALUE '1'.
               20  W001-SW-FLG-PARTICCHG      PIC X(1).
                   88  W001-SW-PAR-YES              VALUE '1'.
               20  W001-SW-FLG-FLWWDRW        PIC X(1).
                   88  W001-SW-FLW-YES              VALUE '1'.
               20  W001-SW-FLG-ADVREPA        PIC X(1).
                   88  W001-SW-ADV-YES              VALUE '1'.
               20  W001-SW-FLG-CANPAR         PIC X(1).
                   88  W001-SW-CAN-YES              VALUE '1'.
               20  W001-SW-FLG-SURROGATIO     PIC X(1).
                   88  W001-SW-SUR-YES              VALUE '1'.
               20  W001-SW-FLG-NEWAL          PIC X(1).
                   88  W001-SW-NEW-YES              VALUE '1'.
               20  W001-SW-FLG-WAITGUAR       PIC X(1).
                   88  W001-SW-WAI-YES              VALUE '1'.
               20  FILLER                     PIC X(20).
             15  W001-COD-NCBA        OCCURS 10 TIMES.
               20  W001-COD-NCBA1             PIC X(10).
               20  W001-COD-NCBA2             PIC X(10).
             15  W001-COD-COMMINDI    OCCURS 20 TIMES    PIC X(4).
             15  W001-FLG-COLLECTIVE          PIC X.
             15  W001-COD-SHARESCHEME.
               20  W001-COD-SHARESEQ  OCCURS  100  TIMES.
                 25  W001-FLG-SHARE           PIC X.
                 25  W001-NUM-SEQUENCE        PIC S9(3).
                 25  W001-SW-FLG-NEEDTYPE     PIC X.
                   88  W001-SW-NEE-MANDATORI        VALUE  'M'.
                   88  W001-SW-NEE-OPTIONAL         VALUE  'O'.
                 25  W001-NUM-MAXSHARE        PIC S9(3).
                 25  W001-SW-FLG-NATURESHARE  PIC X.
                   88  W001-SW-NAT-PHYSICAL         VALUE  'P'.
                   88  W001-SW-NAT-JURIDICAL        VALUE  'L'.
                 25  W001-FLG-RESIDENCE       PIC X.
                 25  W001-TYP-RELATEDSHARE    PIC X.
           10  FILLER                           PIC X(19).
