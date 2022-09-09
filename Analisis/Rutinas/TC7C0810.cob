      * TC7C0810:                                                      *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    TC7C0810.
      *
       AUTHOR.        ALNOVA TECHNOLOGIES CORPORATION.
      *
       DATE-WRITTEN.  2001-07-18.
      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      *.TC  @AS0001     JASL   29-04-04 ACCESO DIRECTO A LAS TABLAS    *
      *                                 TCDT080 Y TCDT081              *
      *A/MD @AS0002DBG  JASL   06/08/04 OPTIMIZAR ACCESO A TABLAS      *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SOURCE-COMPUTER. IBM-4381.
      *
       OBJECT-COMPUTER. IBM-4381.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      ******************************************************************
      *                       DATA DIVISION                            *
      ******************************************************************
       DATA DIVISION.
      *
      ******************************************************************
      *                       FILE SECTION                             *
      ******************************************************************
       FILE SECTION.
      *
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
            COPY QAWCSQL.
      *
           EXEC SQL
                INCLUDE TCGV0811
           END-EXEC.
      *
           EXEC SQL
                INCLUDE TCGV0800
           END-EXEC.
      *
           COPY QGECFEC.
      *
       COPY QCWCL20.
      *
       01  VA-WORKINGVAR.
           02  VN-NULLS                     PIC S9(4) COMP.
           02  VN-COUNTER                   PIC 9(2).
           02  SW-DD-CHG                    PIC X.
           02  SW-CONS                      PIC X.
           02  SW-ERROR                     PIC X.
           02  VA-FCC                       PIC X(3).
      *
           02  VA-DATE.
               03  VA-YY-DATE               PIC 9(4).
               03  CA-DAT-DASH1             PIC X    VALUE '-'.
               03  VA-MM-DATE               PIC 9(2).
               03  VA-DAT-DASH2             PIC X    VALUE '-'.
               03  VA-DD-DATE               PIC 9(2).
      *
           02  VA-DAT-NUM.
               03  VN-DD-DATNUM             PIC 9(2).
               03  VN-MM-DATNUM             PIC 9(2).
               03  VN-YY-DATNUM             PIC 9(4).
      *
           02  VA-DAT-G.
               03  VN-YY-DATG               PIC 9(4).
               03  VA-MM-DATG               PIC 9(2).
               03  VA-DD-DATG               PIC 9(2).
      *
           02  VA-DAT-ANNULG.
               03  VN-YY-DATANNULG          PIC 9(4).
               03  VN-MM-DATANNULG2         PIC 9(2).
               03  VN-DD-DATANNULG          PIC 9(2).
      *
           02  VA-DATE2.
               03  VN-YY-DAT2               PIC 9(4).
               03  CA-DAT-DASH3             PIC X    VALUE '-'.
               03  VN-MM-DATE2              PIC 9(2).
               03  CA-DAT-DASH4             PIC X    VALUE '-'.
               03  VN-DD-DATE2              PIC 9(2).
      *
           02  VA-DAT-ANNUL.
               03  VA-YY-DATANNUL           PIC 9(4).
               03  CA-DAT-ANNULPOINT1       PIC X    VALUE '.'.
               03  VA-MM-DATANNUL2          PIC 9(2).
               03  CA-DAT-ANNULPOINT2       PIC X    VALUE '.'.
               03  VA-DD-DATANN2            PIC 9(2).
      *
       01      CN-NUM-0                     PIC S9 COMP-3 VALUE +0.
       01      CN-NUM-1                     PIC S9 COMP-3 VALUE +1.
       01      CN-NUM-MINUS1                PIC S9 COMP-3 VALUE -1.
       01      CN-NUM-2                     PIC S9 COMP-3 VALUE +2.
       01      CN-NUM-35                    PIC S99 COMP-3 VALUE +35.
       01      CA-00                        PIC X(2)     VALUE '00'.
       01      CA-5                         PIC XX       VALUE '05'.
       01      CA-10                        PIC XX       VALUE '10'.
       01      CA-15                        PIC XX       VALUE '15'.
       01      CA-20                        PIC XX       VALUE '20'.
       01      CA-25                        PIC XX       VALUE '25'.
       01      CA-30                        PIC XX       VALUE '30'.
       01      CA-35                        PIC XX       VALUE '35'.
       01      CA-40                        PIC XX       VALUE '40'.
       01      CA-45                        PIC XX       VALUE '45'.
       01      CA-50                        PIC XX       VALUE '50'.
       01      CA-55                        PIC XX       VALUE '55'.
       01      CA-60                        PIC XX       VALUE '60'.
       01      CA-85                        PIC XX       VALUE '85'.
       01      CA-99                        PIC XX       VALUE '99'.
       01      CA-TC                        PIC XX       VALUE 'TC'.
      *
       01      CA-TCDT081                 PIC X(7)     VALUE 'TCDT081'.
       01      CA-TCDT080                 PIC X(7)     VALUE 'TCDT080'.
       01      CA-QG9CSWA0                PIC X(8)     VALUE 'QG9CSWA0'.
       01      CA-KEY-CARD                PIC X(8)     VALUE 'ICONVIVE'.
      *
       01      CA-NO                        PIC X  VALUE 'N'.
       01      CA-YES                       PIC X  VALUE 'Y'.
       01      CA-S                         PIC X  VALUE 'S'.
      *
       01      CA-POINT                     PIC X  VALUE '.'.
       01      CA-DASH                      PIC X  VALUE '-'.
      *
       01      CA-D                         PIC X  VALUE 'D'.
       01      CA-B                         PIC X  VALUE 'B'.
       01      CA-C                         PIC X  VALUE 'C'.
       01      CA-X                         PIC X  VALUE 'X'.
       01      CA-Y                         PIC X  VALUE 'Y'.
       01      CA-Z                         PIC X  VALUE 'Z'.
       01      CA-T                         PIC X  VALUE 'T'.
       01      CA-G                         PIC X  VALUE 'G'.
      *
       01  CA-IN                            PIC X(3)    VALUE 'INC'.
       01  CA-OUT                           PIC X(3)    VALUE 'OUT'.
      *
       01  CA-RCC2                          PIC X(3)    VALUE 'EUR'.
      *
       01  CA-DAT-RCC                       PIC X(10)
                                   VALUE '1999-01-01'.
      *
       01  SW-EURCTRY                       PIC X(1)   VALUE 'Y'.
           88  SW-ECY-YES                             VALUE 'Y'.
           88  SW-ECY-NO                          VALUE 'N'.
      *
       01 VA-TCEC2220.
          COPY TCEC2220.
      *
          COPY TCWC0110.
      *
       01 VA-TCEC3000.
          COPY TCEC3000.
      *
       COPY TCTC2241.
      *
       COPY TCTC2251.
      *
      ******************************************************************
      *                      LINKAGE SECTION                           *
      ******************************************************************
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
         COPY TCEC0810.
      *
         COPY QBEC999.
      *
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *
           PERFORM 1000-START.
      *
           IF SW-ERROR = CA-NO
             PERFORM GENERAL-PROCESS
           END-IF.
      *
           PERFORM 3000-END.
      *
      ******************************************************************
      *.PN 1000-START.                                                 *
      ******************************************************************
       1000-START.
      *
           INITIALIZE QAWCSQL.
           INITIALIZE TCGV0811.
           INITIALIZE TCGV0800.
      *
           INITIALIZE TCEC0810-OUTPUT.
      *
           INITIALIZE VA-WORKINGVAR.
      *
           MOVE ZEROS TO TCEC0810-SQLCODE.
           MOVE ZEROS TO TCEC0810-COD-RETURN.
      *
           MOVE CA-NO TO SW-ERROR.
      *
           PERFORM VALIDATE-INPUT-DATA.
      *
      ******************************************************************
      *.PN VALIDATE-INPUT-DATA.                                        *
      ******************************************************************
       VALIDATE-INPUT-DATA.
      *
           IF  (QBEC999-COD-ENTITY EQUAL SPACES OR ZEROS OR LOW-VALUES)
               OR
               (QBEC999-ENTITY-DATA EQUAL SPACES OR LOW-VALUES OR ZEROS)
               MOVE CA-10  TO TCEC0810-COD-RETURN
      *
               PERFORM 3000-END
           END-IF.
      *
           IF TCEC0810-COD-ENTITY-A NOT NUMERIC
              MOVE CA-60     TO TCEC0810-COD-RETURN
              MOVE CA-YES         TO SW-ERROR
           ELSE
      *
              IF TCEC0810-COD-ENTITY = 0 OR TCEC0810-COD-ENTITY =
                  LOW-VALUES
                 MOVE CA-10   TO TCEC0810-COD-RETURN
                 MOVE CA-YES       TO SW-ERROR
              END-IF
           END-IF.
      *
           IF SW-ERROR = CA-NO
              IF (TCEC0810-FLG-FCCB3 NOT = CA-D AND
                  TCEC0810-FLG-FCCB3 NOT = CA-B AND
                  TCEC0810-FLG-FCCB3 NOT = CA-X AND
                  TCEC0810-FLG-FCCB3 NOT = CA-Y AND
                  TCEC0810-FLG-FCCB3 NOT = CA-Z AND
                  TCEC0810-FLG-FCCB3 NOT = CA-T AND
                  TCEC0810-FLG-FCCB3 NOT = CA-G)
                  MOVE CA-15 TO TCEC0810-COD-RETURN
                  MOVE CA-YES     TO SW-ERROR
              END-IF
           END-IF.
      *
           IF SW-ERROR = CA-NO
               IF TCEC0810-COD-SWIFTFCCS = SPACES
                   MOVE CA-25 TO TCEC0810-COD-RETURN
                   MOVE CA-YES TO SW-ERROR
               ELSE
                   PERFORM VALIDATE-CHANGE-DATE
                   IF SW-ERROR = CA-NO
                       MOVE TCEC0810-COD-SWIFTFCCS TO VA-FCC
                       PERFORM INFORMED-CODE
                   END-IF
               END-IF
           END-IF.
      *
      ******************************************************************
      *.PN VALIDATE-CHANGE-DATE.                                       *
      ******************************************************************
       VALIDATE-CHANGE-DATE.
      *
           IF TCEC0810-DAT-EXCHANGE = SPACES
               MOVE CA-YES TO SW-DD-CHG
      *
               MOVE QBEC999-COD-ENTITY TO FEC-ENT-RTV
      *
               CALL CA-QG9CSWA0 USING QGECFEC
      *
               IF  FEC-RETURN NOT EQUAL TO ZEROS
                   MOVE 'QGDTSWA'      TO TCEC0810-DES-TABLE
                   MOVE CA-50     TO TCEC0810-COD-RETURN
                   MOVE CA-YES         TO SW-ERROR
               ELSE
                   MOVE FEC-DAT-A      TO V081-DAT-EXCHANGE
                   MOVE V081-DAT-EXCHANGE  TO VA-DATE2
               END-IF
           ELSE
               PERFORM INFORMED-CHANGE-DATE
           END-IF.
      *
      ******************************************************************
      *.PN INFORMED-CHANGE-DATE.                                       *
      ******************************************************************
       INFORMED-CHANGE-DATE.
      *
           MOVE TCEC0810-DAT-EXCHANGE(1:2)  TO   VA-DD-DATE
           MOVE TCEC0810-DAT-EXCHANGE(4:2)  TO   VA-MM-DATE
           MOVE TCEC0810-DAT-EXCHANGE(7:4)  TO   VA-YY-DATE
      *
           MOVE CA-DASH           TO   CA-DAT-DASH1
           MOVE CA-DASH           TO   VA-DAT-DASH2
      *
           IF  VA-DD-DATE IS NOT NUMERIC   OR
              VA-MM-DATE IS NOT NUMERIC    OR
              VA-YY-DATE IS NOT NUMERIC
               MOVE CA-10 TO TCEC0810-COD-RETURN
               MOVE CA-YES TO SW-ERROR
           ELSE
               PERFORM VERIFY-LOGIC-DATE
           END-IF.
      *
      ******************************************************************
      *.PN VERIFY-LOGIC-DATE.                                          *
      ******************************************************************
       VERIFY-LOGIC-DATE.
      *
           MOVE VA-DD-DATE TO VN-DD-DATNUM.
           MOVE VA-MM-DATE TO VN-MM-DATNUM.
           MOVE VA-YY-DATE TO VN-YY-DATNUM.
      *
           INITIALIZE TCEC2220-INPUT.
           INITIALIZE TCEC2220-OUTPUT.
      *
           MOVE CN-NUM-1  TO TCEC2220-COD-REQOPTION.
           MOVE VA-DAT-NUM TO TCEC2220-DAT-GGFMT1.
      *
           EXEC CICS
              LINK PROGRAM ('TC6C2220')
              COMMAREA (VA-TCEC2220)
              LENGTH (TCEC2220-CPYLTH)
           END-EXEC
      *
           IF TCEC2220-COD-RETURN = CA-10
               MOVE CA-10 TO TCEC0810-COD-RETURN
               MOVE CA-YES TO SW-ERROR
           ELSE
               IF TCEC2220-COD-RETURN NOT = CA-00
                  MOVE CA-85 TO TCEC0810-COD-RETURN
                  MOVE CA-YES     TO SW-ERROR
               END-IF
           END-IF.
      *
      ******************************************************************
      *.PN INFORMED-CODE.                                              *
      ******************************************************************
       INFORMED-CODE.
      *
           MOVE VA-FCC      TO V080-COD-FCC.
           MOVE QBEC999-COD-ENTITY TO V080-COD-ENTITY
      *
           EXEC SQL
              SELECT DAT_ANN,
                     FLG_FCCQUOT,
                     EXCHUNIT1,
                     FLG_BKNQUOT,
                     FLG_BLGEMU,
                     EMUEXCHVA,
                     DAT_EMUENTR
                INTO :V080-DAT-ANN:VN-NULLS,
                     :V080-FLG-FCCQUOT,
                     :V080-EXCHUNIT1,
                     :V080-FLG-BKNQUOT,
                     :V080-FLG-BLGEMU,
                     :V080-EMUEXCHVA,
                     :V080-DAT-EMUENTR
      *.TC  @AS0001  I  JASL
      *         FROM TCDV0800
      *A/MD @AS0002DBG  I  JASL
      *         FROM TCDT080
                FROM TCDT080 with (index = BA0TC080, nolock)
      *A/MD @AS0002DBG  F  JASL
      *.TC  @AS0001  F  JASL
                WHERE COD_FCC = :V080-COD-FCC
                  AND COD_ENTITY = :V080-COD-ENTITY
           END-EXEC.
      *
           MOVE CA-TCDT080 TO TCEC0810-DES-TABLE.
           PERFORM DB2CHECK.
      *
           IF SQL-88-NOT-FOUND
               MOVE CA-20         TO TCEC0810-COD-RETURN
               MOVE CA-YES         TO SW-ERROR
           ELSE
               IF SQL-88-OK
                   PERFORM FOREIGN-CURRENCY-EXISTENCE
               END-IF
           END-IF.
      *
      ******************************************************************
      *.PN FOREIGN-CURRENCY-EXISTENCE.                                 *
      ******************************************************************
       FOREIGN-CURRENCY-EXISTENCE.
      *
           IF (V080-FLG-FCCQUOT = CA-NO AND
               TCEC0810-FLG-FCCB3 = CA-D) OR
              (V080-FLG-FCCQUOT = CA-NO AND
               TCEC0810-FLG-FCCB3 = CA-T) OR
              (V080-FLG-FCCQUOT = CA-NO AND
               TCEC0810-FLG-FCCB3 = CA-G) OR
              (V080-FLG-BKNQUOT = CA-NO AND
               TCEC0810-FLG-FCCB3 = CA-B) OR
              (V080-FLG-BKNQUOT = CA-NO AND
               TCEC0810-FLG-FCCB3 = CA-X)
              MOVE CA-35 TO TCEC0810-COD-RETURN
              MOVE CA-YES TO SW-ERROR
           END-IF.
      *
           IF SW-ERROR = CA-NO
               IF SW-DD-CHG = CA-YES
                   MOVE VN-YY-DAT2 TO VN-YY-DATG
                   MOVE VN-MM-DATE2 TO VA-MM-DATG
                   MOVE VN-DD-DATE2 TO VA-DD-DATG
               ELSE
                   MOVE VA-YY-DATE TO VN-YY-DATG
                   MOVE VA-MM-DATE TO VA-MM-DATG
                   MOVE VA-DD-DATE TO VA-DD-DATG
               END-IF
               IF VN-NULLS = CN-NUM-0
                  MOVE V080-DAT-ANN TO VA-DAT-ANNUL
                  MOVE VA-YY-DATANNUL TO VN-YY-DATANNULG
                  MOVE VA-MM-DATANNUL2 TO VN-MM-DATANNULG2
                  MOVE VA-DD-DATANN2 TO VN-DD-DATANNULG
                  IF VA-DAT-ANNULG < VA-DAT-G
                     MOVE CA-30 TO TCEC0810-COD-RETURN
                     MOVE CA-YES TO SW-ERROR
                  END-IF
               END-IF
           END-IF.
      *
      ******************************************************************
      *.PN GENERAL-PROCESS.                                            *
      ******************************************************************
       GENERAL-PROCESS.
      *
           MOVE TCEC0810-COD-ENTITY TO V081-COD-ENTITY.
           MOVE TCEC0810-COD-SWIFTFCCS TO V081-COD-FCC.
           MOVE TCEC0810-FLG-FCCB3 TO V081-FLG-FCCB.
      *
           IF SW-DD-CHG = CA-YES
               MOVE VA-DATE2       TO V081-DAT-EXCHANGE
           ELSE
               MOVE VA-DATE       TO V081-DAT-EXCHANGE
           END-IF.
      *
           EVALUATE  V080-FLG-BLGEMU
               WHEN  CA-IN
                   IF  V081-DAT-EXCHANGE GREATER THAN V080-DAT-EMUENTR
                       PERFORM  3000-FOREIGN-CURRENCY-EMU
                          THRU  3000-FOREIGN-CURRENCY-EMU-EXIT
                   ELSE
                       MOVE TCEC0810-COD-SWIFTFCCS  TO VA-FCC
                       PERFORM  OBTAIN-CHANGES
                   END-IF
               WHEN  CA-OUT
                     MOVE TCEC0810-COD-SWIFTFCCS  TO VA-FCC
                     PERFORM OBTAIN-CHANGES
      *
               WHEN  OTHER
                     MOVE  CA-YES TO SW-ERROR
      *
           END-EVALUATE.
      *
           IF SQL-88-OK AND SW-ERROR = CA-NO
               PERFORM CHANGES-EXISTENCE
           ELSE
               MOVE CA-50 TO TCEC0810-COD-RETURN
           END-IF.
      *
      ******************************************************************
      *.PN 3000-FOREIGN-CURRENCY-EMU.                                  *
      ******************************************************************
       3000-FOREIGN-CURRENCY-EMU.
      *
           IF  QBEC999-88-FLGYESEURCTRY
               MOVE  V080-EMUEXCHVA TO V081-OFFERRATE
                                      V081-BIDRATE
                                      V081-FIXRATE
           ELSE
               MOVE CA-RCC2       TO VA-FCC
               PERFORM OBTAIN-CHANGES
               SET SW-ECY-NO    TO TRUE
      *
               IF  QBEC999-88-FLGDIRCHG
                  COMPUTE V081-OFFERRATE = V081-OFFERRATE /
                      V080-EMUEXCHVA
                  COMPUTE V081-BIDRATE = V081-BIDRATE / V080-EMUEXCHVA
                  COMPUTE V081-FIXRATE  = V081-FIXRATE  / V080-EMUEXCHVA
               ELSE
                  COMPUTE V081-OFFERRATE = V080-EMUEXCHVA *
                      V081-OFFERRATE
                  COMPUTE V081-BIDRATE = V080-EMUEXCHVA * V081-BIDRATE
                  COMPUTE V081-FIXRATE  = V080-EMUEXCHVA * V081-FIXRATE
               END-IF
      *
           END-IF.
      *
      ******************************************************************
      *.PN 3000-FOREIGN-CURRENCY-EMU-EXIT.                             *
      ******************************************************************
       3000-FOREIGN-CURRENCY-EMU-EXIT.
      *
           EXIT.
      *
      ******************************************************************
      *.PN OBTAIN-CHANGES.                                             *
      ******************************************************************
       OBTAIN-CHANGES.
      *
           MOVE VA-FCC          TO V081-COD-FCC
      *
           IF SW-DD-CHG = CA-YES
               MOVE TCEC0810-COD-ENTITY TO V081-COD-ENTITY
               MOVE TCEC0810-FLG-FCCB3 TO V081-FLG-FCCB
             EXEC SQL
               SELECT MAX(DAT_EXCHANGE)
                INTO :V081-DAT-EXCHANGE:Y081-DAT-EXCHANGE
      *.TC  @AS0001  I  JASL
      *         FROM TCDV0811
      *A/MD @AS0002DBG  I  JASL
      *         FROM TCDT081
                FROM TCDT081 with (index = BA0TC081, nolock)
      *A/MD @AS0002DBG  F  JASL
      *.TC  @AS0001  F  JASL
                WHERE COD_ENTITY = :V081-COD-ENTITY
                 AND  COD_FCC = :V081-COD-FCC
                 AND  FLG_FCCB3 = :V081-FLG-FCCB
             END-EXEC
             MOVE CA-TCDT081 TO TCEC0810-DES-TABLE
             PERFORM DB2CHECK
           END-IF
           EXEC SQL
              SELECT OFFERRATE,
                     BIDRATE,
                     FIXRATE,
                     DAT_EXCHANGE,
                     FLG_QUOTATION
                INTO :V081-OFFERRATE,
                     :V081-BIDRATE,
                     :V081-FIXRATE,
                     :V081-DAT-EXCHANGE,
                     :V081-FLG-QUOTATION
      *.TC  @AS0001  I  JASL
      *         FROM TCDV0811
      *A/MD @AS0002DBG  I  JASL
      *         FROM TCDT081
                FROM TCDT081 with (index= BA0TC081, nolock)
      *A/MD @AS0002DBG  I  JASL
      *.TC  @AS0001  F  JASL
                WHERE COD_ENTITY = :V081-COD-ENTITY
                  AND DAT_EXCHANGE = :V081-DAT-EXCHANGE
                  AND COD_FCC = :V081-COD-FCC
                  AND FLG_FCCB3 = :V081-FLG-FCCB
           END-EXEC.
           MOVE CA-TCDT081 TO TCEC0810-DES-TABLE.
           PERFORM DB2CHECK.
      *
      ******************************************************************
      *.PN CHANGES-EXISTENCE.                                          *
      ******************************************************************
       CHANGES-EXISTENCE.
      *
           IF SW-DD-CHG = CA-YES
              MOVE V081-DAT-EXCHANGE TO TCEC0810-DAT-LSTCHANGE
           END-IF.
      *
           IF ((V081-OFFERRATE = CN-NUM-0         OR
                V081-BIDRATE = CN-NUM-0) AND
               (V081-FLG-QUOTATION          = CA-S     OR
                V081-FLG-QUOTATION          = CA-C ))
                MOVE CA-55 TO TCEC0810-COD-RETURN
           ELSE
              IF (V081-OFFERRATE NOT = CN-NUM-0         OR
                  V081-BIDRATE NOT = CN-NUM-0) AND
                  V081-FLG-QUOTATION = CA-C
                  MOVE CA-40 TO TCEC0810-COD-RETURN
              ELSE
                  IF  SW-ECY-NO
                      MOVE CA-RCC2      TO VA-FCC
                      PERFORM INFORMED-CODE
                  END-IF
      *
                  MOVE V080-EXCHUNIT1 TO TCEC0810-EXCHUNIT1
                  MOVE V081-OFFERRATE TO TCEC0810-OFFERRATE
                  MOVE V081-BIDRATE TO TCEC0810-BIDRATE
                  MOVE V081-FIXRATE  TO TCEC0810-FIXRATE
                  MOVE V081-FLG-QUOTATION   TO TCEC0810-FLG-QUOTE
                  MOVE V081-DAT-EXCHANGE TO TCEC0810-DAT-LSTCHANGE
                  IF V081-FLG-QUOTATION = CA-NO
                     MOVE CA-45 TO TCEC0810-COD-RETURN
                  ELSE
                     MOVE CA-00 TO TCEC0810-COD-RETURN
                  END-IF
              END-IF
           END-IF.
      *
           PERFORM OBTAIN-BASE-CURRENCY
      *
           MOVE TCTC2241-FLG-FCC     TO TCEC0810-COD-BCC
      *
           IF  QBEC999-88-FLGYESEURCTRY
               PERFORM OBTAIN-START-DATE
      *
               IF V081-DAT-EXCHANGE LESS THAN CA-DAT-RCC
                  MOVE QBEC999-COD-OCCCTRY  TO TCEC0810-COD-BCC
               END-IF
           END-IF.
      *
      ******************************************************************
      *.PN OBTAIN-BASE-CURRENCY.                                       *
      ******************************************************************
       OBTAIN-BASE-CURRENCY.
      *
           INITIALIZE TCEC3000
                      TCTC2241.
      *
           MOVE  TCWC0110-TBL-0224       TO  TCEC3000-COD-TABLE.
           MOVE  CN-NUM-1               TO  TCEC3000-NUM-KEY.
           MOVE  SPACES                  TO  TCEC3000-COD-BRNLNG.
           MOVE  CA-TC                  TO  TCEC3000-KEY-CARD.
           MOVE  QBEC999-COD-ENTITY            TO  TCEC3000-COD-ENTITY.
      *
           EXEC CICS
                LINK PROGRAM ('TC7C3000')
                     COMMAREA (TCEC3000)
           END-EXEC
      *
           IF  TCEC3000-COD-RETURN1 = ZEROS
               MOVE  TCEC3000-RECCN TO  TCTC2241-DATA
           ELSE
               MOVE CA-85 TO TCEC0810-COD-RETURN
               MOVE CA-YES     TO SW-ERROR
           END-IF.
      *
      ******************************************************************
      *.PN OBTAIN-START-DATE.                                          *
      ******************************************************************
       OBTAIN-START-DATE.
      *
           INITIALIZE TCEC3000
                      TCTC2251.
      *
           MOVE  TCWC0110-TBL-0225        TO  TCEC3000-COD-TABLE.
           MOVE  CN-NUM-1               TO  TCEC3000-NUM-KEY.
           MOVE  SPACES                  TO  TCEC3000-COD-BRNLNG.
           MOVE  CA-KEY-CARD             TO  TCEC3000-KEY-CARD.
           MOVE  QBEC999-COD-ENTITY            TO  TCEC3000-COD-ENTITY.
      *
           EXEC CICS
                LINK PROGRAM ('TC7C3000')
                     COMMAREA (TCEC3000)
           END-EXEC
      *
           IF  TCEC3000-COD-RETURN1 = ZEROS
               MOVE  TCEC3000-RECCN TO  TCTC2251-DATA
               MOVE  TCTC2251-DAT-INIT   TO  CA-DAT-RCC
           ELSE
               MOVE CA-85 TO TCEC0810-COD-RETURN
               MOVE CA-YES     TO SW-ERROR
           END-IF.
      *
      ******************************************************************
      *.PN 3000-END.                                                   *
      ******************************************************************
       3000-END.
      *
           EXEC CICS
             RETURN
           END-EXEC.
      *
      ******************************************************************
      *.PN DB2CHECK.                                                   *
      ******************************************************************
       DB2CHECK.
      *
           MOVE SQLCODE TO TCEC0810-SQLCODE.
      *
           MOVE SQLERRM TO TCEC0810-SQLERRM.
      *
           IF SQLWARN0 NOT EQUAL SPACES
               MOVE SQLWARN    TO DB2-RETURN-CDE
               IF SQLWARN1 NOT = SPACES
                   MOVE 'WARNING1' TO TCEC0810-DTA-SQLERRM
               ELSE
                   IF SQLWARN2 NOT = SPACES
                       MOVE 'WARNING2' TO TCEC0810-DTA-SQLERRM
                   ELSE
                       IF SQLWARN3 NOT = SPACES
                           MOVE 'WARNING3' TO TCEC0810-DTA-SQLERRM
                       ELSE
                           IF SQLWARN4 NOT = SPACES
                               MOVE 'WARNING4' TO TCEC0810-DTA-SQLERRM
                           ELSE
                               IF SQLWARN5 NOT = SPACES
                                   MOVE 'WARNING5' TO
                                     TCEC0810-DTA-SQLERRM
                               ELSE
                                   IF SQLWARN6 NOT = SPACES
                                       MOVE 'WARNING6' TO
                                         TCEC0810-DTA-SQLERRM
                                   ELSE
                                       MOVE 'WARNING7' TO
                                         TCEC0810-DTA-SQLERRM
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
      *
           IF SQLWARN0 NOT = SPACES
               MOVE CA-99 TO TCEC0810-COD-RETURN
               MOVE SQLCODE TO TCEC0810-SQLCODE
               MOVE SQLERRM TO TCEC0810-SQLERRM
               PERFORM 3000-END
           END-IF.
      *
           MOVE SQLCODE TO DB2-RETURN-CDE.
           MOVE SQLCODE TO SQL-VALUES.
           IF NOT SQL-88-OK AND
              NOT SQL-88-NOT-FOUND
               MOVE CA-99 TO TCEC0810-COD-RETURN
               MOVE SQLCODE TO TCEC0810-SQLCODE
               MOVE SQLERRM TO TCEC0810-SQLERRM
               PERFORM 3000-END
           END-IF.
      *
