      ******************************************************************
      *A.OR.S                                                          *
      *  AREA ENLACE RUTINA BATCH DE INTERFASE CON TABLAS GENERALES    *
      *  (TCECR099)                                                    *
      *  LA RUTINA ACCEDE A LA TABLA DE DEFINICION DE TABLAS GENERALES *
      *  PARA RECUPERAR DATOS DE UNA TABLA GENERAL EN AREA DE MEMORIA  *
      *A.OR.E                                                          *
      ******************************************************************
      *B.OR.S                                                          *
      *  COMMAREA OF BATCH INTERFACE ROUTINE TO GENERAL TABLES         *
      *  (TC9CR001)                                                    *
      *  THE ROUTINE ACCESS TO THE DEFINITION TABLE OF GENERAL TABLES  *
      *  TO RETRIEVE DATA OF A GENERAL TABLE FROM A MEMORY AREA        *
      *B.OR.E                                                          *
      ******************************************************************
      *A.MF TCECR099                  AREA ENLACE RUTINA BATCH DE      *
      *A/MF                           INTERFASE CON TABLAS GENERALES   *
      *A.IF INPUT                     CAMPOS DE ENTRADA                *
      *A.IF COD-REQOPTION             OPCION SOLICITADA                *
      *A/IF                           '1' RECUPERA UNA SOLA OCURRENCIA *
      *A/IF                           = CLAVE INFORMADA                *
      *A/IF                           '3' RECUPERA UNA SOLA OCURRENCIA *
      *A/IF                           > CLAVE INFORMADA                *
      *A/IF                           '4' RECUPERA UNA SOLA OCURRENCIA *
      *A/IF                           >= CLAVE INFORMADA               *
      *A.IF KEY                       CLAVE                            *
      *A.IF COD-ENT-INP               CODIGO DE ENTIDAD (CLAVE)        *
      *A.IF LNG-INP                   CODIGO IDIOMA (CLAVE)            *
      *A.OF OUTPUT                    CAMPOS DE SALIDA                 *
      *A.OF COD-RETURN                CODIGO DE RETORNO                *
      *A/OF                           '00' RETORNO OK                  *
      *A/OF                           '10' REGISTRO NO EXISTE(OPCION 1)*
      *A/OF                           '80' ACCESO DB2 ERRORNEO         *
      *A.OF ERR-DB2                   INFORMACION DE ERROR DB2         *
      *A.OF SQLCODE                   CODIGO RETORNO SQL               *
      *A.OF SQLERRM                   SQLERRM DB2                      *
      *A.OF SQLERRMLON                LONGITUD DEL MENSAJE RETORNO SQL *
      *A.OF DTA-SQLERRM               DATOS DEL MENSAJE RETORNO SQL    *
      *A.OF DES-ABLE                  NOMBRE DE LA TABLA DB2           *
      *A.OF FILLER                    FILLER                           *
      *A.OF FLG-DATA                  INDICADOR DE DATOS               *
      *A/OF                           '0' NO HAY MAS DATOS             *
      *A/OF                           '1' HAY MAS DATOS DE LA TABLA    *
      *A.OF NUM-RREC                  NUMERO DE REGISTROS RECUPERADOS  *
      *A/OF                           (SOLO OPCION 2)                  *
      *A.OF TB-TAB-REC                CONTENIDO DEL REGISTRO           *
      ******************************************************************
      ******************************************************************
      *B.MF TCECR099                  INTERFACE ROUTINE BATCH WITH     *
      *B/MF                           GENERAL TABLES LINK AREA         *
      *B.IF INPUT                     INPUT FIELDS                     *
      *B.IF COD-REQOPTION             REQUESTED OPTION                 *
      *B/IF                           '1' RETRIEVES JUST ONE OCCURRENCE*
      *B/IF                           = KEY                            *
      *B/IF                           '3' RETRIEVES JUST ONE OCCURRENCE*
      *B/IF                           > KEY.                           *
      *B/IF                           '4' RETRIEVES JUST ONE OCCURRENCE*
      *B/IF                           >= KEY                           *
      *B.IF KEY                       KEY                              *
      *B.IF COD-ENT-INP               ENTITY CODE (KEY)                *
      *B.IF LNG-INP                   LANGUAGE CODE (KEY)              *
      *B.OF OUTPUT                    OUTPUT FIELDS                    *
      *B.OF COD-RETURN                RETURN CODE                      *
      *B/OF                           '00' RETURN  OK                  *
      *B/OF                           '10' NON-EXISTENT RECORD(OPTION1)*
      *B/OF                           '80' WRONG DB2 ACCESS            *
      *B.OF ERR-DB2                   DB2 ERROR INFORMATION            *
      *B.OF SQLCODE                   SQL RETURN CODE                  *
      *B.OF SQLERRM                   SQLERRM DB2                      *
      *B.OF SQLERRMLON                SQL RETURN MESSAGE LENGTH        *
      *B.OF DTA-SQLERRM               SQL RETURN MESSAGE DATA          *
      *B.OF DES-ABLE                  DB2 TABLE NAME                   *
      *B.OF FILLER                    FILLER                           *
      *B.OF FLG-DATA                  DATA FLAG                        *
      *B/OF                           '0' - NO MORE DATA               *
      *B/OF                           '1' - MORE TABLE DATA            *
      *B.OF NUM-RREC                  RECOVERING RECORD NUMBERS        *
      *B.OF TB-TAB-REC                RECORD CONTENT                   *
      ******************************************************************
       01 TCECR099.
         05 TCECR099-INPUT.
           10 TCECR099-COD-REQOPTION        PIC X(1).
           10 TCECR099-KEY.
             15 TCECR099-COD-ENT-INP        PIC X(4).
             15 TCECR099-LNG-INP            PIC X(1).
         05 TCECR099-OUTPUT.
           10 TCECR099-COD-RETURN           PIC X(2).
           10 TCECR099-ERR-DB2.
             15 TCECR099-SQLCODE            PIC S9(9) COMP.
             15 TCECR099-SQLERRM.
               20 TCECR099-SQLERRMLON       PIC S9(4) COMP.
               20 TCECR099-DTA-SQLERRM      PIC X(30).
               20 TCECR099-DES-ABLE         PIC X(8).
               20 FILLER                    PIC X(3).
           10 TCECR099-FLG-DATA             PIC X(1).
           10 TCECR099-NUM-RREC             PIC 9(2).
           10 TCECR099-TB-TAB-REC.
             15 TCECR099-COD-ENTITY         PIC X(4).
             15 TCECR099-LNG-DATA           PIC X(1).
             15 TCECR099-DES-ENTITY         PIC X(40).
             15 TCECR099-COD                PIC X(2).
             15 TCECR099-ENT-NAC            PIC X(4).
             15 TCECR099-DES-NAC            PIC X(40).
             15 TCECR099-COD-NATCC          PIC X(3).
             15 TCECR099-DES-NATCC          PIC X(20).
             15 TCECR099-SDE-NATCC          PIC X(3).
             15 TCECR099-COD-NATCCSHORT     PIC X.
             15 TCECR099-COD-OCCCTRY        PIC X(3).
             15 TCECR099-DES-OCC            PIC X(20).
             15 TCECR099-SDE-OCC            PIC X(3).
             15 TCECR099-COD-OCCSHORT       PIC X.
             15 TCECR099-COD-RCC            PIC X(3).
             15 TCECR099-DES-RCC            PIC X(20).
             15 TCECR099-COD-RCCSHORT       PIC X.
             15 TCECR099-COD-NRESFCC        PIC X(3).
             15 TCECR099-DES-NRESFCC        PIC X(20).
             15 TCECR099-COD-NATCTRY        PIC X(3).
             15 TCECR099-DES-NATCTRY        PIC X(40).
             15 TCECR099-FLG-EURCTRY        PIC X.
             15 TCECR099-EXCHANGE           PIC X.
             15 TCECR099-LNG-OFDATA         PIC X.
             15 TCECR099-LASTMODUSER        PIC X(8).
             15 TCECR099-LASTMODTRM         PIC X(4).
             15 TCECR099-DAT-LASTMOD        PIC X(26).
             15 TCECR099-DES-ENTABR         PIC X(10).
             15 TCECR099-FLG-OFCACC         PIC X(1).
