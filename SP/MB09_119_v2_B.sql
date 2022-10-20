DECLARE @REG1 char(4000)
DECLARE @REG2 char(4000)
DECLARE @REG3 char(4000)
DECLARE @REG4 char(4000)
DECLARE @REG5 char(4000)
DECLARE @REG6 char(4000)
DECLARE @REG7 char(4000)
DECLARE @REG8 char(4000)
DECLARE @REG9 char(4000)
DECLARE @REG10 char(4000)
DECLARE @REG11 char(4000)
DECLARE @REG12 char(4000)
DECLARE @REG13 char(4000)
DECLARE @REG14 char(4000)
DECLARE @REG15 char(4000)
DECLARE @CuentaN char(4000)
DECLARE @CuentaN710 char(4000)
DECLARE @Cont int
DECLARE @NumReg int
DECLARE @FNum int
DECLARE @403 char(100)
DECLARE @008_041_140 char(300)
DECLARE @DatOp char(10)
DECLARE @NumOperacion char(9)
     
      SET @Cont = 1 
      SET @NumReg=15
      

SELECT @403 = ISNULL(T403_NUM_BIN,'') + ',' + --6
             ISNULL(T403_NUM_CRD,'') + ',' + --10
             ISNULL(T403_NUM_CLTE,'') + ',' + --8
             ISNULL(T403_NUM_CTA,'') + ',' + --20
             ISNULL(T403_TEL_CEL,'') + ',' --15
        FROM MAZP.MCDT403 AS A with (nolock) 
      WHERE T403_BDMID = 'd28f34dfb8024ace9ca47a9af8452eeE'

--***********************************************************************

SELECT @008_041_140 = ISNULL(A.NUM_CUS,'') + ',' + --8
             ISNULL(B.T041_CAC_DIG1,'') + ',' + -- 1
             ISNULL(B.T041_CAC_DIG2,'') + ',' + -- 1
             ISNULL(B.T041_COD_PRODUCT,'') + ',' + -- 2
             ISNULL(B.T041_COD_SPROD,'') + ',' + -- 4
             ISNULL(B.T041_CEN_ACCT,'') + ',' + -- 4
             ISNULL(B.T041_FCC,'') + ',' + -- 4
             ISNULL(C.T140_DES_TABLE,'') + ','-- 250
        FROM MAZP.PEDT008 as A with(nolock) INNER JOIN MAZP.BGDT041 AS B with(nolock) ON
                  NUM_ACCOUNT  = '45958752'
							AND BRN_OPEN     = 0100
							AND COD_PRODSERV = 13   
							AND COD_ENTITY   = 0127  --FIJO
							AND KEY_PARTIC   = 'T' --FIJO
							AND PARTSEQ      = '01' -- FIJO
							AND T041_CEN_REG = 0100
							AND T041_ACC  = '1345958752'
							AND T041_ENT  = 0127
    LEFT JOIN MAZP.BGDT140 AS C with(nolock) ON
		      				T140_KEY_TABLE = B.T041_COD_PRODUCT+B.T041_COD_SPROD
					    AND T140_COD_TABLE = '0406' --FIJO
					    AND T140_LANGUAGE  = 'E' -- FIJO
					    AND T140_ENTITY    = 0127

--***********************************************************************
  
    SELECT @FNum = COUNT(*) FROM       
        (   SELECT TOP 15 
                  A.T071_DAT_OPERATION,
                  A.T071_NUM_OPERATION,
                  T071_DAT_VALUE,
                  T071_TIM_OPERATION,
                  T071_AMOUNT,
                  T071_CODE,
                  T071_OBSERVATIONS,
                  T071_COD_PRODUCT,
                  T071_COD_SPROD,
                  T071_FLG_FREE1,
                  T071_USERUPD,
                  T071_NTNMUPD,
                  T100_BIGALP,
                  T606_ACC,
                  T606_AMOUNT,
                  T606_NUM_OPERATION,
                  T606_DESCRIPTION,
                  T606_PATH,
                  T606_GPS_LAT,
                  T606_GPS_LONG,
                  T606_DAT_OPERATION,
                  T606_FLG_FREE1,
                  T606_CHAR_FREE1,
                  A.T071_INTREF,
                  C.T043_NUM_OPERATION,
                  D.T803_ENT_ACC 
                  --ISNULL(A.T071_DAT_OPERATION,'') AS 'T071_DAT_OPERATION',
                  --ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000') AS 'T071_NUM_OPERATION'

            FROM MAZP.BGDT071 AS A with (nolock) 
                LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                        T100_CODE = '826' AND
                        T100_LANGUAGE = 'E'
                LEFT JOIN  MAZP.BGDT606 with (nolock) ON
                            T606_ACC           = 0115574163
                        AND T606_NUM_OPERATION = 948
                        AND T606_DAT_OPERATION = '2008-01-16'
                LEFT JOIN MAZP.MCDT043 AS C with (nolock) ON 
                        C.T043_ENT_ACC=0127  AND
                            C.T043_BRN_ACC=0100   AND
                        C.T043_TYP_ACC=13 AND
                        C.T043_ACC=45958752 AND 
                            C.T043_DAT_ACCT=A.T071_DAT_OPERATION AND
                        LTRIM(C.T043_NUM_OPE_2)=LTRIM(A.T071_NUM_OPERATION)
                LEFT JOIN MAZP.MCDT803 AS D with(nolock) ON
                        D.T803_ACC=45958752 AND
                        D.T803_BRN_ACC=0100 AND 
                        D.T803_TYP_ACC=13 AND 
                        D.T803_ENT_ACC=0127 AND
                        D.T803_NUM_CARD=C.T043_NUM_CARD AND 
                        D.T803_NUM_BIN_CRD=C.T043_NUM_BIN_CRD AND 
                        D.T803_NUM_OPERATION=C.T043_NUM_OPERATION AND
                        D.T803_TKN_Q2='02'                       
                    WHERE A.T071_ACC = '1345958752' AND 
                        A.T071_CEN_REG = 0100 AND 
                        A.T071_ENT = 0127 AND
                        A.T071_DAT_OPERATION > = '2008-01-16' AND --VAR ENTRADA
                A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) < '9999-12-31 999999999' AND --FIJO
                A.T071_FLG_ANN = 'N' --FIJO
                ORDER BY A.T071_DAT_OPERATION DESC , A.T071_NUM_OPERATION DESC 
        ) AS "Cuenta71"
    --PRINT '@FNum ----> ' + cast(@FNum as varchar(2))

    IF @FNum = 15
   -- PRINT '@FNum IF ----> ' + cast(@FNum as varchar(2))
    BEGIN
      WHILE @Cont <= @NumReg 
      BEGIN
        SELECT @CuentaN =  DCuenta FROM       
        (   SELECT TOP 15 
                  ISNULL(A.T071_DAT_OPERATION,'') + ISNULL(STR(A.T071_NUM_OPERATION),'') COLLATE SQL_Latin1_General_CP1_CI_AS + ',' + --20 
                  ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000') + ',' + -- 9
                  ISNULL(A.T071_DAT_OPERATION,'') + ',' + -- 10
                  ISNULL(T071_DAT_VALUE,'') + ',' + -- 10
                  ISNULL(T071_TIM_OPERATION,'') + ',' + -- 4
                  ISNULL(CAST(T071_AMOUNT as varchar(15)),'+00000000000.00') + ',' + -- 17
                  ISNULL(T071_CODE,'   ') + ',' + -- 3
                  ISNULL(T071_OBSERVATIONS,'') + ',' + -- 31
                  ISNULL(T071_COD_PRODUCT,'') + ',' + -- 2 
                  ISNULL(T071_COD_SPROD,'') + ',' + -- 4 
                  ISNULL(T071_FLG_FREE1,'') + ',' + -- 3
                  ISNULL(T071_USERUPD,'') + ',' + -- 8 
                  ISNULL(T071_NTNMUPD,'') + ',' + -- 8 
                  ISNULL(T100_BIGALP,'') + ',' + -- 34 
                  ISNULL(T606_ACC,'') + ',' + -- 16
                  ISNULL(CAST(T606_AMOUNT as varchar(17)),'+0000000000000.00') + ',' + -- 17
                  ISNULL(CAST(T606_NUM_OPERATION as varchar(9)),'000000000') + ',' + -- 9
                  ISNULL(T606_DESCRIPTION,'') + ',' + -- 150
                  ISNULL(T606_PATH,'') + ',' + -- 250
                  ISNULL(CAST(T606_GPS_LAT as varchar(17)),'+000000000.000000') + ',' + -- 17
                  ISNULL(CAST(T606_GPS_LONG as varchar(17)),'+000000000.000000') + ',' + -- 17
                  ISNULL(T606_DAT_OPERATION,'') + ',' + -- 10
                  ISNULL(T606_FLG_FREE1,'') + ',' + -- 1
                  ISNULL(T606_CHAR_FREE1,'') + ',' +  -- 30
                  ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + ',' + -- 15 
                  ISNULL(CAST(C.T043_NUM_OPERATION AS varchar(9)),'') + ',' + 
                  ISNULL(CAST(D.T803_ENT_ACC AS varchar(4)),'') + ',' AS 'DCuenta',
                  --ISNULL(A.T071_DAT_OPERATION,'') AS 'T071_DAT_OPERATION',
                  --ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000') AS 'T071_NUM_OPERATION',
                  ROW_NUMBER() OVER(ORDER BY A.T071_DAT_OPERATION DESC , A.T071_NUM_OPERATION DESC) As "RowNum"
                FROM MAZP.BGDT071 AS A with (nolock) 
                LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                        T100_CODE = '826' AND
                        T100_LANGUAGE = 'E'
                LEFT JOIN  MAZP.BGDT606 with (nolock) ON
                            T606_ACC           = 0115574163
                        AND T606_NUM_OPERATION = 948
                        AND T606_DAT_OPERATION = '2008-01-16'
                LEFT JOIN MAZP.MCDT043 AS C with (nolock) ON 
                        C.T043_ENT_ACC=0127  AND
                            C.T043_BRN_ACC=0100   AND
                        C.T043_TYP_ACC=13 AND
                        C.T043_ACC=45958752 AND 
                            C.T043_DAT_ACCT=A.T071_DAT_OPERATION AND
                        LTRIM(C.T043_NUM_OPE_2)=LTRIM(A.T071_NUM_OPERATION)
                LEFT JOIN MAZP.MCDT803 AS D with(nolock) ON
                        D.T803_ACC=45958752 AND
                        D.T803_BRN_ACC=0100 AND 
                        D.T803_TYP_ACC=13 AND 
                        D.T803_ENT_ACC=0127 AND
                        D.T803_NUM_CARD=C.T043_NUM_CARD AND 
                        D.T803_NUM_BIN_CRD=C.T043_NUM_BIN_CRD AND 
                        D.T803_NUM_OPERATION=C.T043_NUM_OPERATION AND
                        D.T803_TKN_Q2='02'                       
                    WHERE A.T071_ACC = '1345958752' AND 
                        A.T071_CEN_REG = 0100 AND 
                        A.T071_ENT = 0127 AND
                        A.T071_DAT_OPERATION > = '2008-01-16' AND --VAR ENTRADA
                A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) < '9999-12-31 999999999' AND --FIJO
                A.T071_FLG_ANN = 'N' --FIJO
                ORDER BY A.T071_DAT_OPERATION DESC , A.T071_NUM_OPERATION DESC 
        ) AS "DatosCuenta71"

          WHERE RowNum = @Cont      
          IF @Cont = 1
          BEGIN
                SET @REG1 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 2
          BEGIN
                SET @REG2 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN)
          END
          
          IF @Cont = 3
          BEGIN
                SET @REG3 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN)
          END
          IF @Cont = 4
          BEGIN
                SET @REG4 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          IF @Cont =  5
          BEGIN
                SET @REG5 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          IF @Cont = 6
          BEGIN
                SET @REG6 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 7
          BEGIN
                SET @REG7 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 8
          BEGIN
                SET @REG8 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 9
          BEGIN
                SET @REG9 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 10
          BEGIN
                SET @REG10 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 11
          BEGIN
                SET @REG11 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 12
          BEGIN
                SET @REG12 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 13
          BEGIN
                SET @REG13 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 14
          BEGIN
                SET @REG14 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
           IF @Cont = 15
          BEGIN
                SET @REG15 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN) 
          END
           
          SET @Cont = @Cont + 1
          
      END
           
    END


    IF @FNum < 15
      BEGIN
        SELECT @CuentaN710 =  DCuenta FROM       
        (   SELECT TOP 15
                ISNULL(T071_DAT_OPERATION,'') + ISNULL(STR(T071_NUM_OPERATION),'') COLLATE SQL_Latin1_General_CP1_CI_AS + ',' + --20 
                ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') + ',' + -- 9
                ISNULL(T071_DAT_OPERATION,'') + ',' + -- 10
                ISNULL(T071_DAT_VALUE,'') + ',' + -- 10
                ISNULL(T071_TIM_OPERATION,'') + ',' + -- 4
                ISNULL(CAST(T071_AMOUNT as varchar(15)),'+0000000000000.00') + ',' + -- 17
                ISNULL(T071_CODE,'   ') + ',' + -- 3
                ISNULL(T071_OBSERVATIONS,'') + ',' + -- 31
                ISNULL(T071_COD_PRODUCT,'') + ',' + -- 2 
                  ISNULL(T071_COD_SPROD,'') + ',' + -- 4 
                  ISNULL(T071_FLG_FREE1,'') + ',' + -- 3
                  ISNULL(T071_USERUPD,'') + ',' + -- 8 
                  ISNULL(T071_NTNMUPD,'') + ',' + -- 8 
                  ISNULL(T100_BIGALP,'') + ',' + -- 34 
                  ISNULL(T606_ACC,'') + ',' + -- 16
                  ISNULL(CAST(T606_AMOUNT as varchar(17)),'+0000000000000.00') + ',' + -- 17
                  ISNULL(CAST(T606_NUM_OPERATION as varchar(9)),'000000000') + ',' + -- 9
                  ISNULL(T606_DESCRIPTION,'') + ',' + -- 150
                  ISNULL(T606_PATH,'') + ',' + -- 250
                  ISNULL(CAST(T606_GPS_LAT as varchar(17)),'+000000000.000000') + ',' + -- 17
                  ISNULL(CAST(T606_GPS_LONG as varchar(17)),'+000000000.000000') + ',' + -- 17
                  ISNULL(T606_DAT_OPERATION,'') + ',' + -- 10
                  ISNULL(T606_FLG_FREE1,'') + ',' + -- 1
                  ISNULL(T606_CHAR_FREE1,'') + ',' +   -- 30
                  ISNULL(CAST(A.T071_INTREF as varchar(15)),'') + ',' +   -- 15
                  ISNULL(CAST(C.T043_NUM_OPERATION AS varchar(9)),'') + ',' + 
                  ISNULL(CAST(D.T803_ENT_ACC AS varchar(4)),'') + ',' AS 'DCuenta',
                  --ISNULL(A.T071_DAT_OPERATION,''),
                  --ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000')
                  ROW_NUMBER() OVER(ORDER BY A.T071_NUM_OPERATION DESC) As "RowNum"
            FROM MAZP.BGDT710 as A with (nolock) 
          LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                     T100_CODE = '826' AND
                     T100_LANGUAGE = 'E'
          LEFT JOIN MAZP.BGDT606 with (nolock) ON
                      T606_ACC           = 000
                  AND T606_NUM_OPERATION = 1
                  AND T606_DAT_OPERATION = '2022-02-10'
         LEFT JOIN MAZP.MAZP.MCDT043 AS C with (nolock) ON 
                   C.T043_ENT_ACC=0127  AND
            		   C.T043_BRN_ACC=0100   AND
                   C.T043_TYP_ACC=13 AND
                   C.T043_ACC=45958752 AND 
            		   C.T043_DAT_ACCT=A.T071_DAT_OPERATION and
                   LTRIM(C.T043_NUM_OPE_2)=LTRIM(A.T071_NUM_OPERATION)
        LEFT JOIN MAZP.MCDT803 AS D with(nolock) ON 
                  D.T803_ACC=45958752 AND
                  D.T803_BRN_ACC=0100 AND 
                  D.T803_TYP_ACC=13 AND 
                  D.T803_ENT_ACC=0127 AND
                  D.T803_NUM_CARD=C.T043_NUM_CARD AND 
                  D.T803_NUM_BIN_CRD=C.T043_NUM_BIN_CRD AND 
                  D.T803_NUM_OPERATION=C.T043_NUM_OPERATION AND
                  D.T803_TKN_Q2='02'
            WHERE A.T071_ACC = '0100000016' AND 
                  A.T071_CEN_REG = 0172 AND 
                  A.T071_ENT = 0127 AND
  				  A.T071_DAT_OPERATION > = '2022-05-20' AND --VAR ENTRADA
  				  A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) COLLATE SQL_Latin1_General_CP1_CI_AS < '9999-12-31 999999999' AND --FIJO
  				  A.T071_FLG_ANN = 'N' 
    		 ORDER BY A.T071_NUM_OPERATION DESC) AS "DatosCuenta71"
          WHERE RowNum = @Cont 
   
                
        WHILE @Cont <= @NumReg 
        BEGIN
            
            IF @Cont = 1
            BEGIN
                  SET @REG1 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 2
            BEGIN
                  SET @REG2 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 3
            BEGIN
                  SET @REG3 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 4
            BEGIN
                  SET @REG4 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 5
            BEGIN
                  SET @REG5 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 6
            BEGIN
                  SET @REG6 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 7
            BEGIN
                  SET @REG7 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 8
            BEGIN
                  SET @REG8 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 9
            BEGIN
                  SET @REG9 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 10
            BEGIN
                  SET @REG10 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 11
            BEGIN
                  SET @REG11 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 12
            BEGIN
                  SET @REG12 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 13
            BEGIN
                  SET @REG13 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 14
            BEGIN
                  SET @REG14 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 15
            BEGIN
                  SET @REG15 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+RTRIM(@CuentaN710) 
            END
            
            SET @Cont = @Cont + 1
          
        END
        
      END
      
      IF @Cont = 1
      BEGIN
         SET @REG1 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))
      END


SELECT 
  @REG1  AS "REG1"
  ,@REG2  AS "REG2"
  ,@REG3 
  ,@REG4 
  ,@REG5 
  ,@REG6 
  ,@REG7 
  ,@REG8 
  ,@REG9 
  ,@REG10 
  ,@REG11 
  ,@REG12 
  ,@REG13 
  ,@REG14 
  ,@REG15  AS "REG15"
