-- SALIDA
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
DECLARE @Cont int
DECLARE @403 char(100)     
DECLARE @008_041_140 char(300)
DECLARE @Cuenta char(3000)
DECLARE @NumReg INT

SET @Cont = 1
     SELECT @403 = ISNULL(T403_NUM_BIN,'') + ',' + --6
             ISNULL(T403_NUM_CRD,'') + ',' + --10
             ISNULL(T403_NUM_CLTE,'') + ',' + --8
             ISNULL(T403_NUM_CTA,'') + ',' + --20
             ISNULL(T403_TEL_CEL,'') + ',' --15
        FROM MAZP.MCDT403 AS A with (nolock) 
      WHERE T403_BDMID = '07f700ea463c4331b877e9063baaff03'

--******************************************
                      
 SELECT @008_041_140=ISNULL(A.NUM_CUS,'') + ',' + --8
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
							AND COD_ENTITY   = 0127 --FIJO
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

--*********************************************
 SELECT @NumReg = COUNT(DCuenta) FROM
        (SELECT TOP 15  
		  ISNULL(T089_DAT_REG,'') + ISNULL(STR(T089_NUM_WHD),'') COLLATE SQL_Latin1_General_CP1_CI_AS + ',' + --15
		  ISNULL(CAST(T089_NUM_WHD as varchar(5)),'00000') + ',' + -- 5
		  ISNULL(T089_DAT_REG,'') + ',' + -- 10
		  ISNULL(T089_TIM_REG,'') + ',' + -- 8
		  ISNULL(CAST(T089_AMT_ORIGIN as varchar(15)),'+00000000000.00') + ',' + -- 15
          ISNULL(CAST(T089_AMT_CURRENT as varchar(15)),'+00000000000.00') + ',' + -- 15				  
          ISNULL(T089_CODE,'') + ',' + -- 3
          ISNULL(T089_OBSERVATIONS,'') + ',' + -- 40
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
          ISNULL(T606_CHAR_FREE1,'') + ',' AS 'DCuenta', -- 30
          ROW_NUMBER() OVER(ORDER BY T089_DAT_REG DESC , T089_NUM_WHD DESC) As RowNum
           FROM MAZP.BGDT089 AS A with (nolock) 
        LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                   T100_CODE = T089_CODE AND
                   T100_LANGUAGE = 'E'
        LEFT JOIN MAZP.BGDT606 with (nolock) ON
                    T606_ACC           = T089_ACC
                AND T606_NUM_OPERATION = T089_NUM_WHD
                AND T606_DAT_OPERATION = T089_DAT_REG
                WHERE T089_ACC LIKE '%74%' AND 
                   T089_CEN_REG LIKE '%0%' AND 
                   T089_ENT = 0127 AND
				   T089_DAT_REG > = '2022-07-10' AND --VAR ENTRADARADA
				   T089_DAT_REG + STR(T089_NUM_WHD) < '9999-12-31 999999999' AND --FIJO
				   T089_STATUS = '1' --FIJO
  		 ORDER BY T089_DAT_REG DESC , T089_NUM_WHD DESC) AS "DatosCuenta"


 WHILE @Cont <= @NumReg 
      BEGIN
        SELECT @Cuenta = Cuenta FROM
        (SELECT TOP 15  
		  ISNULL(T089_DAT_REG,'') + ISNULL(STR(T089_NUM_WHD),'') COLLATE SQL_Latin1_General_CP1_CI_AS + ',' + --15
		  ISNULL(CAST(T089_NUM_WHD as varchar(5)),'00000') + ',' + -- 5
		  ISNULL(T089_DAT_REG,'') + ',' + -- 10
		  ISNULL(T089_TIM_REG,'') + ',' + -- 8
		  ISNULL(CAST(T089_AMT_ORIGIN as varchar(15)),'+00000000000.00') + ',' + -- 15
          ISNULL(CAST(T089_AMT_CURRENT as varchar(15)),'+00000000000.00') + ',' + -- 15				  
          ISNULL(T089_CODE,'') + ',' + -- 3
          ISNULL(T089_OBSERVATIONS,'') + ',' + -- 40
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
          ISNULL(T606_CHAR_FREE1,'') + ',' AS 'Cuenta', -- 30
          ROW_NUMBER() OVER(ORDER BY T089_DAT_REG DESC , T089_NUM_WHD DESC) As RowNum
           FROM MAZP.BGDT089 AS A with (nolock) 
        LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                   T100_CODE = T089_CODE AND
                   T100_LANGUAGE = 'E'
        LEFT JOIN MAZP.BGDT606 with (nolock) ON
                    T606_ACC           = T089_ACC
                AND T606_NUM_OPERATION = T089_NUM_WHD
                AND T606_DAT_OPERATION = T089_DAT_REG
              WHERE T089_ACC LIKE '%74%' AND 
                   T089_CEN_REG LIKE '%0%' AND 
                   T089_ENT = 0127 AND
				   T089_DAT_REG > = '2022-07-10' AND --VAR ENTRADARADA
				   T089_DAT_REG + STR(T089_NUM_WHD) < '9999-12-31 999999999' AND --FIJO
				   T089_STATUS = '1' --FIJO
  		 ORDER BY T089_DAT_REG DESC , T089_NUM_WHD DESC) AS "DatosCuenta"

        WHERE RowNum = @cont
                              
          IF @Cont = 1
          BEGIN
                SET @REG1 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 2
          BEGIN
                SET @REG2 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 3
          BEGIN
                SET @REG3 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          IF @Cont = 4
          BEGIN
                SET @REG4 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          IF @Cont =  5
          BEGIN
                SET @REG5 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          IF @Cont = 6
          BEGIN
                SET @REG6 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 7
          BEGIN
                SET @REG7 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 8
          BEGIN
                SET @REG8 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 9
          BEGIN
                SET @REG9 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 10
          BEGIN
                SET @REG10 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 11
          BEGIN
                SET @REG11 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 12
          BEGIN
                SET @REG12 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 13
          BEGIN
                SET @REG13 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 14
          BEGIN
                SET @REG14 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          IF @Cont = 15
          BEGIN
                SET @REG15 = RTRIM(isnull(@403,' , , , ,9, '))+RTRIM(isnull(@008_041_140,' , , , , , , , , '))+@Cuenta
          END
          
          SET @Cont = @Cont + 1
          
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