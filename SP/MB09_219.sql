SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [MAZP].[SP_MB09_MB2CF219_Borrador]
@REG1 char(4000) OUTPUT, 
@REG2 char(4000) OUTPUT, 
@REG3 char(4000) OUTPUT, 
@REG4 char(4000) OUTPUT, 
@REG5 char(4000) OUTPUT, 
@REG6 char(4000) OUTPUT, 
@REG7 char(4000) OUTPUT, 
@REG8 char(4000) OUTPUT, 
@REG9 char(4000) OUTPUT, 
@REG10 char(4000) OUTPUT,
@REG11 char(4000) OUTPUT, 
@REG12 char(4000) OUTPUT, 
@REG13 char(4000) OUTPUT, 
@REG14 char(4000) OUTPUT, 
@REG15 char(4000) OUTPUT

WITH EXEC AS CALLER
AS
SET NOCOUNT ON
SET ANSI_WARNINGS ON
      
      DECLARE @CuentaN char(4000)
      DECLARE @Cont int
      DECLARE @NumReg int
      
      DECLARE @403 char(100)
      DECLARE @008_041_140 char(300)
     
      SET @Cont = 1 
      SET @NumReg=15
      
      SET LOCK_TIMEOUT 300

SELECT  ISNULL(T403_NUM_BIN,' ') AS 'T403_NUM_BIN', --6
        ISNULL(T403_NUM_CRD,' ') AS 'T403_NUM_CRD', --10
        ISNULL(T403_NUM_CLTE,' ') AS 'T403_NUM_CLTE', --8
        ISNULL(T403_NUM_CTA,' ') AS 'T403_NUM_CTA', --20
        ISNULL(T403_TEL_CEL,' ') AS 'T403_TEL_CEL' --15
    INTO #403_TEMP
    FROM MAZP.MCDT403 AS A with (nolock) 
    WHERE T403_BDMID = '07f700ea463c4331b877e9063baaff03'
go

--********************************                
                   
    SELECT   
             ISNULL(A.NUM_CUS,' ') AS 'T008_NUM_CUS', --8
             ISNULL(B.T041_CAC_DIG1,' ') AS 'T041_CAC_DIG1',  -- 1
             ISNULL(B.T041_CAC_DIG2,' ') AS 'T041_CAC_DIG2', -- 1
             ISNULL(B.T041_COD_PRODUCT,' ') AS ' T041_COD_PRODUCT', -- 2
             ISNULL(B.T041_COD_SPROD,' ') AS 'T041_COD_SPROD', -- 4
             ISNULL(B.T041_CEN_ACCT,' ') AS 'T041_CEN_ACCT' ,-- 4
             ISNULL(B.T041_FCC,' ') AS 'T041_FCC', -- 4
             ISNULL(C.T140_DES_TABLE,' ') AS 'T140_DES_TABLE' -- 250
        INTO #008_041_140_TEMP
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
GO

--*****************************************

SELECT *
    INTO #403_008_041_140_TEMP
    FROM #403_TEMP CROSS JOIN  #008_041_140_TEMP
GO

--******************************************

DROP TABLE #403_TEMP, #008_041_140_TEMP

--******************************************

DECLARE @cnt INT
SET @cnt = 1
WHILE (@cnt <= 14)
BEGIN
    PRINT 'CNT --> ' + CONVERT(VARCHAR,@cnt)
    SET @cnt = @cnt + 1
    INSERT INTO #403_008_041_140_TEMP
       SELECT TOP (1) *
       FROM #403_008_041_140_TEMP
END

--**********************************

--DROP TABLE #403_008_041_140_TEMP

--**********************************
              
        SELECT TOP 15  
	    ISNULL(T089_DAT_REG,' ') + 
          ISNULL(STR(T089_NUM_WHD),' ') COLLATE SQL_Latin1_General_CP1_CI_AS + '|@' + --15
	    ISNULL(CAST(T089_NUM_WHD as varchar(5)),'00000') + '|@' + -- 5
	    ISNULL(T089_DAT_REG,' ') + '|@' + -- 10
	    ISNULL(T089_TIM_REG,' ') + '|@' + -- 8
	    ISNULL(CAST(T089_AMT_ORIGIN as varchar(15)),'+00000000000.00') + '|@' + -- 15
          ISNULL(CAST(T089_AMT_CURRENT as varchar(15)),'+00000000000.00') + '|@' + -- 15				  
          ISNULL(T089_CODE,'   ') + '|@' + -- 3
          ISNULL(T089_OBSERVATIONS,' ') + '|@' + -- 40
          ISNULL(T100_BIGALP,' ') + '|@' + -- 34 
          ISNULL(T606_ACC,' ') + '|@' + -- 16
          ISNULL(CAST(T606_AMOUNT as varchar(17)),'+0000000000000.00') + '|@' + -- 17
          ISNULL(CAST(T606_NUM_OPERATION as varchar(9)),'000000000') + '|@' + -- 9
          ISNULL(T606_DESCRIPTION,' ') + '|@' + -- 150
          ISNULL(T606_PATH,' ') + '|@' + -- 250
          ISNULL(CAST(T606_GPS_LAT as varchar(17)),'+000000000.000000') + '|@' + -- 17
          ISNULL(CAST(T606_GPS_LONG as varchar(17)),'+000000000.000000') + '|@' + -- 17
          ISNULL(T606_DAT_OPERATION,' ') + '|@' + -- 10
          ISNULL(T606_FLG_FREE1,' ') + '|@' + -- 1
          ISNULL(T606_CHAR_FREE1,' ') + '|@'   -- 30
          
             FROM MAZP.BGDT089 AS A with (nolock) 
        LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                   T100_CODE = T089_CODE AND
                   T100_LANGUAGE = 'E'
        LEFT JOIN MAZP.BGDT606 with (nolock) ON
                    T606_ACC           = T089_ACC
                AND T606_NUM_OPERATION = T089_NUM_WHD
                AND T606_DAT_OPERATION = T089_DAT_REG
             
               WHERE T089_ACC = '1748707743' AND 
                   T089_CEN_REG = 3370 AND 
                   T089_ENT = 0127 AND
				   T089_DAT_REG > = '2022-07-10' AND --VAR ENTRADA
				   T089_DAT_REG + STR(T089_NUM_WHD) < '9999-12-31 999999999' AND --FIJO
				   T089_STATUS = '1' --FIJO
  		 ORDER BY T089_DAT_REG DESC , T089_NUM_WHD DESC
     
SET NOCOUNT OFF
GO