SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [MAZP].[MB09_MB2CF119_v5]
@ENT_IN char(4), @BDMID_IN char(40), @BRN_OPEN char(4), @COD_PROD char(2), @NUM_ACC char(8), @FECHA char(10), @ULLAVE char(20), @BAN71 CHAR(03),@FECHA_ACCT char(10),
@MOV71 VARCHAR(MAX) ='{}'OUT,@MOV710 VARCHAR(MAX)='{}' OUT, @403_Json VARCHAR(MAX)=' ' OUT,@datosCuenta VARCHAR(MAX)=' ' OUT
WITH EXEC AS CALLER
AS
SET NOCOUNT ON
SET ANSI_WARNINGS ON


      DECLARE @Cont int
      DECLARE @NumReg int = 0
      DECLARE @NumRegRest int =0

      SET @Cont = 1 
      SET @NumReg=0
      SET @MOV71='{}'
      
      SET LOCK_TIMEOUT 300
      
      SET @403_Json  = ISNULL
     ((SELECT ISNULL(T403_NUM_BIN,' ')  AS T403_NUM_BIN,
             ISNULL(T403_NUM_CRD,' ')  AS T403_NUM_CRD,
             ISNULL(T403_NUM_CLTE,' ') AS  T403_NUM_CLTE,
             ISNULL(T403_NUM_CTA,' ') AS T403_NUM_CTA,  
             ISNULL(T403_TEL_CEL,' ')  AS  T403_TEL_CEL
        FROM MAZP.MCDT403 AS A with (nolock) 
      WHERE T403_BDMID = @BDMID_IN FOR JSON PATH,WITHOUT_ARRAY_WRAPPER  ),' ')
--********************************                
                   
      SET  @datosCuenta= ISNULL((SELECT ISNULL(A.NUM_CUS,' ') as NUM_CUS,  --8
             ISNULL(B.T041_CAC_DIG1,' ') as T041_CAC_DIG1, -- 1
             ISNULL(B.T041_CAC_DIG2,' ') as T041_CAC_DIG2, -- 1
             ISNULL(B.T041_COD_PRODUCT,' ') as T041_COD_PRODUCT, -- 2
             ISNULL(B.T041_COD_SPROD,' ') as T041_COD_SPROD, -- 4
             ISNULL(B.T041_CEN_ACCT,' ')as T041_CEN_ACCT,-- 4
             ISNULL(B.T041_FCC,' ') as T041_FCC,-- 4
             ISNULL(C.T140_DES_TABLE,' ') as T140_DES_TABLE-- 250
        FROM MAZP.PEDT008 as A with(nolock) INNER JOIN MAZP.BGDT041 AS B with(nolock) ON
                  NUM_ACCOUNT  = @NUM_ACC
							AND BRN_OPEN     = @BRN_OPEN
							AND COD_PRODSERV = @COD_PROD
							AND COD_ENTITY   = @ENT_IN --FIJO
							AND KEY_PARTIC   = 'T' --FIJO
							AND PARTSEQ      = '01' -- FIJO
							AND T041_CEN_REG = BRN_OPEN
							AND T041_ACC  = COD_PRODSERV+NUM_ACCOUNT
							AND T041_ENT  = COD_ENTITY
    LEFT JOIN MAZP.BGDT140 AS C with(nolock) ON
		      				T140_KEY_TABLE = B.T041_COD_PRODUCT+B.T041_COD_SPROD
					    AND T140_COD_TABLE = '0406' --FIJO
					    AND T140_LANGUAGE  = 'E' -- FIJO
					    AND T140_ENTITY    = @ENT_IN FOR JSON PATH,WITHOUT_ARRAY_WRAPPER  ),' ')
         

      IF  @BAN71 ='071'       
      SET @MOV71  =ISNULL((
          SELECT TOP 15
		  ISNULL(T071_DAT_OPERATION,' ') AS 'A', 
		  ISNULL(CAST(T071_NUM_OPERATION AS varchar(09)),' ') AS 'B',  --20 
		  ISNULL(T071_DAT_VALUE,' ') AS 'C',-- 10
		  ISNULL(SUBSTRING(T071_TIMESTAMP,12,2)+SUBSTRING(T071_TIMESTAMP,15,2)+SUBSTRING(T071_TIMESTAMP,18,2),' ')  AS 'D', -- 4
		  ISNULL(CAST(T071_AMOUNT as varchar(15)),'+00000000000.00') AS 'E', -- 17
		  ISNULL(T071_CODE,'   ') AS 'F',-- 3
          ISNULL(T071_OBSERVATIONS,' ') AS 'G', -- 31
          ISNULL(T071_COD_PRODUCT,' ') AS 'H', -- 2 
          ISNULL(T071_COD_SPROD,' ') AS 'I', -- 4 
          ISNULL(T071_FLG_FREE1,' ') AS 'J', -- 3
          ISNULL(T071_USERUPD,' ') AS 'K', -- 8 
          ISNULL(T071_NTNMUPD,' ') AS 'L', -- 8 
          ISNULL(T100_BIGALP,' ') AS 'M', -- 34 
          ISNULL(CAST(T606_NUM_OPERATION as varchar(9)),'NULL') AS 'N', -- 9
          ISNULL(RTRIM(T606_DESCRIPTION),' ') AS 'O', -- 150
          ISNULL(RTRIM(T606_PATH),' ') AS 'P',-- 250
          ISNULL(T606_FLG_FREE1,' ') AS 'Q', -- 1
          ISNULL(RTRIM(T606_CHAR_FREE1),' ') AS 'R',  -- 30
          ISNULL(CAST(A.T071_INTREF as varchar(15)),' ') AS 'S', -- 15
          ISNULL(T071_DAT_ACCT,'')                       AS 'T'
             FROM MAZP.BGDT071 AS A with (nolock) 
        LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                   T100_CODE = T071_CODE AND
                   T100_LANGUAGE = 'E'
        LEFT JOIN  MAZP.BGDT606 with (nolock) ON
                    T606_ACC           = T071_ACC
                AND T606_NUM_OPERATION = T071_NUM_OPERATION
                AND T606_DAT_OPERATION = T071_DAT_OPERATION
            WHERE A.T071_ACC                               = @COD_PROD+@NUM_ACC  AND 
                  A.T071_CEN_REG                           = @BRN_OPEN           AND 
                  A.T071_ENT                               = @ENT_IN             AND
				  A.T071_DAT_OPERATION                    >=@FECHA               AND
				  T071_DAT_ACCT + STR(T071_NUM_OPERATION) < @ULLAVE              AND
				  T071_DAT_ACCT                           <=@FECHA_ACCT          AND
                  A.T071_FLG_ANN                           = 'N' --FIJO                     
  		 ORDER BY T071_DAT_ACCT DESC , T071_NUM_OPERATION DESC
      FOR JSON PATH),'{}')               
 
      
      select @NumReg=count(1)
      from OPENJSON(@MOV71)
            
      SET @NumRegRest= 15-@NumReg

       IF @NumReg < 15 OR @BAN71 ='710'
       SET @MOV710  =ISNULL((     
          SELECT TOP (@NumRegRest) 
		  ISNULL(T071_DAT_OPERATION,' ') AS 'A', 
		  ISNULL(CAST(T071_NUM_OPERATION AS varchar(09)),' ') AS 'B',  --20 
		  ISNULL(T071_DAT_VALUE,' ') AS 'C',-- 10
		  ISNULL(SUBSTRING(T071_TIMESTAMP,12,2)+SUBSTRING(T071_TIMESTAMP,15,2)+SUBSTRING(T071_TIMESTAMP,18,2),' ')  AS 'D', -- 4
		  ISNULL(CAST(T071_AMOUNT as varchar(15)),'+00000000000.00') AS 'E', -- 17
		  ISNULL(T071_CODE,'   ') AS 'F',-- 3
          ISNULL(T071_OBSERVATIONS,' ') AS 'G', -- 31
          ISNULL(T071_COD_PRODUCT,' ') AS 'H', -- 2 
          ISNULL(T071_COD_SPROD,' ') AS 'I', -- 4 
          ISNULL(T071_FLG_FREE1,' ') AS 'J', -- 3
          ISNULL(T071_USERUPD,' ') AS 'K', -- 8 
          ISNULL(T071_NTNMUPD,' ') AS 'L', -- 8 
          ISNULL(T100_BIGALP,' ') AS 'M', -- 34 
          ISNULL(CAST(T606_NUM_OPERATION as varchar(9)),'NULL') AS 'N', -- 9
          ISNULL(RTRIM(T606_DESCRIPTION),' ') AS 'O', -- 150
          ISNULL(RTRIM(T606_PATH),' ') AS 'P',-- 250
          ISNULL(T606_FLG_FREE1,' ') AS 'Q', -- 1
          ISNULL(RTRIM(T606_CHAR_FREE1),' ') AS 'R',  -- 30
          ISNULL(CAST(A.T071_INTREF as varchar(15)),' ') AS 'S', -- 15
          ISNULL(T071_DAT_ACCT,'')                       AS 'T'
             FROM MAZP.BGDT710 AS A with (nolock) 
        LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                   T100_CODE = T071_CODE AND
                   T100_LANGUAGE = 'E'
        LEFT JOIN  MAZP.BGDT606 with (nolock) ON
                   T606_ACC           = T071_ACC
               AND T606_NUM_OPERATION = T071_NUM_OPERATION
               AND T606_DAT_OPERATION = T071_DAT_OPERATION                                                             
            WHERE A.T071_ACC                               = @COD_PROD+@NUM_ACC  AND 
                  A.T071_CEN_REG                           = @BRN_OPEN           AND 
                  A.T071_ENT                               = @ENT_IN             AND
				  A.T071_DAT_OPERATION                    >=@FECHA               AND
				  T071_DAT_ACCT + STR(T071_NUM_OPERATION) < @ULLAVE              AND
				  T071_DAT_ACCT                           <=@FECHA_ACCT          AND
                  A.T071_FLG_ANN                           = 'N' --FIJO                     
  		 ORDER BY T071_DAT_ACCT DESC , T071_NUM_OPERATION DESC
      FOR JSON PATH),'{}')
                

GO
