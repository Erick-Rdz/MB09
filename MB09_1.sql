SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [MAZP].[SP_MB09_1]
@ENT_IN char(4), @BDMID_IN char(40), @BRN_OPEN char(4), @COD_PROD char(2), @NUM_ACC char(8), @FECHA char(10), @ULLAVE char(20), @REG1 char(4000) OUTPUT, @REG2 char(4000) OUTPUT, @REG3 char(4000) OUTPUT, @REG4 char(4000) OUTPUT, @REG5 char(4000) OUTPUT, @REG6 char(4000) OUTPUT, @REG7 char(4000) OUTPUT, @REG8 char(4000) OUTPUT, @REG9 char(4000) OUTPUT, @REG10 char(4000) OUTPUT, @REG11 char(4000) OUTPUT, @REG12 char(4000) OUTPUT, @REG13 char(4000) OUTPUT, @REG14 char(4000) OUTPUT, @REG15 char(4000) OUTPUT, @REG16 char(4000) OUTPUT, @REG17 char(4000) OUTPUT, @REG18 char(4000) OUTPUT, @REG19 char(4000) OUTPUT, @REG20 char(4000) OUTPUT
WITH EXEC AS CALLER
AS
SET NOCOUNT ON
SET ANSI_WARNINGS ON
      
      DECLARE @CuentaN char(4000)
      DECLARE @CuentaN710 char(4000)
      DECLARE @Cont int
      DECLARE @NumReg int
      
      DECLARE @403 char(100)
      DECLARE @008_041_140 char(300)
     
      SET @Cont = 1 
      SET @NumReg=20
      
      SET LOCK_TIMEOUT 300
      
      SELECT @403=ISNULL(T403_NUM_BIN,' ') + '|' + --6
             ISNULL(T403_NUM_CRD,' ') + '|' + --10
             ISNULL(T403_NUM_CLTE,' ') + '|' + --8
             ISNULL(T403_NUM_CTA,' ') + '|' + --20
             ISNULL(T403_TEL_CEL,' ') + '|' --15
        FROM MCDT403 AS A with (nolock) 
      WHERE T403_BDMID = @BDMID_IN
--********************************                
                   
      SELECT @008_041_140=ISNULL(A.NUM_CUS,' ') + '|' + --8
             ISNULL(B.T041_CAC_DIG1,' ') + '|' + -- 1
             ISNULL(B.T041_CAC_DIG2,' ') + '|' + -- 1
             ISNULL(B.T041_COD_PRODUCT,' ') + '|' + -- 2
             ISNULL(B.T041_COD_SPROD,' ') + '|' + -- 4
             ISNULL(B.T041_CEN_ACCT,' ') + '|' + -- 4
             ISNULL(B.T041_FCC,' ') + '|' + -- 4
             ISNULL(C.T140_DES_TABLE,' ') + '|'-- 250
        FROM PEDT008 as A with(nolock) INNER JOIN BGDT041 AS B with(nolock) ON
                  NUM_ACCOUNT  = @NUM_ACC
							AND BRN_OPEN     = @BRN_OPEN
							AND COD_PRODSERV = @COD_PROD
							AND COD_ENTITY   = @ENT_IN --FIJO
							AND KEY_PARTIC   = 'T' --FIJO
							AND PARTSEQ      = '01' -- FIJO
							AND T041_CEN_REG = BRN_OPEN
							AND T041_ACC  = COD_PRODSERV+NUM_ACCOUNT
							AND T041_ENT  = COD_ENTITY
    LEFT JOIN BGDT140 AS C with(nolock) ON
		      				T140_KEY_TABLE = B.T041_COD_PRODUCT+B.T041_COD_SPROD
					    AND T140_COD_TABLE = '0406' --FIJO
					    AND T140_LANGUAGE  = 'E' -- FIJO
					    AND T140_ENTITY    = @ENT_IN
         
      DECLARE CuentasCursor CURSOR 
        FOR 
                 
--********************************
--** CONSULTA CUENTA
              
      SELECT TOP 20 
			    --ISNULL(T071_DAT_OPERATION,' ') + ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') + '|' + --20 
          ISNULL(T071_DAT_OPERATION,' ') + ISNULL(str(T071_NUM_OPERATION),'000000000') + '|' + --20 
				  ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') + '|' + -- 9
				  ISNULL(T071_DAT_OPERATION,' ') + '|' + -- 10
				  ISNULL(T071_DAT_VALUE,' ') + '|' + -- 10
				  ISNULL(T071_TIM_OPERATION,' ') + '|' + -- 4
				  ISNULL(CAST(T071_AMOUNT as varchar(15)),'+00000000000.00') + '|' + -- 17
				  ISNULL(T071_CODE,'   ') + '|' + -- 3
          ISNULL(T071_OBSERVATIONS,' ') + '|' + -- 31
          ISNULL(T071_COD_PRODUCT,' ') + '|' + -- 2 
          ISNULL(T071_COD_SPROD,' ') + '|' + -- 4 
          ISNULL(T071_FLG_FREE1,' ') + '|' + -- 3
          ISNULL(T071_USERUPD,' ') + '|' + -- 8 
          ISNULL(T071_NTNMUPD,' ') + '|' + -- 8 
          ISNULL(T100_BIGALP,' ') + '|' + -- 34 
          ISNULL(T606_ACC,' ') + '|' + -- 16
          ISNULL(CAST(T606_AMOUNT as varchar(17)),'+0000000000000.00') + '|' + -- 17
          ISNULL(CAST(T606_NUM_OPERATION as varchar(9)),'000000000') + '|' + -- 9
          ISNULL(T606_DESCRIPTION,' ') + '|' + -- 150
          ISNULL(T606_PATH,' ') + '|' + -- 250
          ISNULL(CAST(T606_GPS_LAT as varchar(17)),'+000000000.000000') + '|' + -- 17
          ISNULL(CAST(T606_GPS_LONG as varchar(17)),'+000000000.000000') + '|' + -- 17
          ISNULL(T606_DAT_OPERATION,' ') + '|' + -- 10
          ISNULL(T606_FLG_FREE1,' ') + '|' + -- 1
          ISNULL(T606_CHAR_FREE1,' ') + '|' +  -- 30
          ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + '|'  -- 15
          
             FROM BGDT071 AS A with (nolock) 
        LEFT JOIN BLDT002 with (nolock) ON 
                   T100_CODE = T071_CODE AND
                   T100_LANGUAGE = 'E'
        LEFT JOIN  BGDT606 with (nolock) ON
                    T606_ACC           = T071_ACC
                AND T606_NUM_OPERATION = T071_NUM_OPERATION
                AND T606_DAT_OPERATION = T071_DAT_OPERATION
             
             WHERE T071_ACC = @COD_PROD+@NUM_ACC AND 
                   T071_CEN_REG = @BRN_OPEN AND 
                   T071_ENT = @ENT_IN AND
				   T071_DAT_OPERATION > = @FECHA AND --VAR ENTRADA
				   T071_DAT_OPERATION + CAST(T071_NUM_OPERATION as varchar(9)) < @ULLAVE AND --FIJO
				   T071_FLG_ANN = 'N' --FIJO
  		 ORDER BY T071_DAT_OPERATION DESC , T071_NUM_OPERATION DESC
      FOR READ ONLY
              
      OPEN CuentasCursor

      FETCH NEXT FROM CuentasCursor INTO @CuentaN
      
      WHILE @@fetch_status = 0 and @Cont < ( @NumReg + 1) 
      BEGIN
                              
          IF @Cont = 1
          BEGIN
                SET @REG1 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 2
          BEGIN
                SET @REG2 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 3
          BEGIN
                SET @REG3 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          IF @Cont = 4
          BEGIN
                SET @REG4 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          IF @Cont =  5
          BEGIN
                SET @REG5 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          IF @Cont = 6
          BEGIN
                SET @REG6 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 7
          BEGIN
                SET @REG7 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 8
          BEGIN
                SET @REG8 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 9
          BEGIN
                SET @REG9 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 10
          BEGIN
                SET @REG10 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 11
          BEGIN
                SET @REG11 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 12
          BEGIN
                SET @REG12 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 13
          BEGIN
                SET @REG13 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 14
          BEGIN
                SET @REG14 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 15
          BEGIN
                SET @REG15 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 16
          BEGIN
                SET @REG16 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 17
          BEGIN
                SET @REG17 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 18
          BEGIN
                SET @REG18 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 19
          BEGIN
                SET @REG19 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          IF @Cont = 20
          BEGIN
                SET @REG20 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN
          END
          
          SET @Cont = @Cont + 1
          
          FETCH NEXT FROM CuentasCursor INTO @CuentaN
          
      END
           
      CLOSE  CuentasCursor
      DEALLOCATE CuentasCursor

      IF @Cont < 20
      BEGIN
          DECLARE CuentasCursor710 CURSOR 
          FOR 
      
          SELECT TOP 20
  			    --ISNULL(T071_DAT_OPERATION,' ') + ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') + '|' + --20 
            ISNULL(T071_DAT_OPERATION,' ') + ISNULL(str(T071_NUM_OPERATION),'000000000') + '|' + --20 
  				  ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') + '|' + -- 9
  				  ISNULL(T071_DAT_OPERATION,' ') + '|' + -- 10
  				  ISNULL(T071_DAT_VALUE,' ') + '|' + -- 10
  				  ISNULL(T071_TIM_OPERATION,' ') + '|' + -- 4
  				  ISNULL(CAST(T071_AMOUNT as varchar(15)),'+0000000000000.00') + '|' + -- 17
  				  ISNULL(T071_CODE,'   ') + '|' + -- 3
  				  ISNULL(T071_OBSERVATIONS,' ') + '|' + -- 31
            ISNULL(T071_COD_PRODUCT,' ') + '|' + -- 2 
            ISNULL(T071_COD_SPROD,' ') + '|' + -- 4 
            ISNULL(T071_FLG_FREE1,' ') + '|' + -- 3
            ISNULL(T071_USERUPD,' ') + '|' + -- 8 
            ISNULL(T071_NTNMUPD,' ') + '|' + -- 8 
            ISNULL(T100_BIGALP,' ') + '|' + -- 34 
            ISNULL(T606_ACC,' ') + '|' + -- 16
            ISNULL(CAST(T606_AMOUNT as varchar(17)),'+0000000000000.00') + '|' + -- 17
            ISNULL(CAST(T606_NUM_OPERATION as varchar(9)),'000000000') + '|' + -- 9
            ISNULL(T606_DESCRIPTION,' ') + '|' + -- 150
            ISNULL(T606_PATH,' ') + '|' + -- 250
            ISNULL(CAST(T606_GPS_LAT as varchar(17)),'+000000000.000000') + '|' + -- 17
            ISNULL(CAST(T606_GPS_LONG as varchar(17)),'+000000000.000000') + '|' + -- 17
            ISNULL(T606_DAT_OPERATION,' ') + '|' + -- 10
            ISNULL(T606_FLG_FREE1,' ') + '|' + -- 1
            ISNULL(T606_CHAR_FREE1,' ') + '|' +   -- 30
            ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + '|'   -- 15
   
               FROM BGDT710 as A with (nolock) 
          LEFT JOIN BLDT002 with (nolock) ON 
                     T100_CODE = T071_CODE AND
                     T100_LANGUAGE = 'E'
          LEFT JOIN  BGDT606 with (nolock) ON
                      T606_ACC           = T071_ACC
                  AND T606_NUM_OPERATION = T071_NUM_OPERATION
                  AND T606_DAT_OPERATION = T071_DAT_OPERATION
               
               WHERE T071_ACC = @COD_PROD+@NUM_ACC AND 
                     T071_CEN_REG = @BRN_OPEN AND 
                     T071_ENT = @ENT_IN AND
  				   T071_DAT_OPERATION > = @FECHA AND --VAR ENTRADA
  				   T071_DAT_OPERATION + CAST(T071_NUM_OPERATION as varchar(9)) < @ULLAVE AND --FIJO
  				   T071_FLG_ANN = 'N' --FIJO
    		 ORDER BY T071_DAT_OPERATION DESC , T071_NUM_OPERATION DESC
        FOR READ ONLY
                
        OPEN CuentasCursor710

        FETCH NEXT FROM CuentasCursor710 INTO @CuentaN710
                
        WHILE @@fetch_status = 0 and @Cont < ( @NumReg + 1) 
        BEGIN
                                
            IF @Cont = 1
            BEGIN
                  SET @REG1 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 2
            BEGIN
                  SET @REG2 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 3
            BEGIN
                  SET @REG3 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 4
            BEGIN
                  SET @REG4 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 5
            BEGIN
                  SET @REG5 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 6
            BEGIN
                  SET @REG6 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 7
            BEGIN
                  SET @REG7 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 8
            BEGIN
                  SET @REG8 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 9
            BEGIN
                  SET @REG9 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 10
            BEGIN
                  SET @REG10 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 11
            BEGIN
                  SET @REG11 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 12
            BEGIN
                  SET @REG12 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 13
            BEGIN
                  SET @REG13 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 14
            BEGIN
                  SET @REG14 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 15
            BEGIN
                  SET @REG15 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 16
            BEGIN
                  SET @REG16 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 17
            BEGIN
                  SET @REG17 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 18
            BEGIN
                  SET @REG18 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 19
            BEGIN
                  SET @REG19 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            IF @Cont = 20
            BEGIN
                  SET @REG20 = RTRIM(@403)+RTRIM(@008_041_140)+@CuentaN710
            END
            
            SET @Cont = @Cont + 1
          
            FETCH NEXT FROM CuentasCursor710 INTO @CuentaN710
            
        END
        
        CLOSE  CuentasCursor710
        DEALLOCATE CuentasCursor710
      
      END
      


            
SET NOCOUNT OFF
GO
