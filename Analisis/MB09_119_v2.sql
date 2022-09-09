SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [MAZP].[MB09_MB2CF119_v2]
@ENT_IN char(4), @BDMID_IN char(40), @BRN_OPEN char(4), @COD_PROD char(2), @NUM_ACC char(8), @FECHA char(10), @ULLAVE char(20), @REG1 char(4000) OUTPUT, @REG2 char(4000) OUTPUT, @REG3 char(4000) OUTPUT, @REG4 char(4000) OUTPUT, @REG5 char(4000) OUTPUT, @REG6 char(4000) OUTPUT, @REG7 char(4000) OUTPUT, @REG8 char(4000) OUTPUT, @REG9 char(4000) OUTPUT, @REG10 char(4000) OUTPUT, @REG11 char(4000) OUTPUT, @REG12 char(4000) OUTPUT, @REG13 char(4000) OUTPUT, @REG14 char(4000) OUTPUT, @REG15 char(4000) OUTPUT
WITH EXEC AS CALLER
AS
SET NOCOUNT ON
SET ANSI_WARNINGS ON
      
      DECLARE @CuentaN char(4000)
      DECLARE @CuentaN710 char(4000)
      DECLARE @Cont int
      DECLARE @NumReg int
      DECLARE @NumOperacion char(9)
      DECLARE @DatOp char(10)
      DECLARE @NumOp043 char(10)
      DECLARE @Ent803 char(4)
      DECLARE @NUM_CARD char(13)
      DECLARE @NUM_BIN_CRD char(6)
      
      DECLARE @403 char(100)
      DECLARE @008_041_140 char(300)
     
      SET @Cont = 1 
      SET @NumReg=15
      
      SET LOCK_TIMEOUT 300
      
      SELECT @403=ISNULL(T403_NUM_BIN,' ') + '|@' + --6
             ISNULL(T403_NUM_CRD,' ') + '|@' + --10
             ISNULL(T403_NUM_CLTE,' ') + '|@' + --8
             ISNULL(T403_NUM_CTA,' ') + '|@' + --20
             ISNULL(T403_TEL_CEL,' ') + '|@' --15
        FROM MAZP.MCDT403 AS A with (nolock) 
      WHERE T403_BDMID = @BDMID_IN
--********************************                
                   
      SELECT @008_041_140=ISNULL(A.NUM_CUS,' ') + '|@' + --8
             ISNULL(B.T041_CAC_DIG1,' ') + '|@' + -- 1
             ISNULL(B.T041_CAC_DIG2,' ') + '|@' + -- 1
             ISNULL(B.T041_COD_PRODUCT,' ') + '|@' + -- 2
             ISNULL(B.T041_COD_SPROD,' ') + '|@' + -- 4
             ISNULL(B.T041_CEN_ACCT,' ') + '|@' + -- 4
             ISNULL(B.T041_FCC,' ') + '|@' + -- 4
             ISNULL(C.T140_DES_TABLE,' ') + '|@'-- 250
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
					    AND T140_ENTITY    = @ENT_IN
--*********************************       
     


      DECLARE CuentasCursor CURSOR 
        FOR 
                 
--********************************
             
      SELECT TOP 15 
		  ISNULL(A.T071_DAT_OPERATION,' ') + ISNULL(STR(A.T071_NUM_OPERATION),' ') COLLATE SQL_Latin1_General_CP1_CI_AS + '|@' + --20 
      ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000') + '|@' + -- 9
		  ISNULL(A.T071_DAT_OPERATION,' ') + '|@' + -- 10
		  ISNULL(T071_DAT_VALUE,' ') + '|@' + -- 10
		  ISNULL(T071_TIM_OPERATION,' ') + '|@' + -- 4
		  ISNULL(CAST(T071_AMOUNT as varchar(15)),'+00000000000.00') + '|@' + -- 17
		  ISNULL(T071_CODE,'   ') + '|@' + -- 3
          ISNULL(T071_OBSERVATIONS,' ') + '|@' + -- 31
          ISNULL(T071_COD_PRODUCT,' ') + '|@' + -- 2 
          ISNULL(T071_COD_SPROD,' ') + '|@' + -- 4 
          ISNULL(T071_FLG_FREE1,' ') + '|@' + -- 3
          ISNULL(T071_USERUPD,' ') + '|@' + -- 8 
          ISNULL(T071_NTNMUPD,' ') + '|@' + -- 8 
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
          ISNULL(T606_CHAR_FREE1,' ') + '|@' +  -- 30
          ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + '|@' + -- 15 
          ISNULL(CAST(C.T043_NUM_OPERATION AS varchar(9)),' ') + '|@' + 
          ISNULL(CAST(D.T803_ENT_ACC AS varchar(4)),' ') + '|@',
          ISNULL(A.T071_DAT_OPERATION,' '),
          ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000')
          
             FROM MAZP.BGDT071 AS A with (nolock) 
     
        LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                   T100_CODE = T071_CODE AND
                   T100_LANGUAGE = 'E'
        LEFT JOIN  MAZP.BGDT606 with (nolock) ON
                    T606_ACC           = T071_ACC
                AND T606_NUM_OPERATION = T071_NUM_OPERATION
                AND T606_DAT_OPERATION = T071_DAT_OPERATION
        LEFT JOIN MAZP.MCDT043 AS C with (nolock) ON 
                   C.T043_ENT_ACC=@ENT_IN  AND
            		   C.T043_BRN_ACC=@BRN_OPEN   AND
                   C.T043_TYP_ACC=@COD_PROD AND
                   C.T043_ACC=@NUM_ACC AND 
            		   C.T043_DAT_ACCT=A.T071_DAT_OPERATION AND
                   LTRIM(C.T043_NUM_OPE_2)=LTRIM(A.T071_NUM_OPERATION)
        LEFT JOIN MAZP.MCDT803 AS D with(nolock) ON
                  D.T803_ACC=@NUM_ACC AND
                  D.T803_BRN_ACC=@BRN_OPEN AND 
                  D.T803_TYP_ACC=@COD_PROD AND 
                  D.T803_ENT_ACC=@ENT_IN AND
                  D.T803_NUM_CARD=C.T043_NUM_CARD AND 
                  D.T803_NUM_BIN_CRD=C.T043_NUM_BIN_CRD AND 
                  D.T803_NUM_OPERATION=C.T043_NUM_OPERATION AND
                  D.T803_TKN_Q2='02'
                                                             
            WHERE A.T071_ACC = @COD_PROD+@NUM_ACC AND 
                  A.T071_CEN_REG = @BRN_OPEN AND 
                  A.T071_ENT = @ENT_IN AND
				   A.T071_DAT_OPERATION > = @FECHA AND --VAR ENTRADA
           A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) < @ULLAVE AND --FIJO
           A.T071_FLG_ANN = 'N' --FIJO
           
  		 ORDER BY A.T071_DAT_OPERATION DESC , A.T071_NUM_OPERATION DESC
      FOR READ ONLY
              
      OPEN CuentasCursor

      FETCH NEXT FROM CuentasCursor INTO @CuentaN,@DatOp,@NumOperacion
      
      WHILE @@fetch_status = 0 and @Cont < ( @NumReg + 1) 
      BEGIN
                              
          IF @Cont = 1
          BEGIN
                SET @REG1 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN)
          END
          
          IF @Cont = 2
          BEGIN
                SET @REG2 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN)
          END
          
          IF @Cont = 3
          BEGIN
                SET @REG3 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN)
          END
          IF @Cont = 4
          BEGIN
                SET @REG4 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          IF @Cont =  5
          BEGIN
                SET @REG5 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          IF @Cont = 6
          BEGIN
                SET @REG6 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 7
          BEGIN
                SET @REG7 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 8
          BEGIN
                SET @REG8 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 9
          BEGIN
                SET @REG9 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 10
          BEGIN
                SET @REG10 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 11
          BEGIN
                SET @REG11 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 12
          BEGIN
                SET @REG12 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 13
          BEGIN
                SET @REG13 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 14
          BEGIN
                SET @REG14 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          IF @Cont = 15
          BEGIN
                SET @REG15 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN) 
          END
          
          
          SET @Cont = @Cont + 1
          
          FETCH NEXT FROM CuentasCursor INTO @CuentaN,@DatOp,@NumOperacion
          
      END
           
      CLOSE  CuentasCursor
      DEALLOCATE CuentasCursor

      IF @Cont < 15
      BEGIN
          DECLARE CuentasCursor710 CURSOR 
          FOR 
      
          SELECT TOP 15
  			ISNULL(T071_DAT_OPERATION,' ') + ISNULL(STR(T071_NUM_OPERATION),'') COLLATE SQL_Latin1_General_CP1_CI_AS + '|@' + --20 
  			ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') + '|@' + -- 9
  			ISNULL(T071_DAT_OPERATION,' ') + '|@' + -- 10
  			ISNULL(T071_DAT_VALUE,' ') + '|@' + -- 10
  			ISNULL(T071_TIM_OPERATION,' ') + '|@' + -- 4
  			ISNULL(CAST(T071_AMOUNT as varchar(15)),'+0000000000000.00') + '|@' + -- 17
  			ISNULL(T071_CODE,'   ') + '|@' + -- 3
  			ISNULL(T071_OBSERVATIONS,' ') + '|@' + -- 31
            ISNULL(T071_COD_PRODUCT,' ') + '|@' + -- 2 
            ISNULL(T071_COD_SPROD,' ') + '|@' + -- 4 
            ISNULL(T071_FLG_FREE1,' ') + '|@' + -- 3
            ISNULL(T071_USERUPD,' ') + '|@' + -- 8 
            ISNULL(T071_NTNMUPD,' ') + '|@' + -- 8 
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
            ISNULL(T606_CHAR_FREE1,' ') + '|@' +   -- 30
            ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + '|@' +   -- 15
            ISNULL(CAST(C.T043_NUM_OPERATION AS varchar(9)),' ') + '|@' + 
            ISNULL(CAST(D.T803_ENT_ACC AS varchar(4)),' ') + '|@', 
            ISNULL(A.T071_DAT_OPERATION,' '),
            ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000')
         
   
               FROM MAZP.BGDT710 as A with (nolock) 
          LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                     T100_CODE = T071_CODE AND
                     T100_LANGUAGE = 'E'
          LEFT JOIN MAZP.BGDT606 with (nolock) ON
                      T606_ACC           = T071_ACC
                  AND T606_NUM_OPERATION = T071_NUM_OPERATION
                  AND T606_DAT_OPERATION = T071_DAT_OPERATION
         LEFT JOIN MAZP.MAZP.MCDT043 AS C with (nolock) ON 
                   C.T043_ENT_ACC=@ENT_IN  AND
            		   C.T043_BRN_ACC=@BRN_OPEN   AND
                   C.T043_TYP_ACC=@COD_PROD AND
                   C.T043_ACC=@NUM_ACC AND 
            		   C.T043_DAT_ACCT=A.T071_DAT_OPERATION and
                   LTRIM(C.T043_NUM_OPE_2)=LTRIM(A.T071_NUM_OPERATION)
        LEFT JOIN MAZP.MCDT803 AS D with(nolock) ON 
                  D.T803_ACC=@NUM_ACC AND
                  D.T803_BRN_ACC=@BRN_OPEN AND 
                  D.T803_TYP_ACC=@COD_PROD AND 
                  D.T803_ENT_ACC=@ENT_IN AND
                  D.T803_NUM_CARD=C.T043_NUM_CARD AND 
                  D.T803_NUM_BIN_CRD=C.T043_NUM_BIN_CRD AND 
                  D.T803_NUM_OPERATION=C.T043_NUM_OPERATION AND
                  D.T803_TKN_Q2='02'
       
            WHERE A.T071_ACC = @COD_PROD+@NUM_ACC AND 
                  A.T071_CEN_REG = @BRN_OPEN AND 
                  A.T071_ENT = @ENT_IN AND
  				  A.T071_DAT_OPERATION > = @FECHA AND --VAR ENTRADA
  				  A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) COLLATE SQL_Latin1_General_CP1_CI_AS < @ULLAVE AND --FIJO
  				  A.T071_FLG_ANN = 'N' 
    		 ORDER BY A.T071_NUM_OPERATION DESC
        FOR READ ONLY
                
        OPEN CuentasCursor710

        FETCH NEXT FROM CuentasCursor710 INTO @CuentaN710,@DatOp,@NumOperacion
                
        WHILE @@fetch_status = 0 and @Cont < ( @NumReg + 1) 
        BEGIN
         
           
            IF @Cont = 1
            BEGIN
                  SET @REG1 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 2
            BEGIN
                  SET @REG2 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 3
            BEGIN
                  SET @REG3 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 4
            BEGIN
                  SET @REG4 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 5
            BEGIN
                  SET @REG5 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 6
            BEGIN
                  SET @REG6 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 7
            BEGIN
                  SET @REG7 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 8
            BEGIN
                  SET @REG8 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 9
            BEGIN
                  SET @REG9 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 10
            BEGIN
                  SET @REG10 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 11
            BEGIN
                  SET @REG11 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 12
            BEGIN
                  SET @REG12 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 13
            BEGIN
                  SET @REG13 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 14
            BEGIN
                  SET @REG14 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            IF @Cont = 15
            BEGIN
                  SET @REG15 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+RTRIM(@CuentaN710) 
            END
            
            SET @Cont = @Cont + 1
          
            FETCH NEXT FROM CuentasCursor710 INTO @CuentaN710,@DatOp,@NumOperacion
            
        END
        
        CLOSE  CuentasCursor710
        DEALLOCATE CuentasCursor710
      
      END
      
      IF @Cont = 1
      BEGIN
         SET @REG1 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))
      END
            
SET NOCOUNT OFF
GO