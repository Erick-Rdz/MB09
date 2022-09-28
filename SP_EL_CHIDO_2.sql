SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER  PROCEDURE [MAZP].[SP_MB09Prueba01]

--- VARIABLES DE ENTRADA PARA DIFERENTES PROCEDIMIENTOS 
@IP_CAA_CEN_ACCOUNT VARCHAR(4),
@IP_CAA_CHANN varchar (2),
@IP_WSS_RET_CTA char(5),
@IP_WSS_CUENTA char(5),
@IP_WSS_CUENTA_710 char(5),
@IP_PRKEY char (2),
@IP_NUMCUEN char (14),
---------------------------------

--- VARIABLES ENTRADA SP's
@IP_OPCION CHAR (1),
@IP_ENT_IN char(4), 
@IP_BDMID_IN char(40), 
--@IP_BRN_OPEN char(4), 
--@COD_PROD char(2), 
--@IP_NUM_ACC char(8), 
--@IP_FECHA_ACCT char(10), 
@IP_ULLAVE char(20),
---------------------------------

--- VARIABLES SALIDA SP's
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
@REG15 char(4000) OUTPUT,
---------------------------------

@BAN71 CHAR(03), 
--@IP_FECHA_ACCT char(10),
@MOV71 VARCHAR(MAX) ='{}'OUT,
@MOV710 VARCHAR(MAX)='{}' OUT, 
@403_Json VARCHAR(MAX)=' ' OUT, 
@datosCuenta VARCHAR(MAX)=' ' OUT

WITH EXEC AS CALLER
AS
SET NOCOUNT ON
SET ANSI_WARNINGS ON

    DECLARE @FECHA_CALC Date
    DECLARE @CuentaN char(4000)
    DECLARE @CuentaN710 char(4000)
    DECLARE @Cont int
    DECLARE @NumReg int
    DECLARE @NumOperacion char(9)
    DECLARE @DatOp char(10)
    DECLARE @403 char(100)

    -- VARIABLES DP V5
    DECLARE @BRN_OPEN char(4) 
    DECLARE @COD_PROD char(2) 
    DECLARE @NUM_ACC char(8)
    DECLARE @FECHA_ACCT char(10) 
    DECLARE @NumRegRest int = 0

    --- CONSULTA-PARAM-DIGITALES
    DECLARE @VN_MARCA_AUX VARCHAR (30) = NULL
    DECLARE @VN_VAL_INI INTEGER = NULL

    --- USUARIO SAPP
    DECLARE @OK_SAPP char(5) = NULL
    DECLARE @NOK_SAPP char(5) = NULL

    --FIRST SECTION VAR
    DECLARE @403_NUM_BIN CHAR(6) = NULL
    DECLARE @403_NUM_CARD CHAR(10) = NULL
    DECLARE @403_NUM_CLTE CHAR(8) = NULL
    DECLARE @403_NUM_CTA CHAR(20) = NULL
    DECLARE @403_TEL_CEL CHAR(15) = NULL

    --SECOND SECTION VAR
    DECLARE @008_041_140 CHAR (8) = NULL--8
    DECLARE @T041_CAC_DIG1 CHAR (1) = NULL -- 1
    DECLARE @T041_CAC_DIG2 CHAR (1) = NULL -- 1
    DECLARE @T041_COD_PRODUCT CHAR (2) = NULL -- 2
    DECLARE @T041_COD_SPROD CHAR (4) = NULL -- 4
    DECLARE @T041_CEN_ACCT CHAR (4) = NULL-- 4
    DECLARE @T041_FCC CHAR (4) = NULL -- 4
    DECLARE @T140_DES_TABLE CHAR (250) = NULL-- 250

--SUMA SOBRES
    DECLARE @OP_T039_SALDO CHAR (5) = NULL 
    DECLARE @OP_T039_ID_CTA_META CHAR (5) = NULL 
    DECLARE @OP_T039_SALDO_SOBRES CHAR (5) = NULL 
    DECLARE @OP_T039_ID_CTA_META_SOBRES CHAR (5) = NULL
---SOBRES
    DECLARE @T039_NUM_CLIENTE VARCHAR (08) = NULL
    DECLARE @T039_CTA_EJE VARCHAR (20) = NULL

--- ALCANCIA 
    DECLARE @OP_T039_SALDO_ALCANCIA CHAR (5) = NULL
    DECLARE @OP_COUNT_ALCANCIA CHAR (5) = NULL

    SET @Cont = 1 
    SET @NumReg=15

    SET @BRN_OPEN = SUBSTRING(@IP_NUMCUEN,1,4)
    SET @COD_PROD = SUBSTRING(@IP_NUMCUEN,5,2)
    SET @NUM_ACC = SUBSTRING(@IP_NUMCUEN,7,8)
    PRINT '@BRN_OPEN --> ' + @BRN_OPEN + ' @COD_PROD -->  ' + @COD_PROD + '  @NUM_ACC ---> ' + @NUM_ACC
   
    SET LOCK_TIMEOUT 300

-----------------------------------------------------------------------------------
--    PROCESO  --------------------------------------------------------------------                              
-----------------------------------------------------------------------------------

---
------------ 22000-CALCULA-FECHA (SE RESTAN 3 MESES A LA FECHA EN CURSO) ----------
   SET @FECHA_CALC = DATEADD(MONTH, -3,GETDATE())
------------------------------------------------------------------------------------

------------ 22000-LLAMADO-SP -----------
    DECLARE @APAGA_JSON char(5) = 'FALSE';
    DECLARE @PRENDE_JSON char(5) = 'FALSE';
        
    IF EXISTS(SELECT T140_DES_TABLE --6,            
            FROM MBDT140 with (nolock)
               WHERE T140_KEY_TABLE  = 'MB09'
                 AND T140_COD_TABLE  = 'JSON'
                 AND T140_ENTITY     =  0127
                 AND T140_LANGUAGE   = 'E') 
                 SET @PRENDE_JSON = 'TRUE'
    ELSE
        SET @APAGA_JSON = 'TRUE'
---BORRAR
    PRINT 'PRENDE_JSON --> ' + @PRENDE_JSON
    PRINT 'APAGA_JSON --> ' + @APAGA_JSON
 
------------ CONSULTA-PARAM-DIGITALES
    SELECT  @T140_DES_TABLE = T140_DES_TABLE
                FROM MBDT140 with(nolock)
               WHERE T140_KEY_TABLE = 'BINVIRTUAL'
                 AND T140_COD_TABLE = 'MBT1'
                 AND T140_LANGUAGE  = 'E'
                 AND T140_ENTITY    = '0127'
    IF(@T140_DES_TABLE IS NOT NULL)
        SET @VN_MARCA_AUX = SUBSTRING(@T140_DES_TABLE,1,CHARINDEX('|', @T140_DES_TABLE)-1)
        SET @VN_VAL_INI = CHARINDEX('|', @T140_DES_TABLE)-1 + 2
        --- PRINT PARA VER DATOS BORRAR
        PRINT 'T140_DES_TABLE --> ' + @T140_DES_TABLE 
        PRINT CONCAT ('VN_MARCA_AUX --> ', @VN_MARCA_AUX)   
        PRINT CONCAT ('VN_VAL_INI --> ', @VN_VAL_INI)
                     

 ------ 1100-VAL-USUADIO-SAPP (Super APP)
    IF (@IP_CAA_CEN_ACCOUNT = 1156 AND @IP_CAA_CHANN = 54)
        IF EXISTS(SELECT T140_DES_TABLE
             FROM MBDT140 with (nolock)
             WHERE  T140_KEY_TABLE  ='BINVIRTUAL' AND
                    T140_COD_TABLE  ='SAPP' AND
                    T140_LANGUAGE   ='E' AND
                    T140_ENTITY     ='0127')
                    SET @OK_SAPP = '00'
        ELSE
            SET @NOK_SAPP = '10'

---BORRAR                 
    PRINT 'SAPP S --> ' + @OK_SAPP
    PRINT 'SAPP N --> ' + @NOK_SAPP

----------------------- FUNCIONA ---------------------------
    
    
----------- METODOS DE EXTRACCION DE DATOS -----------

----------- 21100-PRENDE-TABLA
IF @IP_WSS_CUENTA = 'TRUE'
    SET @BAN71 = '071'
ELSE IF (@IP_WSS_CUENTA_710 = 'TRUE')
    SET @BAN71 = '710'

----------- SP MB09_MB2CF219 -----------    
    IF (@IP_WSS_RET_CTA = 'TRUE') 
        
        BEGIN
        ---
            PRINT 'ENTRO MB09_MB2CF219 --- ' + @IP_WSS_RET_CTA

--- OBTENER LOS DATOS PRINCIPALES DEL PROCESO --
SELECT @403=ISNULL(T403_NUM_BIN,' ') + '|@' + --6
             ISNULL(T403_NUM_CRD,' ') + '|@' + --10
             ISNULL(T403_NUM_CLTE,' ') + '|@' + --8
             ISNULL(T403_NUM_CTA,' ') + '|@' + --20
             ISNULL(T403_TEL_CEL,' ') + '|@' --15
        FROM MAZP.MCDT403 AS A with (nolock) 
      WHERE T403_BDMID = @IP_BDMID_IN
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
							AND COD_ENTITY   = @IP_ENT_IN --FIJO
							AND KEY_PARTIC   = 'T' --FIJO
							AND PARTSEQ      = '01' -- FIJO
							AND T041_CEN_REG = BRN_OPEN
							AND T041_ACC  = COD_PRODSERV+NUM_ACCOUNT
							AND T041_ENT  = COD_ENTITY
    LEFT JOIN MAZP.BGDT140 AS C with(nolock) ON
		      				T140_KEY_TABLE = B.T041_COD_PRODUCT+B.T041_COD_SPROD
					    AND T140_COD_TABLE = '0406' --FIJO
					    AND T140_LANGUAGE  = 'E' -- FIJO
					    AND T140_ENTITY    = @IP_ENT_IN
         
      DECLARE CuentasCursor CURSOR 
        FOR 
                 
--********************************
              
      SELECT TOP 15  
		  ISNULL(T089_DAT_REG,' ') + ISNULL(STR(T089_NUM_WHD),' ') COLLATE SQL_Latin1_General_CP1_CI_AS + '|@' + --15
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
             
             WHERE T089_ACC = @COD_PROD+@NUM_ACC AND 
                   T089_CEN_REG = @BRN_OPEN AND 
                   T089_ENT = @IP_ENT_IN AND
				   T089_DAT_REG > = @FECHA_CALC AND --VAR ENTRADA
				   T089_DAT_REG + STR(T089_NUM_WHD) < @IP_ULLAVE AND --FIJO
				   T089_STATUS = '1' --FIJO
  		 ORDER BY T089_DAT_REG DESC , T089_NUM_WHD DESC
      FOR READ ONLY
              
      OPEN CuentasCursor

      FETCH NEXT FROM CuentasCursor INTO @CuentaN
      
      WHILE @@fetch_status = 0 and @Cont < ( @NumReg + 1) 
      BEGIN
                              
          IF @Cont = 1
          BEGIN
                SET @REG1 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 2
          BEGIN
                SET @REG2 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 3
          BEGIN
                SET @REG3 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          IF @Cont = 4
          BEGIN
                SET @REG4 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          IF @Cont =  5
          BEGIN
                SET @REG5 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          IF @Cont = 6
          BEGIN
                SET @REG6 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 7
          BEGIN
                SET @REG7 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 8
          BEGIN
                SET @REG8 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 9
          BEGIN
                SET @REG9 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 10
          BEGIN
                SET @REG10 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 11
          BEGIN
                SET @REG11 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 12
          BEGIN
                SET @REG12 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 13
          BEGIN
                SET @REG13 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 14
          BEGIN
                SET @REG14 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          IF @Cont = 15
          BEGIN
                SET @REG15 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN
          END
          
          SET @Cont = @Cont + 1
          
          FETCH NEXT FROM CuentasCursor INTO @CuentaN
          
      END
           
      CLOSE  CuentasCursor
      DEALLOCATE CuentasCursor
      
      IF @Cont = 1
      BEGIN
         SET @REG1 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))
      END
        
    END

----------- SP MB09_MB2CF119 V2 -----------
    ELSE IF (@IP_WSS_CUENTA = 'TRUE' AND @APAGA_JSON = 'TRUE')         
        BEGIN
            ---
            PRINT 'ENTRO MB09_MB2CF119 V2 --- WC ' + @IP_WSS_CUENTA + ' --- AJ  ' + @APAGA_JSON
            -- GET MAIN DATA FOR NEXT QUERYS
            SELECT @403=ISNULL(T403_NUM_BIN,' ') + '|@' + --6
                    ISNULL(T403_NUM_CRD,' ') + '|@' + --10
                    ISNULL(T403_NUM_CLTE,' ') + '|@' + --8
                    ISNULL(T403_NUM_CTA,' ') + '|@' + --20
                    ISNULL(T403_TEL_CEL,' ') + '|@' --15
            FROM MAZP.MCDT403 AS A with (nolock) 
            WHERE T403_BDMID = @IP_BDMID_IN
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
                                    AND COD_ENTITY   = @IP_ENT_IN --FIJO
                                    AND KEY_PARTIC   = 'T' --FIJO
                                    AND PARTSEQ      = '01' -- FIJO
                                    AND T041_CEN_REG = BRN_OPEN
                                    AND T041_ACC  = COD_PRODSERV+NUM_ACCOUNT
                                    AND T041_ENT  = COD_ENTITY
            LEFT JOIN MAZP.BGDT140 AS C with(nolock) ON
                                    T140_KEY_TABLE = B.T041_COD_PRODUCT+B.T041_COD_SPROD
                                AND T140_COD_TABLE = '0406' --FIJO
                                AND T140_LANGUAGE  = 'E' -- FIJO
                                AND T140_ENTITY    = @IP_ENT_IN
        --*********************************       
            DECLARE CuentasCursor CURSOR 
                FOR 
                        
        --********************************
                    
            SELECT TOP 15 
                ISNULL(A.T071_DAT_OPERATION,' ') + ISNULL(STR(A.T071_NUM_OPERATION),' ') COLLATE SQL_Latin1_General_CP1_CI_AS + '|@' + --20 
            ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000') + '|@' + -- 9
                ISNULL(A.T071_DAT_OPERATION,' ') + '|@' + -- 10
                ISNULL(T071_DAT_VALUE,' ') + '|@' + -- 10
                ISNULL(SUBSTRING(T071_TIMESTAMP,12,2)+SUBSTRING(T071_TIMESTAMP,15,2)+SUBSTRING(T071_TIMESTAMP,18,2),' ') + '|@' + -- 6
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
                ISNULL(T606_FLG_FREE1,'') + '|@' + -- 1
                ISNULL(T606_CHAR_FREE1,'') + '|@' +  -- 30
                ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + '|@' + -- 15 
                ' ' + '|@' + 
                ' ' + '|@',
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
                                                                    
                    WHERE A.T071_ACC = @COD_PROD+@NUM_ACC AND 
                        A.T071_CEN_REG = @BRN_OPEN AND 
                        A.T071_ENT = @IP_ENT_IN AND
                        A.T071_DAT_OPERATION > = @FECHA_CALC AND --VAR ENTRADA
                A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) < @IP_ULLAVE AND --FIJO
                A.T071_FLG_ANN = 'N' --FIJO
                
                ORDER BY A.T071_NUM_OPERATION DESC
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
                    ISNULL(SUBSTRING(T071_TIMESTAMP,12,2)+SUBSTRING(T071_TIMESTAMP,15,2)+SUBSTRING(T071_TIMESTAMP,18,2),' ') + '|@' + -- 6
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
                    '' + '|@' + 
                    '' + '|@', 
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
            
                    WHERE A.T071_ACC = @COD_PROD+@NUM_ACC AND 
                        A.T071_CEN_REG = @BRN_OPEN AND 
                        A.T071_ENT = @IP_ENT_IN AND
                        A.T071_DAT_OPERATION > = @FECHA_CALC AND --VAR ENTRADA
                        A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) COLLATE SQL_Latin1_General_CP1_CI_AS < @IP_ULLAVE AND --FIJO
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
    END
    
----------- SP MB09_MB2CF119 V5 -----------
    ELSE IF (@PRENDE_JSON = 'TRUE' AND (@IP_WSS_CUENTA = 'TRUE' OR @IP_WSS_CUENTA_710 = 'TRUE')) 
        BEGIN
        ---
            SET @FECHA_ACCT = SUBSTRING(@IP_ULLAVE,1,10)
            PRINT 'ENTRO MB09_MB2CF119 V5 --- WC ' + @IP_WSS_CUENTA + ' --- AJ  ' + @IP_WSS_CUENTA_710
        ---
            SET @403_Json  = ISNULL
            ((SELECT ISNULL(T403_NUM_BIN,' ')  AS T403_NUM_BIN,
                    ISNULL(T403_NUM_CRD,' ')  AS T403_NUM_CRD,
                    ISNULL(T403_NUM_CLTE,' ') AS  T403_NUM_CLTE,
                    ISNULL(T403_NUM_CTA,' ') AS T403_NUM_CTA,  
                    ISNULL(T403_TEL_CEL,' ')  AS  T403_TEL_CEL
                FROM MAZP.MCDT403 AS A with (nolock) 
            WHERE T403_BDMID = @IP_BDMID_IN FOR JSON PATH,WITHOUT_ARRAY_WRAPPER  ),' ')
--********************************                         
            SET  @datosCuenta = ISNULL((SELECT ISNULL(A.NUM_CUS,' ') as NUM_CUS,  --8
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
                                    AND COD_ENTITY   = @IP_ENT_IN --FIJO
                                    AND KEY_PARTIC   = 'T' --FIJO
                                    AND PARTSEQ      = '01' -- FIJO
                                    AND T041_CEN_REG = BRN_OPEN
                                    AND T041_ACC  = COD_PRODSERV+NUM_ACCOUNT
                                    AND T041_ENT  = COD_ENTITY
            LEFT JOIN MAZP.BGDT140 AS C with(nolock) ON
                                    T140_KEY_TABLE = B.T041_COD_PRODUCT+B.T041_COD_SPROD
                                AND T140_COD_TABLE = '0406' --FIJO
                                AND T140_LANGUAGE  = 'E' -- FIJO
                                AND T140_ENTITY    = @IP_ENT_IN FOR JSON PATH,WITHOUT_ARRAY_WRAPPER  ),' ')        

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
                    WHERE A.T071_ACC                             = @COD_PROD+@NUM_ACC  AND 
                        A.T071_CEN_REG                           = @BRN_OPEN           AND 
                        A.T071_ENT                               = @IP_ENT_IN             AND
                        A.T071_DAT_OPERATION                    >=@FECHA_CALC              AND
                        T071_DAT_ACCT + STR(T071_NUM_OPERATION) < @IP_ULLAVE              AND
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
                        A.T071_ENT                               = @IP_ENT_IN             AND
                        A.T071_DAT_OPERATION                    >=@FECHA_CALC               AND
                        T071_DAT_ACCT + STR(T071_NUM_OPERATION) < @IP_ULLAVE              AND
                        T071_DAT_ACCT                           <=@FECHA_ACCT          AND
                        A.T071_FLG_ANN                           = 'N' --FIJO                     
                ORDER BY T071_DAT_ACCT DESC , T071_NUM_OPERATION DESC
            FOR JSON PATH),'{}')    
    END
    

    --IF (@NOK_SAPP = TRUE)
    --    BEGIN
    --       @403_TEL_CEL 
    --    END

----> PENDIENTE ---> VALIDA-RELACION-BDMID
    IF @IP_PRKEY = '02' AND @NOK_SAPP != NULL


----------- SUMA SOBRES ----------- 
    SELECT 
        @OP_T039_SALDO_SOBRES = SUM(T039_SALDO),
        @OP_T039_ID_CTA_META_SOBRES = COUNT(T039_ID_CTA_META)                 
              FROM  MAZP.MBDT039 with(nolock)
              WHERE T039_NUM_CLIENTE = @T039_NUM_CLIENTE
                AND T039_CTA_EJE     = @T039_CTA_EJE
                AND T039_ESTAT_CTA_META IN ('SA','SP','SV','SS')
                AND T039_LOG_METAS   = 'SOBRES'

----------- SUMA ALCANCIA -----------
      SELECT @OP_T039_SALDO_ALCANCIA = SUM(T039_SALDO),
             @OP_COUNT_ALCANCIA = COUNT(T039_ID_CTA_META)
              FROM  MAZP.MBDT039 with(nolock)
              WHERE T039_NUM_CLIENTE = @T039_NUM_CLIENTE
                AND T039_CTA_EJE     = @T039_CTA_EJE
                AND T039_ESTAT_CTA_META IN ('AC','IN','PA')
                AND T039_LOG_METAS   = 'ALCANCIA'
SET NOCOUNT OFF
GO
