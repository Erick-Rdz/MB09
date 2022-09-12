SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [MAZP].[MB09_MB2CF119]
@IP_OPCION CHAR (1),@ENT_IN char(4), @BDMID_IN char(40), @BRN_OPEN char(4), @COD_PROD char(2), @NUM_ACC char(8), @FECHA char(10), @ULLAVE char(20), @REG1 char(4000) OUTPUT, @REG2 char(4000) OUTPUT, @REG3 char(4000) OUTPUT, @REG4 char(4000) OUTPUT, @REG5 char(4000) OUTPUT, @REG6 char(4000) OUTPUT, @REG7 char(4000) OUTPUT, @REG8 char(4000) OUTPUT, @REG9 char(4000) OUTPUT, @REG10 char(4000) OUTPUT, @REG11 char(4000) OUTPUT, @REG12 char(4000) OUTPUT, @REG13 char(4000) OUTPUT, @REG14 char(4000) OUTPUT, @REG15 char(4000) OUTPUT
,@BAN71 CHAR(03),@FECHA_ACCT char(10),
@MOV71 VARCHAR(MAX) ='{}'OUT,@MOV710 VARCHAR(MAX)='{}' OUT, @403_Json VARCHAR(MAX)=' ' OUT,@datosCuenta VARCHAR(MAX)=' ' OUT
WITH EXEC AS CALLER
AS
SET NOCOUNT ON
SET ANSI_WARNINGS ON

    DECLARE @CuentaN char(4000)
    DECLARE @CuentaN710 char(4000)
    DECLARE @Cont int
    DECLARE @NumReg int
      
    DECLARE @403 char(100)
    --DECLARE @008_041_140 char(300)

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

    --V5
    DECLARE @NumRegRest int =0

    --SUMA SOBRES
    DECLARE @OP_T039_SALDO CHAR (5) = NULL 
    DECLARE @OP_T039_ID_CTA_META CHAR (5) = NULL 


    SET @Cont = 1 
    SET @NumReg=15

    SET LOCK_TIMEOUT 300

    
    --CONSULTA-PARAM-DIGITALES
        SELECT  @T140_DES_TABLE = T140_DES_TABLE
               WHERE T140_KEY_TABLE = 'BINVIRTUAL'
                 AND T140_COD_TABLE = 'MBT1'
                 AND T140_LANGUAGE  = 'E'
                 AND T140_ENTITY    = '0127'
    --------------------------------------------------

    --1100-VAL-USUADIO-SAPP
    SELECT @T140_DES_TABLE =T140_DES_TABLE
             FROM MBDT140 with (nolock)
             WHERE  T140_KEY_TABLE  =T140-KEY-TABLE AND
                    T140_COD_TABLE  ='SAPP' AND
                    T140_LANGUAGE   ='E' AND
                    T140_ENTITY     ='0127'
    --------------------------------------------------

    -- 22000-LLAMADO-SP
    SELECT @T140_DES_TABLE=ISNULL(T140_DES_TABLE,' ') --6,            
            FROM MBDT140 with (nolock)
               WHERE T140_KEY_TABLE  = 'MB09'
                 AND T140_COD_TABLE  = 'JSON'
                 AND T140_ENTITY     =  0127
                 AND T140_LANGUAGE   = 'E'
    
    
    -- EN CASO DE QUE SEA ALMACENADO
    IF (@IP_OPCION = 'A') --MB09_MB2CF119

        BEGIN

            --GET MAIN DATA FOR NEXT QUERYS
            SELECT @403_NUM_BIN=ISNULL(T403_NUM_BIN,' ') ,--6,
                    @403_NUM_CARD=ISNULL(T403_NUM_CRD,' '), --10
                    @403_NUM_CLTE=ISNULL(T403_NUM_CLTE,' '), --8
                    @403_NUM_CTA=ISNULL(T403_NUM_CTA,' '), --20
                    @403_TEL_CEL=ISNULL(T403_TEL_CEL,' ')
                FROM MAZP.MCDT403 AS A with (nolock) 
            WHERE T403_BDMID = @BDMID_IN
            


            -- SECOND SECTION    
            SELECT @008_041_140=ISNULL(A.NUM_CUS,' '),--8
                    @T041_CAC_DIG1 = ISNULL(B.T041_CAC_DIG1,' '), -- 1
                    @T041_CAC_DIG2= ISNULL(B.T041_CAC_DIG2,' '),-- 1
                    @T041_COD_PRODUCT= ISNULL(B.T041_COD_PRODUCT,' ') , -- 2
                    @T041_COD_SPROD=ISNULL(B.T041_COD_SPROD,' '), -- 4
                    @T041_CEN_ACCT= ISNULL(B.T041_CEN_ACCT,' ') , -- 4
                    @T041_FCC= ISNULL(B.T041_FCC,' ') , -- 4
                    @T140_DES_TABLE=ISNULL(C.T140_DES_TABLE,' ')-- 250
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


            --CURSORES  TERCERA SECCION
            DECLARE CuentasCursor CURSOR 
            FOR 
                          
            SELECT TOP 15 
                ISNULL(T071_DAT_OPERATION,' ') + ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') COLLATE SQL_Latin1_General_CP1_CI_AS + '|@' + --20 
                ISNULL(T071_DAT_OPERATION,' ') + ISNULL(STR(T071_NUM_OPERATION),' ') COLLATE SQL_Latin1_General_CP1_CI_AS + '|@' + --20 
                ISNULL(CAST(T071_NUM_OPERATION as varchar(9)),'000000000') + '|@' + -- 9
                ISNULL(T071_DAT_OPERATION,' ') + '|@' + -- 10
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
                ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + '|@'  -- 15
                
                    FROM MAZP.BGDT071 AS A with (nolock) 
                LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                        T100_CODE = T071_CODE AND
                        T100_LANGUAGE = 'E'
                LEFT JOIN  MAZP.BGDT606 with (nolock) ON
                            T606_ACC           = T071_ACC
                        AND T606_NUM_OPERATION = T071_NUM_OPERATION
                        AND T606_DAT_OPERATION = T071_DAT_OPERATION
                    
                    WHERE T071_ACC = @COD_PROD+@NUM_ACC AND 
                        T071_CEN_REG = @BRN_OPEN AND 
                        T071_ENT = @ENT_IN AND
                        T071_DAT_OPERATION > = @FECHA AND --VAR ENTRADA
                        --T071_DAT_OPERATION + CAST(T071_NUM_OPERATION as varchar(9)) COLLATE SQL_Latin1_General_CP1_CI_AS < @ULLAVE AND --FIJO
                T071_DAT_OPERATION + STR(T071_NUM_OPERATION) < @ULLAVE AND --FIJO
                T071_FLG_ANN = 'N' --FIJO
                ORDER BY T071_DAT_OPERATION DESC , T071_NUM_OPERATION DESC  
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
                    ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + '|@'   -- 15
        
                    FROM MAZP.BGDT710 as A with (nolock) 
                LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                            T100_CODE = T071_CODE AND
                            T100_LANGUAGE = 'E'
                LEFT JOIN MAZP.BGDT606 with (nolock) ON
                            T606_ACC           = T071_ACC
                        AND T606_NUM_OPERATION = T071_NUM_OPERATION
                        AND T606_DAT_OPERATION = T071_DAT_OPERATION
                    
                    WHERE T071_ACC = @COD_PROD+@NUM_ACC AND 
                            T071_CEN_REG = @BRN_OPEN AND 
                            T071_ENT = @ENT_IN AND
                        T071_DAT_OPERATION > = @FECHA AND --VAR ENTRADA
                        T071_DAT_OPERATION + STR(T071_NUM_OPERATION) COLLATE SQL_Latin1_General_CP1_CI_AS < @ULLAVE AND --FIJO
                        T071_FLG_ANN = 'N' --FIJO
                    ORDER BY T071_DAT_OPERATION DESC , T071_NUM_OPERATION DESC
                FOR READ ONLY
                
                OPEN CuentasCursor710

                FETCH NEXT FROM CuentasCursor710 INTO @CuentaN710
                
                WHILE @@fetch_status = 0 and @Cont < ( @NumReg + 1) 
                BEGIN
                                        
                    IF @Cont = 1
                    BEGIN
                        SET @REG1 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 2
                    BEGIN
                        SET @REG2 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 3
                    BEGIN
                        SET @REG3 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 4
                    BEGIN
                        SET @REG4 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 5
                    BEGIN
                        SET @REG5 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 6
                    BEGIN
                        SET @REG6 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 7
                    BEGIN
                        SET @REG7 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 8
                    BEGIN
                        SET @REG8 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 9
                    BEGIN
                        SET @REG9 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 10
                    BEGIN
                        SET @REG10 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 11
                    BEGIN
                        SET @REG11 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 12
                    BEGIN
                        SET @REG12 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 13
                    BEGIN
                        SET @REG13 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 14
                    BEGIN
                        SET @REG14 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    IF @Cont = 15
                    BEGIN
                        SET @REG15 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))+@CuentaN710
                    END
                    
                    SET @Cont = @Cont + 1
                
                    FETCH NEXT FROM CuentasCursor710 INTO @CuentaN710
                    
                END
                
                CLOSE  CuentasCursor710
                DEALLOCATE CuentasCursor710
      
            END
      
                IF @Cont = 1
                BEGIN
                    SET @REG1 = RTRIM(isnull(@403,' |@ |@ |@ |@9|@ '))+RTRIM(isnull(@008_041_140,' |@ |@ |@ |@ |@ |@ |@ |@ '))
                END
        END

    

    ELSE IF (@IP_OPCION = 'B') -- MB09_MB2CF119 V2
        BEGIN

            --GET MAIN DATA FOR NEXT QUERYS
            SELECT @403_NUM_BIN=ISNULL(T403_NUM_BIN,' ') ,--6,
                    @403_NUM_CARD=ISNULL(T403_NUM_CRD,' '), --10
                    @403_NUM_CLTE=ISNULL(T403_NUM_CLTE,' '), --8
                    @403_NUM_CTA=ISNULL(T403_NUM_CTA,' '), --20
                    @403_TEL_CEL=ISNULL(T403_TEL_CEL,' ')
                FROM MAZP.MCDT403 AS A with (nolock) 
            WHERE T403_BDMID = @BDMID_IN            

            -- SECOND SECTION    
            SELECT @008_041_140=ISNULL(A.NUM_CUS,' '),--8
                    @T041_CAC_DIG1 = ISNULL(B.T041_CAC_DIG1,' '), -- 1
                    @T041_CAC_DIG2= ISNULL(B.T041_CAC_DIG2,' '),-- 1
                    @T041_COD_PRODUCT= ISNULL(B.T041_COD_PRODUCT,' ') , -- 2
                    @T041_COD_SPROD=ISNULL(B.T041_COD_SPROD,' '), -- 4
                    @T041_CEN_ACCT= ISNULL(B.T041_CEN_ACCT,' ') , -- 4
                    @T041_FCC= ISNULL(B.T041_FCC,' ') , -- 4
                    @T140_DES_TABLE=ISNULL(C.T140_DES_TABLE,' ')-- 250
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

        END
        
    
    
    ELSE IF (@IP_OPCION = 'C') --MB09_MB2CF119 V5
        BEGIN
            SET @403_Json  = ISNULL
                    ((SELECT ISNULL(T403_NUM_BIN,' ')  AS T403_NUM_BIN,
                    ISNULL(T403_NUM_CRD,' ')  AS T403_NUM_CRD,
                    ISNULL(T403_NUM_CLTE,' ') AS  T403_NUM_CLTE,
                    ISNULL(T403_NUM_CTA,' ') AS T403_NUM_CTA,  
                    ISNULL(T403_TEL_CEL,' ')  AS  T403_TEL_CEL
                FROM MAZP.MCDT403 AS A with (nolock) 
            WHERE T403_BDMID = @BDMID_IN FOR JSON PATH,WITHOUT_ARRAY_WRAPPER  ),' ')

                    
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
    END
    


    --SUMA SOBRES
    SELECT 
        @OP_T039_SALDO_SOBRES= SUM(T039_SALDO),
        @OP_T039_ID_CTA_META_SOBRES=COUNT(T039_ID_CTA_META)                 
              FROM  MAZP.MBDT039 with(nolock)
              WHERE T039_NUM_CLIENTE = @T039-NUM-CLIENTE
                AND T039_CTA_EJE     = @T039-CTA-EJE
                AND T039_ESTAT_CTA_META IN ('SA','SP','SV','SS')
                AND T039_LOG_METAS   = 'SOBRES'

    --SUMA ALCANCIA
      SELECT @OP_T039_SALDO_ALCANCIA = SUM(T039_SALDO),
             @OP_COUNT_ALCANCIA= COUNT(T039_ID_CTA_META)
              FROM  MAZP.MBDT039 with(nolock)
              WHERE T039_NUM_CLIENTE = @T039-NUM-CLIENTE
                AND T039_CTA_EJE     = @T039-CTA-EJE
                AND T039_ESTAT_CTA_META IN ('AC','IN','PA')
                AND T039_LOG_METAS   = 'ALCANCIA'



SET NOCOUNT OFF
GO