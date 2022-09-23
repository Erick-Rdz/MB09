--- VARIABLES DE ENTRADA PARA DIFERENTES PROCEDIMIENTOS 
DECLARE @CAA_CEN_ACCOUNT VARCHAR(4) = 9546
DECLARE @CAA_CHANN varchar (2) = 06
DECLARE @fecha_tsm Date
DECLARE @T140_DES_TABLE CHAR (250) = NULL-- 250


 ------------ 22000-CALCULA-FECHA (SE RESTAN 3 MESES A LA FECHA EN CURSO)
   SET @fecha_tsm = DATEADD(MONTH, -3,GETDATE())


 ------------ 22000-LLAMADO-SP
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
 
 ------------ CONSULTA-PARAM-DIGITALES
        SELECT  @T140_DES_TABLE = T140_DES_TABLE
                FROM MBDT140 with(nolock)
               WHERE T140_KEY_TABLE = 'BINVIRTUAL'
                 AND T140_COD_TABLE = 'MBT1'
                 AND T140_LANGUAGE  = 'E'
                 AND T140_ENTITY    = '0127'

        
 --------------------------------------------------

    DECLARE @OK_SAPP char(5) = 'FALSE'
    DECLARE @NOK_SAPP char(5) = 'FALSE'

    --1100-VAL-USUADIO-SAPP
    IF (@CAA_CEN_ACCOUNT = 1156 AND @CAA_CHANN = 54)
        IF EXISTS(SELECT T140_DES_TABLE
             FROM MBDT140 with (nolock)
             WHERE  T140_KEY_TABLE  ='BINVIRTUAL' AND
                    T140_COD_TABLE  ='SAPP' AND
                    T140_LANGUAGE   ='E' AND
                    T140_ENTITY     ='0127')
                    SET @OK_SAPP = 'TRUE'
                    ELSE
                    SET @NOK_SAPP = 'TRUE'
                    
    --------------------------------------------------
    PRINT 'SAPP S ' + @OK_SAPP
    PRINT 'SAPP N ' + @NOK_SAPP
    PRINT 'T140_DES_TABLE ' + @T140_DES_TABLE
    PRINT 'PRENDE_JSON ' +  @PRENDE_JSON
    PRINT 'APAGA_JSON ' + @APAGA_JSON


--- VARIABLES DE ENTRADA PARA DIFERENTES PROCEDIMIENTOS 

-- @CAA_CEN_ACCOUNT --  9546
-- @CAA_CHANN -- 06
-- @IP_WSS_RET_CTA -- FALSE
-- @IP_WSS_CUENTA -- TRUE
-- @IP_WSS_CUENTA_710 -- TRUE



--- VARIABLES ENTRADA SP's

-- IP_OPCION -- Null
-- ENT_IN  -- 0127
-- BDMID_IN -- 693d4754f284494c89b23b0276559834
-- BRN_OPEN -- 0673
-- COD_PROD -- 30
-- NUM_ACC -- 0291521
-- FECHA -- @fecha_tsm
-- ULLAVE --'9999-12-31 999999999'
--------------------
DECLARE @fecha_tsm Date;
DECLARE @REG1 char(4000);    
DECLARE @REG2 char(4000);    
DECLARE @REG3 char(4000);    
DECLARE @REG4 char(4000);    
DECLARE @REG5 char(4000);    
DECLARE @REG6 char(4000);    
DECLARE @REG7 char(4000);    
DECLARE @REG8 char(4000);    
DECLARE @REG9 char(4000);    
DECLARE @REG10 char(4000);    
DECLARE @REG11 char(4000);    
DECLARE @REG12 char(4000);    
DECLARE @REG13 char(4000);    
DECLARE @REG14 char(4000);    
DECLARE @REG15 char(4000);

DECLARE @BAN71 CHAR(03); 
DECLARE @FECHA_ACCT char(10);
DECLARE @MOV71 VARCHAR(MAX) ='{}';
DECLARE @MOV710 VARCHAR(MAX)='{}'; 
DECLARE @403_Json VARCHAR(MAX)=' '; 
DECLARE @datosCuenta VARCHAR(MAX)=' ';

SET @fecha_tsm = GETDATE()
PRINT @fecha_tsm
EXEC SP_MB09Prueba01 '9546', '06', 'FALSE', 'TRUE', 'TRUE', NULL, '0127', '693d4754f284494c89b23b0276559834', '0673', '30', '0291521', @fecha_tsm, '9999-12-31 999999999', @REG1, @REG2, @REG3, @REG4, @REG5, @REG6, @REG7, @REG8, @REG9, @REG10, @REG11, @REG12, @REG13, @REG14, @REG15, @BAN71, @FECHA_ACCT, @MOV71, @MOV710, @403_Json, @datosCuenta


--06730100291521