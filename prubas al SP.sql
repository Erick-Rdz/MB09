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

DECLARE @fecha_tsm Date
SET @fecha_tsm = GETDATE()
PRINT
EXEC SP_MB09Prueba01 9546, 06, @fecha_tsm,NULL," ",0127, '693d4754f284494c89b23b0276559834', '0673', '30', '9999-12-31 999999999'

067
30100291521