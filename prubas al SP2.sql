
DECLARE @RC int
DECLARE @IP_CAA_CEN_ACCOUNT varchar(4)
DECLARE @IP_CAA_CHANN varchar(2)
DECLARE @IP_WSS_RET_CTA char(5)
DECLARE @IP_WSS_CUENTA char(5)
DECLARE @IP_WSS_CUENTA_710 char(5)
DECLARE @IP_OPCION char(1)
DECLARE @IP_ENT_IN char(4)
DECLARE @IP_BDMID_IN char(40)
DECLARE @IP_NUMCUEN char (14)
--DECLARE @IP_BRN_OPEN char(4)
--DECLARE @IP_COD_PROD char(2)
--DECLARE @IP_NUM_ACC char(8)
DECLARE @IP_FECHA char(10)
DECLARE @IP_ULLAVE char(20)
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
DECLARE @BAN71 char(3)
DECLARE @IP_FECHA_ACCT char(10)
DECLARE @MOV71 varchar(max)
DECLARE @MOV710 varchar(max)
DECLARE @403_Json varchar(max)
DECLARE @datosCuenta varchar(max)

-- TODO: Set parameter values here.
SET @IP_CAA_CEN_ACCOUNT = 9546
SET @IP_CAA_CHANN = 06
SET @IP_WSS_RET_CTA = 'TRUE'
SET @IP_WSS_CUENTA ='TRUE'
SET @IP_WSS_CUENTA_710 ='TRUE'
SET @IP_OPCION =null
SET @IP_ENT_IN =0127
SET @IP_BDMID_IN = '693d4754f284494c89b23b0276559834'
SET @IP_NUMCUEN = '06730100291521'
--SET @IP_BRN_OPEN = 0673
--SET @IP_COD_PROD = 0673
--SET @IP_NUM_ACC = 0291521
SET @IP_FECHA = GETDATE()
SET @IP_ULLAVE = '9999-12-31 999999999'

EXECUTE @RC = [MAZP].[SP_MB09Prueba01] 
   @IP_CAA_CEN_ACCOUNT
  ,@IP_CAA_CHANN
  ,@IP_WSS_RET_CTA
  ,@IP_WSS_CUENTA
  ,@IP_WSS_CUENTA_710
  ,@IP_OPCION
  ,@IP_ENT_IN
  ,@IP_BDMID_IN
  ,@IP_NUMCUEN
  ,@IP_FECHA
  ,@IP_ULLAVE
  ,@REG1 OUTPUT
  ,@REG2 OUTPUT
  ,@REG3 OUTPUT
  ,@REG4 OUTPUT
  ,@REG5 OUTPUT
  ,@REG6 OUTPUT
  ,@REG7 OUTPUT
  ,@REG8 OUTPUT
  ,@REG9 OUTPUT
  ,@REG10 OUTPUT
  ,@REG11 OUTPUT
  ,@REG12 OUTPUT
  ,@REG13 OUTPUT
  ,@REG14 OUTPUT
  ,@REG15 OUTPUT
  ,@BAN71
  ,@IP_FECHA_ACCT
  ,@MOV71 OUTPUT
  ,@MOV710 OUTPUT
  ,@403_Json OUTPUT
  ,@datosCuenta OUTPUT


  select 
   @IP_CAA_CEN_ACCOUNT
  ,@IP_CAA_CHANN
  ,@IP_WSS_RET_CTA
  ,@IP_WSS_CUENTA
  ,@IP_WSS_CUENTA_710
  ,@IP_OPCION
  ,@IP_ENT_IN
  ,@IP_BDMID_IN
  ,@IP_BRN_OPEN
  ,@IP_COD_PROD
  ,@IP_NUM_ACC
  ,@IP_FECHA
  ,@IP_ULLAVE
  ,@REG1 
  ,@REG2 
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
  ,@REG15 
  ,@BAN71
  ,@IP_FECHA_ACCT
  ,@MOV71 
  ,@MOV710 
  ,@403_Json 
  ,@datosCuenta 
GO