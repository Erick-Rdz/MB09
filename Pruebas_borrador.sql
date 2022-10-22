
DECLARE @CuentaN char(4000)
 SELECT @CuentaN =  DCuenta FROM       
        (   SELECT TOP 15 
                  ISNULL(A.T071_DAT_OPERATION,'') + ISNULL(STR(A.T071_NUM_OPERATION),'') COLLATE SQL_Latin1_General_CP1_CI_AS + ',' + --20 
                  ISNULL(CAST(A.T071_NUM_OPERATION as varchar(9)),'000000000') + ',' + -- 9
                  ISNULL(A.T071_DAT_OPERATION,'') + ',' + -- 10
                  ISNULL(T071_DAT_VALUE,'') + ',' + -- 10
                  ISNULL(T071_TIM_OPERATION,'') + ',' + -- 4
                  ISNULL(CAST(T071_AMOUNT as varchar(15)),'+00000000000.00') + ',' + -- 17
                  ISNULL(T071_CODE,'   ') + ',' + -- 3
                  ISNULL(T071_OBSERVATIONS,'') + ',' + -- 31
                  ISNULL(T071_COD_PRODUCT,'') + ',' + -- 2 
                  ISNULL(T071_COD_SPROD,'') + ',' + -- 4 
                  ISNULL(T071_FLG_FREE1,'') + ',' + -- 3
                  ISNULL(T071_USERUPD,'') + ',' + -- 8 
                  ISNULL(T071_NTNMUPD,'') + ',' + -- 8 
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
                  ISNULL(T606_CHAR_FREE1,'') + ',' +  -- 30
                  ISNULL(CAST(A.T071_INTREF as varchar(15)),'               ') + ',' + -- 15 
                  ISNULL(CAST(C.T043_NUM_OPERATION AS varchar(9)),'') + ',' + 
                  ISNULL(CAST(D.T803_ENT_ACC AS varchar(4)),'') + ',' AS 'DCuenta',
                  ROW_NUMBER() OVER(ORDER BY A.T071_DAT_OPERATION DESC , A.T071_NUM_OPERATION DESC) As "RowNum"
                FROM MAZP.BGDT071 AS A with (nolock) 
                LEFT JOIN MAZP.BLDT002 with (nolock) ON 
                        T100_CODE = '826' AND
                        T100_LANGUAGE = 'E'
                LEFT JOIN  MAZP.BGDT606 with (nolock) ON
                            T606_ACC           = 0115574163
                        AND T606_NUM_OPERATION = 948
                        AND T606_DAT_OPERATION = '2008-01-16'
                LEFT JOIN MAZP.MCDT043 AS C with (nolock) ON 
                        C.T043_ENT_ACC=0127  AND
                            C.T043_BRN_ACC=0100   AND
                        C.T043_TYP_ACC=13 AND
                        C.T043_ACC=45958752 AND 
                            C.T043_DAT_ACCT=A.T071_DAT_OPERATION AND
                        LTRIM(C.T043_NUM_OPE_2)=LTRIM(A.T071_NUM_OPERATION)
                LEFT JOIN MAZP.MCDT803 AS D with(nolock) ON
                        D.T803_ACC=45958752 AND
                        D.T803_BRN_ACC=0100 AND 
                        D.T803_TYP_ACC=13 AND 
                        D.T803_ENT_ACC=0127 AND
                        D.T803_NUM_CARD=C.T043_NUM_CARD AND 
                        D.T803_NUM_BIN_CRD=C.T043_NUM_BIN_CRD AND 
                        D.T803_NUM_OPERATION=C.T043_NUM_OPERATION AND
                        D.T803_TKN_Q2='02'                       
                    WHERE A.T071_ACC = '1345958752' AND 
                        A.T071_CEN_REG = 0100 AND 
                        A.T071_ENT = 0127 AND
                        A.T071_DAT_OPERATION > = '2008-01-16' AND --VAR ENTRADA
                A.T071_DAT_OPERATION + STR(A.T071_NUM_OPERATION) < '9999-12-31 999999999' AND --FIJO
                A.T071_FLG_ANN = 'N' --FIJO
                ORDER BY A.T071_DAT_OPERATION DESC , A.T071_NUM_OPERATION DESC 
        ) AS "DatosCuenta71"

        --SELECT * from string_split(@CuentaN, ',')
        -- SELECT * from string_split(@CuentaN, ',', 1)
SELECT * FROM (SELECT  value,
        row_number() OVER (ORDER BY current_timestamp) AS 'RW'
FROM    string_split(@CuentaN, ',') C) AS "SLC" WHERE RW = 4
