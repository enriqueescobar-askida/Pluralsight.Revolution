SELECT    FUNCMO.FunctionName
        , FUNCSO.ObjectID                    AS FunctionID
        , FUNCMO.FunctionType
        , FUNCMO.FunctionDesc
        , FUNCMO.FunctionCreated
        , FUNCMO.FunctionModified
FROM
(
    SELECT  [name]                           AS FunctionName
            ,[type]                          AS FunctionType
            ,type_desc                       AS FunctionDesc
            ,create_date                     AS FunctionCreated
            ,modify_date                     AS FunctionModified
    FROM    sys.sql_modules                  AS SYSM
    INNER JOIN
            sys.objects                      AS SYSO
    ON      SYSM.object_id=SYSO.object_id
    WHERE   type_desc LIKE '%function%'
)                                            AS FUNCMO
INNER JOIN
(
    SELECT  [name]                           AS ObjectName
            ,[id]                            AS ObjectID
            ,[type]                          AS ObjectType
            ,[crdate]                        AS ObjectCreated
    FROM    dbo.sysobjects
    WHERE   [type]  IN ('FN', 'IF', 'TF')
)                                            AS FUNCSO
ON      FUNCMO.FunctionName = FUNCSO.ObjectName
ORDER BY
        FUNCMO.FunctionName
;

