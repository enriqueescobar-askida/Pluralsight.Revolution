SELECT   
        SYSO.[name]                     AS FunctionName
        ,SCHEMA_NAME(SCHEMA_ID)         AS SchemaName
        ,SYSO.type                      AS FunctionType
        ,SYSO.type_desc                 AS FunctionDesc
        ,SYSP.parameter_id              AS ParameterID
        ,SYSP.[name]                    AS ParameterName
        ,TYPE_NAME(SYSP.user_type_id)   AS ParameterType
        ,SYSP.max_length                AS ParamMaxLength
        ,SYSP.[precision]               AS ParameterPrecision
        ,SYSP.scale                     AS ParameterScale
        ,SYSP.is_output                 AS IsParamOutput
FROM    sys.objects                     AS SYSO
INNER   JOIN
        sys.parameters                  AS SYSP
ON      SYSO.object_id = SYSP.object_id
WHERE   SYSO.type_desc NOT LIKE '%PROCEDURE%'
ORDER BY
        FunctionName, FunctionDesc, ParameterID
;

