SELECT 
        SYSO.[name]                     AS ObjectName
        ,SCHEMA_NAME(SCHEMA_ID)         AS SchemaName
        ,SYSO.type_desc                 AS ObjectTypeDesc
        ,SYSP.parameter_id              AS ParameterID
        ,SYSP.[name]                    AS ParameterName
        ,TYPE_NAME(SYSP.user_type_id)   AS ParameterType
        ,SYSP.max_length                AS ParameterMaxLength
        ,SYSP.[precision]               AS ParameterPrecision
        ,SYSP.max_length                AS ParamMaxBytes
        ,SYSP.scale                     AS ParameterScale
        ,SYSP.is_output                 AS IsParameterOutput
FROM    sys.objects                     AS SYSO
INNER   JOIN
        sys.parameters
    AS  SYSP
ON      SYSO.object_id = SYSP.object_id
ORDER BY
        ObjectName, ObjectTypeDesc, ParameterID
;

