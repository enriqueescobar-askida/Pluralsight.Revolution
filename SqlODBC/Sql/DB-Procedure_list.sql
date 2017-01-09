SELECT
        name                              AS ProcedureName
        , object_id                       AS ProcedureID
        , type                            AS ProcedureType
        , type_desc                       AS ProcedureDesc
        , create_date                     AS ProcedureCreated
        , modify_date                     AS ProcedureModified
        , is_ms_shipped                   AS IsProcedureMSShipped
FROM    sys.procedures
ORDER BY
        ProcedureName
;

