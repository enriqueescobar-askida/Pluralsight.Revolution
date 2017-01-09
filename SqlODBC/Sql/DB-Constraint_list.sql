SELECT
        DB_NAME()                                       AS DBName
        , OBJECT_NAME(parent_object_id)                 AS TableName
        , parent_column_id                              AS ColumnNumber
        , COL_NAME(parent_object_id, parent_column_id)  AS ColumnName
        , Name                                          AS ConstraintName
        , type                                          AS ConstraintType
        , type_desc                                     AS ConstraintDescription
        , create_date                                   AS CreatedDate
        , OBJECT_DEFINITION(object_id)                  AS ConstraintDefinition
        , *
FROM    sys.Check_constraints
ORDER BY
        TableName, ColumnNumber
;

