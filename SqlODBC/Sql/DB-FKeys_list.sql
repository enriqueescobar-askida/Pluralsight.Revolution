SELECT 
        OBJECT_NAME(SYSFK.parent_object_id)                          AS TableName
        , COL_NAME(FC.parent_object_id, FC.parent_column_id)         AS ColumnName
        , SYSFK.name                                                 AS ForeignKey
        , SYSFK.object_id                                            AS ForeignKeyID
        , OBJECT_NAME(SYSFK.referenced_object_id)                    AS ReferenceTableName
        , COL_NAME(FC.referenced_object_id, FC.referenced_column_id) AS ReferenceColumnName
        , SYSFK.create_date                                          AS FKCreated
        , SYSFK.modify_date                                          AS FKModified
        , SYSFK.is_not_trusted                                       AS FKnotTrusted
        , SYSFK.delete_referential_action_desc                       AS OnDelete
        , SYSFK.update_referential_action_desc                       AS OnUpdate
FROM    sys.foreign_keys                                             AS SYSFK
INNER JOIN
        sys.foreign_key_columns                                      AS FC
ON      SYSFK.OBJECT_ID = FC.constraint_object_id
ORDER BY
        TableName, ColumnName, ForeignKey
;

