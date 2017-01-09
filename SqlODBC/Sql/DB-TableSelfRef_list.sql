SELECT  DISTINCT
        OBJECT_NAME(referenced_object_id)                       AS ParentTableName
        , COL_NAME(referenced_object_id, referenced_column_id)  AS ParentColumnName
        , OBJECT_NAME(parent_object_id)                         AS ChildTableName
        , COL_NAME(parent_object_id, parent_column_id)          AS ChildColumnName
        , OBJECT_NAME(constraint_object_id)                     AS ForeignKeyName
FROM    sys.foreign_key_columns
WHERE   referenced_object_id = parent_object_id
ORDER BY
        ParentTableName, ChildTableName
;

