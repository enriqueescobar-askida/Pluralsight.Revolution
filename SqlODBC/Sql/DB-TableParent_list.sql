SELECT  DISTINCT
        OBJECT_NAME(referenced_object_id)       AS TableParent
        , OBJECT_NAME(parent_object_id)         AS TableChild
        , OBJECT_NAME(constraint_object_id)     AS FKName
FROM    sys.foreign_key_columns
ORDER BY
        TableParent, TableChild
;

