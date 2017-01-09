SELECT  DISTINCT
        OBJECT_NAME(parent_object_id)       AS TableLeafName
FROM    sys.foreign_key_columns
WHERE   parent_object_id
        NOT IN
        (
            SELECT  referenced_object_id
            FROM    sys.foreign_key_columns
        )
ORDER BY
        TableLeafName
;

