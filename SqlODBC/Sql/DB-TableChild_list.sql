SELECT  DISTINCT
        OBJECT_NAME(referenced_object_id)   AS TableChild
FROM    sys.foreign_key_columns
WHERE   referenced_object_id
        IN
        (
            SELECT  parent_object_id
            FROM    sys.foreign_key_columns
        )
ORDER BY
        TableChild
;

