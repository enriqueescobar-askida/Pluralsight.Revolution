SELECT  DISTINCT
        OBJECT_NAME(referenced_object_id)   AS TableTrunkName
FROM    sys.foreign_key_columns
WHERE   referenced_object_id
        NOT IN
        (
            SELECT  parent_object_id
            FROM    sys.foreign_key_columns
        )
ORDER BY
        TableTrunkName
;

