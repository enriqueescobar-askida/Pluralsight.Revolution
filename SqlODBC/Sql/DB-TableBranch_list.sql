SELECT  DISTINCT
        OBJECT_NAME(referenced_object_id)       AS TableBranchName
FROM    sys.foreign_key_columns
WHERE   referenced_object_id
        IN
        (
            SELECT  parent_object_id
            FROM    sys.foreign_key_columns
        )
        AND
        parent_object_id
        NOT IN
        (
            SELECT  referenced_object_id
            FROM    sys.foreign_key_columns
        )
ORDER BY TableBranchName
;

