SELECT
        Name                    AS TableWoParentWoDependents
        , object_id             AS TableID
        , [type]                AS TableType
        , type_desc             AS TableDesc
        , create_date           AS TableCreated
        , modify_date           AS TableModified
        , max_column_id_used    AS MaxColumnIDUsed
        , uses_ansi_nulls       AS IsUsingANSINulls
        , lob_data_space_id     AS LOBDataSpaceID
FROM    sys.tables
WHERE
        object_id
        NOT IN
        (
            SELECT  referenced_object_id
            FROM    sys.foreign_key_columns
        )
        AND
        object_id
        NOT IN
        (
            SELECT  parent_object_id
            FROM    sys.foreign_key_columns
        )
ORDER BY TableWoParentWoDependents
;

