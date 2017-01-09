SELECT 
        [name]                    AS TableName
        ,object_id                AS TableID
        ,[type]                    AS TableType
        ,type_desc                AS TableDesc
        ,create_date            AS TableCreated
        ,modify_date            AS TableModified
        ,max_column_id_used        AS MaxColumnIDUsed
        ,uses_ansi_nulls        AS IsUsingANSINulls
        ,lob_data_space_id        AS LOBDataSpaceID
FROM    sys.tables
ORDER BY
        TableName
;

