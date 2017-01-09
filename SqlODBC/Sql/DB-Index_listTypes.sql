SELECT       
        SYSO.name                                   AS TableName
        , SYSO.type                                 AS TableType
        , SYSO.type_desc                            AS TableDesc
        , SYSP.index_id                             AS TableIndexID
        , COALESCE(SYSI.name, 'NA')                 AS IndexName
        , SYSI.object_id                            AS IndexID
        , SYSI.type_desc                            AS IndexType
        , COALESCE(TC.CONSTRAINT_TYPE, 'NA')        AS ConstraintType
        , SYSA.allocation_unit_id                   AS AllocUnitID
FROM    sys.partitions                              AS SYSP
INNER JOIN
        sys.allocation_units                        AS SYSA
ON      SYSP.hobt_id = SYSA.container_id
INNER JOIN
        sys.objects                                 AS SYSO
ON      SYSP.[object_id] = SYSO.[object_id]
INNER JOIN
        sys.indexes                                 AS SYSI
ON      SYSO.[object_id] = SYSI.[object_id]
        AND
        SYSP.index_id = SYSI.index_id
LEFT OUTER JOIN
        INFORMATION_SCHEMA.TABLE_CONSTRAINTS        AS TC
ON        TC.CONSTRAINT_NAME=SYSI.name
ORDER BY
        TableType, TableName, TableIndexID, IndexName, IndexType
;

