SELECT
        SYST.name                                   AS TableName,
        SYSI.name                                   AS IndexName,
        SYSP.[Rows]                                 AS RecordCount,
        sum(SYSA.total_pages)                       AS TotalPages,
        sum(SYSA.used_pages)                        AS UsedPages,
        sum(SYSA.data_pages)                        AS DataPages,
        (sum(SYSA.total_pages) * 8) / 1024          AS TotalSpaceMB,
        (sum(SYSA.used_pages) * 8) / 1024           AS UsedSpaceMB,
        (sum(SYSA.data_pages) * 8) / 1024           AS DataSpaceMB
FROM    sys.tables                                  AS SYST
INNER JOIN
        sys.indexes                                 AS SYSI
ON      SYST.object_id = SYSI.object_id
INNER JOIN
        sys.partitions                              AS SYSP
ON      SYSI.object_id = SYSP.OBJECT_ID
        AND SYSI.index_id = SYSP.index_id
INNER JOIN
        sys.allocation_units                        AS SYSA
ON      SYSP.partition_id = SYSA.container_id
WHERE
        SYST.name NOT LIKE 'dt%'
        AND SYSI.OBJECT_ID > 255
        AND SYSI.index_id <= 1
GROUP BY
        SYST.name, SYSI.object_id, SYSI.index_id, SYSI.name, SYSP.[Rows]
ORDER BY
        TableName
;

