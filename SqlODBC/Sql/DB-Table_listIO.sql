SELECT
        object_schema_name(USTAT.object_id)                                         AS ObjectSchema
        , object_name(USTAT.object_id)                                              AS [ObjectName]
        ,CASE
            WHEN SUM(User_Updates + User_Seeks + User_Scans + User_Lookups) = 0
                THEN NULL
            ELSE
                CAST(SUM(User_Seeks + User_Scans + User_Lookups)                    AS DECIMAL)
                        / CAST(SUM(User_Updates
                                    + User_Seeks
                                    + User_Scans
                                    + User_Lookups)                                 AS DECIMAL(19,2))
            END                                                                     AS [ReadRatio]
        , CASE
            WHEN SUM(User_Updates + User_Seeks + User_Scans + User_Lookups) = 0
                THEN NULL
            ELSE
                CAST(SUM(User_Updates)                                              AS DECIMAL)
                    / CAST(SUM(User_Updates
                                + User_Seeks
                                + User_Scans
                                + User_Lookups)                                     AS DECIMAL(19,2))
            END                                                                     AS [WriteRatio]
        , SUM(User_Seeks + User_Scans + User_Lookups)                               AS [TotalReads]
        , SUM(User_Updates)                                                         AS [TotalWrites]
FROM
        sys.dm_db_Index_Usage_Stats                                                 AS USTAT
JOIN
        Sys.Indexes                                                                 AS SYSI
ON      USTAT.object_id = SYSI.object_id
        AND USTAT.index_Id = SYSI.index_Id
JOIN    sys.tables                                                                  AS SYST
ON      SYST.object_id = USTAT.object_id
WHERE   SYSI.Type_Desc      IN ( 'Clustered', 'Heap' )
GROUP BY
        USTAT.object_id
ORDER BY
        object_schema_name(USTAT.object_id) + '.' + object_name(USTAT.object_id)
;

