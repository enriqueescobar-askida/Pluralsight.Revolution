SELECT  @@SERVERNAME                                    AS ServerName
        , @@SERVICENAME                                 AS ServiceName
        , d.database_id                                 AS DBIdentifier
        , d.name                                        AS DBName
        , m.name                                        AS OriginalDBName
        , recovery_model_Desc                           AS RecoveryModel
        , Compatibility_level                           AS CompatiblityLevel
        , m.size                                        AS DBSize
        , m.growth                                      AS DBGrowth
        , m.is_percent_growth                           AS IsPercentGrowth
        , create_date                                   AS CreatedDate
        , d.state_desc                                  AS CurrentState
        , is_auto_shrink_on                             AS AutoShrink
        , snapshot_isolation_state_desc                 AS SnapshotState
        , is_auto_update_stats_on                       AS IsAutoUpdate
        , is_arithabort_on                              AS IsArithAbort
        , page_verify_option_desc                       AS PageVerifyOption
        , collation_name                                AS Collation
        , m.physical_name                               AS FilePath
        , source_database_id                            AS IdSourceDB
FROM
        sys.databases                                   AS d
JOIN
        sys.master_files                                AS m
ON      d.database_id = m.database_id
WHERE   m.[type] = 0
ORDER BY
        DBName
;

