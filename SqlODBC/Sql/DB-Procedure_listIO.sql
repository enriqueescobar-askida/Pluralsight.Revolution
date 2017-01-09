SELECT
        SYSP.name                                       AS [StoreProcName]
        , PSTA.total_physical_reads                     AS [TotalPhysicalReads]
        , PSTA.total_elapsed_time                       AS [TotalElapsedTime]
        , PSTA.execution_count                          AS [ExecutionCount]
        , PSTA.cached_time                              AS [CachedTime]
        , PSTA.total_logical_reads                      AS [TotalLogicalReads]
        , PSTA.total_logical_writes						AS [TotalLogicalWrites]
FROM    sys.procedures                                  AS SYSP
INNER JOIN
        sys.dm_exec_procedure_stats                     AS PSTA
ON      SYSP.[object_id] = PSTA.[object_id]
WHERE   PSTA.database_id = DB_ID()
ORDER BY
        [StoreProcName], [TotalPhysicalReads], [TotalLogicalReads]
;

