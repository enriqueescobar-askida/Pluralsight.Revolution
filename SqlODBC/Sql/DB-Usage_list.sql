SELECT  database_id                     AS DBIdentifier
        , CASE [database_id]
        WHEN 32767
            THEN 'Resource DB'
            ELSE DB_NAME([database_id])
        END                             AS DBName
        , COUNT_BIG(*)                  AS DBBufferPages
        , COUNT_BIG(*) / 128            AS DBBufferMB
FROM    sys.dm_os_buffer_descriptors
GROUP BY
        database_id
;
