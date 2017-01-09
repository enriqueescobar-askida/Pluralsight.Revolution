SELECT  @@SERVERNAME                                    AS ServerName
        , @@SERVICENAME                                 AS ServiceName
        , d.Name                                        AS DBName
        , b.Backup_finish_date
        , bmf.Physical_Device_name
FROM
        sys.databases                                   AS d
LEFT OUTER JOIN
        msdb..backupset                                 AS b
ON
        b.database_name = d.name
        AND
        b.[type] = 'D'
LEFT OUTER JOIN
        msdb.dbo.backupmediafamily                      AS bmf
ON
        b.media_set_id = bmf.media_set_id
ORDER BY
        d.Name
;

