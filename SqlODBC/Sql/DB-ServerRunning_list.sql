SELECT  @@SERVERNAME                                    AS ServerName
        , @@SERVICENAME                                 AS ServiceName
        , create_date                                   AS ServerStarted
        , DATEDIFF(s, create_date, GETDATE()) / 86400.0 AS DaysRunning
FROM    sys.databases
;

