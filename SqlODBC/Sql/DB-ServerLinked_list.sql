SELECT  @@SERVERNAME            AS ServerName
        , Server_Id             AS LinkedServerID
        , name                  AS LinkedServer
        , Product
        , Provider
        , Data_Source           AS DataSource
        , Modify_Date           AS ModificationDate
        , is_linked             AS IsLinked
FROM    sys.servers
ORDER BY
        LinkedServer
;

