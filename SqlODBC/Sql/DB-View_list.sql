SELECT   
        name                                        AS ViewName
        , OBJECT_ID                                 AS ViewID
        , type                                      AS ViewType
        , type_desc                                 AS ViewDesc
        , create_date                               AS ViewCreated
        , modify_date                               AS ViewModified
        , OBJECTPROPERTYEX(object_id,'IsIndexed')   AS IsIndexed
        , OBJECTPROPERTYEX(object_id,'IsIndexable') AS IsIndexable
FROM    sys.views                                   AS SYSV
ORDER BY
        ViewName
;

