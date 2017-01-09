SELECT  COUNT(*)
FROM    dbo.sysobjects
WHERE   [type]    IN (
    'TF' -- table-valued functions
)
;

