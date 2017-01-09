SELECT  COUNT(*)
FROM    dbo.sysobjects
WHERE   [type]    IN (
    'IF' -- inline table-valued functions
)
;

