SELECT  COUNT(*)
FROM    dbo.sysobjects
WHERE   [type]    IN (
    'FN' -- scalar functions
)
;

