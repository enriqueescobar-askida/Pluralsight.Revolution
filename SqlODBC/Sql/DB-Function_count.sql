SELECT  COUNT(*)
FROM    dbo.sysobjects
WHERE   [type]  IN ('FN', 'IF', 'TF')
;

