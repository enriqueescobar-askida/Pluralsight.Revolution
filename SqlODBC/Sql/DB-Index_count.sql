SELECT	COUNT(*)
FROM
(
SELECT  ISNULL(name,'NA')                       AS IndexName
    		, object_id                             AS IndexID
    		, type                                  AS IndexType
    		, type_desc                             AS IndexDesc
    		, is_unique                             AS IsUnique
    		, is_primary_key                        AS IsPrimary
    		, is_unique_constraint                  AS IsConstraint
    		, COALESCE(TC.CONSTRAINT_TYPE, 'NA')    AS ConstraintType
    		, data_space_id                         AS DataSpaceID
    		, fill_factor                           AS FillFctr
    		, allow_page_locks                      AS CanPageLock
FROM    sys.indexes                             AS SYSI
LEFT OUTER JOIN
		INFORMATION_SCHEMA.TABLE_CONSTRAINTS        AS TC
ON        TC.CONSTRAINT_NAME=SYSI.name
)			AS TABI
;
