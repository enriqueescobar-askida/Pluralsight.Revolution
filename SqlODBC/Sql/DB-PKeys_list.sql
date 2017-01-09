SELECT
		PKTab.TableName
		, ColumnName
		, PrincipalKey
		, PrincipalKeyID
		, PKCreated
		, PKModified
		, PKOrdinal
FROM
(
	SELECT
			OBJECT_NAME(parent_object_id)		AS TableName
			, OBJECT_NAME(OBJECT_ID)			AS PrincipalKey
			, OBJECT_ID							AS PrincipalKeyID
			, create_date						AS PKCreated
			, modify_date						AS PKModified
	FROM	sys.objects
	WHERE	type_desc IN ('PRIMARY_KEY_CONSTRAINT')
)												AS PKTab
LEFT OUTER JOIN
(
	SELECT
			Table_Name							AS TableName
			, Constraint_Name					AS ConstraintName
			, Column_Name						AS ColumnName
			, ORDINAL_POSITION					AS PKOrdinal
	FROM	INFORMATION_SCHEMA.KEY_COLUMN_USAGE
)												AS AllTab
ON
(
	PKTab.TableName = AllTab.TableName
	AND
	PKTab.PrincipalKey = AllTab.ConstraintName
)
ORDER BY
		PKTab.TableName, ColumnName
;
