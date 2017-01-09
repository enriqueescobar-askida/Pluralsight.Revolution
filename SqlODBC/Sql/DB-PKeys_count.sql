SELECT  COUNT(*)
FROM
(
	SELECT
	OBJECT_NAME(parent_object_id)   AS PKTableName,
	OBJECT_NAME(OBJECT_ID)          AS PKName
	FROM	sys.objects
	WHERE	type_desc IN ('PRIMARY_KEY_CONSTRAINT')
)									AS PKTab
LEFT OUTER JOIN
(
	SELECT
			Table_Name				AS TableName,
			Constraint_Name			AS ConstraintName,
			Column_Name				AS ColumnName
	FROM	INFORMATION_SCHEMA.KEY_COLUMN_USAGE
)									AS AllTab
ON
(
	PKTab.PKTableName = AllTab.TableName
	AND
	PKTab.PKName = AllTab.ConstraintName
)
;
