SELECT  PKeyTab.TableName,
        PKeyTab.PKName,
        FKeyTab.FKName,
        PKeyTab.ColumnName
FROM    (
         SELECT TableName,
                PKName,
                ColumnName
         FROM   (
                SELECT
                OBJECT_NAME(parent_object_id)   AS PKTableName,
                OBJECT_NAME(OBJECT_ID)          AS PKName
                FROM
                        sys.objects
                WHERE   type_desc IN ('PRIMARY_KEY_CONSTRAINT')
                )
        AS      PKTab
        LEFT OUTER JOIN
                (
                SELECT  Table_Name      AS TableName,
                        Constraint_Name AS ConstraintName,
                        Column_Name     AS ColumnName
                FROM
                        INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                )
        AS      AllTab
        ON      (
                PKTab.PKTableName = AllTab.TableName
                AND
                PKTab.PKName = AllTab.ConstraintName
                )
         )
    AS  PKeyTab
    LEFT OUTER JOIN
    (
    SELECT  TableName,
            FKName,
            ColumnName
    FROM
    (
    SELECT  OBJECT_NAME(parent_object_id)   AS FKTableName,
            OBJECT_NAME(OBJECT_ID)          AS FKName
    FROM
            sys.objects
    WHERE   type_desc IN ('FOREIGN_KEY_CONSTRAINT')
    )
    AS  FKTab
    LEFT OUTER JOIN
    (
    SELECT  Table_Name      AS TableName,
            Constraint_Name AS ConstraintName,
            Column_Name     AS ColumnName
    FROM
            INFORMATION_SCHEMA.KEY_COLUMN_USAGE
    )
    AS  AllTab
    ON (
        FKTab.FKTableName = AllTab.TableName
        AND
        FKTab.FKName = AllTab.ConstraintName
        )
    )
    AS  FKeyTab
    ON  (
        PKeyTab.TableName = FKeyTab.TableName
        AND
        PKeyTab.ColumnName = PKeyTab.ColumnName
        )
ORDER BY
    PKeyTab.TableName, PkeyTab.PKName
;

