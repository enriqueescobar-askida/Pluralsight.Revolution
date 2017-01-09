SELECT  COUNT(*)
FROM    sys.foreign_key_columns     AS fk
JOIN    sys.tables                  AS t
ON      fk.parent_object_id = t.object_id
JOIN    sys.columns                 AS c
ON      fk.parent_object_id = c.object_id
        AND
        fk.parent_column_id = c.column_id
;

