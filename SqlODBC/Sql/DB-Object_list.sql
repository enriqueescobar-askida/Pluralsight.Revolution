SELECT
    CASE TYPE
        WHEN 'U'
            THEN 'User Defined Tables'
        WHEN 'S'
            THEN 'System Tables'
        WHEN 'IT'
            THEN 'Internal Tables'
        WHEN 'P'
            THEN 'Stored Procedures'
        WHEN 'PC'
            THEN 'CLR Stored Procedures'
        WHEN 'X'
            THEN 'Extended Stored Procedures'
        WHEN 'FN'
            THEN 'Function-Scalar'
        WHEN 'TF'
            THEN 'Function-TabValued'
        WHEN 'IF'
            THEN 'Function-InlineTabValued'
        WHEN 'V'
            THEN 'Views'
    END         AS  ObjectName
    , COUNT(*)  AS  ObjectCount
FROM    SYS.OBJECTS
WHERE   TYPE IN ('U', 'P', 'PC', 'S', 'IT', 'X', 'FN', 'TF', 'IF', 'V')
GROUP BY
        [type]
;

