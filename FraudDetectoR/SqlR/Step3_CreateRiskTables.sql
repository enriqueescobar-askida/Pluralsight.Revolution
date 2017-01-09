/* assume two tables has been created by step2: sql_tagged_training and sql_tagged_testing */
/*
The procedure to create all risk tables
*/

use [OnlineFraudDetection]
go

set ansi_nulls on
go

set quoted_identifier on
go

DROP PROCEDURE IF EXISTS dbo.CreateRiskTable_ForAll
GO

create procedure dbo.CreateRiskTable_ForAll
as
begin

DROP TABLE IF EXISTS dbo.sql_risk_var

-- create a table to store names of variables and risk tables. will be used as referrence in the loop later
create table dbo.sql_risk_var (ID int,var_names varchar(255), table_names varchar(255));
insert into sql_risk_var values (1, 'transactionCurrencyCode', 'sql_risk_transactionCurrencyCode');
insert into sql_risk_var values (2, 'localHour', 'sql_risk_localHour');
insert into sql_risk_var values (3, 'ipState', 'sql_risk_ipState');
insert into sql_risk_var values (4, 'ipPostCode', 'sql_risk_ipPostCode');
insert into sql_risk_var values (5, 'ipCountryCode', 'sql_risk_ipCountryCode');
insert into sql_risk_var values (6, 'browserLanguage', 'sql_risk_browserLanguage');
insert into sql_risk_var values (7, 'paymentBillingPostalCode', 'sql_risk_paymentBillingPostalCode');
insert into sql_risk_var values (8, 'paymentBillingState', 'sql_risk_paymentBillingState');
insert into sql_risk_var values (9, 'paymentBillingCountryCode', 'sql_risk_paymentBillingCountryCode');
insert into sql_risk_var values (10, 'shippingPostalCode', 'sql_risk_shippingPostalCode');
insert into sql_risk_var values (11, 'shippingState', 'sql_risk_shippingState');
insert into sql_risk_var values (12, 'shippingCountry', 'sql_risk_shippingCountry');
insert into sql_risk_var values (13, 'accountPostalCode', 'sql_risk_accountPostalCode');
insert into sql_risk_var values (14, 'accountState', 'sql_risk_accountState');
insert into sql_risk_var values (15, 'accountCountry', 'sql_risk_accountCountry');

-- loops to create risk tables for all variables
DECLARE @name_1 NVARCHAR(100)
DECLARE @name_2 NVARCHAR(100)
DECLARE @getname CURSOR

SET @getname = CURSOR FOR
SELECT var_names,
	   table_names
FROM   sql_risk_var
OPEN @getname
FETCH NEXT
FROM @getname INTO @name_1,@name_2
WHILE @@FETCH_STATUS = 0
BEGIN
    EXEC CreateRiskTable @name_1,@name_2 -- create risk table by calling stored procedure CreateRiskTable
    FETCH NEXT
    FROM @getname INTO @name_1, @name_2
END

CLOSE @getname
DEALLOCATE @getname
end
