library('RSQLite')
con <- dbConnect(dbDriver("SQLite"), dbname=":memory:")

# DDL
rs <- dbSendQuery(con, "create table CREW_MEMBERS (id number, user_name string, first_name string, last_name string, birth_date DATE)")

# DML
dbSendQuery(con, "insert into CREW_MEMBERS values (1, 'notBadMal', 'Malcom', 'Reynolds', '2468-09-20')")
dbSendQuery(con, "insert into CREW_MEMBERS values (2, 'warriorwoman2511', 'Zoe', 'Washburne', '2484-02-15')")
dbSendQuery(con, "insert into CREW_MEMBERS values (3, 'veras-man', 'Jayne', 'Cobb', NULL)")

# SQL
rs <- dbSendQuery(con, "select * from CREW_MEMBERS")
fetch(rs)
dbClearResult(rs)





# sqldf is using SQLite behind the scenes.
# This does not rely on sqldf but is simple,
# basic SQL interaction with an (in-memory) 
# database
rs <- dbSendQuery(con, "SELECT user_name FROM CREW_MEMBERS ORDER BY user_name")
df <- fetch(rs)
dbClearResult(rs)

rs <- dbSendQuery(con, "SELECT user_name FROM CREW_MEMBERS WHERE id < 3 ORDER BY user_name")
df <- fetch(rs)
dbClearResult(rs)