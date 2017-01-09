library(sqldf) 

df <- data.frame(
  c(1, 'notBadMal',        'Malcom', 'Reynolds',  '2468-09-20'),
  c(2, 'warriorwoman2511', 'Zoe',    'Washburne', '2484-02-15'),
  c(3, 'veras-man',        'Jayne',   'Cobb',     'N/A')
)

# Underscores are commonly used in SQL.  Exception to the R convention of avoiding them for the
# sake of traceability
crew_members <- as.data.frame(t(df))

colnames(crew_members) <- c('id','user_name','first_name','last_name','birth_date')
rownames(crew_members) <- crew_members[,'id']

crew_members
class(crew_members)

sqldf('SELECT * FROM crew_members')

sqldf('SELECT user_name FROM crew_members ORDER BY user_name')

sqldf('SELECT user_name FROM crew_members WHERE id < 3 ORDER BY user_name')