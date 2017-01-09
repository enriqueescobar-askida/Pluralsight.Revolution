#---------------
# Sequence
#---------------
x <- 1
is.numeric(x)
is.logical(x)
y <- as.logical(x)
is.logical(y)







#---------------
# Selection
#---------------
if ( FALSE ) print('it is true') else print('it is false')

if ( 8==9 ) {
  print('it is true') 
} else { 
  print('it is false')
}

# Others selection possibilities: ifelse
print(paste('it is ',ifelse(F,'true','false')))

?switch






#---------------
# Repetition
#---------------

# for
for (i in 1:5) print(i)

# while
i=0
while(i < 5) {  
  i=i+1
  print(i)
}

# Next
i=0
while(i < 5) {
  i=i+1
  if (i == 3)
    next
  print(i)  
}

# Repeat / break
i = 1
repeat {
  print(i)
  if (i > 4) break 
  i=i+1
}

# ...mention apply ( functional programming) 
# ?apply and show looking at help
# functions  
# break, next
# Mention ooo (http://stackoverflow.com/questions/9521651/r-and-object-oriented-programming)

