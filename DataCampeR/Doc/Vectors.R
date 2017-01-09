numeric_vector <- c(1, 10, 49)
character_vector <- c("a", "b", "c")
# Complete the code for 'boolean_vector'
boolean_vector <- c(TRUE,FALSE,TRUE)
boolean_vector
#
A_vector <- c(1, 2, 3)
B_vector <- c(4, 5, 6)
# Take the sum of 'A_vector' and 'B_vector'
total_vector <- A_vector + B_vector
# Print 'total_vector' to the console
total_vector
# Poker winnings from Monday to Friday
poker_vector <- c(140, -50, 20, -120, 240)
# Roulette winnings from Monday to Friday
roulette_vector <- c(-24, -50, 100, -350, 10)
roulette_vector
# Give names to both 'poker_vector' and 'roulette_vector'
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(roulette_vector) <- days_vector
names(poker_vector) <- days_vector
# What days of the week did you make money on poker?
selection_poker_vector <- poker_vector > 0
selection_poker_vector
# Select from poker_vector these days
poker_winning_days <- poker_vector[selection_poker_vector]
poker_winning_days
# What days of the week did you make money on roulette?
selection_roulette_vector <- roulette_vector > 0
selection_roulette_vector
# Select from roulette_vector these days
roulette_winning_days <- roulette_vector[selection_roulette_vector]
roulette_winning_days
# Define a new variable based on a selection Wednesday
poker_wednesday <- poker_vector[3]
poker_wednesday
# Define a new variable based on a selection Tuesday to Thursday
poker_midweek <- poker_vector[c(2,3,4)]
poker_midweek
# average midweek gain
average_midweek_gain <- mean(poker_vector[c("Monday", "Tuesday", "Wednesday")])
# Define a new variable based on a selection
roulette_selection_vector <- roulette_vector[2:5]
roulette_selection_vector
# Up to you now:
total_daily <- poker_vector + roulette_vector
total_daily
# Calculate total gains for poker and roulette
total_poker <- sum(poker_vector)
total_roulette <- sum(roulette_vector)
# total week
total_week <- total_poker + total_roulette
total_week
# Check if you realized higher total gains in poker than in roulette 
answer <- total_poker > total_roulette
answer
