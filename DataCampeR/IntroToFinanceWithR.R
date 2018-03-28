
# Variables for starting_cash and 5% return during January
starting_cash <- 200
jan_ret <- 5
jan_mult <- 1 + (jan_ret / 100)

# How much money do you have at the end of January?
post_jan_cash <- starting_cash * jan_mult

# Print post_jan_cash
post_jan_cash

# January 10% return multiplier
jan_ret_10 <- 10
jan_mult_10 <- 1 + (jan_ret_10 / 100)

# How much money do you have at the end of January now?
post_jan_cash_10 <- starting_cash * jan_mult_10

# Print post_jan_cash_10
post_jan_cash_10
##

# Starting cash and returns 
starting_cash <- 200
jan_ret <- 4
feb_ret <- 5

# Multipliers
jan_mult <- 1 + (jan_ret / 100)
feb_mult <- 1 + (feb_ret / 100)

# Total cash at the end of the two months
total_cash <- starting_cash * jan_mult * feb_mult

# Print total_cash
total_cash
##

# Apple's stock price is a numeric
apple_stock <- 150.45

# Bond credit ratings are characters
credit_rating <- "AAA"

# You like the stock market. TRUE or FALSE?
my_answer <- FALSE

# Print my_answer
my_answer
##
# Vectors of 12 months of returns, and month names
ret <- c(5, 2, 3, 7, 8, 3, 5, 9, 1, 4, 6, 3)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Add names to ret
names(ret) <- months

# Print out ret to see the new names!
ret
##
# Weights and returns
micr_ret <- 7
sony_ret <- 9
micr_weight <- .2
sony_weight <- .8

# Portfolio return
portf_ret <- micr_ret * micr_weight + sony_ret * sony_weight
portf_ret
##
# Weights, returns, and company names
ret <- c(7, 9)
weight <- c(.2, .8)
companies <- c("Microsoft", "Sony")

# Assign company names to your vectors
names(ret) <- companies
names(weight) <- companies

# Multiply the returns and weights together 
ret_X_weight <- ret * weight

# Print ret_X_weight
ret_X_weight

# Sum to get the total portfolio return
portf_ret <- sum(ret_X_weight)

# Print portf_ret
portf_ret
##
# Print ret
ret

# Assign 1/3 to weight
weight <- 1/3

# Create ret_X_weight
ret_X_weight <- ret * weight

# Calculate your portfolio return
portf_ret <- sum(ret_X_weight)

# Vector of length 3 * Vector of length 2?
ret * c(.2, .6)
##
# First 6 months of returns
ret[1:6]

# Just March and May
ret[c(3,5)]

# Omit the first month of returns
ret[-1]
##

# A vector of 9 numbers
my_vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

# 3x3 matrix
my_matrix <- matrix(data = my_vector, nrow = 3, ncol = 3)

# Print my_matrix
my_matrix

# Filling across using byrow = TRUE
matrix(data = c(2, 3, 4, 5), nrow = 2, ncol = 2, byrow = TRUE)
##
# cbind the vectors together
cbind_stocks <- cbind(apple, ibm, micr)

# Print cbind_stocks
cbind_stocks

# rbind the vectors together
rbind_stocks <- rbind(apple, ibm, micr)

# Print rbind_stocks
rbind_stocks
##
# View the data
apple_micr_matrix

# Scatter plot of Microsoft vs Apple
plot(apple_micr_matrix)
##
# Correlation of Apple and IBM
cor(apple, ibm)

# stock matrix
stocks <- cbind(apple, micr, ibm)

# cor() of all three
cor(stocks)

