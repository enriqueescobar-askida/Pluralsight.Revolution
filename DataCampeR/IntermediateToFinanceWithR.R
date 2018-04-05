# What is the current date?
Sys.Date()

# What is the current date and time?
Sys.time()

# Create the variable today
today <- Sys.Date()

# Confirm the class of today
class(today)
## Create crash
crash <- as.Date("2008-09-29")

# Print crash
crash

# crash as a numeric
as.numeric(crash)

# Current time as a numeric
as.numeric(Sys.time())

# Incorrect date format
as.Date("09/29/2008")
## Create dates from "2017-02-05" to "2017-02-08" inclusive.
dates <- c("2017-02-05", "2017-02-06","2017-02-07", "2017-02-08")

# Add names to dates
names(dates)<-c("Sunday", "Monday", "Tuesday", "Wednesday")

# Subset dates to only return the date for Monday
dates[2]
## "08,30,30"
as.Date("08,30,1930", format = "%m, %d, %Y")

# "Aug 30,1930"
as.Date("Aug 30,1930", format = "%b %d, %Y")

# "30aug1930"
as.Date("30aug1930", format = "%d%b%Y")
## char_dates
char_dates <- c("1jan17", "2jan17", "3jan17", "4jan17", "5jan17")

# Create dates using as.Date() and the correct format 
dates <- as.Date(char_dates, format = "%d%b%y")

# Use format() to go from "2017-01-04" -> "Jan 04, 17"
format(dates, format = "%b %d, %y")

# Use format() to go from "2017-01-04" -> "01,04,2017"
format(dates, format = "%m,%d,%Y")
## Dates
dates <- as.Date(c("2017-01-01", "2017-01-02", "2017-01-03"))

# Create the origin
origin <- as.Date("1970-01-01")

# Use as.numeric() on dates
as.numeric(dates)

# Find the difference between dates and origin
dates - origin
## dates
dates <- as.Date(c("2017-01-02", "2017-05-03", "2017-08-04", "2017-10-17"))

# Extract the months
months(dates)

# Extract the quarters
quarters(dates)

# dates2
dates2 <- as.Date(c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05"))

# Assign the weekdays() of dates2 as the names()
names(dates2) <- weekdays(dates2)

# Print dates2
dates2
## Stock prices
apple <- 48.99
micr <- 77.93

# Apple vs Microsoft
apple > micr

# Not equals
apple != micr

# Dates - today and tomorrow
today <- as.Date(Sys.Date())
tomorrow <- as.Date(Sys.Date() + 1)

# Today vs Tomorrow
tomorrow < today
## Print stocks
stocks

# IBM range
stocks$ibm_buy <- stocks$ibm < 175

# Panera range
stocks$panera_sell <- stocks$panera > 213

# IBM vs Panera
stocks$ibm_vs_panera <- stocks$ibm > stocks$panera

# Print stocks
stocks
## IBM buy range
stocks$ibm_buy_range <- (stocks$ibm > 171) & (stocks$ibm < 176)

# Panera spikes
stocks$panera_spike <- (stocks$panera < 213.20) | (stocks$panera > 216.50)

# Date range
stocks$good_dates <- (stocks$date > as.Date("2017-01-21")) & (stocks$date < as.Date("2017-01-25"))

# Print stocks
stocks
## IBM range
!(stocks$ibm > 176)

# Missing data
missing <- c(24.5, 25.7, NA, 28, 28.6, NA)

# Is missing?
is.na(missing)

# Not missing?
!is.na(missing)
## Panera range
subset(stocks, panera > 216)

# Specific date
subset(stocks, date == as.Date("2017-01-23"))

# IBM and Panera joint range
subset(stocks, ibm < 175 & panera < 216.50)
## View stocks
stocks

# Weekday investigation
stocks$weekday <- weekdays(stocks$date)

# View stocks again
stocks

# Remove missing data
stocks_no_NA <- subset(stocks, !is.na(apple))

# Apple and Microsoft joint range
subset(stocks_no_NA, apple > 117 | micr > 63)
## micr
micr <- 48.55

# Fill in the blanks
if( micr < 55 ) {
    print("Buy!")
}
## micr
micr <- 57.44

# Fill in the blanks
if( micr < 55 ) {
    print("Buy!")
} else {
    print("Do nothing!")
}
## micr
micr <- 105.67

# Fill in the blanks
if( micr < 55 ) {
    print("Buy!")
} else if( micr >= 55 & micr < 75 ){
    print("Do nothing!")
} else { 
    print("Sell!")
}
## micr
micr <- 105.67
shares <- 1

# Fill in the blanks
if( micr < 55 ) {
    print("Buy!")
} else if( micr >= 55 & micr < 75 ) {
    print("Do nothing!")
} else { 
    if( shares >= 1 ) {
        print("Sell!")
    } else {
        print("Not enough shares to sell!")
    }
}
## Microsoft test
stocks$micr_buy <- ifelse(test = stocks$micr > 60 & stocks$micr < 62, yes = 1, no = 0)

# Apple test
stocks$apple_date <- ifelse(test = stocks$apple > 117, yes = stocks$date, no = NA)

# Print stocks
stocks

# Change the class() of apple_date.
class(stocks$apple_date) <- "Date"

# Print stocks again
stocks

