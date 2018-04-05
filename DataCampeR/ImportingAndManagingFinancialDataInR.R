## Load the quantmod package
library(quantmod)

# Import QQQ data from Yahoo! Finance
getSymbols("QQQ", auto.assign = TRUE, src="yahoo")

# Look at the structure of the object getSymbols created
str(QQQ)

# Look at the first few rows of QQQ
head(QQQ)
## Import QQQ data from Alpha Vantage
getSymbols("QQQ", auto.assign = TRUE, src = "av")

# Look at the structure of QQQ
str(QQQ)

# Import GDP data from FRED
getSymbols("GDP", auto.assign = TRUE, src = "FRED")

# Look at the structure of GDP
str(GDP)
## Load the quantmod package
library(quantmod)

# Assign SPY data to 'spy' using auto.assign argument
spy <- getSymbols("SPY", auto.assign = FALSE, src="yahoo")

# Look at the structure of the 'spy' object
str(spy)

# Assign JNJ data to 'jnj' using env argument
jnj <- getSymbols("JNJ", env = NULL, src="yahoo")

# Look at the structure of the 'jnj' object
str(jnj)
