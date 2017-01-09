# Create my_first_data_table
my_first_data_table <- data.table(x = c("a", "b", "c", "d", "e"), 
                                  y = c(1, 2, 3, 4, 5)) 
# Create a data.table using recycling
DT <- data.table(a = c(1L, 2L), b = LETTERS[1:4])
# Print the third row to the console
DT[3]
# Print the second and third row to the console, but do not commas
DT[2:3]
# Print the penultimate row of DT using .N
DT[.N - 1]
# Print the penultimate column of DT using .N
DT[,.N - 1]
# Print the column names of DT, and number of rows and number of columns
colnames(DT)
dim(DT)
# Select row 2 twice and row 3, returning a data.table with three rows where row 2 is a duplicate of row 1.
DT[c(2, 2, 3)]
# Subset rows 1 and 3, and columns B and C
DT[c(1, 3), .(B, C)]
# Assign to ans the correct value
ans <- DT[, .(B, val = A * C)]
# Fill in the blanks such that ans2 equals target
target <- data.table(B = c("a", "b", "c", "d", "e", "a", "b", "c", "d", "e"), 
                     val = as.integer(c(6:10, 1:5)))
ans2 <- DT[, .(B, val = c(C, A))]
# Convert iris to a data.table: DT
DT <- as.data.table(iris)
# For each Species, print the mean Sepal.Length
DT[, mean(Sepal.Length), by = Species]
# Print mean Sepal.Length, grouping by first letter of Species
DT[, mean(Sepal.Length), by = substr(Species, 1, 1)]
# data.table version of iris: DT
DT <- as.data.table(iris)
# Group the specimens by Sepal area (to the nearest 10 cm2) and count how many occur in each group.
DT[, .N, by = 10 * round(Sepal.Length * Sepal.Width / 10)]
# Now name the output columns `Area` and `Count`
DT[, .(Count = .N), by = .(Area = 10 * round(Sepal.Length * Sepal.Width / 10))]
# Create the data.table DT
set.seed(1L)
DT <- data.table(A=rep(LETTERS[2:1], each=4L), B=rep(1:4, each=2L), C=sample(8))
# Create the new data.table, DT2
DT2 <- DT[, .(C=cumsum(C)), by=.(A,B)]
# Select from DT2 the last two values from C while you group by A
DT2[, .(C=tail(C,2)), by=A]
