# Character
typeof('1')
# Complex
typeof(2i)
# Double
typeof(1)
# Integer
typeof(1L)
# Logical
typeof(TRUE)
# Raw
typeof(charToRaw('1'))



all.character <- c('1' ,2i, 1, 1L, TRUE, charToRaw('1'))
all.complex   <- c(     2i, 1, 1L, TRUE, charToRaw('1'))
all.double    <- c(         1, 1L, TRUE, charToRaw('1'))
all.integer   <- c(            1L, TRUE, charToRaw('1'))
all.logical   <- c(                TRUE, charToRaw('1'))
a.raw         <- c(                      charToRaw('1'))



typeof(all.character)
typeof(all.complex)
typeof(all.double)
typeof(all.integer)
typeof(all.logical)
typeof(a.raw)