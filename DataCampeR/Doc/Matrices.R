# Construction of a matrix with 3 rows that contain the numbers 1 up to 9
matrix(1:9, byrow = TRUE, nrow = 3)
# Box office Star Wars trilogy 1! The first element: US, the second element: Non-US 
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)
# Box office Star Wars trilogy 1 matrix
star_wars_matrix <- matrix(c(new_hope,empire_strikes,return_jedi), byrow = TRUE, nrow = 3)
# Box office Star Wars trilogy 2! The first element: US, the second element: Non-US 
phantom_menace <- c(474.5,552.5)
attack_clones <- c(310.7,338.7)
revenge_sith <- c(380.3,468.5)
# Box office Star Wars trilogy 2 matrix
star_wars_matrix2 <- matrix(c(phantom_menace,attack_clones,revenge_sith), byrow = TRUE, nrow = 3)
# column names
col_titles <- c("US", "non-US")
movie_names <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")
movie_names2 <- c("The Phantom Menace", "Attack of the Clones", "Revenge of the Sith")
ticket_prices <- c(5, 5, 6, 6, 7, 7)
# Box office Star Wars trilogy 1 matrix rows and columns
colnames(star_wars_matrix) <- col_titles
rownames(star_wars_matrix) <- movie_names
star_wars_matrix
# Box office Star Wars trilogy 2 matrix rows and columns
colnames(star_wars_matrix2) <- col_titles
rownames(star_wars_matrix2) <- movie_names2
star_wars_matrix2
# Box office Star Wars trilogy 1 matrix 
box_office_all <- c(461, 314.4, 290.5, 247.9, 309.3, 165.8)
star_wars_matrix <- matrix(box_office_all, nrow=3, byrow = TRUE, dimnames = list(movie_names, col_titles))
# Box office Star Wars trilogy 1 Average non-US revenue per movie
star_wars_non_us_all  <- mean(star_wars_matrix[,2])
# Box office Star Wars trilogy 1 Average non-US revenue of first two movies
star_wars_non_us_some <- mean(star_wars_matrix[1:2,2])
# Box office Star Wars trilogy 1 Estimation of visitors 5$/ticket
star_wars_visitors <- star_wars_matrix/5
# Box office Star Wars trilogy 1 Estimation of visitors per ticket matrix
ticket_prices_matrix <- matrix(ticket_prices, nrow = 3, byrow = TRUE, dimnames = list(movie_names, col_titles))
star_wars_visitor_matrix <- star_wars_matrix / ticket_prices_matrix
# Box office Star Wars trilogy 1 Average number of US visitors
average_us_visitors <- mean(star_wars_visitor_matrix[ ,1])
# Box office Star Wars trilogy 1 Average number of non-US visitors
average_non_us_visitors <- mean(star_wars_visitor_matrix[ ,2])
# Box office Star Wars trilogy 2 matrix 
box_office_all2 <- c(474.5,552.5, 310.7,338.7, 380.3,468.5)
star_wars_matrix2 <- matrix(box_office_all2, nrow=3, byrow = TRUE, dimnames = list(movie_names2, col_titles))
# Box office Star Wars trilogy 2 Average non-US revenue per movie
star_wars_non_us_all2 <- mean(star_wars_matrix2[,2])
# Box office Star Wars trilogy 2 Average non-US revenue of first two movies
star_wars_non_us_some2 <- mean(star_wars_matrix2[1:2,2])
# Box office Star Wars trilogy 2 Estimation of visitors 5$/ticket
star_wars_visitors2 <- star_wars_matrix2/5
# Box office Star Wars trilogy 2 Estimation of visitors per ticket matrix
ticket_prices_matrix2 <- matrix(ticket_prices, nrow = 3, byrow = TRUE, dimnames = list(movie_names2, col_titles))
star_wars_visitor_matrix2 <- star_wars_matrix2 / ticket_prices_matrix2
# Box office Star Wars trilogy 2 Average number of US visitors
average_us_visitors2 <- mean(star_wars_visitor_matrix2[ ,1])
# Box office Star Wars trilogy 2 Average number of non-US visitors
average_non_us_visitors2 <- mean(star_wars_visitor_matrix2[ ,2])
# Box office Star Wars trilogy 1 worldwide box office figures
worldwide_vector <- rowSums(star_wars_matrix)
# Box office Star Wars trilogy 2 worldwide box office figures
worldwide_vector2 <- rowSums(star_wars_matrix2)
# Box office Star Wars trilogy 1 add worldwide_vector as a column to star_wars_matrix
all_wars_matrix <- cbind(star_wars_matrix,worldwide_vector)
# Box office Star Wars trilogy 1 add worldwide_vector as a column to star_wars_matrix
all_wars_matrix2 <- cbind(star_wars_matrix2,worldwide_vector2)
#
all_star_wars <- rbind(star_wars_matrix,star_wars_matrix2)
all_star_wars2 <- rbind(all_wars_matrix,all_wars_matrix2)