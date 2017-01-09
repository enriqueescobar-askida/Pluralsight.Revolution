# gender factor
gender_vector <- c("Male", "Female", "Female", "Male", "Male")
# Define factor_gender_vector using 'factor()'
factor_gender_vector <- factor(gender_vector)
factor_gender_vector
# animal factor
animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
factor_animals_vector <- factor(animals_vector)
factor_animals_vector
# temperature factor
temperature_vector <- c("High", "Low", "High","Low", "Medium")
# ordered factors
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector
# Code to form the factor vector
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)
# Specify the levels of 'factor_survey_vector'
levels(factor_survey_vector) <- c("Female", "Male")
factor_survey_vector
# Type your code here for 'survey_vector'
summary(survey_vector)
# Type your code here for 'factor_survey_vector'
summary(factor_survey_vector)
# Male
factor_survey_vector[1]
# Female
factor_survey_vector[2]
# Create 'speed_vector'
speed_vector <- c("Fast", "Slow", "Slow", "Fast", "Ultra-fast")
speed_character_vector <- c("Slow", "Fast", "Ultra-fast")
# Add your code below
factor_speed_vector <- factor(speed_vector, order = TRUE, levels = speed_character_vector)
factor_speed_vector
# R prints automagically in the right order
summary(factor_speed_vector)
# Is data analyst 2 faster than data analyst 5?
compare_them <- factor_speed_vector[2] > factor_speed_vector[5] 
compare_them
