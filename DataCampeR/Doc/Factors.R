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
## Subset of emails with big numbers: email50_big
email50_big <- email50 %>%
  filter(number == "big")
  
# Glimpse the subset
glimpse(email50_big)
## Table of number variable
table(email50_big$number)

# Drop levels
email50_big$number <- droplevels(email50_big$number)

# Another table of number variable
table(email50_big$number)
## Calculate median number of characters: med_num_char
med_num_char <- median(email50$num_char)

# Create num_char_cat variable in email50
email50 <- email50 %>%
  mutate(num_char_cat = ifelse(num_char < med_num_char, "below median", "at or above median"))
  
# Count emails in each category
table(email50$num_char_cat)
## Create number_yn column in email50
email50 <- email50 %>%
  mutate(number_yn = ifelse(number == "none", "no", "yes"))

# Visualize number_yn
ggplot(email50, aes(x = number_yn)) +
  geom_bar()
#