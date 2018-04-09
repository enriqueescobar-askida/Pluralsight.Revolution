
# Study types and cautionary tales

## Observational studies and experiments

## Identify study type

## Identify the type of study

## Random sampling and random assignment

## Random sampling or random assignment?

## Identify the scope of inference of study

## Simpson's paradox

## Number of males and females admitted

## Proportion of males admitted overall
### Load packages
library(dplyr)
library(tidyr)

### Count number of male and female applicants admitted
ucb_counts <- ucb_admit %>%
  count(Gender, Admit)

### View result
ucb_counts

### Spread the output across columns
ucb_counts %>%
  spread(Admit, n)
ucb_admit %>%
##  # Table of counts of admission status and gender
  count(Gender, Admit) %>%
##  # Spread output across columns based on admission status
  spread(Admit, n) %>%
##  # Create new variable
  mutate(Perc_Admit = Admitted / (Admitted + Rejected))
## Proportion of males admitted for each department
### Table of counts of admission status and gender for each department
admit_by_dept <- ucb_admit %>%
  count(Dept, Gender, Admit) %>%
  spread(Admit, n)
  
### View result
admit_by_dept

### Percentage of those admitted to each department
admit_by_dept %>%
  mutate(Perc_Admit = Admitted / (Admitted + Rejected))
## Contingency table results by group

## Recap: Simpson's paradox

# Sampling strategies and experimental design
### Simple random sample
states_srs <- us_regions %>%
  sample_n(8)

### Count states by region
states_srs %>%
  group_by(region) %>%
  count()
### Stratified sample
states_str <- us_regions %>%
  group_by(region) %>%
  sample_n(2)

### Count states by region
states_str %>%
  group_by(region) %>%
  count()

# Case Study
# Inspect evals
glimpse(evals)

# Alternative solutions
dim(evals)
str(evals)
# Inspect variable types
glimpse(evals)
str(evals) # Another option

# Remove non-factor variables from this vector
cat_vars <- c("rank", "ethnicity", "gender", "language",
              "cls_level", "cls_profs", "cls_credits",
              "pic_outfit", "pic_color") 
# Recode cls_students as cls_type: evals
evals <- evals %>%
  # Create new variable
  mutate(cls_type = ifelse(cls_students <= 18, "small",
                      ifelse(cls_students >= 19 & cls_students <= 59, "midsize", 
                        "large")))
						


