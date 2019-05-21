# a3-using-data

# Before you get started, set your working directory using the Session menu

###################### Data Frame Manipulation (24 POINTS) #####################

# Create a vector `students` holding 1,000 values representing students
# They should have the values "Student 1", "Student 2",..., "Student 1000"
students <- paste("Student", 1:1000)

# Create a vector `math_grades` that holds 1000 random values in it
# (these represent grades in a math course)
# These values should be normally distributed with a mean of 88 and a
# standard deviation of 10
math_grades <- rnorm(1000, 88, 10)

# Replace any values in the `math_grades vector` that are above 100 with
# the number 100
math_grades <- replace(math_grades, math_grades > 100, 100)

# Create a vector `spanish` that holds 1000 random values in it
# (these represent grades in a spanish course)
# These values should be normally distributed with a mean of 85 and a
# standard deviation of 12
spanish_grades <- rnorm(1000, 85, 12)

# Replace any values in the `spanish_grades` that are above 100 with
# the number 100
spanish_grades <- replace(spanish_grades, spanish_grades > 100, 100)

# Create a data.frame variable `grades` by combining
# the vectors `first_names`, `math_grades`, and `spanish_grades`
# Make sure to properly handle strings
grades <- data.frame(students, math_grades,
  spanish_grades,
  stringsAsFactors = FALSE
)

# Create a variable `num_students` that contains the
# number of rows in your dataframe `grades`
num_students <- nrow(grades)

# Create a variable `num_courses` that contains the number of columns
# in your dataframe `grades` minus one (b/c of their names)
num_courses <- ncol(grades) - 1

# Add a new column `grade_diff` to your dataframe, which is equal to
# `students$math_grades` minus `students$spanish_grades`
grades$grade_diff <- grades$math_grades - grades$spanish_grades

# Add another column `better_at_math` as a boolean (TRUE/FALSE) variable that
# indicates that a student got a better grade in math
grades$better_at_math <- grades$math_grades > grades$spanish_grades

# Create a variable `num_better_at_math` that is the number
# (i.e., one numeric value) of students better at math
num_better_at_math <- nrow(grades[grades$better_at_math == TRUE, ])

# Write your `grades` dataframe to a new .csv file inside your data/ directory
# with the filename `grades.csv`. Make sure *not* to write row names.
# (you'll need to create the `data/` directory, which you can do outside of R)
write.csv(grades, "data/my_new_grades_file.csv", row.names = FALSE)

########################### Built in R Data (28 points) ########################

# In this section, you'll work with the `Titanic` data set
# Which is built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic)

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Be sure to **not** treat strings as factors!
titanic_df <- data.frame(Titanic, stringsAsFactors = FALSE)

# It's important to understand the _meaning_ of each column before analyzing it
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: the level of ticket or even the job (for crew) of the people onboard; 1st, 2nd, 3rd, crew
# Sex: sex of each person onboard; Male, Female
# Age: age of each person onboard; Child, Adult
# Survived: whether each person onboard survived or not from the Titanic incident; Yes, No
# Freq: number of people in each category; 0~total number of people

# Create a variable `children` that are the *only* the rows of the data frame
# with information about the number children on the Titanic.
children <- titanic_df[titanic_df$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: remember the `sum()` function!
num_children <- sum(children$Freq)

# Create a variable `most_lost` which has the *row* with the
# largest absolute number of losses (people who did not survive).
# Tip: if you want, you can use multiple statements (lines of code)
# if you find that helpful to create this variable.
dead <- titanic_df[titanic_df$Survived == "No", ]
most <- max(dead$Freq)
most_lost <- dead[dead$Freq == most, ]

# Define a function called `survival_rate()` that takes in two arguments:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
# This function should return the following
# sentence that states the *survival rate* (# survived / # in group)
# of adult men and "women and children" in that ticketing class.
# It should read (for example):
# Of Crew class, 87% of women and children survived and 22% of men survived.
# The approach you take to generating the sentence to return is up to you.
# A good solution will likely utilize filtering to produce the required data.
# You must round values and present them as percentages in the sentence.

survival_rate <- function(ticket_class, dataframe) {
  class <- dataframe[dataframe$Class == ticket_class, ]
  alladults <- class[class$Age == "Adult", ]
  allchildren <- class[class$Age == "Child", ]
  alladultmen <- alladults[alladults$Sex == "Male", ]
  alladultwomen <- alladults[alladults$Sex == "Female", ]

  survivedmen <- alladultmen[alladultmen$Survived == "Yes", ]
  survivedwomen <- alladultwomen[alladultwomen$Survived == "Yes", ]
  survivedchildren <- allchildren[allchildren$Survived == "Yes", ]

  num_allmen <- sum(alladultmen$Freq)
  num_allwomen <- sum(alladultwomen$Freq)
  num_allchildren <- sum(allchildren$Freq)
  num_allwomenandchildren <- num_allwomen + num_allchildren
  num_survivedmen <- sum(survivedmen$Freq)
  num_survivedwomen <- sum(survivedwomen$Freq)
  num_survivedchildren <- sum(survivedchildren$Freq)
  num_survivedwomenandchildren <- num_survivedwomen + num_survivedchildren

  percentmen <- round(num_survivedmen / num_allmen * 100, digits = 0)
  percentchildrenandwomen <- round(
    num_survivedwomenandchildren / num_allwomenandchildren * 100,
    digits = 0
  )
  read <- paste0(
    "Of ", ticket_class, " class, ", percentchildrenandwomen,
    "% of women and children survived and ", percentmen, "% of men survived."
  )
  read
}


# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to your `survival_rate` function
# (`Crew`, `1st`, `2nd`, and `3rd`), passing int
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)

# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
# The higher the class of passengers is, the higher their chances of survival are.
# Survival rate of crews are higher than that of 3rd class - the lowest class.

# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
# Survival rates of women and children are always higher than that of men.
# The difference of survival rate between the women and chldren versus
# the men are the biggest in 2nd class.

########################### Reading in Data (43 points)#########################
# In this section, you'll work with .csv data of life expectancy by country
# First, you should download a .csv file of Life Expectancy data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory

# Before getting started, you should explore the GapMinder website to understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., 1 - 2 sentences
# per data source)
# For 1800-1970 period, the data is from v7, what is mainly based on several sources
# collected by Mattias Lindgren.
# For 1970-2016 period, the data is based on Global Burden of Disease Study 2016,
# from the Institute for Health Metrics and Evaluation of UW-Seattle.
# This main source of 1970-2016 data was published September 2017.
# For 2017-2099 period, the data is based on World Population Prospects 2017
# published by the UN.

# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`. Makes sure not to read strings as factors
life_exp <- read.csv("data/life_expectancy_years.csv",
  stringsAsFactors = FALSE
)

# Write a function `get_col_mean()` that takes in a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
get_col_mean <- function(column, dataframe) {
  dataframe <- na.omit(dataframe)
  mean <- lapply(dataframe[column], mean)
  mean
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
col_means <- list(get_col_mean(colnames(life_exp)[-1], life_exp))

# Create a variable `avg_diff` that holds the difference in average country life
# expectancy between 1800 and 2018?
avg_diff <- mean(
  life_exp$X2018, na.rm = TRUE) - mean(life_exp$X1800, na.rm = TRUE)

# Create a column `life_exp$change` that is the change
# in life expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*
life_exp$change <- life_exp$X2018 - life_exp$X2000

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy
# Make sure to filter NA values!
naomit <- na.omit(life_exp)
most_improved <- life_exp[naomit$change == max(naomit$change), "country"]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values!
naomit <- na.omit(life_exp)
num_small_gain <- nrow(naomit[life_exp$change < 1, ])

# Write a function `country_change()` that takes in a country's name,
# two (numeric) years, and the `life_exp` dataframe as parameters.
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"
country_change <- function(name, first, second, dataframe) {
  firstyear <- paste0("X", first)
  secondyear <- paste0("X", second)
  country <- dataframe[dataframe$country == name, ]
  difference <- country[[firstyear]] - country[[secondyear]]
  difference <- round(difference, digits = 1)
  direction
  if (difference > 0) {
    direction <- "down"
  } else {
    direction <- "up"
    difference <- abs(difference)
  }
  read <- paste(
    "Between", first, "and", second, ", the life expectancy in",
    name, "went", direction, "by", difference, "years"
  )
  read
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
sweden_change <- country_change("Sweden", 1960, 1990, life_exp)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations!)
compare_change <- function(first, second, dataframe) {
  first_change <- dataframe[dataframe$country == first, "change"]
  second_change <- dataframe[dataframe$country == second, "change"]
  if (first_change > second_change) {
    country1 <- first
    country2 <- second
    num1 <- first_change
    num2 <- second_change
    difference <- first_change - second_change
  } else if (first_change < second_change) {
    country1 <- second
    country2 <- first
    num1 <- second_change
    num2 <- first_change
    difference <- second_change - first_change
  }
  difference <- round(difference, digits = 1)
  num1 <- round(num1, digits = 1)
  num2 <- round(num2, digits = 1)
  read <- paste0(
    "The country with the bigger change in life expectancy was ", country1,
    " (gain=", num1, "), whose life expectancy grew by ", difference,
    " years more than ", country2, "'s (gain=", num2, ")."
  )
  read
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- compare_change("United States", "France", life_exp)

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.
write.csv(life_exp, "data/my_new_le_file.csv", row.names = FALSE)
