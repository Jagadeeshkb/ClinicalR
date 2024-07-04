
#masking
with(mtcars,mean(cyl+am))

#data-masking function
my_mean <- function(data, var1, var2) {
  dplyr::summarise(data, mean(var1 + var2))
}

my_mean(mtcars, cyl, am)

# """
# The problem here is that summarise() defuses the R code it was supplied, i.e. mean(var1 + var2). Instead we want it to see mean(cyl + am). 
# This is why we need injection, we need to modify that piece of code by injecting the code supplied to the function in place of var1 and var2.
# """

my_mean <- function(data, var1, var2) {
  dplyr::summarise(data, mean({{ var1 }} + {{ var2 }}))
}

my_mean(mtcars, cyl, am)


#---
# Defining an env-variable
cyl <- 1000

# Referring to a data-variable
dplyr::summarise(mtcars, mean(cyl))

#program executed value from dataframe and not from global env.


cyl <- 1000

mtcars %>%
  dplyr::summarise(
    mean_data = mean(.data$cyl),
    mean_env = mean(.env$cyl)
  )


#pronouns

#.data .env
m <- 10
mtcars %>% mutate(disp = .data$disp * .env$m) #.data --> from data frame and .env from environment

