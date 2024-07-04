lsf.str("package:evaluate")

library(evaluate)
library(dplyr)
?evaluate 

a = "10+1"
b=unlist(evaluate(a)[[2]])
b

print
View(print)

library(rlang)
lsf.str("package:rlang")
