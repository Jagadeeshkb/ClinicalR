library(arsenal)


dat <- data.frame(
  SSN = c(23,434,565,878,231), 
  Name=c("hamburgers","butter","cheeze","coffee","teacher"), 
  Age = c(7,8,6,43,56), 
  Gender = c(0,1,0,1,0)
)

dat2 <- data.frame(
  SSN = c(210,345,456,745,245), 
  Name=c("fruits","cupcakes","mangoes","toffee","student"), 
  Number= c(3,5,5,6,77), 
  Different = c(0,0,1,1,0)
)

summary(comparedf(dat,dat2))

#compare log of same datasets

summary(comparedf(dat,dat))

