library(lobstr)

#ast - abstract tree of R expressions
a= 1
b=2
c=3
ast(a+b+c)

#--
ast(function(x = 1) {
  if (x > 0) print("Hi!")
})


#---ref - how R objects shared across data structures

x <- 1:1e6

obj_size(x) #returns object size

ref(x) # return memory address
?ref

y <- list(x, x, x)
ref(y)

obj_size(y)

z <- list(x, x, x,x)

obj_size(z)

a = list()
obj_size(a)


#---Call stact trees
f <- function(x){ 
  g <- function(x) h(x) 
  g(x)
  } 

h <- function(x) x

f(cst())
