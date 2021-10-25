source('knapsack_configuration.R')
config_ga_knapsack() # add rstudio = FALSE if not on rstudio



fitness_zero <- function(solution, profit, weights, c){
  weight <- weights %*% solution
  if(weight > c){
    return(0)
  } else {
    dot_product <- profit %*% solution
    return(dot_product)
  }
}

fitness_constraint_adjust <- function(solution, profit, weights, c){
  weight <- weights %*% solution
  max_profit = sum(profit)
  max_weight = sum(weights)
  if (weight > c){
    g = 0
    h = 1 - (weight - c)/(max_weight - c)
  } else{
    g <- profit %*% solution
    h = 1
  }
  return(h*max_profit + g)
}

sp_mutation <- function(object, parent,weights, profit)
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j]-1)
  return(mutate)
}

weight_prob_mutation <- function(object, parent,weights, profit)
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j]-1)
  return(mutate)
}

profit_prob_mutation <- function(object, parent,weights, profit)
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j]-1)
  return(mutate)
}

ratio_prob_mutation <- function(object, parent,weights, profit)
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j]-1)
  return(mutate)
}

profit = c(32,22,3,67,6,10,22,54,67,55)
weights = c(42,24,1,57,7,11,12,64,77,35)
c = 77+35+1+24+3


GA1 <- ga(type = "binary", fitness = function(x) fitness_constraint_adjust(x, profit, weights, c),
          mutation = function(object, parents, ...) sp_mutation(object, parents, weights, profit),
          nBits = 10, 
          popSize = 100, maxiter = 1000, run = 200)


summary(GA1)

solution = c(0,1,1,1,0,1,1,0,0,1)
if(solution %*% weights <= c){
  print("Good")
  print(c)
  print(solution %*% weights)
} else {
  print("bad")
}