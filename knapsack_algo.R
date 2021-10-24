source('knapsack_configuration.R')
config_ga_knapsack() # add rstudio = FALSE if not on rstudio



fitness <- function(solution, profit, weights, c){
  weight <- weights %*% solution
  if(weight > c){
    return(0)
  } else {
    dot_product <- profit %*% solution
    return(dot_product)
  }
}

gabin_raMutation <- function(object, parent, ...)
{
  if(gaControl("useRcpp"))
    gabin_raMutation_Rcpp(object, parent)
  else
    gabin_raMutation_R(object, parent)
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

weights = c(2,2,2,2,2,2,2,9)
profit = c(3,2,3,2,3,2,2,50)
c = 8
fitness(solution, profit, weights, c)

GA <- ga(type = "binary",
         fitness = function(x) fitness(x,profit, weights, c), 
         mutation = function(object, parent, ...) sp_mutation(object, parent, weights, profit),
         nBits=8,
         popSize = 100,
         maxiter = 1000,
         pcrossover = 0.8, 
         pmutation = 0.1, 
         elitism = base::max(1, round(popSize*0.05)),
         run = 100)
summary(GA)
