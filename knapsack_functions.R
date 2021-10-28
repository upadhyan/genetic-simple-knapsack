conditional_install <- function(package_name){
  if(package_name %in% rownames(installed.packages()) == FALSE){
    install.packages(package_name, character.only = TRUE)
  }
  require(package_name, character.only = TRUE)
}

config_ga_knapsack <- function(rstudio = TRUE){
  conditional_install("utils")
  if(rstudio){
    conditional_install("rstudioapi")
    current_path = rstudioapi::getActiveDocumentContext()$path 
    setwd(dirname(current_path ))
    print(getwd())
  } else {
    setwd(utils::getSrcDirectory()[1])
  }
  
  conditional_install("rstan")
  conditional_install("GA")
  conditional_install("dplyr")
  conditional_install("ggplot2")
  conditional_install("ROI")
  conditional_install("foreach")
  conditional_install("iterators")
  conditional_install("stringr")
}

config_ga_knapsack()

########### Fitness Functions
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
  fitness_calls <<- fitness_calls + 1
  weight <- weights %*% solution
  max_profit = sum(profit)
  max_weight = sum(weights)
  if (weight > c){
    g = 0
    h = 1 - (weight - c)/(max_weight - c)
  } else{
    tryCatch(
      expr = {
        g <- profit %*% solution
      },
      error = function(e){ 
        print("error")
        print(typeof(profit))
        print(typeof(solution))
        print(length(profit))
        print(length(solution))
      }
    )
    
    h = 1
  }
  return(h*max_profit + g)
}
########### Mutation Functions
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
########### Crossover Functions Functions



########### Testing Functions

############# Part a
crossover_mutation_test<- function(file_name, crossover){
  expression = "knapPI_([0-9]*)_([0-9]*)_([0-9]*)_([0-9]*)_([0-9]*)_([0-9]*)_([0-9]*).csv"
  crossover = 0.8
  regex_result = str_match(file_name, expression)
  n = strtoi(regex_result[1,3])
  c = strtoi(regex_result[1,6])
  optimal_value = strtoi(regex_result[1,7])
  instance_type = strtoi(regex_result[1,2])
  range = strtoi(regex_result[1,4])
  df = read.csv(file_name)
  profits <- df[['v']]
  weights <- df[['w']]
  mutation_values <- seq(.01, .1, length.out = 10)
  result_frame <- data.frame(pmutation=double(),
                   profit=double(),
                   weight=double(),
                   n=double(),
                   optimal_difference=double(),
                   constraint_met = logical(),
                   pop_size = integer(),
                   pcrossover = double(),
                   fitnessCalls = integer()) 
  for(value in mutation_values){
    for(i in 1:30){
      print(paste0("Current value: ", value))
      fitness_calls <<-0
      invisible(capture.output(GA <- ga(type = "binary", 
                                        fitness = function(x) fitness_constraint_adjust(x, profits, weights, c), 
                                        nBits = n, 
                                        popSize = 100,
                                        pcrossover = crossover, 
                                        pmutation = value, 
                                        maxiter = 1000,
                                        run =300)))
      final_solution = GA@solution[1,]
      final_fitness = final_solution %*% profits
      final_weight = final_solution %*% weights
      rm(GA)
      result = c(pmutation = value,
                 profit = final_fitness, 
                 weight = final_weight,
                 n = n,
                 optimal_difference = 1 - (optimal_value- final_fitness)/optimal_value,
                 contraint_met = as.logical(final_weight <= c),
                 pop_size = 100,
                 pcrossover = crossover,
                 fitnessCalls = fitness_calls)
      result_frame <- rbind(result_frame, t(result))
    }
  }
  return(result_frame)
}





#summary(GA1)

#solution = c(0,1,1,1,0,1,1,0,0,1)
#if(solution %*% weights <= c){
#  print("Good")
#  print(c)
#  print(solution %*% weights)
#} else {
#  print("bad")
#}

