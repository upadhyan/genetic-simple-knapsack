source('knapsack_configuration.R')
config_ga_knapsack() # add rstudio = FALSE if not on rstudio

fitness <- function(solution, profit){
 return(solution %*% profit)
}

profits = c(1,2,3,5,5)
solution = c(1,0,1,0, 1)
print(fitness(solution, profits))