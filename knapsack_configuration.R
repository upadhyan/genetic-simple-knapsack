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
}


#
#processFile = function(filepath) {
#  con = file(filepath, "r")
#  while ( TRUE ) {
#    line = readLines(con, n = 1)
#    if (length(line) == 0 ) {
#      break
#    }
#    print(line)
#  }
#  
#  close(con)
#}

config_ga_knapsack()