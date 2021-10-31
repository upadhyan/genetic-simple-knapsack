#one_way_analysis <- function(csv_file1, csv_file2, csv_file3, csv_file4, csv_file5, csv_file6){

  ##Read all CSVs
  analyze_df1 <- read.csv("./a_mutation_output/mutation_test_results_1.csv") #csv to df
  analyze_df2 <- read.csv("./a_mutation_output/mutation_test_results_2.csv") #csv to df
  analyze_df3 <- read.csv("./a_mutation_output/mutation_test_results_3.csv") #csv to df
  analyze_df4 <- read.csv("./a_mutation_output/mutation_test_results_4.csv") #csv to df
  analyze_df5 <- read.csv("./a_mutation_output/mutation_test_results_5.csv") #csv to df
  analyze_df6 <- read.csv("./a_mutation_output/mutation_test_results_6.csv") #csv to df

  ##Combine all CSVs
  analyze_df <- rbind(analyze_df1, analyze_df2, analyze_df3, analyze_df4, analyze_df5, analyze_df6)
  blocking_df <- as.data.frame(matrix(nrow=0, ncol=1))
  colnames(blocking_df) <- "block"
  j = 1
  k = nrow(analyze_df)-1
  blocking_df[1,1] <- j
  for(i in 1:k){
    q <- as.numeric(analyze_df[i,1])
    r <- as.numeric(analyze_df[i+1,1])
    if(q > r){
      blocking_df[i+1,1] <- j+1
      j = j + 1
    } else {
      blocking_df[i+1,1] <- j
    }
  }
  
  analyze_df <- cbind(analyze_df, blocking_df)
  
  ##For fitness level
  par(mfrow=c(3,2))
  boxplot(profit~pmutation, data = analyze_df1, 
          main="Fitness in Respect to Mutation Rate - Sample 1", 
          ylab="Fitness", 
          xlab="Probability of Mutation")
  boxplot(profit~pmutation, data = analyze_df2, 
          main="Fitness in Respect to Mutation Rate - Sample 2", 
          ylab="Fitness", 
          xlab="Probability of Mutation")
  boxplot(profit~pmutation, data = analyze_df3, 
          main="Fitness in Respect to Mutation Rate - Sample 3", 
          ylab="Fitness", 
          xlab="Probability of Mutation")
  boxplot(profit~pmutation, data = analyze_df4, 
          main="Fitness in Respect to Mutation Rate - Sample 4", 
          ylab="Fitness", 
          xlab="Probability of Mutation")
  boxplot(profit~pmutation, data = analyze_df5, 
          main="Fitness in Respect to Mutation Rate - Sample 5", 
          ylab="Fitness", 
          xlab="Probability of Mutation")
  boxplot(profit~pmutation, data = analyze_df6, 
          main="Fitness in Respect to Mutation Rate - Sample 6", 
          ylab="Fitness", 
          xlab="Probability of Mutation")
  
  fitness.aov <- aov(profit ~ block + pmutation, data = analyze_df)
  summary(fitness.aov)

  ##For fitness calls
  par(mfrow=c(3,2))
  boxplot(fitnessCalls~pmutation, data = analyze_df1, 
          main="Fitness Calls in Respect to Mutation Rate - Sample 1", 
          ylab="Fitness Calls", 
          xlab="Probability of Mutation")
  boxplot(fitnessCalls~pmutation, data = analyze_df1, 
          main="Fitness Calls in Respect to Mutation Rate - Sample 2", 
          ylab="Fitness Calls", 
          xlab="Probability of Mutation") 
  boxplot(fitnessCalls~pmutation, data = analyze_df1, 
          main="Fitness Calls in Respect to Mutation Rate - Sample 3", 
          ylab="Fitness Calls", 
          xlab="Probability of Mutation")
  boxplot(fitnessCalls~pmutation, data = analyze_df1, 
          main="Fitness Calls in Respect to Mutation Rate - Sample 4", 
          ylab="Fitness Calls", 
          xlab="Probability of Mutation")
  boxplot(fitnessCalls~pmutation, data = analyze_df1, 
          main="Fitness Calls in Respect to Mutation Rate - Sample 5", 
          ylab="Fitness Calls", 
          xlab="Probability of Mutation")
  boxplot(fitnessCalls~pmutation, data = analyze_df1, 
          main="Fitness Calls in Respect to Mutation Rate - Sample 6", 
          ylab="Fitness Calls", 
          xlab="Probability of Mutation")
  
  calls.aov <- aov(fitnessCalls ~ block + pmutation, data = analyze_df)
  summary(calls.aov)
  
##
#}

#two_way_analysis <- function(csv_file){
  #Insert heat map
  #Insert 2 way anova
  
#}
  
