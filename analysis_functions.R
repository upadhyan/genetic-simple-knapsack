one_way_analysis <- function(csv_file1, csv_file2, csv_file3, csv_file4, csv_file5, csv_file6){
  
  analyze_df1 <- read.csv(csv_file1, col.names = TRUE) #csv to df
  analyze_df2 <- read.csv(csv_file2, col.names = TRUE) #csv to df
  analyze_df3 <- read.csv(csv_file3, col.names = TRUE) #csv to df
  analyze_df4 <- read.csv(csv_file4, col.names = TRUE) #csv to df
  analyze_df5 <- read.csv(csv_file5, col.names = TRUE) #csv to df
  analyze_df6 <- read.csv(csv_file6, col.names = TRUE) #csv to df
  
  analyze_df <- rbind(analyze_df1, analyze_df2, analyze_df3, analyze_df4, analyze_df5, analyze_df6)
  
  #For fitness level
  boxplot(profit~pmutation, data = analyze_df, 
       main="Fitness in Respect to Mutation Rate", 
       ylab="Fitness", 
       xlab="Probability of Mutation")
  
  fitness.aov <- aov(profit ~ pmutation, data = analyze_df)
  summary(fitness.aov)
  
  #Insert Simultaneous CIs
  
  #For fitness calls
  boxplot(fitnessCalls~pmutation, data = analyze_df, 
          main="Fitness Calls in Respect to Mutation Rate", 
          ylab="Fitness Calls", 
          xlab="Probability of Mutation")
  
  calls.aov <- aov(profit ~ pmutation, data = analyze_df)
  summary(calls.aov)
  
  #Insert Simultaneous CIs
}

two_way_analysis <- function(csv_file){
  #Insert heat map
  #Insert 2 way anova
  
}