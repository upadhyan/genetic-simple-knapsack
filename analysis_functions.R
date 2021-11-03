one_way_analysis_mutation <- function(csv_file1, csv_file2, csv_file3, csv_file4, csv_file5, csv_file6){

  ##Read all CSVs
  analyze_df1 <- read.csv(csv_file1) #csv to df
  analyze_df2 <- read.csv(csv_file2) #csv to df
  analyze_df3 <- read.csv(csv_file3) #csv to df
  analyze_df4 <- read.csv(csv_file4) #csv to df
  analyze_df5 <- read.csv(csv_file5) #csv to df
  analyze_df6 <- read.csv(csv_file6) #csv to df

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
          xlab="Probability of Mutation",
          ylim = c(43800, 43850))
  boxplot(profit~pmutation, data = analyze_df3, 
          main="Fitness in Respect to Mutation Rate - Sample 3", 
          ylab="Fitness", 
          xlab="Probability of Mutation",
          ylim = c(35000, 46850))
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
  
  #For Optimal Difference
  par(mfrow=c(1,1))
  boxplot(optimal_difference~pmutation, data = analyze_df, 
          main="Optimal Difference in Respect to Mutation Rate", 
          ylab="Optimal Difference", 
          xlab="Probability of Mutation")
  
  calls.aov <- aov(optimal_difference ~ pmutation, data = analyze_df) #ANOVA Test
  summary(calls.aov)
  
##
}




one_way_analysis_crossover <- function(csv_file1, csv_file2, csv_file3, csv_file4, csv_file5, csv_file6){ 
  
  ##Read all CSVs
  analyze_df1 <- read.csv(csv_file1) #csv to df
  analyze_df2 <- read.csv(csv_file2) #csv to df
  analyze_df3 <- read.csv(csv_file3) #csv to df
  analyze_df4 <- read.csv(csv_file4) #csv to df
  analyze_df5 <- read.csv(csv_file5) #csv to df
  analyze_df6 <- read.csv(csv_file6) #csv to df
  
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
  boxplot(profit~pcrossover, data = analyze_df1, 
          main="Fitness in Respect to Crossover Rate - Sample 1", 
          ylab="Fitness", 
          xlab="Probability of Crossover")
  boxplot(profit~pcrossover, data = analyze_df2, 
          main="Fitness in Respect to Crossover Rate - Sample 2", 
          ylab="Fitness", 
          xlab="Probability of Crossover")
  boxplot(profit~pcrossover, data = analyze_df3, 
          main="Fitness in Respect to Crossover Rate - Sample 3", 
          ylab="Fitness", 
          xlab="Probability of Crossover")
  boxplot(profit~pcrossover, data = analyze_df4, 
          main="Fitness in Respect to Crossovern Rate - Sample 4", 
          ylab="Fitness", 
          xlab="Probability of Crossover")
  boxplot(profit~pcrossover, data = analyze_df5, 
          main="Fitness in Respect to Crossover Rate - Sample 5", 
          ylab="Fitness", 
          xlab="Probability of Crossover")
  boxplot(profit~pcrossover, data = analyze_df6, 
          main="Fitness in Respect to Crossover Rate - Sample 6", 
          ylab="Fitness", 
          xlab="Probability of Crossover")
  
  fitness.aov <- aov(profit ~ block + pcrossover, data = analyze_df) #ANOVA Test
  summary(fitness.aov)
  
  ##For fitness calls
  par(mfrow=c(3,2))
  boxplot(fitnessCalls~pcrossover, data = analyze_df1, 
          main="Fitness Calls in Respect to Crossover Rate - Sample 1", 
          ylab="Fitness Calls", 
          xlab="Probability of Crossover")
  boxplot(fitnessCalls~pcrossover, data = analyze_df1, 
          main="Fitness Calls in Respect to Crossover Rate - Sample 2", 
          ylab="Fitness Calls", 
          xlab="Probability of Crossover")
  boxplot(fitnessCalls~pcrossover, data = analyze_df1, 
          main="Fitness Calls in Respect to Crossover Rate - Sample 3", 
          ylab="Fitness Calls", 
          xlab="Probability of Crossover")
  boxplot(fitnessCalls~pcrossover, data = analyze_df1, 
          main="Fitness Calls in Respect to Crossover Rate - Sample 4", 
          ylab="Fitness Calls", 
          xlab="Probability of Crossover")
  boxplot(fitnessCalls~pcrossover, data = analyze_df1, 
          main="Fitness Calls in Respect to Crossover Rate - Sample 5", 
          ylab="Fitness Calls", 
          xlab="Probability of Crossover")
  boxplot(fitnessCalls~pcrossover, data = analyze_df1, 
          main="Fitness Calls in Respect to Crossover Rate - Sample 6", 
          ylab="Fitness Calls", 
          xlab="Probability of Crossover")
  
  calls.aov <- aov(fitnessCalls ~ block + pcrossover, data = analyze_df)
  summary(calls.aov)
  
  #For Optimal Difference
  par(mfrow=c(1,1))
  boxplot(optimal_difference~pcrossover, data = analyze_df, 
          main="Optimal Difference in Respect to Crossover Rate", 
          ylab="Optimal Difference", 
          xlab="Probability of Crossover")
  
  calls.aov <- aov(optimal_difference ~ pcrossover, data = analyze_df) #ANOVA Test
  summary(calls.aov)
  
  ##
  }




  two_way_analysis_combination <- function(csv_file1, csv_file2, csv_file3, csv_file4, csv_file5, csv_file6){
  library("gplots")
  library("multcomp")
  library(effectsize)
  
  ##Read all CSVs
  analyze_df1 <- read.csv(csv_file1) #csv to df
  analyze_df2 <- read.csv(csv_file2) #csv to df
  analyze_df3 <- read.csv(csv_file3) #csv to df
  analyze_df4 <- read.csv(csv_file4) #csv to df
  analyze_df5 <- read.csv(csv_file5) #csv to df
  analyze_df6 <- read.csv(csv_file6) #csv to df
  
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
  
  ##Compile data for heat map - optimal difference

  hm_df <- as.data.frame(matrix(nrow=0, ncol=3))
  colnames(hm_df) <- c("mean", "sd", "median")
  t = 1
  
  for(i in seq(0.01, 0.91, by = 0.10)){
    for(j in seq(0.01, 0.10, by = 0.01)){
      temp_df1 <- as.data.frame(matrix(nrow=0, ncol=12)) #reset df
      temp_df1 <- filter(analyze_df, (pcrossover <= i+0.001 & pcrossover >= i-0.001) & (pmutation <= j+0.001 & pmutation >= j-0.001)) #filter for specific xover and mut.
   
      hm_df[t,1] <- mean(temp_df1$optimal_difference) #store mean
      hm_df[t,2] <- sd(temp_df1$optimal_difference) #store std. dev
      hm_df[t,3] <- median(temp_df1$optimal_difference) #store median
      t = t + 1
      
    }
  }
  
  
  #Make heat maps - optimal difference

  colnames = c("0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.10")
  rownames = c("0.01","0.11", "0.21", "0.31", "0.41", "0.51", "0.61","0.71", "0.81", "0.91")
  hm_mean <- matrix(hm_df[,1], nrow = 10, byrow = TRUE, dimnames= list(rownames, colnames))
  heatmap.2(hm_mean, trace = "none", key = TRUE, dendrogram = "none",Colv= FALSE, Rowv=FALSE, main="Mean of Optimal Difference", xlab="Mutation Rate", ylab="Crossover Rate")
  
  colnames = c("0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.10")
  rownames = c("0.01","0.11", "0.21", "0.31", "0.41", "0.51", "0.61","0.71", "0.81", "0.91")
  hm_sd <- matrix(hm_df[,2], nrow = 10, byrow = TRUE, dimnames= list(rownames, colnames))
  heatmap.2(hm_sd, trace = "none", key = TRUE, dendrogram = "none",Colv= FALSE, Rowv=FALSE, main="Standard Deviation of Optimal Difference", xlab="Mutation Rate", ylab="Crossover Rate")
  
  colnames = c("0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.10")
  rownames = c("0.01","0.11", "0.21", "0.31", "0.41", "0.51", "0.61","0.71", "0.81", "0.91")
  hm_median <- matrix(hm_df[,3], nrow = 10, byrow = TRUE, dimnames= list(rownames, colnames))
  heatmap.2(hm_median, trace = "none", key = TRUE, dendrogram = "none",Colv= FALSE, Rowv=FALSE, main="Median of Optimal Difference", xlab="Mutation Rate", ylab="Crossover Rate")
  
  ##Compile data for heat map - fitness calls
  
  hm_df1 <- as.data.frame(matrix(nrow=0, ncol=6))
  colnames(hm_df1) <- c("mean", "sd", "median", "crossover", "mutation", "block")
  t = 1
  
  for(s in 1:6){
    for(i in seq(0.01, 0.91, by = 0.10)){
      for(j in seq(0.01, 0.10, by = 0.01)){
        temp_df2 <- as.data.frame(matrix(nrow=0, ncol=12)) #reset df
        temp_df2 <- filter(analyze_df, (pcrossover <= i+0.001 & pcrossover >= i-0.001) & (pmutation <= j+0.001 & pmutation >= j-0.001) & (block == s)) #filter for specific xover and mut.
        
        hm_df1[t,1] <- mean(temp_df2$fitnessCalls) #store mean
        hm_df1[t,2] <- sd(temp_df2$fitnessCalls) #store std. dev
        hm_df1[t,3] <- median(temp_df2$fitnessCalls) #store median
        hm_df1[t,4] <- i #crossover
        hm_df1[t, 5] <- j #mutation
        hm_df1[t, 6] <- s #block
        t = t + 1
      }
    }
  }
  
  hm_df2 <- as.data.frame(matrix(nrow=0, ncol=5))
  colnames(hm_df2) <- c("mean", "sd", "median", "crossover", "mutation")
  z = 1
  for(i in seq(0.01, 0.91, by = 0.10)){
    for(j in seq(0.01, 0.10, by = 0.01)){
      temp_df3 <- as.data.frame(matrix(nrow=0, ncol=6)) #reset df
      temp_df3 <- filter(hm_df1, (crossover <= i+0.001 & crossover >= i-0.001) & (mutation <= j+0.001 & mutation >= j-0.001)) #filter for specific xover and mut.
      
      hm_df2[z,1] <- mean(temp_df3$mean) #store mean
      hm_df2[z,2] <- sd_pooled(temp_df3$sd) #store std. dev
      hm_df2[z,3] <- median(temp_df3$median) #store median
      hm_df2[z,4] <- i #crossover
      hm_df2[z, 5] <- j #block
      z = z + 1
    }
  }
  
  #Make heat maps - fitness calls
  
  colnames = c("0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.10")
  rownames = c("0.01","0.11", "0.21", "0.31", "0.41", "0.51", "0.61","0.71", "0.81", "0.91")
  hm_mean1 <- matrix(hm_df2[,1], nrow = 10, byrow = TRUE, dimnames= list(rownames, colnames))
  heatmap.2(hm_mean1, trace = "none", key = TRUE, dendrogram = "none",Colv= FALSE, Rowv=FALSE, main="Mean of Fitness Calls", xlab="Mutation Rate", ylab="Crossover Rate")
  
  colnames = c("0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.10")
  rownames = c("0.01","0.11", "0.21", "0.31", "0.41", "0.51", "0.61","0.71", "0.81", "0.91")
  hm_sd1 <- matrix(hm_df2[,2], nrow = 10, byrow = TRUE, dimnames= list(rownames, colnames))
  heatmap.2(hm_sd1, trace = "none", key = TRUE, dendrogram = "none",Colv= FALSE, Rowv=FALSE, main="Standard Deviation of Fitness Calls", xlab="Mutation Rate", ylab="Crossover Rate")
  
  colnames = c("0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.10")
  rownames = c("0.01","0.11", "0.21", "0.31", "0.41", "0.51", "0.61","0.71", "0.81", "0.91")
  hm_median1 <- matrix(hm_df2[,3], nrow = 10, byrow = TRUE, dimnames= list(rownames, colnames))
  heatmap.2(hm_median1, trace = "none", key = TRUE, dendrogram = "none",Colv= FALSE, Rowv=FALSE, main="Median of Fitness Calls", xlab="Mutation Rate", ylab="Crossover Rate")
  
  
  #Insert 2 way anova
  opt2way.aov <- aov(optimal_difference ~ pcrossover + pmutation, analyze_df)
  summary(opt2way.aov)
  calls2way.aov <- aov(fitnessCalls ~ block + pcrossover + pmutation, analyze_df)
  summary(calls2way.aov)
}

  new_mutation_xover <- function(csv_base1, csv_base2, csv_base3, csv_base4, csv_base5, 
                                 csv_base6, csv_base7, csv_base8, csv_base9, csv_base10,
                                 csv_mut1, csv_mut2, csv_mut3, csv_mut4, csv_mut5,
                                 csv_mut6, csv_mut7, csv_mut8, csv_mut9, csv_mut10,
                                 csv_xover1, csv_xover2, csv_xover3, csv_xover4, csv_xover5,
                                 csv_xover6, csv_xover7, csv_xover8, csv_xover9, csv_xover10){
    
    ##Read all CSVs
    base_df1 <- read.csv(csv_base1) #csv to df
    base_df2 <- read.csv(csv_base2) #csv to df
    base_df3 <- read.csv(csv_base3) #csv to df
    base_df4 <- read.csv(csv_base4) #csv to df
    base_df5 <- read.csv(csv_base5) #csv to df
    base_df6 <- read.csv(csv_base6) #csv to df
    base_df7 <- read.csv(csv_base7) #csv to df
    base_df8 <- read.csv(csv_base8) #csv to df
    base_df9 <- read.csv(csv_base9) #csv to df
    base_df10 <- read.csv(csv_base10) #csv to df
    mut_df1 <- read.csv(csv_mut1) #csv to df
    mut_df2 <- read.csv(csv_mut2) #csv to df
    mut_df3 <- read.csv(csv_mut3) #csv to df
    mut_df4 <- read.csv(csv_mut4) #csv to df
    mut_df5 <- read.csv(csv_mut5) #csv to df
    mut_df6 <- read.csv(csv_mut6) #csv to df
    mut_df7 <- read.csv(csv_mut7) #csv to df
    mut_df8 <- read.csv(csv_mut8) #csv to df
    mut_df9 <- read.csv(csv_mut9) #csv to df
    mut_df10 <- read.csv(csv_mut10) #csv to df
    xover_df1 <- read.csv(csv_xover1) #csv to df
    xover_df2 <- read.csv(csv_xover2) #csv to df
    xover_df3 <- read.csv(csv_xover3) #csv to df
    xover_df4 <- read.csv(csv_xover4) #csv to df
    xover_df5 <- read.csv(csv_xover5) #csv to df
    xover_df6 <- read.csv(csv_xover6) #csv to df
    xover_df7 <- read.csv(csv_xover7) #csv to df
    xover_df8 <- read.csv(csv_xover8) #csv to df
    xover_df9 <- read.csv(csv_xover9) #csv to df
    xover_df10 <- read.csv(csv_xover10) #csv to df
    
    ##Combine all CSVs
    base_df <- rbind(base_df1, base_df2, base_df3, base_df4, base_df5, base_df6, base_df7, base_df8, base_df9, base_df10)
    mut_df <- rbind(mut_df1, mut_df2, mut_df3, mut_df4, mut_df5, mut_df6, mut_df7, mut_df8, mut_df9, mut_df10)
    xover_df <- rbind(xover_df1, xover_df2, xover_df3, xover_df4, xover_df5, xover_df6, xover_df7, xover_df8, xover_df9, xover_df10)
    
    
    ##Insert blocking component
    
    blocking_df <- as.data.frame(matrix(nrow=0, ncol=1))
    colnames(blocking_df) <- "block"
    j = 1
    k = nrow(base_df)-1
    blocking_df[1,1] <- j
    for(i in 1:k){
      q <- as.numeric(base_df[i,1])
      r <- as.numeric(base_df[i+1,1])
      if(q > r){
        blocking_df[i+1,1] <- j+1
        j = j + 1
      } else {
        blocking_df[i+1,1] <- j
      }
    }
    
    base_df <- cbind(base_df, blocking_df)
  
    mut_df <- cbind(mut_df, blocking_df)
    xover_df <- cbind(xover_df, blocking_df)
    
    
    ##Analysis for baseline / mutation
    
    #ftness calls
    bm_fit_ttest <- t.test(base_df$fitnessCalls, mut_df$fitnessCalls, conf.level = 0.99) #t-test
    #return(bm_fit_ttest)
    #return(summary(base_df$fitnessCalls)) #summary values for base fitness calls
    #return(summary(mut_df$fitnessCalls)) #summary values for mutation fitness calls
    
    #optimal_difference
    bm_opt_ttest <- t.test(base_df$optimal_difference, mut_df$optimal_difference, conf.level = 0.99) #t-test
    #return(bm_opt_ttest)
    #return(summary(base_df$optimal_difference)) #summary values for base optimal difference
    #return(summary(mut_df$optimal_difference)) #summary values for mutation optimal difference    
    
    ##Analysis for baseline / xover
    
    #ftness calls
    bx_fit_ttest <- t.test(base_df$fitnessCalls, xover_df$fitnessCalls, conf.level = 0.99) #t-test
    #return(bx_fit_ttest)
    #return(summary(xover_df$fitnessCalls)) #summary values for mutation fitness calls
    
    
    
    #optimal_difference
    bx_opt_ttest <- t.test(base_df$optimal_difference, xover_df$optimal_difference, conf.level = 0.99) #t-test
    #return(bx_opt_ttest)
    return(summary(xover_df$optimal_difference)) #summary values for mutation optimal difference   
    
    
  }
  
  
  var_pop_size <- function(csv_pop1, csv_pop2, csv_pop3, csv_pop4, csv_pop5, csv_pop6){
    
    pop_df1 <- read.csv(csv_pop1) #csv to df
    pop_df2 <- read.csv(csv_pop2) #csv to df
    pop_df3 <- read.csv(csv_pop3) #csv to df
    pop_df4 <- read.csv(csv_pop4) #csv to df
    pop_df5 <- read.csv(csv_pop5) #csv to df
    pop_df6 <- read.csv(csv_pop6) #csv to df
    
    ##Combine all CSVs
    pop_df <- rbind(pop_df1, pop_df2, pop_df3, pop_df4, pop_df5, pop_df6)
   
    plot.default(pop_df$pop_size, pop_df$optimal_difference,
                        main= "Proportion of Optimal Value Obtained by Population Size", 
                        xlab = "Population Size",
                        ylab =  "Optimal Difference",
                        col = 'blue',
                        axes = FALSE)
    axis(1, at = seq(20, 200, by = 5))
    axis(2, at = seq(0, 1, by = 0.02))
  }

