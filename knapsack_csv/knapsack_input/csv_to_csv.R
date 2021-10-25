csv_to_csv <- function(input_data){
  
  data_txt<-read.delim(input_data, header = FALSE)
  i = 0 #Iterate through all instances in each experiment
  j = 1 #Used to terminate function once all experiments are iterated through
  
  
  while(j <= 1){
    fun_name <- as.character(data_txt[i+1,]) #Function name
    n <- as.numeric(substring(data_txt[i+2,], 3)) #Number of instances
    c <- as.numeric(substring(data_txt[i+3,], 3)) #profit
    z <- as.numeric(substring(data_txt[i+4,], 3)) #Optimal value
    run_time <- as.numeric(substring(data_txt[i+5,], 6)) #Run time of to compute optimal solution
    
    data_df <- as.data.frame(matrix(nrow=0, ncol=4)) #initialize dataframe
    data_df_fin <- as.data.frame(matrix(nrow=0, ncol=3)) #initialize final dataframe to be outputted
    
    i = i + 5
    l = 1 #Counter for the final df rows

     while(l < n+1){
      i = i + 1
      data_string <- toString(data_txt[i,]) #Convert to string
      data_string <- as.numeric(unlist(strsplit(data_string,","))) #split string by commas and make numerical
      data_df[l,] <- data_string #Insert into dataframe
      l = l + 1
    }
    
    i = i - 1 #Compensate for overcounting by 1
    
    data_df_fin <- data_df[c(2, 3, 4)] #Insert all columns except for n into final df
    colnames(data_df_fin) <- c("v","w","x") #Name columns
    csv_name <- paste0(fun_name,'_',c,'_',z,'_',run_time,'.csv') #Write output csv file name
    write.csv(data_df_fin, paste0(csv_name), row.names = TRUE) #Write csv file
    
    if (is.na(data_txt[i+3,])){
      j = j + 1
    }
    i = i + 2 #Skip non-important lines
  }

}
  
  