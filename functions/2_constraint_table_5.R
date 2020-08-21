#-------------------------------------------------------------------------------
#-------------------------------CONSTRAINT TABLE--------------------------------
#-------------------------------------------------------------------------------

#df is the dataframe
#Con1, Con2, Con3, Con4 and Con5 are columns names of the dataframe.
#Var1 is the probability we want to calculate.
#Need to put ConX and Var1 in "" to ensure the function works.

constraint_table_5 <- function(df,Con1,Con2,Con3,Con4,Con5,Var1){
  
  #Create a contigency table with the constraints as the rows, and the variable 
  # to explore as the column.
  contingency_table <- ftable(xtabs(~ df[,Con1] + df[,Con2] + df[,Con3] + df[,Con4] + df[,Con5] + df[,Var1], data = df)) 
  
  #Turn the contigency table into a dataframe.
  contingency_df <- data.frame(expand.grid(rev(attr(contingency_table, "row.vars"))), unclass(contingency_table))
  
  #Create a vector of the unique values for the variable, excluding NA, as these 
  # are not included in the contigency df.
  Var1_values <- sort(unique(df[,Var1]), decreasing = FALSE, na.last = NA)
  
  #Relabel the columns of the constraint table.
  colnames(contingency_df) <- c(Con5, Con4, Con3, Con2, Con1, paste0(Var1,"_",Var1_values))
  
  #Need to convert the constraint columns from factors to numeric for future calculations.
  contingency_df[, Con1] <- as.numeric(as.character(contingency_df[, Con1]))
  contingency_df[, Con2] <- as.numeric(as.character(contingency_df[, Con2]))
  contingency_df[, Con3] <- as.numeric(as.character(contingency_df[, Con3]))
  
  #Replace values equal to 0 with 0.001. Although none of the respondents met the criteria,
  # there could be people in the wider population who do.
  for(i in 1:(length(Var1_values)+5)){
    contingency_df[, i] <- ifelse(contingency_df[,i] == 0 , 0.1, contingency_df[,i]) 
  }
  
  #Add a new column called total_freq. Each value in this column is the total amount 
  # of times the Con1 and Con2 pairs occur.
  contingency_df$total_freq <- rowSums(contingency_df[, 6:(5+length(Var1_values))])
  
  #These columns will hold the probability of an event occuring.
  #Loop through to calculate the probability that Var1 occurs depending on Con1 and Con2.
  for(i in 1:length(Var1_values)){
    contingency_df[,paste0("p_",Var1,i)] <- (contingency_df[ ,paste0(Var1,"_",i)])/contingency_df[, paste0("total_freq")]
  } 
  
  #Convert the probability of Var1 occuring into a percentage by multiplying the 
  # probability by 100.
  for(i in 1:length(Var1_values)){
    contingency_df[,paste0("p%", Var1, i)] <- (contingency_df[ ,paste0("p_", Var1, i)])*100
  }
  
  #These columns will hold the probability distribution between outcomes which will 
  # help with the Monte Carlo sampling method.
  #Create a column of the probabilities for the first value of Var1 into its representive column. 
  contingency_df[, paste0("ProbDist", Var1, 1)] <- contingency_df[,paste0("p_", Var1, 1)]
  
  #For loop to input the probability distribution for the remaining values of Var1
  for(i in 1:nrow(contingency_df)){
    for(j in 2:length(Var1_values)){
      contingency_df[i, paste0("ProbDist", Var1, j)] <- contingency_df[i,paste0("ProbDist", Var1, j-1)] + contingency_df[i,paste0("p_", Var1, j)]
    }
    
  }
  
  #The distribution should be from 0 to 1, if the final mark along the distrubtion is greater 
  # than 1 then replace value with one.
  contingency_df[,paste0("ProbDist", Var1, length(Var1_values))] <- ifelse(contingency_df[,paste0("ProbDist", Var1, length(Var1_values))] > 1, 1, contingency_df[,paste0("ProbDist", Var1, length(Var1_values))])
  
  #Remove columns that will not be used for future analysis.
  var1_position <- grep(paste0(Var1,"_"), colnames(contingency_df))
  contingency_df <- contingency_df[, -c(var1_position)]
  
  var1_position <- grep("total_freq", colnames(contingency_df))
  contingency_df <- contingency_df[, -c(var1_position)]
  
  var1_position <- grep("p_", colnames(contingency_df))
  contingency_df <- contingency_df[, -c(var1_position)]
  
  return(contingency_df)
  
}




