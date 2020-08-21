#------------------------------COUNT TRIP PURPOSE-------------------------------
#This function counts the number of times each purpose and distance combination 
# occurs for each individual. This will be useful to then approximate the distance
# the indiviudal travelled within a week.
#In total, this functions creates 6 (trip purpose types) * 6 (trip distance groups)
# = 36 columns.
#For example, column CountTP_1_dist_1 indicates the number of times the individual 
# was rolled trip purpose type 1 with trip distance group 1.

trip_purpose_distance_count <- function(dataframe){
  
  #Extract a dataframe
  test_1 <- dataframe
  
  #Get the position of the trippurpose assignment columns and store the columns as a vector
  position_trip <- grep("Assignment_", colnames(test_1))
  trip_vec <- c()
  for(i in 1:length(position_trip)){
    trip_vec <- c(trip_vec, test_1[,position_trip[i]] )
  }
  
  #Get the position of the trip distance assignment columns and store the columns as a vector
  position_dist <- grep("Assignment2_", colnames(test_1))
  distance_vec <- c()
  for(i in 1:length(position_trip)){
    distance_vec <- c(distance_vec, test_1[,position_dist[i]] )
  }
  
  
  for(j in 1:6){
    
    #Create a vector that gets the vector position of all the times a trip purpose equals j
    number_trip <- grep(j, trip_vec)
    
    #Create a vector that stores the vector position of the trip distance when the trip 
    # purpose equals j
    number_dist <- distance_vec[number_trip]
    
    #Count how many times trip purpose j occurs for every indiviudal
    test_1[,paste0("CountTP_",j,"_Total")] <- rowSums(test_1[,c(position_trip)] == j)
    
    #Create empty vectors that will store the position of when a particular trip distance 
    # occurs depending on trip purpose j
    #These are the same length as trip_vec1
    count_tp_i_dist_1 <- vector(mode = "numeric", length = length(trip_vec))
    count_tp_i_dist_2 <- vector(mode = "numeric", length = length(trip_vec))
    count_tp_i_dist_3 <- vector(mode = "numeric", length = length(trip_vec))
    count_tp_i_dist_4 <- vector(mode = "numeric", length = length(trip_vec))
    count_tp_i_dist_5 <- vector(mode = "numeric", length = length(trip_vec))
    count_tp_i_dist_6 <- vector(mode = "numeric", length = length(trip_vec))
    
    #Loop to find the vector position of which trip distance occurs for trip purpose 
    # j, then replace the value with 1, else the value is 0.
    #For example, if the trip distance was 1 for trip purpose j, where that value is
    # positioned in distance_vec1 ie i replace the value of count_tp_i_dist_1[i] with 1
    # because length(distance_vec) = length(count_tp_i_dist_1)
    for(i in 1:length(number_trip)){
      
      if(distance_vec[number_trip[i]] == 1){
        count_tp_i_dist_1[number_trip[i]] <- 1
      } else if (distance_vec[(number_trip[i])] == 2){
        count_tp_i_dist_2[number_trip[i]] <- 1
      } else if (distance_vec[(number_trip[i])] == 3){
        count_tp_i_dist_3[number_trip[i]] <- 1
      } else if (distance_vec[(number_trip[i])] == 4){
        count_tp_i_dist_4[number_trip[i]] <- 1
      } else if (distance_vec[(number_trip[i])] == 5){
        count_tp_i_dist_5[number_trip[i]] <- 1
      } else if (distance_vec[(number_trip[i])] == 6){
        count_tp_i_dist_6[number_trip[i]] <- 1
      } else {}
    }
    
    #Create a dataframe of the counts for trip distance depending on the trip purpose
    y <- cbind(count_tp_i_dist_1, count_tp_i_dist_2, count_tp_i_dist_3, count_tp_i_dist_4, count_tp_i_dist_5, count_tp_i_dist_6)
    
    #Extract each column from y, and sum up the number of times a trip purpose versus 
    # trip distance occurred for every individual in the dataframe.
    for(i in 1:6){
      #need to add every length(test_1) element together
      x <- matrix(y[,i], nrow = nrow(test_1), byrow = FALSE)
      x <- data.frame(x)
      
      #Append the rowSum column to the main dataframe
      test_1[, paste0("CountTP_",j,"_dist_",i)] <- rowSums(x)
      
    }
    
    #Clear unused memory
    gc()
    
  }
  
  # Delete the assignment columns from the dataframe as they are not required, 
  # and it will reduce the data size.
  delete <- grep("Assignment", colnames(test_1))
  
  test_1 <- select(test_1, -c(delete))
  
  #Clear unused memory
  gc()
  
  return(test_1)
  
  
}

