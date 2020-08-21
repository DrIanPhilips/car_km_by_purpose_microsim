#-------------------------------------------------------------------------------
#------------------------------DISTANCE ASSIGNMENT------------------------------
#-------------------------------------------------------------------------------

#As part of the Monte Carlo sampling process this function first rolls a number 
# between 0 and 1 ("diceRoll_") for each individual's trip purpose. It looks to 
# see where this dice roll lies along the individual's trip purpose probability 
# distribution and assigns the indiviudal a trip purpose ("Assignment_"). Then, 
# depending on the trip purpose assigned, it joins the relevant trip distance 
# constraint table to the dataframe. For example, if the indiviudal was assigned 
# trip purpose 3, it would join the trip distance constraint table for trip 
# purpose 3 to the individual. It is joined again by age, gender, National 
# Statistics Socio-economic Classification, adjusted Government Operational Region 
# and Rural/Urban Classification.the indiviudal. 

#Next, it rolls another number between 0 and 1 ("diceRoll2_") to then assign the 
# indiviudal a trip distance ("Assignment2_") depending on where the dice roll lies 
# along the individual's trip distance probability distrubtion.

#This process is repeated the same number of times that the user inputs into the
# function for every individual in the synthetic population. 

#To work the function the user needs to input a "dataframe", typical one that 
# holds the population data from the NTS survery and SPENSER.

distance_assignment <- function(dataframe, number){
  
  practice_df <- dataframe
  
  for(m in 1:number){
    
    #Add assignment and dice roll columns, the dice rolls a number between 0 and 1.
    practice_df[, paste0("diceRoll_", m)] <- runif(n = nrow(practice_df),min = 0,max = 1) 
    
    practice_df[, paste0("Assignment_", m)] <- 0
    
    #Get the position of the probability distribution columns for trip purpose
    dist_position <- grep("ProbDistTripPurpose", colnames(practice_df))
    sort_column <- sort(colnames(practice_df)[dist_position[1:length(dist_position)]])
    
    #Create vectors that stores the values of the ProbDistTripPurpose columns.
    for(j in 1:length(sort_column)){
      assign(paste0("vec_ProbDist_",j, sep =""), practice_df[,sort_column[j]])
    }
    
    #Make an assignment vector which is the same length as the dataframe.
    assignmentvec <- vector(mode = "numeric", length = nrow(practice_df))  
    
    #Make a vector for the dice roll. 
    vecDiceRoll <- practice_df[, paste0("diceRoll_", m)]
    
    #Compare the value of the diceRoll vector with the ProbDistTripPurpose vectors 
    # to see where the value lies, then assignment a numeric value which indicates 
    # a particular behaviour.
    assignmentvec[vecDiceRoll <= vec_ProbDist_1] <- 1
    assignmentvec[vecDiceRoll > vec_ProbDist_1 & vecDiceRoll <= vec_ProbDist_2 ] <- 2
    assignmentvec[vecDiceRoll > vec_ProbDist_2 & vecDiceRoll <= vec_ProbDist_3 ] <- 3
    assignmentvec[vecDiceRoll > vec_ProbDist_3 & vecDiceRoll <= vec_ProbDist_4 ] <- 4
    assignmentvec[vecDiceRoll > vec_ProbDist_4 & vecDiceRoll <= vec_ProbDist_5 ] <- 5
    assignmentvec[vecDiceRoll > vec_ProbDist_5 ] <- 6
    
    # Write the assignmentvec to the dataframe.
    practice_df[, paste0("Assignment_", m)] <- assignmentvec
    
    # Create an empty dataframe.
    result <- data.frame()
    
    #Split the dataframe by each trip purpose type, then append the relevant trip 
    # distance constraint table to the main dataframe.
    for(k in 1:6){
      
      #p% columns for trip distance are not required.
      dist_position_2 <- grep("p%", colnames(tripPurposeCtList[[k]]))
      
      ct <- select(tripPurposeCtList[[k]], -c(dist_position_2))
      
      #Create a only contains trip purpose assigments that are equal to k.
      test1 <- practice_df[practice_df[, paste0("Assignment_",m)] == k,]
      
      #Append the two dataframes.
      test1 <- test1 %>%
        left_join(ct, by = c("Sex" = "Sex", 
                             "Age_B04" = "Age_B04", 
                             "NSSEC" = "NSSEC",
                             "GOR_new" = "GOR_new",
                             "ONSRuralUrban" = "ONSRuralUrban"))
      
      #Append each new dataframe on top of the other.
      result <- rbind(result, test1)
      
      
    }
    
    practice_df <- result
    
    
    #Add assignment column and dice roll for trip distance based on trip purpose
    practice_df[, paste0("diceRoll2_", m)] <- runif(n = nrow(practice_df),min = 0,max = 1) 
    
    practice_df[, paste0("Assignment2_", m)] <- 0
    
    #Get the position of the probability distribution columns for trip distance.
    dist_position_2 <- grep("ProbDistTripDistance", colnames(practice_df))
    sort_column_2 <- sort(colnames(practice_df)[dist_position_2[1:length(dist_position_2)]])
    
    #Create vectors that stores the values of the ProbDistTripDistance_km columns.
    for(l in 1:length(sort_column_2)){
      assign(paste0("vec_ProbDist_",l, sep =""), practice_df[,sort_column_2[l]])
    }
    
    #Make an assignment vector which is the same length as the dataframe.
    distancevec <- vector(mode = "numeric", length = nrow(practice_df))  
    
    #Make a vector for the dice roll. 
    vecDiceRoll2 <- practice_df[, paste0("diceRoll2_", m)]
    
    #Compare the value of the diceRoll vector with the ProbDistTripDistance_km 
    # vectors to see where the value lies, then assign a numeric value for trip
    # distance for that individual.
    distancevec[vecDiceRoll2 <= vec_ProbDist_1] <- 1
    distancevec[vecDiceRoll2 > vec_ProbDist_1 & vecDiceRoll2 <= vec_ProbDist_2 ] <- 2
    distancevec[vecDiceRoll2 > vec_ProbDist_2 & vecDiceRoll2 <= vec_ProbDist_3 ] <- 3
    distancevec[vecDiceRoll2 > vec_ProbDist_3 & vecDiceRoll2 <= vec_ProbDist_4 ] <- 4
    distancevec[vecDiceRoll2 > vec_ProbDist_4 & vecDiceRoll2 <= vec_ProbDist_5 ] <- 5
    distancevec[vecDiceRoll2 > vec_ProbDist_5 ] <- 6
    
    # Write the distance assignment vector (distancevec) to the dataframe.
    practice_df[, paste0("Assignment2_", m)] <- distancevec
    
    # Delete these columns from the dataframe as they are not required, and it reduce
    # the data size.
    delete <- grep("ProbDistTripDistance", colnames(practice_df))
    delete2 <- grep("p%TripDistance", colnames(practice_df))
    practice_df <- select(practice_df, -c(delete, delete2))
    
    
  }
  # Delete these columns from the dataframe as they are not required, and it reduce
  # the data size.
  # Tested that the dice roll worked for 
  delete <- grep("diceRoll", colnames(practice_df))
  delete2 <- grep("ProbDist", colnames(practice_df))
  
  practice_df <- select(practice_df, -c(delete, delete2))

  return(practice_df)
  
}
