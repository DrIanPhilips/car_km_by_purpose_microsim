#-------------------------------------------------------------------------------
#----------------------CONVERT COUNT INTO KMS PER PURPOSE-----------------------
#-------------------------------------------------------------------------------

#Remind of what the trip purpose and distance groups are:
# new NTS code Trip Purpose
# Commute 1
# Business 2
# Education 3
# Shop personal trips 4
# Social Interaction 5
# Leisure 6

# Group 1: x <= 2km
# Group 2: 2 < x <= 5km
# Group 3: 5 < x <= 10km
# Group 4: 10 < x <= 25km
# Group 5: 25 < x <= 50km
# Group 6: x > 50km

#We will take the upper bound of each group to calculate distance.

#The function multiplies the count by the appropiate value to get an approxiate
# distance travelled in kms.
#It creates new columns for the distance travelled and deletes the count columns.

count_to_km <- function(dataframe){
  
  for(j in 1:6){
    
    #Create a vector of each purpose/distance column combination and multiple
    # the count by the appropiate distance group value.
    dist_1_post <- grep(paste0("CountTP_",j,"_dist_1"), colnames(dataframe))
    dist_1 <- c(dataframe[,dist_1_post[1]] )
    dist_1 <- dist_1*2 
    dataframe[,paste0("Total_kms_TP_",j,"_dist_1")] <- dist_1 
    
    dist_2_post <- grep(paste0("CountTP_",j,"_dist_2"), colnames(dataframe))
    dist_2 <- c(dataframe[,dist_2_post[1]] )
    dist_2 <- dist_2*5 
    dataframe[,paste0("Total_kms_TP_",j,"_dist_2")] <- dist_2 
    
    dist_3_post <- grep(paste0("CountTP_",j,"_dist_3"), colnames(dataframe))
    dist_3 <- c(dataframe[,dist_3_post[1]] )
    dist_3 <- dist_3*10 
    dataframe[,paste0("Total_kms_TP_",j,"_dist_3")] <- dist_3 
    
    dist_4_post <- grep(paste0("CountTP_",j,"_dist_4"), colnames(dataframe))
    dist_4 <- c(dataframe[,dist_4_post[1]] )
    dist_4 <- dist_4*25 
    dataframe[,paste0("Total_kms_TP_",j,"_dist_4")] <- dist_4 
    
    dist_5_post <- grep(paste0("CountTP_",j,"_dist_5"), colnames(dataframe))
    dist_5 <- c(dataframe[,dist_5_post[1]] )
    dist_5 <- dist_5*50 
    dataframe[,paste0("Total_kms_TP_",j,"_dist_5")] <- dist_5 
    
    dist_6_post <- grep(paste0("CountTP_",j,"_dist_6"), colnames(dataframe))
    dist_6 <- c(dataframe[,dist_6_post[1]] )
    dist_6 <- dist_6*50 
    dataframe[,paste0("Total_kms_TP_",j,"_dist_6")] <- dist_6 
    
    #Get the position of all of the new "Total_kms_TP", add an extra column
    # which is the sum of the 6 newly created "Total_kms_TP" columns.
    postvec <- grep(paste0("Total_kms_TP_",j), colnames(dataframe))
   
    dataframe[, paste0("Weekly_kms_TP_",j)] <- rowSums(dataframe[,postvec])
    
    
  }
  
  #Delete unnecessary columns
  delete <- grep(paste0("Count"), colnames(dataframe))
  delete2 <- grep(paste0("_dist_"), colnames(dataframe))
  dataframe <- select(dataframe, -c(delete, delete2))
  
  
  return(dataframe)
  
}  

  



