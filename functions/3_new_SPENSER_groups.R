#-------------------------------CREATE NEW GROUPS-------------------------------
#This script loads two new functions new_age() and new_NSSEC(), which create new
# age and NSSEC groups that corresponds with those found in the NTS dataset.


#--------------------------------------AGE--------------------------------------
#Create age groups of the spenser data that match the age groups of the NTS dataset.
#Group 4: 17 - 20 years
#Group 5: 21 - 29 years
#Group 6: 30 - 39 years
#Group 7: 40 - 49 years
#Group 8: 50 - 59 years
#Group 9: 60 years +

new_age <- function(dataframe){
  #Create an empty vector and a vector of the column Age_B04
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  age_vec <-  dataframe[, paste0("Age_B04")]
  
  #This for loop numerically codes Age_B04 if the value falls within a 
  # particular group. It also considers whether the value is NA.
  for(j in 1:length(assignmentvec)){
    
    if (is.na(age_vec[j]) == TRUE){
      assignmentvec[j] <- NA
    } else if (age_vec[j] >= 17 && age_vec[j] <= 20){
      assignmentvec[j] <- 4
    } else if (age_vec[j] >= 21 && age_vec[j] <= 29){
      assignmentvec[j] <- 5
    } else if (age_vec[j] >= 30 && age_vec[j] <= 39){
      assignmentvec[j] <- 6
    } else if (age_vec[j] >= 40 && age_vec[j] <= 49){
      assignmentvec[j] <- 7
    } else if (age_vec[j] >= 50 && age_vec[j] <= 59){
      assignmentvec[j] <- 8
    } else {assignmentvec[j] <- 9}
    
  }
  
  #Now replace the orginal Age_B04 column with assignmentvec  
  dataframe[, paste0("Age_B04")] <- assignmentvec
  
  return(dataframe)
}

#-------------------------------------NSSEC-------------------------------------
#Create new NSSEC groups that match the NSSEC groups of the NTS dataset.

# SPENSER codes -> NTS code
# Higher managerial, administrative and professional occupations 1 -> Managerial and professional occupations 1
# Lower managerial, administrative and professional occupations 2 -> Managerial and professional occupations 1
# Intermediate occupations 3 -> Intermediate occupations and small employers 2
# Small employers and own account workers 4 -> Intermediate occupations and small employers 2
# Lower supervisory and technical occupations 5 -> Intermediate occupations and small employers 2
# Semi-routine occupations 6 -> Routine and manual occupations 3
# Routine occupations 7 -> Routine and manual occupations 3
# Never worked and long-term unemployed 8 -> Never worked and long-term unemployed 4
# L15 Full-time students 9 -> Not classified (including students) 5
# NSSEC unknown -1 -> NA/DNA -1

new_NSSEC <- function(dataframe){
  #Create an empty vector and a vector of the column NSSEC
  assignmentvec <- vector(mode = "numeric", length = nrow(dataframe))  
  NSSEC <-  dataframe[, paste0("NSSEC")]
  
  #This for loop numerically codes for NSSEC and allocates it a new numeric group
  # based on the NTS data. Any other code it is replaces with -1.
  for(k in 1:length(assignmentvec)){
    if (is.na(NSSEC[k]) == TRUE){
      assignmentvec[k] <- NA
    } else if (NSSEC[k] == 1){
      assignmentvec[k] <- 1
    } else if (NSSEC[k] == 2){
      assignmentvec[k] <- 1
    } else if (NSSEC[k] == 3){
      assignmentvec[k] <- 2
    } else if (NSSEC[k] == 4){
      assignmentvec[k] <- 2
    } else if (NSSEC[k] == 5){
      assignmentvec[k] <- 2
    } else if (NSSEC[k] == 6){
      assignmentvec[k] <- 3  
    } else if (NSSEC[k] == 7){
      assignmentvec[k] <- 3 
    } else if (NSSEC[k] == 8){
      assignmentvec[k] <- 4 
    } else if (NSSEC[k] == 9){
      assignmentvec[k] <- 5 
    } else {assignmentvec[k] <- -1}
    
  }
  
  #Now replace the orginal NSSEC column with assignmentvec  
  dataframe[, paste0("NSSEC")] <- assignmentvec
  
  return(dataframe)
}




































