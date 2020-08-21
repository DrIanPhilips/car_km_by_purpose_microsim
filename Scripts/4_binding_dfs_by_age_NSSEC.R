#-------------------------------------------------------------------------------
#---------------------------AGE AND NSSEC DATAFRAMES----------------------------
#-------------------------------------------------------------------------------

#Join all of the LAD dataframes together from "dfList" by NSSEC and age group.
#This will be useful for the Monte Carlo process because age and NSSEC group 
# affect how often an individual travels during a week.
#The output is a list "ageNSSEClist" which contains 30 massive dataframes for each
# combination of NSSEC and age. 
#(6 age groups and 5 NSSEC groups -> 5*6 = 30)

#This process takes 1 hour to extract the age and NSSEC groups.

#Create empty dataframes that will store the population who belong to each age group.
age4 <- data.frame()
age5 <- data.frame()
age6 <- data.frame()
age7 <- data.frame()
age8 <- data.frame()
age9 <- data.frame()

#Loop through and append each dataframe underneath each other based on the age 
# group the indiviudals belong to.

#for(i in 1:30){
for(i in 1:length(dfList)){
  age4 <- rbind(age4, dfList[[i]][dfList[[i]]$Age_B04 %in% 4, ])
  age5 <- rbind(age5, dfList[[i]][dfList[[i]]$Age_B04 %in% 5, ])
  age6 <- rbind(age6, dfList[[i]][dfList[[i]]$Age_B04 %in% 6, ])
  age7 <- rbind(age7, dfList[[i]][dfList[[i]]$Age_B04 %in% 7, ])
  age8 <- rbind(age8, dfList[[i]][dfList[[i]]$Age_B04 %in% 8, ])
  age9 <- rbind(age9, dfList[[i]][dfList[[i]]$Age_B04 %in% 9, ])
  
}

#Store the dataframes in a list.
agelist <- list()
agelist[[1]] <- age4
agelist[[2]] <- age5
agelist[[3]] <- age6
agelist[[4]] <- age7
agelist[[5]] <- age8
agelist[[6]] <- age9


#CReate an empty list and vector.
ageNSSEClist <- list()
namesvec <- c()

#Loop through to extract each age group and NSSEC combination.
for(i in 1:6){
  
  test <- agelist[[i]]
  
  for(j in 1:5){
    
    ageNSSEClist[[(j + ((i-1)*5))]] <- test[test$NSSEC %in% j,]
    
    namesvec <- c(namesvec,paste0("age",(i+3),"NSSEC",j))
    
    names(ageNSSEClist) <- namesvec
  }
  
}

remove(agelist, test, i, j, namesvec)



