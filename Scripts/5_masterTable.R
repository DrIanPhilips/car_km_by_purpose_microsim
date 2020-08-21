#-------------------------------------------------------------------------------
#---------------------------------MASTER TABLE----------------------------------
#-------------------------------------------------------------------------------
#This scripts joins every age/NSSEC dataframe with the trip purpose constraint.
#It is comprehensive in the sense that it joins 5 elements together; these are
# Sex, Age, NSSEC, GOR_new, ONS Rural Urban classification. These attributes 
# are known factors that affects one's trip purpose and trip distance.
#It stores each joint dataframe and constraint table in a list.

#Use the ageNSSEClist and store the names as a vector to rename the elements in
# the new List.
namesageNSSECvec <- names(ageNSSEClist)


#Create an empty list to store each master table and an empty vector to store the
# name of each dataframe.
masterTableList <- list()

namesvec <- c()


#For loop to join age/NSSEC data from ageNSSEClist with the constraint table.
for (i in 1:length(namesageNSSECvec)){
  masterTable <- left_join(ageNSSEClist[[namesageNSSECvec[i]]], trip_purpose_ct, 
                           by = c("Sex" = "Sex", 
                                  "Age_B04" = "Age_B04", 
                                  "NSSEC" = "NSSEC", 
                                  "GOR_new" = "GOR_new", 
                                  "ONSRuralUrban" = "ONSRuralUrban"))
  
  masterTableList[[i]] <- masterTable
  
  namesvec <- c(namesvec,paste0(namesageNSSECvec[i]))
  
  names(masterTableList) <- namesvec
}


#Remove unnecessary functions
remove(masterTable, 
       i, 
       namesvec,
       namesageNSSECvec)

