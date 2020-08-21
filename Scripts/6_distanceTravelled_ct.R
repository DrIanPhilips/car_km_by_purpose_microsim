#-------------------------------------------------------------------------------
#-------------------------------DISTANCE TRAVELLED------------------------------
#-------------------------------------------------------------------------------
#This script details how to creates 6 distance travelled constraint tables for 
# each of the trip purpose types. 
#The different types of trip purpose and the trip distance groups are detailed 
# in "Functions/1_new_NTS_groups.R".
#The constraints are age, gender, National Statistics Socio-economic Classification, 
# adjusted Government Operational Region and Rural/Urban Classification.

#The output is a list tripPurposeCtList(), which contains the 6 constraint
# tables.

#The following codes for the trip purpose type are as followed:
# 1 -> commuting
# 2 -> business
# 3 -> education
# 4 -> shopping or personal trips
# 5 -> social interaction
# 6 -> leisure

#Empty list to hold the trip purpose constraint tables
tripPurposeCtList <- list()

#Empty vector to hold the names of the elements in the list.
namesvec <- c()


for (i in 1:6){
  #Select the trip purpose in the dataframe over_17_nts that equal i.
  tripPurpose_df <- car_only_nts[car_only_nts$TripPurpose == i,]
  
  ct <- constraint_table_5(tripPurpose_df, "Sex", "Age_B04", "NSSEC", "GOR_new", "ONSRuralUrban", "TripDistance_km")
  
  tripPurposeCtList[[i]] <- ct
  
  namesvec <- c(namesvec,paste0("trip_purpose_ct_",i))
  
  names(tripPurposeCtList) <- namesvec
  
}

remove(ct, tripPurpose_df, namesvec,i)

